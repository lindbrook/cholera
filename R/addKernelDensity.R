#' Add 2D kernel density contours.
#'
#' Add 2D kernel density contours based on selected sets of observations.
#' @param pump.subset Character or Numeric: "pooled", "individual", or numeric vector. "pooled" treats all observations as a single set. "individual" is a shortcut for all individual pump neighborhoods. Use of vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} selects all pumps in \code{pump.select}.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param neighborhood.type Character. "voronoi" or "walking"
#' @param bandwidth Numeric. Bandwidth for kernel density estimation.
#' @param color Character. Color of contour lines.
#' @param line.type Character. Line type for contour lines.
#' @param cases Character. Unit of observation: "unstacked" uses \code{fatalities.unstacked}; "address" uses \code{fatalities.address}; "fatality" uses \code{fatalities}.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @return Add contours to a graphics plot.
#' @import graphics
#' @note This function uses \code{KernSmooth::bkde2D()}.
#' @export
#' @examples
#' \dontrun{
#'
#' snowMap()
#' addKernelDensity()
#'
#' snowMap()
#' addKernelDensity("individual")
#'
#' snowMap()
#' addKernelDensity(c(6, 8))
#'
#' snowMap()
#' addKernelDensity(pump.select = c(6, 8))
#' }

addKernelDensity <- function(pump.subset = "pooled", pump.select = NULL,
  neighborhood.type = "walking", cases = "unstacked", bandwidth = 0.5,
  color = "black", line.type = "solid", multi.core = FALSE) {

  if (!is.null(cases) & !all(cases %in%
      c("unstacked", "address", "fatality"))) {
    stop('cases must be "unstacked", "address" or "fatality".')
  }

  if (!all(neighborhood.type %in% c("voronoi", "walking"))) {
    stop('neighborhood.type must either be "voronoi" or "walking".')
  }

  if (is.character(pump.subset)) {
    if (pump.subset %in% c("individual", "pooled") == FALSE) {
      stop("If not numeric, pump.subset must either be 'individual' or 'pooled'.")
    }
  }

  cores <- multiCore(multi.core)
  vars <- c("x", "y")
  bw <- rep(bandwidth, 2)

  if (is.null(pump.select)) {
    if (all(is.character(pump.subset))) {
      if (pump.subset == "pooled") {
        if (cases == "unstacked") {
          dat <- cholera::fatalities.unstacked[, vars]
        } else if (cases == "address") {
          dat <- cholera::fatalities.address[, vars]
        } else if (cases == "fatality") {
          dat <- cholera::fatalities[, vars]
        }

        kde <- KernSmooth::bkde2D(dat, bandwidth = bw)

        graphics::contour(x = kde$x1, y = kde$x2, z = kde$fhat, col = color,
          lty = line.type, add = TRUE)

      } else if (pump.subset == "individual") {
        if (neighborhood.type == "walking") {
          n.data <- neighborhoodWalking(multi.core = cores)
          cases <- pumpCase(n.data)

        } else if (neighborhood.type == "voronoi") {
          n.data <- neighborhoodVoronoi()
          cases <- pumpCase(n.data)
          empty.cell <- vapply(cases, length, numeric(1L))
          cases <- cases[empty.cell != 0]
        }

        kde <- lapply(cases, function(id) {
          sel <- cholera::fatalities.address$anchor %in% id
          dat <- cholera::fatalities.address[sel, vars]
          KernSmooth::bkde2D(dat, bandwidth = bw)
        })

        invisible(lapply(names(kde), function(nm) {
          dat <- kde[[nm]]
          graphics::contour(x = dat$x1, y = dat$x2, z = dat$fhat,
            col = snowColors()[nm], lty = line.type, add = TRUE)
        }))
      }

    } else if (all(is.numeric(pump.subset))) {
      if (neighborhood.type == "walking") {
        n.data <- neighborhoodWalking(multi.core = cores)
        obs.neighborhood <- as.numeric(names(n.data$paths))

        if (any(abs(pump.subset) %in% obs.neighborhood == FALSE)) {
          stop('For walking neighborhoods, only 3 through 12 are valid.')
        }

        cases.list <- pumpCase(n.data)

        if (all(pump.subset > 0)) {
          cases <- cases.list[paste0("p", pump.subset)]
        } else if (all(pump.subset < 0)) {
          sel <- names(cases.list) %in% paste0("p", abs(pump.subset)) == FALSE
          cases <- cases.list[sel]
        } else {
          stop("Use all positive or all negative numbers for pump.subset.")
        }

      } else if (neighborhood.type == "voronoi") {
        n.data <- neighborhoodVoronoi()
        cases <- pumpCase(n.data)
        empty.cell <- vapply(cases, length, numeric(1L))
        cases <- cases[empty.cell != 0]
      }

      kde <- lapply(cases, function(id) {
        sel <- cholera::fatalities.address$anchor %in% id
        dat <- cholera::fatalities.address[sel, vars]
        KernSmooth::bkde2D(dat, bandwidth = bw)
      })

      invisible(lapply(names(kde), function(nm) {
        dat <- kde[[nm]]
        contour(x = dat$x1, y = dat$x2, z = dat$fhat, col = snowColors()[nm],
          lty = line.type, add = TRUE)
      }))
    }

  } else {
    n.data <- neighborhoodWalking(pump.select, multi.core = cores)
    cases <- pumpCase(n.data)

    kde <- lapply(cases, function(id) {
      sel <- cholera::fatalities.address$anchor %in% id
      dat <- cholera::fatalities.address[sel, vars]
      KernSmooth::bkde2D(dat, bandwidth = bw)
    })

    invisible(lapply(names(kde), function(nm) {
      dat <- kde[[nm]]
      graphics::contour(x = dat$x1, y = dat$x2, z = dat$fhat,
        col = snowColors()[nm], lty = line.type, add = TRUE)
    }))
  }
}
