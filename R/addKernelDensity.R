#' Add 2D kernel density contours.
#'
#' Uses KernSmooth::bkde2D().
#' @param neighborhood Character or Numeric: "pooled", "all", or numeric vector of observed pump neighborhoods (3 through 12 or [3, 12]).
#' @param pump.select Numeric. A vector of pumps to consider.
#' @param walking.neighborhood Logical. FALSE uses voronoi neighborhoods.
#' @param bandwidth Numeric. Bandwidth for kernel density estimation.
#' @param color Character. Color of contour lines.
#' @param line.type Character. Line type for contour lines.
#' @param data Character. NULL uses \code{fatalities.unstacked}. "address" uses \code{fatalities.address}. "fatality" uses \code{fatalities}.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @param ... Additional plotting parameters.
#' @return Add contours to a graphics plot.
#' @seealso \code{\link{snowMap}},
#' \code{\link{addIndexCase}},
#' \code{\link{addLandmarks}},
#' \code{\link{addPlaguePit}},
#' \code{\link{addSnow}},
#' \code{\link{addVoronoi}},
#' \code{\link{addWhitehead}}
#' @import graphics
#' @export
#' @examples
#' # snowMap()
#' # addKernelDensity()
#'
#' # snowMap()
#' # addKernelDensity("all")
#'
#' # snowMap()
#' # addKernelDensity(c(6, 8))
#'
#' # snowMap()
#' # addKernelDensity(pump.select = c(6, 8))

addKernelDensity <- function(neighborhood = "pooled", pump.select = NULL,
  walking.neighborhood = TRUE, data = NULL, bandwidth = 0.5, color = "black",
  line.type = "solid", multi.core = FALSE, ...) {

  if (is.logical(multi.core)) {
    if (multi.core == TRUE) {
      cores <- parallel::detectCores()
    } else {
      if (is.numeric(multi.core)) {
        if (is.integer(multi.core)) {
          cores <- multi.core
        } else {
          cores <- as.integer(multi.core)
        }
      } else {
        cores <- 1L
      }
    }
  } else if (is.numeric(multi.core)) {
    if (is.integer(multi.core)) {
      cores <- multi.core
    } else {
      cores <- as.integer(multi.core)
    }
  }

  if (!is.null(data) & !all(data %in% c("address", "fatality"))) {
    stop('If specified, "data" must either be "address" or "fatality".')
  }

  bw <- rep(bandwidth, 2)

  if (is.null(pump.select)) {
    if (all(is.character(neighborhood))) {
      if (neighborhood == "pooled") {
        if (is.null(data)) {
          kde <- KernSmooth::bkde2D(cholera::fatalities.unstacked[,
            c("x", "y")], bandwidth = bw)
        } else if (data == "address") {
          kde <- KernSmooth::bkde2D(cholera::fatalities.address[, c("x", "y")],
            bandwidth = bw)
        } else if (data == "fatality") {
          kde <- KernSmooth::bkde2D(cholera::fatalities[, c("x", "y")],
            bandwidth = bw)
        }

        contour(x = kde$x1, y = kde$x2, z = kde$fhat, col = color,
          lty = line.type, add = TRUE)

      } else if (neighborhood == "all") {
        if (walking.neighborhood) {
          n.data <- cholera::neighborhoodWalking(multi.core = cores)
          cases <- cholera::pumpCase(n.data)
        } else {
          n.data <- cholera::neighborhoodVoronoi()
          cases <- cholera::pumpCase(n.data)
          empty.cell <- vapply(cases, length, numeric(1L))
          cases <- cases[empty.cell != 0]
        }

        kde <- lapply(cases, function(id) {
          sel <- cholera::fatalities.address$anchor.case %in% id
          dat <- cholera::fatalities.address[sel, c("x", "y")]
          KernSmooth::bkde2D(dat, bandwidth = bw)
        })

        invisible(lapply(names(kde), function(nm) {
          dat <- kde[[nm]]
          contour(x = dat$x1, y = dat$x2, z = dat$fhat, col = snowColors()[nm],
            lty = line.type, add = TRUE)
        }))
      }

    } else if (all(is.numeric(neighborhood))) {
      if (walking.neighborhood) {
        n.data <- cholera::neighborhoodWalking(multi.core = cores)
        obs.neighborhood <- as.numeric(names(n.data$paths))
        if (any(neighborhood %in% obs.neighborhood == FALSE)) {
          stop('For walking neighborhoods, only 3 through 12 are valid.')
        }
        cases <- cholera::pumpCase(n.data)[paste0("p", neighborhood)]
      } else {
        n.data <- cholera::neighborhoodVoronoi()
        cases <- cholera::pumpCase(n.data)
        empty.cell <- vapply(cases, length, numeric(1L))
        cases <- cases[empty.cell != 0]
      }

      kde <- lapply(cases, function(id) {
        sel <- cholera::fatalities.address$anchor.case %in% id
        dat <- cholera::fatalities.address[sel, c("x", "y")]
        KernSmooth::bkde2D(dat, bandwidth = bw)
      })

      invisible(lapply(names(kde), function(nm) {
        dat <- kde[[nm]]
        contour(x = dat$x1, y = dat$x2, z = dat$fhat, col = snowColors()[nm],
          lty = line.type, add = TRUE)
      }))
    }

  } else {
    n.data <- cholera::neighborhoodWalking(pump.select, multi.core = cores)
    cases <- cholera::pumpCase(n.data)
    kde <- lapply(cases, function(id) {
      sel <- cholera::fatalities.address$anchor.case %in% id
      dat <- cholera::fatalities.address[sel, c("x", "y")]
      KernSmooth::bkde2D(dat, bandwidth = bw)
    })

    invisible(lapply(names(kde), function(nm) {
      dat <- kde[[nm]]
      contour(x = dat$x1, y = dat$x2, z = dat$fhat, col = snowColors()[nm],
        lty = line.type, add = TRUE)
    }))
  }
}
