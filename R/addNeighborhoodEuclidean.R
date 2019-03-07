#' Add expected Euclidean pump neighborhoods.
#'
#' @param pump.subset Numeric. Vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} selects all pumps in \code{pump.select}.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param case.location Character. "address" or "nominal". "address" is the x-y coordinates of \code{sim.ortho.proj}. "nominal" is the x-y coordinates of \code{regular.cases}.
#' @param type Character. Type of plot: "star", "area.points" or "area.polygons".
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param polygon.method Character. Method of computing polygon vertices: "pearl.string" or "traveling.salesman".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @return R graphic elements.
#' @note This uses an approximate computation of polygons, using the 'TSP' package, that may produce non-simple and/or overlapping polygons.
#' @export
#' @examples
#' \dontrun{
#'
#' streetNameLocator("marshall street", zoom = 0.5, highlight = FALSE,
#'   add.subtitle = FALSE)
#' addNeighborhoodEuclidean()
#'
#' streetNameLocator("marshall street", zoom = 0.5, highlight = FALSE,
#'   add.subtitle = FALSE)
#' addNeighborhoodEuclidean(type = "area.points")
#' }

addNeighborhoodEuclidean <- function(pump.subset = NULL, pump.select = NULL,
  vestry = FALSE, case.location = "nominal", type = "star", alpha.level = 0.5,
  polygon.method = "traveling.salesman", multi.core = FALSE) {

  if (case.location %in% c("address", "nominal") == FALSE) {
    stop('case.location must be "address" or "nominal".')
  }

  if (type == "area.polygons") {
    if (polygon.method == "pearl.string") {
      verticesFn <- pearlString
    } else if (polygon.method == "traveling.salesman") {
      verticesFn <- travelingSalesman
    } else {
      stop('polygon.method must be "pearl.string" or "traveling.salesman".')
    }
  }

  cores <- multiCore(multi.core)

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  p.count <- nrow(pump.data)
  p.ID <- seq_len(p.count)
  snow.colors <- snowColors(vestry = vestry)

  if (is.null(pump.select)) {
    pump.id <- pump.data$id
  } else {
    if (is.numeric(pump.select) == FALSE) stop("pump.select must be numeric.")
    if (any(abs(pump.select) %in% p.ID) == FALSE) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ', p.count, ".")
    }

    if (all(pump.select > 0)) {
      pump.id <- pump.data$id[pump.select]
      snow.colors <- snow.colors[pump.select]
    } else if (all(pump.select < 0)) {
      sel <- pump.data$id %in% abs(pump.select) == FALSE
      pump.id <- pump.data$id[pump.select]
      snow.colors <- snow.colors[sel]
    } else {
      stop("Use all positive or all negative numbers for pump.select.")
    }
  }

  anchors <- seq_len(nrow(cholera::regular.cases))

  nearest.pump <- parallel::mclapply(anchors, function(x) {
    euclideanPath(x, destination = pump.id, vestry = vestry,
      observed = FALSE, case.location = case.location)$data$pump
  }, mc.cores = cores)

  if (is.null(pump.subset)) {
    x <- list(pump.data = pump.data,
                pump.select = pump.select,
                vestry = vestry,
                pump.id = pump.id,
                snow.colors = snow.colors,
                anchors = anchors,
                nearest.pump = unlist(nearest.pump),
                cores = cores)
  } else {
    if (all(pump.subset > 0)) {
      anchors.subset <- anchors[unlist(nearest.pump) %in% pump.subset]
      nearest.pump.subset <- nearest.pump[unlist(nearest.pump) %in% pump.subset]
    } else if (all(pump.subset < 0)) {
      anchors.subset <- anchors[unlist(nearest.pump) %in%
        abs(pump.subset) == FALSE]
      nearest.pump.subset <- nearest.pump[unlist(nearest.pump) %in%
        abs(pump.subset) == FALSE]
    } else {
      stop('Use all positive or all negative numbers for "pump.subset".')
    }

    x <- list(pump.data = pump.data,
              pump.subset = pump.subset,
              pump.select = pump.select,
              vestry = vestry,
              pump.id = pump.id,
              snow.colors = snow.colors,
              anchors = anchors.subset,
              nearest.pump = unlist(nearest.pump.subset),
              cores = cores)
  }

  class(x) <- "euclidean"

  anchors <- x$anchors
  nearest.pump <- x$nearest.pump

  if (type == "star") {
    invisible(lapply(seq_along(anchors), function(i) {
      p.data <- pump.data[pump.data$id == nearest.pump[[i]], ]
      n.color <- x$snow.colors[paste0("p", nearest.pump[[i]])]
      n.data <- cholera::regular.cases[anchors[i], ]

      lapply(seq_len(nrow(n.data)), function(case) {
        c.data <- n.data[case, ]
        segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color,
          lwd = 0.5)
      })
    }))

  } else if (type == "area.points") {
    invisible(lapply(seq_along(anchors), function(i) {
      n.color <- x$snow.colors[paste0("p", nearest.pump[[i]])]
      n.data <- cholera::regular.cases[anchors[i], ]

      lapply(seq_len(nrow(n.data)), function(case) {
        c.data <- n.data[case, ]
        points(c.data$x, c.data$y, pch = 15, cex = 1.25,
          col = grDevices::adjustcolor(n.color, alpha.f = alpha.level))
      })
    }))

  } else if (type == "area.polygons") {
    p.num <- sort(unique(unlist(nearest.pump)))

    neighborhood.cases <- lapply(p.num, function(n) {
      which(nearest.pump == n)
    })

    periphery.cases <- parallel::mclapply(neighborhood.cases, peripheryCases,
      mc.cores = x$cores)
    pearl.string <- parallel::mclapply(periphery.cases, verticesFn,
      mc.cores = x$cores)
    names(pearl.string) <- p.num

    invisible(lapply(names(pearl.string), function(nm) {
      sel <- paste0("p", nm)
      polygon(cholera::regular.cases[pearl.string[[nm]], ],
        col = grDevices::adjustcolor(x$snow.colors[sel], alpha.f = alpha.level))
    }))
  }
}
