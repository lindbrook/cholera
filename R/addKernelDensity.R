#' Add 2D kernel density contours.
#'
#' Uses KernSmooth::bkde2D().
#' @param bandwidth Numeric. Bandwidth for kernel density estimation.
#' @param color Character. Color of contour lines.
#' @param line.type Character. Line type for contour lines.
#' @param data Character. NULL uses \code{fatalities.unstacked}. "address" uses \code{fatalities.address}. "fatality" uses \code{fatalities}.
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
#' snowMap(add.landmarks = FALSE)
#' addKernelDensity()

addKernelDensity <- function(bandwidth = 0.5, color = "black",
  line.type = "solid", data = NULL, ...) {

  if (!is.null(data) & !all(data %in% c("address", "fatality"))) {
    stop('If specified, "data" must either be "address" or "fatality".')
  }

  bw.value <- bandwidth
  bw <- rep(bw.value, 2)

  if (is.null(data)) {
    kde2d <- KernSmooth::bkde2D(cholera::fatalities.unstacked[, c("x", "y")],
      bandwidth = bw)
  } else if (data == "address") {
    kde2d <- KernSmooth::bkde2D(cholera::fatalities.address[, c("x", "y")],
      bandwidth = bw)
  } else if (data == "fatality") {
    kde2d <- KernSmooth::bkde2D(cholera::fatalities[, c("x", "y")],
      bandwidth = bw)
  }

  contour(x = kde2d$x1, y = kde2d$x2, z = kde2d$fhat, col = color,
    lty = line.type, add = TRUE)
}
