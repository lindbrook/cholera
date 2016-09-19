#' Add 2D kernel density contours.
#'
#' Uses KernSmooth::bkde2D().
#' @param bandwidth Numeric. bandwidth for kernel density esitmation.
#' @param color Character. Color of cotour lines.
#' @param line.type Character. Line type for contour.
#' @param data Character. NULL, uses \code{fatalities.unstacked}. "address" uses \code{fatalities.address}. "stacked" uses \code{fatalities}.
#' @return Add contours to a graphics plot.
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addKernelDensity()

addKernelDensity <- function(bandwidth = 0.5, color = "black",
  line.type = "solid", data = NULL) {

  if (is.null(data) == FALSE) {
    if (all(data %in% c("address", "stacked") == FALSE)) {
      stop('If specified, "output" must either be "address" or "stacked".')
    }
  }

  bw.value <- bandwidth
  bw <- rep(bw.value, 2)

  if (is.null(data)) {
    kde2d <- KernSmooth::bkde2D(cholera::fatalities.unstacked[, c("x", "y")],
      bandwidth = bw)
  } else if (data == "address") {
    kde2d <- KernSmooth::bkde2D(cholera::fatalities.address[, c("x", "y")],
      bandwidth = bw)
  } else if (data == "stacked") {
    kde2d <- KernSmooth::bkde2D(cholera::fatalities[, c("x", "y")],
      bandwidth = bw)
  }

  contour(x = kde2d$x1, y = kde2d$x2, z = kde2d$fhat, col = color,
    lty = line.type, add = TRUE)
}
