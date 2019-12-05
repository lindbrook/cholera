#' Extract points from GeoTiff (prototype).
#'
#' @param quadrant Numeric. 1:4.
#' @param add.roads Logical.

roadsPlot <- function(quadrant = 1, add.roads = TRUE) {
  center.x <- min(cholera::roads$x) +
    (max(cholera::roads$x) - min(cholera::roads$x)) / 2
  center.y <- min(cholera::roads$y) +
    (max(cholera::roads$y) - min(cholera::roads$y)) / 2

  if (quadrant == 1) {
    range.x <- c(min(cholera::roads$x), center.x)
    range.y <- c(min(cholera::roads$y), center.y)
  } else if (quadrant == 2) {
    range.x <- c(min(cholera::roads$x), center.x)
    range.y <- c(center.y, max(cholera::roads$y))
  } else if (quadrant == 3) {
    range.x <- c(center.x, max(cholera::roads$x))
    range.y <- c(center.y, max(cholera::roads$y))
  } else if (quadrant == 4) {
    range.x <- c(center.x, max(cholera::roads$x))
    range.y <- c(min(cholera::roads$y), center.y)
  } else stop("quadrant must be in 1:4")

  plot(cholera::roads[cholera::roads$name != "Map Frame", c("x", "y")],
    pch = 1, xlim = range.x, ylim = range.y, main = paste("Quadrant", quadrant))
  if (add.roads) addRoads()
}
