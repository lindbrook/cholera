#' Compute vertices of Voronoi tiles.
#'
#' For use with polygon functions.
#' @param sites Object. Data frame of sites to construct Voronoi diagram with variables "x" and "y".
#' @param rw.data Object. Data frame of secondary source of data for rectangular window or bounding box: observations, cases, addresses, etc. with variables "x" and "y".
#' @param rw Numeric. Vector of corners of the rectangular window or bounding box: xmin, xmax, ymin, ymax. For deldir::deldir().
#' @return A list of data frames.
#' @export
#' @examples
#' polygon.vertices <- voronoiPolygons(cholera::pumps)
#' cholera::snowMap()
#' invisible(lapply(polygon.vertices, polygon))
#'
#' polygon.vertices <- voronoiPolygons(cholera::pumps, cholera::roads)
#' cholera::snowMap()
#' invisible(lapply(polygon.vertices, polygon))

voronoiPolygons <- function(sites, rw.data = NULL, rw = NULL) {
  if (is.null(rw.data) & is.null(rw)) {
    x.rng <- range(sites$x)
    y.rng <- range(sites$y)
  } else if (is.null(rw.data) == FALSE & is.null(rw)) {
    x.rng <- range(rw.data$x)
    y.rng <- range(rw.data$y)
  } else if (is.null(rw.data) & is.null(rw) == FALSE) {
    x.rng <- rw[1:2]
    y.rng <- rw[3:4]
  } else stop("Use either 'rw.data' or 'rw'; not both.")

  voronoi <- deldir::deldir(sites[, c("x", "y")], rw = c(x.rng, y.rng),
    suppressMsge = TRUE)
  cell.data <- deldir::tile.list(voronoi)
  lapply(cell.data, function(dat) data.frame(x = dat$x, y = dat$y))
}
