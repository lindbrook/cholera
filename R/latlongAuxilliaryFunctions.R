#' Compute rectangle vertices.
#'
#' @param x Object. Points/pixel count.
#' @export

kmeansRectanlge <- function(x) {
  if (length(unique(x)) > 1) {
    km <- stats::kmeans(x, 2)
    km.df <- data.frame(ct = x, cluster = km$cluster)
    sel <- km.df[km.df$cluster == which.max(km$centers), ]
    as.numeric(row.names(sel))
  } else seq_along(x)
}

#' Extract points from GeoTiff.
#'
#' @param x Object. GeoTIFF.
#' @export

pointsFromGeoTIFF <- function(x) {
  ras <- raster::raster(x)
  pts <- raster::rasterToPoints(ras)
  data.frame(pts)
}

#' Index between native and latlong coordinates.
#'
#' @param max.ct Integer. Upper count of observations.
#' @export

pointIndex <- function(max.ct = 321) {
  alpha <- seq(1, max.ct, 50)
  omega <- c(alpha[-1] - 1, max.ct)
  data.frame(start = alpha, stop = omega)
}

#' Estimate of georeferencing rotation (radians).
#'
#' QGIS georeferencing realigns map: left side approximately parallel to y-axis.
#' @param id1 Numeric. Road segment endpoint ID. Margaret Street.
#' @param id2 Numeric. Road segment endpoint ID. Phoenix Yard.
#' @note The two default points are the first two observations on the top left.
#' @export

referenceRadian <- function(id1 = 66, id2 = 171) {
  rd <- cholera::roads
  # rd[order(rd$x, rd$y), ] # first two observations on top left side
  x1 <- rd[rd$id == id1, "x"]
  y1 <- rd[rd$id == id1, "y"]
  x2 <- rd[rd$id == id2, "x"]
  y2 <- rd[rd$id == id2, "y"]
  atan((x1 - x2) / (y2 - y1))
}
