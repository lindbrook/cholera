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
