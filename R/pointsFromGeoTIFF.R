#' Extract points from GeoTiff (prototype).
#'
#' @param x Object. GeoTIFF.

pointsFromGeoTIFF <- function(x) {
  ras <- raster::raster(x)
  pts <- raster::rasterToPoints(ras)
  data.frame(pts)
}
