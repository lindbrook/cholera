#' Extract points from GeoTiff (prototype).
#'
#' @param x Object. GeoTIFF.
#' @export

pointsFromGeoTIFF <- function(x) {
  ras <- raster::raster(x)
  pts <- raster::rasterToPoints(ras)
  data.frame(pts)
}
