#' Tanaka contour plot.
#'
#' Soho elevation data.
#' @importFrom elevatr get_elev_raster
#' @importFrom tanaka tanaka
#' @importFrom terra rast
#' @noRd

tanakaContourPlot <- function() {
  vars <- c("lon", "lat")
  map.frame <- cholera::roads[cholera::roads$name == "Map Frame", vars]
  map.range <- data.frame(x = range(map.frame$lon), y = range(map.frame$lat))
  elev.soho <- elevatr::get_elev_raster(locations = map.range, z = 10,
    prj = "EPSG:4326", clip = "locations")
  elev.raster <- terra::rast(elev.soho)
  tanaka::tanaka(elev.raster)
}

