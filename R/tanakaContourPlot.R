#' Tanaka contour plot.
#'
#' Soho elevation data.
#' @param add Logical. Add to existing plot.
#' @importFrom curl has_internet
#' @importFrom elevatr get_elev_raster
#' @importFrom tanaka tanaka
#' @importFrom terra rast
#' @export

tanakaContourPlot <- function(add = FALSE) {
  if (!curl::has_internet()) {
    stop("This function requires an active internet connection.", call. = FALSE)
  }
  vars <- c("lon", "lat")
  map.frame <- cholera::roads[cholera::roads$name == "Map Frame", vars]
  map.range <- data.frame(x = range(map.frame$lon), y = range(map.frame$lat))
  elev.soho <- elevatr::get_elev_raster(locations = map.range, z = 10,
    prj = "EPSG:4326", clip = "locations", verbose = TRUE)
  elev.raster <- terra::rast(elev.soho)
  tanaka::tanaka(elev.raster, add = add)
}
