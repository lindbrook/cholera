#' Tanaka contour plot.
#'
#' Soho elevation data.
#' @param add Logical. Add to exisiting plot.
#' @param alpha.level Numeric.
#' @importFrom elevatr get_elev_raster
#' @importFrom tanaka tanaka
#' @importFrom terra rast
#' @export

tanakaContourPlot <- function(add = FALSE, alpha.level = 1) {
  vars <- c("lon", "lat")
  map.frame <- cholera::roads[cholera::roads$name == "Map Frame", vars]
  map.range <- data.frame(x = range(map.frame$lon), y = range(map.frame$lat))
  elev.soho <- elevatr::get_elev_raster(locations = map.range, z = 10,
    prj = "EPSG:4326", clip = "locations")
  elev.raster <- terra::rast(elev.soho)
  # tanaka::tanaka(elev.raster)

  if (alpha.level < 1) {
    tanaka.palette <- c("#FBDEE1", "#F7D2D6", "#F3C7CB", "#F0BCC0", "#ECB1B5",
                        "#E9A6AB", "#E59BA0", "#E29095", "#DE858A", "#D9767B", 
                        "#D5676D", "#D0585E", "#CC4950", "#C73A41", "#BB2D34",
                        "#9B262B", "#7C1E23", "#5C171A", "#3C0F11", "#1D0809")
  
    alpha.col <- grDevices::adjustcolor(tanaka.palette, alpha.f = alpha.level)
    tanaka::tanaka(elev.raster, add = add, col = alpha.col)
  } else {
    tanaka::tanaka(elev.raster, add = add)
  }
}


    