#' Adds Snow's graphical annotation of the Broad Street pump walking neighborhood.
#'
#' @param latlong Logical.
#' @param type Character. Type of annotation plot: "area" or "perimeter".
#' @param color Character. Neighborhood color.
#' @param alpha.level Numeric. Alpha level transparency: a value in [0, 1] when type = "area".
#' @param line.width Numeric. Line width for \code{type = "street"} and \code{type = "perimeter"}.
#' @import graphics
#' @export
#' @examples
#' \dontrun{
#' plot(neighborhoodVoronoi())
#' addSnow()
#' }

addSnow <- function(latlong = FALSE, type = "area", color = "dodgerblue",
  alpha.level = 0.25, line.width = 2) {

  if (type %in% c("area", "perimeter") == FALSE) {
    stop('type must be "area" or "perimeter".')
  }

  snow <- snowNeighborhoodB(latlong = latlong)

  if (latlong) {
    reg.data <- cholera::latlong.regular.cases
    vars <- c("lon", "lat")
  } else {
    reg.data <- cholera::regular.cases
    vars <- c("x", "y")
  }

  snow.area <- row.names(reg.data[snow$sim.case, ])
  periphery.cases <- peripheryCases(snow.area, latlong = latlong)
  pearl.string <- travelingSalesman(periphery.cases, latlong = latlong)

  if (type == "perimeter") {
    polygon(reg.data[pearl.string, vars], border = color, lwd = line.width)
  } else if (type == "area") {
    snow.col <- grDevices::adjustcolor(color, alpha.f = alpha.level)
    polygon(reg.data[pearl.string, vars], border = "black", col = snow.col)
  }
}
