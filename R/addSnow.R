#' Adds Snow's graphical annotation of the Broad Street pump walking neighborhood.
#'
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

addSnow <- function(type = "area", color = "dodgerblue", alpha.level = 0.25,
  line.width = 2) {

  if (type %in% c("area", "perimeter") == FALSE) {
    stop('type must be "area" or "perimeter".')
  }

  snow <- snowNeighborhoodB(latlong = FALSE)
  snow.area <- row.names(cholera::regular.cases[snow$sim.case, ])
  periphery.cases <- peripheryCases(snow.area)
  pearl.string <- travelingSalesman(periphery.cases)

  if (type == "perimeter") {
    polygon(cholera::regular.cases[pearl.string, ], border = color,
      lwd = line.width)
  } else if (type == "area") {
    snow.col <- grDevices::adjustcolor(color, alpha.f = alpha.level)
    polygon(cholera::regular.cases[pearl.string, ], border = "black",
      col = snow.col)
  }
}
