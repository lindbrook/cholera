#' Add Whitehead's Broad Street pump neighborhood.
#'
#' Adds a circle with a given radius from a specified water pump. By default, the function draws Whitehead's Broad Street pump neighborhood: a circle, centered on the Broad Street pump, with a radius of 210 yards.
#' @param radius Numeric. Distance from a pump in yards
#' @param pump Character. The name of the pump, the street name where it is located. See \code{pumps} or \code{pumps.vestry}.
#' @param col Character. Color of circle.
#' @param lty Character. Circle line type.
#' @return Draws a circle, based on multiple line segments, to a graphics plot.
#' @seealso \code{\link[cholera]{addLandmarks}}
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addWhitehead()
addWhitehead <- function(radius = 210, pump = "Broad Street", col = "black",
  lty = "solid") {

  r <- radius / 59
  unit.base <- 100
  unit.radians <- 2 * pi / unit.base
  circumference.x <- cholera::pumps[cholera::pumps$street == pump, "x"] +
    r * cos(0:unit.base * unit.radians)
  circumference.y <- cholera::pumps[cholera::pumps$street == pump, "y"] +
    r * sin(0:unit.base * unit.radians)
  lines(circumference.x, circumference.y, col = col, lty = lty)
}
