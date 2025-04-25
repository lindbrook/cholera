#' Add plague pit (Marshall Street).
#'
#' Draws a polygon that approximates the plague pit located around Marshall Street. From Vestry Report map.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param color Character. Color of polygon.
#' @param line.type Character. Polygon line type.
#' @return Adds a polygon to a graphics plot.
#' @note In progress.
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addPlaguePit()

addPlaguePit <- function(latlong = FALSE, color = "black",
  line.type = "solid") {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")
  polygon(cholera::plague.pit[, vars], border = color, lty = line.type)
}
