#' Add plague pit (Marshall Street).
#'
#' Draws a polygon that approximates the plague pit located around Marshall Street. From Vestry Report map.
#' @param color Character. Color of polygon.
#' @param line.type Character. Polygon line type.
#' @return Adds a polygon to a graphics plot.
#' @note In progress.
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addPlaguePit()

addPlaguePit <- function(color = "black", line.type = "solid") {
  polygon(cholera::plague.pit, border = color, lty = line.type)
}
