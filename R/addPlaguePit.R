#' Add plague pit (Marshall Street).
#'
#' Draws a polygon that approximates the plague pit located around Marshall Street. From Vestry Report map.
#' @param color Character. Color of circle.
#' @param line.type Character. Type of line for circle.
#' @param ... Additional plotting parameters.
#' @return Adds a polygon, based on multiple base R line segments, to a graphics plot.
#' @seealso \code{\link[cholera]{snowMap}},
#' \code{\link[cholera]{addKernelDensity}},
#' \code{\link[cholera]{addLandmarks}},
#' \code{\link[cholera]{addSnow}},
#' \code{\link[cholera]{addVoronoi}}.
#' \code{\link[cholera]{addWhitehead}}
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addPlaguePit()
addPlaguePit <- function(color = "black", line.type = "solid", ...) {
  polygon(cholera::plague.pit, border = color, lty = line.type)
}
