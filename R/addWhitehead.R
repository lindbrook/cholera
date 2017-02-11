#' Add Whitehead's Broad Street pump neighborhood.
#'
#' Adds a circle with a given radius from a specified water pump. By default, the function draws Whitehead's Broad Street pump neighborhood: a circle, centered on the Broad Street pump, with a radius of 210 yards.
#' @param radius Numeric. Distance from a pump in yards
#' @param pump Character. The name of the pump, the street name where it is located. See \code{pumps} or \code{pumps.vestry}.
#' @param col Character. Color of circle.
#' @param lty Character. Circle line type.
#' @param vestry Logical. TRUE uses the 14 pumps and locations from Vestry report. FALSE uses original 13 pumps.
#' @param ... Additional plotting parameters.
#' @return Draws a circle, based on multiple line segments, to a graphics plot.
#' @seealso \code{\link[cholera]{addLandmarks}}
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addWhitehead()

addWhitehead <- function(pump = "Broad Street", radius = 210, col = "black",
  lty = "solid", vestry = FALSE, ...) {
  r <- radius / 59
  unit.base <- 100
  unit.radians <- 2 * pi / unit.base

  if (vestry) {
    if (is.character(pump)) {
      if (pump %in% cholera::pumps.vestry$street == FALSE) {
        text.a <- "Invalid Vestry pump name."
        text.b <- "Check spelling or see pumps.vestry$street."
        stop(paste(text.a, text.b))
      } else {
        circumference.x <- cholera::pumps.vestry[cholera::pumps.vestry$street ==
          pump, "x"] + r * cos(0:unit.base * unit.radians)
        circumference.y <- cholera::pumps.vestry[cholera::pumps.vestry$street ==
          pump, "y"] + r * sin(0:unit.base * unit.radians)
      }
    } else if (is.numeric(pump)) {
      if (pump %in% cholera::pumps.vestry$id == FALSE) {
        stop("Vestry pump ID must be a whole number between 1 and 14.")
      } else {
        circumference.x <- cholera::pumps.vestry[cholera::pumps.vestry$id ==
          pump, "x"] + r * cos(0:unit.base * unit.radians)
        circumference.y <- cholera::pumps.vestry[cholera::pumps.vestry$id ==
          pump, "y"] + r * sin(0:unit.base * unit.radians)
      }
    }
  } else {
    if (is.character(pump)) {
      if (pump %in% cholera::pumps$street == FALSE) {
        text.a <- "Invalid Snow pump name."
        text.b <- "Check spelling or see pumps$street."
        stop(paste(text.a, text.b))
      } else {
        circumference.x <- cholera::pumps[cholera::pumps$street == pump, "x"] +
          r * cos(0:unit.base * unit.radians)
        circumference.y <- cholera::pumps[cholera::pumps$street == pump, "y"] +
          r * sin(0:unit.base * unit.radians)
      }
    } else if (is.numeric(pump)) {
      if (pump %in% cholera::pumps.vestry$id == FALSE) {
        stop("Snow pump ID must be a whole number between 1 and 13.")
      } else {
        circumference.x <- cholera::pumps[cholera::pumps$id == pump, "x"] +
          r * cos(0:unit.base * unit.radians)
        circumference.y <- cholera::pumps[cholera::pumps$id == pump, "y"] +
          r * sin(0:unit.base * unit.radians)
      }
    }
  }
  lines(circumference.x, circumference.y, col = col, lty = lty)
}
