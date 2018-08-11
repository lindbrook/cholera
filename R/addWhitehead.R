#' Add Rev. Henry Whitehead's Broad Street pump neighborhood.
#'
#' A circle (polygon), centered around a desired pump with a radius of 210 yards. The Broad Street pump is the default.
#' @param pump Character or Numeric. Name (road name) or numerical ID of selected pump. See \code{pumps} or \code{pumps.vestry}.
#' @param radius Numeric. Distance from a pump in yards.
#' @param color Character. Color of circle.
#' @param line.type Character. Circle line type.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps and locations from Vestry report. \code{FALSE} uses original 13 pumps.
#' @param subtitle Logical. Add subtitle with estimated "walking" time in seconds.
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param ... Additional plotting parameters.
#' @seealso \code{\link{snowMap}},
#' \code{\link{addIndexCase}},
#' \code{\link{addKernelDensity}},
#' \code{\link{addLandmarks}},
#' \code{\link{addPlaguePit}},
#' \code{\link{addSnow}},
#' \code{\link{addVoronoi}}
#' @return Adds a circle (polygon) to a graphics plot.
#' @seealso \code{\link{addLandmarks}}
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addWhitehead()

addWhitehead <- function(pump = "Broad Street", radius = 210, color = "black",
  line.type = "solid", vestry = FALSE, subtitle = FALSE, walking.speed = 5,
  ...) {

  r <- radius / cholera::unitMeter(1, "yard")
  unit.base <- 100
  unit.radians <- 2 * pi / unit.base

  if (vestry) {
    if (is.character(pump)) {
      if (pump %in% cholera::pumps.vestry$street == FALSE) {
        text.a <- "Invalid Vestry pump name."
        text.b <- "Check spelling or see cholera::pumps.vestry$street."
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
        text.b <- "Check spelling or see cholera::pumps$street."
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
  lines(circumference.x, circumference.y, col = color, lty = line.type)

  if (subtitle) {
    est.time <- distanceTime(unitMeter(r, "native"), speed = walking.speed)
    title(sub = paste(round(est.time, 1), "secs."))
  }
}
