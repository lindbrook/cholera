#' Add Rev. Henry Whitehead's Broad Street pump neighborhood.
#'
#' A circle (polygon), centered around a desired pump with a radius of 210 yards. The Broad Street pump is the default.
#' @param pump Character or Numeric. Name (road name) or numerical ID of selected pump. See \code{pumps} or \code{pumps.vestry}.
#' @param radius Numeric. Distance from a pump.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param latlong Logical. Longitude-Latitude coordinates.
#' @param color Character. Color of circle.
#' @param line.type Character. Circle line type.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps and locations from Vestry report. \code{FALSE} uses original 13 pumps.
#' @param add.title Logical. Add title.
#' @param add.subtitle Logical. Add subtitle with estimated "walking" time in seconds.
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @return Adds a circle (polygon) to a graphics plot.
#' @import graphics
#' @export
#' @examples
#' snowMap()
#' addWhitehead()

addWhitehead <- function(pump = "Broad Street", radius = 210,
  distance.unit = "yard", latlong = FALSE, color = "black", line.type = "solid",
  vestry = FALSE, add.title = FALSE, add.subtitle = FALSE, walking.speed = 5) {

  r <- radius / unitMeter(1, distance.unit)
  r.meters <- radius / cholera::meter.to.yard

  if (latlong) {
    if (vestry) p.data <- cholera::pumps.vestry
    else p.data <- cholera::pumps

    if (is.character(pump)) {
      if (!pump %in% p.data$street) {
        text.a <- "Invalid pump name."
        if (vestry){
          text.b <- "Check spelling or see cholera::pumps.vestry$street."
        } else {
          text.b <- "Check spelling or see cholera::pumps$street."
        }
        stop(paste(text.a, text.b))
      } else {
        p.sel <- p.data[p.data$street == pump,  c("lon", "lat")]
      }
    } else if (is.numeric(pump)) {
      if (pump %in% p.data$id == FALSE) {
        stop("Pump ID must be a whole number between 1 and ", max(p.data$id),
          ".", call. = FALSE)
      } else {
        p.sel <- p.data[p.data$id == pump,  c("lon", "lat")]
      }
    }

    circle <- do.call(rbind, lapply(1:360, function(deg) {
      geosphere::destPoint(p.sel, deg, r.meters)
    }))

    lines(circle, col = color, lty = line.type)

  } else {
    unit.base <- 100
    unit.radians <- 2 * pi / unit.base

    if (vestry) {
      if (is.character(pump)) {
        if (pump %in% cholera::pumps.vestry$street == FALSE) {
          text.a <- "Invalid Vestry pump name."
          text.b <- "Check spelling or see cholera::pumps.vestry$street."
          stop(paste(text.a, text.b))
        } else {
          sel <- cholera::pumps.vestry$street == pump
          circumference.x <- cholera::pumps.vestry[sel, "x"] +
            r * cos(0:unit.base * unit.radians)
          circumference.y <- cholera::pumps.vestry[sel, "y"] +
            r * sin(0:unit.base * unit.radians)
        }
      } else if (is.numeric(pump)) {
        if (pump %in% cholera::pumps.vestry$id == FALSE) {
          stop("Vestry pump ID must be a whole number between 1 and 14.")
        } else {
          sel <- cholera::pumps.vestry$id == pump
          circumference.x <- cholera::pumps.vestry[sel, "x"] +
            r * cos(0:unit.base * unit.radians)
          circumference.y <- cholera::pumps.vestry[sel, "y"] +
            r * sin(0:unit.base * unit.radians)
        }
      }
    } else {
      if (is.character(pump)) {
        if (pump %in% cholera::pumps$street == FALSE) {
          text.a <- "Invalid Snow pump name."
          text.b <- "Check spelling or see cholera::pumps$street."
          stop(paste(text.a, text.b))
        } else {
          sel <- cholera::pumps$street == pump
          circumference.x <- cholera::pumps[sel, "x"] +
            r * cos(0:unit.base * unit.radians)
          circumference.y <- cholera::pumps[sel, "y"] +
            r * sin(0:unit.base * unit.radians)
        }
      } else if (is.numeric(pump)) {
        if (pump %in% cholera::pumps.vestry$id == FALSE) {
          stop("Snow pump ID must be a whole number between 1 and 13.")
        } else {
          sel <- cholera::pumps$id == pump
          circumference.x <- cholera::pumps[sel, "x"] +
            r * cos(0:unit.base * unit.radians)
          circumference.y <- cholera::pumps[sel, "y"] +
            r * sin(0:unit.base * unit.radians)
        }
      }
    }

    lines(circumference.x, circumference.y, col = color, lty = line.type)
  }

  if (add.title) title("Whitehead's Broad Street Pump Neighborhood")

  if (add.subtitle) {
    circumference.time <- distanceTime(2 * pi * r,
      walking.speed = walking.speed, time.unit = "minute")
    radius.time <- distanceTime(r, walking.speed = walking.speed,
      time.unit = "minute")
    c.time <- round(circumference.time, 1)
    r.time <- round(radius.time, 1)
    title(sub = paste(paste0("circumference = ", c.time, " mins;"),
                      paste0("radius = ", r.time, " mins.")))
  }
}
