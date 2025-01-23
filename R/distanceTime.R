#' Convert distance to elapsed time.
#'
#' @param x Numeric. Nominal map distance.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param time.unit Character. Unit of measurement: "hour", "minute" or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @return An R vector.
#' @noRd

distanceTime <- function(x, distance.unit = "meter", time.unit = "second",
  walking.speed = 5) {

  if (is.numeric(x) == FALSE) stop('"x" must be numeric.')

  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
    stop('distance.unit must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('unit must be "hour", "minute" or "second".')
  }

  if (distance.unit == "native") {
    speed <- walking.speed / unitMeter(1)
  } else if (distance.unit == "yard") {
    speed <- walking.speed * cholera::meter.to.yard
    x <- unitMeter(x, distance.unit = distance.unit)
  } else if (distance.unit == "meter") {
    x <- unitMeter(x, distance.unit = distance.unit)
    speed <- walking.speed
  }

  if (time.unit == "hour") {
    x / (1000 * speed)
  } else if (time.unit == "minute") {
    (60 * x) / (1000 * speed)
  } else if (time.unit == "second") {
    (3600 * x) / (1000 * speed)
  }
}

#' Compute unit of distance (for labels and text).
#'
#' @param unit Character. Distance unit.
#' @return An R character vector.
#' @noRd

distanceUnit <- function(unit) {
  if (unit == "native") {
    d.unit <- "units;"
  } else if (unit == "meter") {
    d.unit <- "m;"
  } else if (unit == "yard") {
    d.unit <- "yd;"
  }
  d.unit
}

#' Compute approximate travel time (for labels and text).
#'
#' @param time Numeric. Travel time.
#' @param unit Character Time unit.
#' @return An R character vector.
#' @noRd

nominalTime <- function(time, unit) {
  if (unit == "hour") {
    nominal.time <- paste(round(time, 1), "hr")
  } else if (unit == "minute") {
    nominal.time <- paste(round(time, 1), "min")
  } else if (unit == "second") {
    nominal.time <- paste(round(time, 1), "sec")
  }
  nominal.time
}
