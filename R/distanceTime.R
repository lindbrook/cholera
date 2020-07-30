#' Convert distance to elapsed time.
#'
#' @param x Numeric. Nominal map distance.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param time.unit Character. Unit of measurement: "hour", "minute" or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @return An R vector.
#' @export

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
    speed <- walking.speed * unitMeter(1)
  } else if (distance.unit == "yard") {
    speed <- walking.speed * 1.0936 # convert to yd/hr
  } else if (distance.unit == "meter") {
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
