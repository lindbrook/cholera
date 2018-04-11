#' Convert distance to time.
#'
#' @param x Numeric. Nominal map distance.
#' @param unit Character. Unit of measurement: "hour", "minute" or "second".
#' @param meter.unit Numeric. 1 meter is approximately 54 map units.
#' @param speed Numeric. Walking speed: 5 km/hr.
#' @return An R vector.
#' @export

distanceTime <- function(x, unit = "second", meter.unit = 54, speed = 5) {
  if (is.numeric(x) == FALSE) {
    stop('"x" must be numeric.')
  }

  if (unit %in% c("minute", "hour", "second") == FALSE) {
    stop('"unit" must be "hour", "minute" or "second".')
  }

  if (unit == "hour") {
    (x * meter.unit) / (1000 * speed)
  } else if (unit == "minute") {
    60 * (x * meter.unit) / (1000 * speed)
  } else if (unit == "second") {
    3600 * (x * meter.unit) / (1000 * speed)
  }
}
