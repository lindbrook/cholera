#' Convert distance to time.
#'
#' @param x Numeric. Nominal map distance.
#' @param unit Character. Unit of measurement: "hour" or "minute".
#' @param meter.unit Numeric. Estimate of meters per map unit: 54.
#' @param speed Numeric. Walking speed: 3 meters per hour.
#' @return An R vector.
#' @export

distanceTime <- function(x, unit = "minute", meter.unit = 54, speed = 3) {
  if (is.numeric(x) == FALSE) {
    stop('"x" must be numeric.')
  }

  if (unit %in% c("minute", "hour") == FALSE) {
    stop('"unit" must be "minute" or "hour"')
  }

  if (unit == "hour") {
    (x * meter.unit / 1000) * (1 / speed)
  } else if (unit == "minute") {
    60 * (x * meter.unit / 1000) * (1 / speed)
  }
}
