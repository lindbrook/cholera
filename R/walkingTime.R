#' Compute walking time.
#'
#' @param dist.data Object. Data frame of distances.
#' @param time.unit Character. "hour", "minute" or "second".
#' @param walking.speed Numeric. km/hr.
#' @noRd

walkingTime <- function(dist.data, time.unit = "second", walking.speed = 5) {
  if (time.unit == "hour") {
    out <- dist.data / (1000L * walking.speed)
  } else if (time.unit == "minute") {
    out <- (60L * dist.data) / (1000L * walking.speed)
  } else if (time.unit == "second") {
    out <- (3600L * dist.data) / (1000L * walking.speed)
  }
  out
}
