#' Compute walking distance for simulated cases.
#'
#' @param pump.select Numeric.
#' @export
#' @note This function documents sim.walking.distance data frame.

simWalkingDistance <- function(pump.select = 7) {
  nearestPump(pump.select, case.set = "expected")
}
