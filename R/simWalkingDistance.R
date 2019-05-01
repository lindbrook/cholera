#' Compute walking distance for simulated cases.
#'
#' @param pump.select Numeric.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @export
#' @note This function documents sim.walking.distance data frame.

simWalkingDistance <- function(pump.select = 7, multi.core = FALSE) {
  nearestPump(pump.select, case.set = "expected", multi.core = multi.core)
}
