#' Compute walking distance for simulated cases.
#'
#' @param pump.select Numeric.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export
#' @note This function documents the code that create the sim.walking.distance data frame.

simWalkingDistance <- function(pump.select = 7, multi.core = FALSE,
  dev.mode = FALSE) {
  nearestPump(pump.select, case.set = "expected", multi.core = multi.core,
    dev.mode = dev.mode)
}
