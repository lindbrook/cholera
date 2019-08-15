#' Compute walking distance for simulated cases.
#'
#' @param pump.select Numeric.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @param compute Logical.
#' @note This function is computationally intensive. See \code{vignette("Parallelization")} for details. This functions document the code that generates \code{\link{sim.walking.distance}}.
#' @export

simulateWalkingDistance <- function(pump.select = 7, multi.core = FALSE,
  dev.mode = FALSE, compute = FALSE) {

  if (compute == FALSE) {
    cholera::sim.walking.distance
  } else {
    nearestPump(pump.select = pump.select, case.set = "expected",
      multi.core = multi.core, dev.mode = dev.mode)$distance
  }
}
