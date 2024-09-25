#' Compute shortest distances to selected pumps.
#'
#' @param pump.select Numeric. Pump candidates to consider. Default is \code{NULL}: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param metric Character. "euclidean" or "walking".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed" or "expected" # or "snow".
#' @param latlong Logical. \code{TRUE} Use longitude and latitude coordinates.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export

nearestPump <- function(pump.select = NULL, metric = "walking", vestry = FALSE,
  weighted = TRUE, case.set = "observed", latlong = FALSE, multi.core = TRUE,
  dev.mode = FALSE) {

  if (latlong) {
    cores <- multiCore(multi.core)
    nearestPumpLatlong(pump.select = pump.select, metric = metric,
      vestry = vestry, case.set = case.set, multi.core = cores)
  } else {
    nearestPumpNominal(pump.select = pump.select, metric = metric,
      vestry = vestry, case.set = case.set)
  }
}
