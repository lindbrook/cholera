#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected" or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @param latlong Logical.
#' @export
#' @examples
#' \dontrun{
#' neighborhoodWalking()
#' neighborhoodWalking(pump.select = -6)
#' }

neighborhoodWalking <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, case.set = "observed", multi.core = TRUE, dev.mode = FALSE,
  latlong = FALSE) {

  if (latlong) {
    args <- list(pump.select = pump.select, vestry = vestry,
      weighted = weighted, case.set = case.set, multi.core = multi.core)
    out <- do.call("walkingLatlong", args)
  } else {
    args <- list(pump.select = pump.select, vestry = vestry,
      weighted = weighted, case.set = case.set, multi.core = multi.core,
      dev.mode = dev.mode)
    out <- do.call("walkingNominal", args)
  }
  out
}
