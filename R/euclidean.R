#' Compute Euclidean path pump neighborhoods.
#'
#' Plots star graph from pump to its cases.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param case.set Character. "observed" or "expected".
#' @param case.select Character. Fatalities: "all" or "address".
#' @param latlong Logical. Longitude and latitude coordinates
#' @param location Character. "nominal", "anchor" or "orthogonal".
#' @param brute.force Logical. For latlong = FALSE. TRUE computes nearest pump for each case. FALSE uses Voronoi cells as shortcut.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @return An R list.
#' @export

neighborhoodEuclidean <- function(pump.select = NULL, vestry = FALSE,
   case.set = "observed", case.select = "address", latlong = FALSE,
   location = "nominal", brute.force = FALSE, multi.core = FALSE,
   dev.mode = FALSE) {

  if (latlong) {
    args <- list(pump.select = pump.select, vestry = vestry,
      case.set = case.set, location = location)
    out <- do.call("euclideanLatlong", args)
  } else {
    args <- list(pump.select = pump.select, vestry = vestry,
      case.set = case.set, location = location, brute.force = brute.force,
      multi.core = multi.core, dev.mode = dev.mode)
    out <- do.call("euclideanNominal", args)
  }
  out
}
