#' Add expected Euclidean pump neighborhoods.
#'
#' Plots star graph from pump to its cases.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param case.set Character. "observed" or "expected".
#' @param case.select Character. Fatalities: "all" or "address".
#' @param latlong Logical. Longitude and latitude coordinates
#' @param location Character. "nominal", "anchor" or "orthogonal".
#' @param brute.force Logical. For latlong = FALSE. TRUE computes nearest pump for each case. FALSE uses Voronoi cells as shortcut.
#' @param type Character. "star", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param add.observed.points Logical. Add observed fatality "addresses".
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param polygon.type Character. "border" or "solid".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @return R graphic elements.
#' @export
#' @examples
#' \dontrun{
#' streetNameLocator("marshall street", zoom = 0.5, highlight = FALSE,
#'   add.subtitle = FALSE)
#' addNeighborhoodEuclidean()
#'
#' streetNameLocator("marshall street", zoom = -50, highlight = FALSE)
#' addNeighborhoodEuclidean(case.set = "expected", type = "area.polygons")
#' }

addNeighborhoodEuclidean <- function(pump.select = NULL, vestry = FALSE,
  case.set = "observed", case.select = "address", latlong = FALSE,
  location = "nominal", brute.force = FALSE, type = "star",
  add.observed.points = TRUE, alpha.level = 0.25, polygon.type = "solid",
  multi.core = FALSE, dev.mode = FALSE) {

  if (.Platform$OS.type == "windows") cores <- 1L
  else cores <- multiCore(multi.core)

  x <- neighborhoodEuclidean(pump.select = pump.select, vestry = vestry,
    case.set = case.set, case.select = case.select, latlong = latlong,
    location = location, brute.force = brute.force, multi.core = cores,
    dev.mode = dev.mode)

  plot(x, type = type, add = TRUE, add.observed.points = add.observed.points,
    alpha.level = alpha.level, polygon.type = polygon.type)
}
