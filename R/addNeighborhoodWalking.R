#' Add walking neighborhoods.
#'
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected" or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param latlong Logical.
#' @param type Character. "roads", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param tsp.method Character. Traveling salesperson problem algorithm.
#' @param path.width Numeric. Set width of paths.
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param polygon.type Character. "border" or "solid".
#' @param polygon.col Character.
#' @param polygon.lwd Numeric.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export
#' @examples
#' \dontrun{
#' streetNameLocator("marshall street", zoom = 0.5)
#' addNeighborhoodWalking()
#' }

addNeighborhoodWalking <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, case.set = "expected", latlong = FALSE,
  type = "area.polygons", tsp.method = "repetitive_nn", path.width = 2,
  alpha.level = 0.5, polygon.type = "solid", polygon.col = NULL,
  polygon.lwd = 2, multi.core = FALSE) {

  if (.Platform$OS.type == "windows") cores <- 1L
  else cores <- multiCore(multi.core)

  x <- neighborhoodWalking(pump.select = pump.select, vestry = vestry,
    weighted = weighted, case.set = case.set, latlong = latlong,
    multi.core = cores)

  plot(x, type = type, tsp.method = tsp.method, add = TRUE,
    path.width = path.width, alpha.level = alpha.level,
    polygon.type = polygon.type, polygon.col = polygon.col,
    polygon.lwd = polygon.lwd)
}
