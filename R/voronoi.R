#' Compute Voronoi pump neighborhoods.
#'
#' Group cases into neighborhoods using Voronoi tessellation.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param latlong Logical. Longitude and latitude coordinates
#' @param location Character. "nominal" or "orthogonal". "nominal" uses the x-y coordinates of \code{fatalities.address}. "orthogonal"uses the x-y coordinates of \code{ortho.proj}.
#' @param polygon.vertices Logical. \code{TRUE} returns a list of x-y coordinates of the vertices of Voronoi cells. Useful for \code{sp::point.in.polygon()} as used in \code{print.voronoi()} method.
#' @importFrom deldir deldir
#' @importFrom sp point.in.polygon
#' @return An R list with 12 objects.
#' \itemize{
#'   \item{\code{pump.id}: vector of selected pumps}
#'   \item{\code{voronoi}: output from deldir::deldir().}
#'   \item{\code{snow.colors}: neighborhood color based on snowColors().}
#'   \item{\code{x.rng}: range of x for plot.}
#'   \item{\code{y.rng}: range of y for plot.}
#'   \item{\code{select.string}: description of "pump.select" for plot title.}
#'   \item{\code{expected.data}: expected neighborhood fatality counts, based on Voronoi cell area.}
#'   \item{\code{coordinates}: polygon vertices of Voronoi cells.}
#'   \item{\code{statistic.data}: observed neighborhood fatality counts.}
#'   \item{\code{pump.select}: "pump.select" from neighborhoodVoronoi().}
#'   \item{\code{statistic}: "statistic" from neighborhoodVoronoi().}
#'   \item{\code{vestry}: "vestry" from neighborhoodVoronoi().}
#' }
#' @export
#' @examples
#' \dontrun{
#' neighborhoodVoronoi()
#' neighborhoodVoronoi(vestry = TRUE)
#' neighborhoodVoronoi(pump.select = 6:7)
#' neighborhoodVoronoi(pump.select = -6)
#' neighborhoodVoronoi(pump.select = -6, polygon.vertices = TRUE)
#'
#' # coordinates for vertices also available in the returned object.
#' dat <- neighborhoodVoronoi(pump.select = -6)
#' dat$coordinates
#' }

neighborhoodVoronoi <- function(pump.select = NULL, vestry = FALSE,
  latlong = FALSE, location = "nominal", polygon.vertices = FALSE) {

  if (!is.null(pump.select)) {
    if (length(pump.select) == 1 & all(pump.select > 0))
      stop("Use at least 2 pumps for 'pump.select' with Voronoi diagram.",
        call. = FALSE)
  }

  if (latlong) {
    args <- list(pump.select = pump.select, vestry = vestry,
      location = location, polygon.vertices = polygon.vertices)
    out <- do.call("voronoiLatlong", args)
    out$latlong <- TRUE
  } else {
    args <- list(pump.select = pump.select, vestry = vestry,
      location = location, polygon.vertices = polygon.vertices)
    out <- do.call("voronoiNominal", args)
    out$latlong <- FALSE
  }
  out
}
