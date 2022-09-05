#' Add Voronoi cells (latlong).
#'
#' @param pump.select Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Exclusion (negative selection) is possible (e.g., -6).
#' @param vestry Logical. \code{FALSE} for original 13 pumps. TRUE for 14 pumps in Vestry Report.
#' @export

addVoronoiB <- function(pump.select = NULL, vestry = FALSE) {
  cells <- latlongVoronoiC(pump.select = pump.select, vestry = vestry)
  invisible(lapply(cells, function(x) polygon(x[, c("lon", "lat")])))
}
