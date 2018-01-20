#' Compute shortest walking paths from case to pump.
#'
#' Compute path from anchor case to nearest pump (or among selected pumps).
#' @param pump.select Numeric. Pump candidates to consider. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected", or "snow".
#' @param cores Numeric. The number logical cores to use. Default is 1, which is the only possible value for Windows.
#' @export
#' @return A R list of vectors of nodes.

nearestPath <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE,
  case.set = "observed", cores = 1L) {

  dat <- neighborhoodData(vestry)
  path.data <- pathData(dat, weighted, case.set, cores)
  distances <- path.data$distances
  paths <- path.data$paths
  nodes.pump <- dat$nodes.pump

  if (is.null(pump.select)) {
    nearest <- vapply(distances, function(x) names(which.min(x)), character(1L))
  } else {
    if (all(pump.select > 0)) {
      nearest <- vapply(distances, function(x) {
        candidates <- x[names(x) %in% pump.select]
        sel <- which.min(candidates)
        names(sel)
      }, character(1L))
    } else if (all(pump.select < 0)) {
      nearest <- vapply(distances, function(x) {
        candidates <- x[names(x) %in% abs(pump.select) == FALSE]
        sel <- which.min(candidates)
        names(sel)
      }, character(1L))
    }
  }

  lapply(seq_along(paths), function(i) {
    out <- names(paths[[i]][[nearest[i]]])
  })
}
