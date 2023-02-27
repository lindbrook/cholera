#' Compute distance between case fatalities.
#'
#' @param a Numeric. Case ID.
#' @param b Numeric. Case ID.
#' @param meters Logical. Compute metric (meters) or nominal distance.
#' @export

caseDistance <- function(a = 19, b = 263, meters = FALSE) {
  sel <- cholera::fatalities$case %in% c(a, b)
  d <- stats::dist(cholera::fatalities[sel, c("x", "y")])
  ifelse(meters, cholera::unitMeter(d), d)
}
