#' Compute distance between endpoints of road segments.
#'
#' @param a Numeric. Road point ID.
#' @param b Numeric. Road point ID.
#' @param meters Logical. Compute metric (meters) or nominal distance.
#' @noRd

roadPointDistance <- function(a = 85, b = 86, meters = FALSE) {
  sel <- cholera::roads$id %in% c(a, b)
  d <- stats::dist(cholera::roads[sel, c("x", "y")])
  ifelse(meters, cholera::unitMeter(d), d)
}
