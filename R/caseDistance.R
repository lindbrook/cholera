#' Compute Euclidean distance between case fatalities (meters).
#'
#' @param a Numeric. Case ID.
#' @param b Numeric. Case ID.
#' @param latlong Logical.
#' @noRd

caseDistance <- function(a = 19, b = 263, latlong = FALSE) {
  if (latlong) {
    vars <- c("lon", "lat")
    p1 <- cholera::fatalities[cholera::fatalities$case == a, vars]
    p2 <- cholera::fatalities[cholera::fatalities$case == b, vars]
    geosphere::distGeo(p1, p2)
  } else {
    sel <- cholera::fatalities$case %in% c(a, b)
    unitMeter(stats::dist(cholera::fatalities[sel, c("x", "y")]))
  }
}
