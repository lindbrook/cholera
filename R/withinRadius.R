#' Test whether b is within a's radius.
#'
#' @param a Numeric. Data frame of x-y coordinates.
#' @param b Numeric. Data frame of x-y coordinates.
#' @param radius Numeric.
#' @noRd

withinRadius <- function(a, b, radius = 2) {
  (a$x - b$x)^2 + (a$y - b$y)^2 <= radius^2
}
