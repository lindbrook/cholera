#' Test whether point "b" is within a given radius of point "a".
#'
#' @param a Numeric. Data frame of x-y coordinates.
#' @param b Numeric. Data frame of x-y coordinates.
#' @param radius Numeric.
#' @export

withinRadius <- function(a, b, radius = 2) {
  (a$x - b$x)^2 + (a$y - b$y)^2 <= radius^2
}
