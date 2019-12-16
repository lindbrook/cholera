#' Rotate points (prototype).
#'
#' @param id Point ID.
#' @export

rotatePoint <- function(id) {
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rd <- rd[order(rd$x, rd$y), ]

  ols <- stats::lm(y ~ x, data = rd[c(1, id), c("x", "y")])
  segment.slope <- stats::coef(ols)[2]
  delta.theta <- atan(segment.slope) + pi / 2

  x0 <- rd[1, "x"]
  y0 <- rd[1, "y"]
  x.sel <- rd[id, "x"]
  y.sel <- rd[id, "y"]

  data.frame(x = xPrime(x.sel, y.sel, x0, y0, delta.theta),
             y = yPrime(x.sel, y.sel, x0, y0, delta.theta))
}

xPrime <- function(x, y, x0, y0, delta.theta) {
  cos(delta.theta) * (x - x0) + sin(delta.theta) * (y - y0) + x0
}

yPrime <- function(x, y ,x0, y0, delta.theta) {
  -sin(delta.theta) * (x - x0) + cos(delta.theta) * (y - y0) + y0
}
