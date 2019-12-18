#' Rotate points (prototype).
#'
#' @param id Numeric. Road segment endpoint ID.
#' @param unique.coords Logical. Use unique coordinates.
#' @export

rotatePoint <- function(id = 66, unique.coords = TRUE) {
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rd <- rd[order(rd$x, rd$y), ]

  if (unique.coords) {
    rd$point.id <- paste0(rd$x, "-", rd$y)
    rd <- rd[!duplicated(rd$point.id), ]
  }

  center <- data.frame(x = mean(range(rd$x)), y = mean(range(rd$y)))

  points.data <- rbind(center, rd[rd$id == id, c("x", "y")])
  theta <- theta(points.data)
  h <- stats::dist(points.data)

  theta.delta <- referenceRadians()

  if (points.data$x[1] - points.data$x[2] >= 0) {
    x.prime <- c(center$x - cos(theta - theta.delta) * h)
    y.prime <- c(center$y - sin(theta - theta.delta) * h)
  } else {
    x.prime <- c(center$x + cos(theta - theta.delta) * h)
    y.prime <- c(center$y + sin(theta - theta.delta) * h)
  }

  data.frame(x = x.prime, y = y.prime, row.names = NULL)
}

referenceRadians <- function(id1 = 1, id2 = 2) {
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rd <- rd[order(rd$x, rd$y), ]
  x1 <- rd[id1, "x"]
  y1 <- rd[id1, "y"]
  x2 <- rd[id2, "x"]
  y2 <- rd[id2, "y"]
  atan((x1 - x2) / (y2 - y1))
}

theta <- function(points.data) {
  ols <- stats::lm(y ~ x, data = points.data)
  segment.slope <- stats::coef(ols)[2]
  atan(segment.slope)
}
