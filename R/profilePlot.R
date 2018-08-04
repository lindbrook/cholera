#' Profile Plot.
#'
#' @noRd

profilePlot <- function() NULL

axisSlope <- function(theta) tan(pi * theta / 180)
axisIntercept <- function(m, x, y) {
  int <- y - m * x
  ifelse(m * x == 0, y, int)
}

orthogonalSlope <- function(b) -1 / b
orthogonalIntercept <- function(b, ortho.slope, y) y - ortho.slope + b

#' Intercept and slope of selected axis.
#' @param pump Numeric. Numeric ID of pump (focal point).
#' @param theta Numeric. Angle of axis in degrees.
#' @param vestry Logical. TRUE uses the 14 pumps from the map in the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @noRd

ols <- function(pump = 7, theta = 0, vestry = FALSE) {
  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }
  center <- p.data[p.data$id == pump, ]
  b <- axisSlope(theta)
  a <- axisIntercept(b, center$x, center$y)
  data.frame(intercept = a, slope = b)
}

#' Coordinate of projection onto axis
#' @param case Numeric. Numeric ID of case.
#' @param theta Numeric. Angle of axis in degrees.
#' @param vestry Logical. TRUE uses the 14 pumps from the map in the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param observed Logical.
#' @noRd

orthogonalCoordinates <- function(case, pump = 7, theta = 0, vestry = FALSE,
  observed = TRUE) {

  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }

  axis.focus <- p.data[p.data$id == pump, ]

  if (observed) {
    obs <- cholera::fatalities.address[cholera::fatalities.address$anchor.case
      == case, c("x", "y")]
  } else {
    obs <- cholera::regular.cases[case, ]
  }

  axis.data <- ols(pump, theta, vestry)

  if (theta != 0) {
    ortho.slope <- orthogonalSlope(axis.data$slope)
    ortho.intercept <- orthogonalIntercept(obs$x, ortho.slope, obs$y)

    x.proj <- (ortho.intercept - axis.data$intercept) /
              (axis.data$slope - ortho.slope)

    y.proj <- axis.data$slope * x.proj + axis.data$intercept

  } else {
    x.proj <- obs$x
    y.proj <- axis.focus$y
  }

  data.frame(x = x.proj, y = y.proj, row.names = NULL)
}
