#' Compute coordinates of orthogonal projection from case to road segment.
#'
#' @param case Numeric. case ID from \code{fatalities}.
#' @param street Numeric. Road segment ID.
#' @return An R data frame.
#' @export

orthogonalProjection <- function(case = 12, street = 216) {
  if (is.numeric(case) == FALSE) {
    stop("case must be numeric.")
  } else {
    caseID <- cholera::fatalities$case
    if (case %in% caseID == FALSE) {
      stop("case must lie between 1 and ", max(caseID), ".")
    }
  }

  if (is.numeric(street) == FALSE) {
    stop("street must be numeric.")
  } else {
    if (street %in% unique(cholera::roads$street) == FALSE) {
      stop("street must lie between 1 and 528.")
    }
  }

  case.data <- cholera::fatalities[cholera::fatalities$case == case, ]
  ols <- stats::lm(y ~ x, data = cholera::roads[cholera::roads$street ==
    street, ])
  road.intercept <- stats::coef(ols)[1]
  road.slope <- stats::coef(ols)[2]
  orthogonal.slope <- -1 / road.slope
  orthogonal.intercept <- case.data$y - orthogonal.slope * case.data$x
  x.proj <- (orthogonal.intercept - road.intercept) /
            (road.slope - orthogonal.slope)
  y.proj <- road.slope * x.proj + road.intercept
  data.frame(x = x.proj, y = y.proj)
}
