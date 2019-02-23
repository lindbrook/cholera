#' Compute coordinates of orthogonal projection from case to road segment.
#'
#' @param case Numeric. case ID from \code{fatalities}.
#' @param segment.id Character. Road segment ID.
#' @return An R data frame.
#' @export

orthogonalProjection <- function(case = 12, segment.id = "216-1") {
  if (is.numeric(case) == FALSE) {
    stop("case must be numeric.")
  } else {
    caseID <- cholera::fatalities$case
    if (case %in% caseID == FALSE) {
      stop("case must lie between 1 and ", max(caseID), ".")
    }
  }

  if (is.character(segment.id) == FALSE) {
    stop("segment.id must be character")
  } else {
    streetID <- road.segments$id
    if (segment.id %in% streetID == FALSE) {
      stop("See road.segments$id for valid segment.id.")
    }
  }

  case.data <- cholera::fatalities[cholera::fatalities$case == case, ]
  segment.data <- road.segments[road.segments$id == segment.id,
    c("x1", "y1", "x2", "y2")]
    
  road.segment <- data.frame(x = c(segment.data$x1, segment.data$x2),
                             y = c(segment.data$y1, segment.data$y2))

  ols <- stats::lm(y ~ x, data = road.segment)
  road.intercept <- stats::coef(ols)[1]
  road.slope <- stats::coef(ols)[2]
  orthogonal.slope <- -1 / road.slope
  orthogonal.intercept <- case.data$y - orthogonal.slope * case.data$x
  x.proj <- (orthogonal.intercept - road.intercept) /
            (road.slope - orthogonal.slope)
  y.proj <- road.slope * x.proj + road.intercept
  data.frame(x = x.proj, y = y.proj, row.names = NULL)
}
