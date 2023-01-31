#' Compute coordinates of orthogonal projection from case to road segment.
#'
#' @param case Numeric. case ID from \code{fatalities}.
#' @param segment.id Character. Road segment ID.
#' @param observed Logical. \code{FALSE} observed case; \code{TRUE} simulated case (\code{regular.cases}).
#' @param use.pump Logical. Use pump ID as case.
#' @param case.data Object. For use with \code{simulateFatalities}.
#' @param vestry Logical. Use vestry pump data.
#' @return An R data frame.
#' @noRd

orthogonalProjection <- function(case = 12, segment.id = "216-1",
  observed = TRUE, use.pump = FALSE, vestry = FALSE, case.data = NULL) {

  if (is.numeric(case) == FALSE) {
    stop("case must be numeric.")
  } else {
    if (use.pump == FALSE) {
      if (observed) {
        caseID <- cholera::fatalities$case
        if (case %in% caseID) {
          case.data <- cholera::fatalities[caseID == case, ]
        } else stop("case must lie between 1 and ", max(caseID), ".")
      }
    } else {
      if (vestry) {
        caseID <- cholera::pumps.vestry$id
        if (case %in% caseID) {
          case.data <- cholera::pumps.vestry[cholera::pumps.vestry$id == case, ]
        } else {
          stop("case must lie between 1 and ", max(caseID), ".")
        }
      } else {
        caseID <- cholera::pumps$id
        if (case %in% caseID) {
          case.data <- cholera::pumps[cholera::pumps$id == case, ]
        } else {
          stop("case must lie between 1 and ", max(caseID), ".")
        }
      }
    }
  }

  if (is.character(segment.id) == FALSE) {
    stop("segment.id must be character")
  } else {
    streetID <- cholera::road.segments$id
    if (segment.id %in% streetID == FALSE) {
      stop("See cholera::road.segments$id for valid segment.id.")
    }
  }

  sel <- cholera::road.segments$id == segment.id
  segment.data <- cholera::road.segments[sel, c("x1", "y1", "x2", "y2")]
  road.segment <- data.frame(x = c(segment.data$x1, segment.data$x2),
                             y = c(segment.data$y1, segment.data$y2))

  ols <- stats::lm(y ~ x, data = road.segment)
  road.intercept <- stats::coef(ols)[1]
  road.slope <- stats::coef(ols)[2]
  ortho.slope <- -1 / road.slope
  ortho.intercept <- case.data$y - ortho.slope * case.data$x
  x.proj <- (ortho.intercept - road.intercept) / (road.slope - ortho.slope)
  y.proj <- road.slope * x.proj + road.intercept
  data.frame(x.proj, y.proj, row.names = NULL)
}
