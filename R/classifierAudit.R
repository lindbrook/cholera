#' Test if observed case "belongs" to segment.
#'
#' Uses orthogonal projector from an observed case to see if it intersects a segment.
#' @param case Numeric or Integer. Numeric ID of observed case.
#' @param segment Character. Segment ID. See cholera::road.segments
#' @note This is a diagnostics function. It is not a guarantee of correct classification.
#' @return Logical TRUE or FALSE
#' @export

classifierAudit <- function(case = 483, segment = "326-2") {
  obs <- cholera::fatalities[cholera::fatalities$case == case, c("x", "y")]
  seg.data <- cholera::road.segments[cholera::road.segments$id == segment,
    c("x1", "y1", "x2", "y2")]

  seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                       y = c(seg.data$y1, seg.data$y2))

  ols <- stats::lm(y ~ x, data = seg.df)
  segment.slope <- stats::coef(ols)[2]
  segment.intercept <- stats::coef(ols)[1]
  orthogonal.slope <- -1 / segment.slope
  orthogonal.intercept <- obs$y - orthogonal.slope * obs$x

  x.proj <- (orthogonal.intercept - segment.intercept) /
            (segment.slope - orthogonal.slope)

  y.proj <- segment.slope * x.proj + segment.intercept

  # Bisection / Intersection test
  distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
           stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

  out <- list(segment = segment,
              ols = ols,
              seg.df = seg.df,
              obs = obs,
              test = signif(stats::dist(seg.df)) == signif(distB))

  class(out) <- "classifier_audit"
  out
}

#' Return result of classifierAudit().
#'
#' @param x An object of class "classifier_audit" created by classifierAudit().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export
#' @examples
#' classifierAudit(case = 483, segment = "326-2")
#' print(classifierAudit(case = 483, segment = "326-2"))

print.classifier_audit <- function(x, ...) {
  if (class(x) != "classifier_audit") {
    stop('"x"\'s class needs to be "classifier_audit".')
  }

  print(x$test)
}

#' Plot result of classifierAudit().
#'
#' Plot case, segment and orthogonal projector.
#' @param x An object of class "classifier_audit" created by classifierAudit().
#' @param radius Numeric. Controls the degree of zoom.
#' @param ... Additional parameters.
#' @return A base R graphic.
#' @export
#' @examples
#' plot(classifierAudit(case = 483, segment = "326-2"))

plot.classifier_audit <- function(x, radius = 0.1, ...) {
  obs <- x$obs
  segment.slope <- stats::coef(x$ols)[2]
  segment.intercept <- stats::coef(x$ols)[1]
  orthogonal.slope <- -1 / segment.slope
  orthogonal.intercept <- obs$y - orthogonal.slope * obs$x

  x.proj <- (orthogonal.intercept - segment.intercept) /
            (segment.slope - orthogonal.slope)

  y.proj <- segment.slope * x.proj + segment.intercept

  cholera::segmentLocator(x$segment, radius = radius)
  points(x.proj, y.proj, pch = 0)

  # Bisection / Intersection test
  distB <- stats::dist(rbind(x$seg.df[1, ], c(x.proj, y.proj))) +
           stats::dist(rbind(x$seg.df[2, ], c(x.proj, y.proj)))

  if (x$test) {
    arrows(obs$x, obs$y, x.proj, y.proj, length = 0.1, col = "green")
  } else {
    arrows(obs$x, obs$y, x.proj, y.proj, length = 0.1)
  }

  title(sub = x$test)
}
