#' Test if case is orthogonal to segment.
#'
#' Diagnostic to check classification of case to a road segment.
#' @param case Numeric or Integer. Numeric ID of observed case.
#' @param segment Character. Segment ID. See cholera::road.segments
#' @param observed Logical. TRUE observed case; FALSE simulated case (cholera::regular.cases).
#' @param coordinates Logical. Orthogonal projection coordinates.
#' @note This function is a diagnostic. It is not a guarantee of correct classification.
#' @return Logical TRUE or FALSE
#' @export
#' @examples
#' classifierAudit(case = 483, segment = "326-2")
#' plot(classifierAudit(case = 483, segment = "326-2"))

classifierAudit <- function(case = 483, segment = "326-2", observed = TRUE,
  coordinates = FALSE) {

  if (!is.numeric(case)) {
    stop("case must be numeric.")
  }

  if (observed) {
    if (case %in% unique(cholera::fatalities$case) == FALSE) {
      stop("Observed case must be a whole number between 1 and 578.")
    }
    obs <- cholera::fatalities[cholera::fatalities$case == case, c("x", "y")]
  } else {
    if (case %in% seq_len(nrow(cholera::regular.cases)) == FALSE) {
      stop("Simulated case must be a whole number between 1 and 4993.")
    }
    obs <- cholera::regular.cases[case, c("x", "y")]
  }

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

  if (coordinates) {
    data.frame(x = x.proj, y = y.proj)
  } else {
    # Bisection / Intersection test
    distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
             stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

    distance <- stats::dist(seg.df)

    out <- list(case = case,
                segment = segment,
                ols = ols,
                seg.df = seg.df,
                obs = obs,
                distance = distance,
                test = signif(distance) == signif(distB))

    class(out) <- "classifier_audit"
    out
  }
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
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param unit Character. Unit of distance: "meter" (the default), "yard" or "native". "native" returns the map's native scale. "unit" is meaningful only when "weighted" is TRUE. See \code{vignette("roads")} for information on unit distances.
#' @param ... Additional parameters.
#' @return A base R graphic.
#' @export
#' @examples
#' plot(classifierAudit(case = 483, segment = "326-2"))

plot.classifier_audit <- function(x, zoom = TRUE, radius = 0.5, unit = "meter",
  ...) {

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('"unit" must be "meter", "yard" or "native".')
  }

  obs <- x$obs
  segment.slope <- stats::coef(x$ols)[2]
  segment.intercept <- stats::coef(x$ols)[1]
  orthogonal.slope <- -1 / segment.slope
  orthogonal.intercept <- obs$y - orthogonal.slope * obs$x

  x.proj <- (orthogonal.intercept - segment.intercept) /
            (segment.slope - orthogonal.slope)

  y.proj <- segment.slope * x.proj + segment.intercept

  cholera::segmentLocator(x$segment, zoom = zoom, radius = radius,
    title = FALSE, subtitle = FALSE)

  # Bisection / Intersection test
  distB <- stats::dist(rbind(x$seg.df[1, ], c(x.proj, y.proj))) +
           stats::dist(rbind(x$seg.df[2, ], c(x.proj, y.proj)))

  if (x$test) {
    points(obs, col = "green")
    points(x.proj, y.proj, pch = 0, col = "green")
    arrows(obs$x, obs$y, x.proj, y.proj, length = 0.1, col = "green", lwd = 3)
  } else {
    points(obs, col = "dodgerblue")
    points(x.proj, y.proj, pch = 0, col = "dodgerblue")
    arrows(obs$x, obs$y, x.proj, y.proj, length = 0.1, col = "dodgerblue",
      lwd = 3)
  }

  if (unit == "meter") {
    ortho.dist <- cholera::unitMeter(x$distance, "meter")
    d.unit <- "m"
  } else if (unit == "yard") {
    ortho.dist <- cholera::unitMeter(x$distance, "yard")
    d.unit <- "yd"
  } else if (unit == "native") {
    ortho.dist <- cholera::unitMeter(x$distance, "native")
    d.unit <- "units"
  }

  nm <- cholera::road.segments[cholera::road.segments$id == x$segment, "name"]

  title(main = paste0(nm, ": Segment # ", x$segment, ", Case # ", x$case),
        sub = paste(x$test, "; dist =", round(ortho.dist, 1), d.unit))
}
