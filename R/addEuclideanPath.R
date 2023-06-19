#' Add the path for the Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param case.location Character. For \code{case.set = "observed"}: "address" or "orthogonal". "address" is the x-y coordinate of a bar. "orthogonal" is the x-y coordinate of a stack's "anchor" case.
#' @param case.set Character. "observed" or "expected".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param mileposts Logical. Plot mile/time posts.
#' @param milepost.unit Character. "distance" for mileposts; "time" for timeposts.
#' @param milepost.interval Numeric. Sets interval between \code{milepost.unit}.
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @note Walking time is computed using \code{distanceTime()}.
#' @return An R list with 3 data frames: x-y coordinates for the origin and destination, and a summary of results.
#' @export

addEuclideanPath <- function(origin = 1, destination = NULL, type = "case-pump",
  case.location = "address", case.set = "observed", vestry = FALSE,
  distance.unit = "meter", time.unit = "second", walking.speed = 5,
  mileposts = TRUE, milepost.unit = "distance", milepost.interval = NULL,
  alpha.level = 1) {

  arguments <- list(origin = origin,
                    destination = destination,
                    type = type,
                    case.location = case.location,
                    case.set = case.set,
                    vestry = vestry,
                    distance.unit = distance.unit,
                    time.unit = time.unit,
                    walking.speed = walking.speed)

  x <- do.call(euclideanPath, arguments)

  colors <- snowColors(x$vestry)
  origin.coords <- x$ego
  alter.coords <- x$alter
  dat <- stats::setNames(rbind(alter.coords, origin.coords), c("x", "y"))

  if (x$type == "case-pump") {
    destination.pump <- row.names(x$alter)
    case.color <- colors[paste0("p", destination.pump)]
    points(origin.coords, col = "red")
    text(origin.coords, labels = x$data$case, pos = 1, col = "red")
  } else if (x$type == "cases" | x$type == "pumps") {
    case.color <- "blue"
    destination.case <- row.names(x$alter)
    points(origin.coords, col = case.color)
    points(alter.coords, col = case.color)
    text(origin.coords, labels = x$origin, pos = 1, col = case.color)
    text(alter.coords, labels = destination.case, pos = 1, col = case.color)
  }

  d.unit <- distanceUnit(x$distance.unit)
  nominal.time <- nominalTime(x$t, x$time.unit)

  # mileposts #

  if (mileposts == FALSE) {
    arrows(origin.coords$x, origin.coords$y, alter.coords$x, alter.coords$y,
           col = case.color, lwd = 3, length = 0.075)
  } else {
    if (milepost.unit %in% c("distance", "time") == FALSE) {
      stop('If specified, milepost.unit must be "distance" or "time".',
        call. = FALSE)
    } else {
      if (is.null(milepost.interval)) {
        if (milepost.unit == "distance") milepost.interval <- 50
        else if (milepost.unit == "time") milepost.interval <- 60
      } else {
        if (!is.numeric(milepost.interval)) {
          stop('milepost.interval must be numeric.', call. = FALSE)
        }
      }

      if (milepost.unit == "distance") {
        tot <- cholera::unitMeter(stats::dist(dat))
        h <- seq(0, tot, milepost.interval) / cholera::unitMeter(1)
      } else if (milepost.unit == "time") {
        tot <- distanceTime(cholera::unitMeter(stats::dist(dat),
          distance.unit = "nominal"), walking.speed = x$walking.speed)
        h <- seq(0, tot, milepost.interval) * 1000 * x$walking.speed / 60^2 /
          cholera::unitMeter(1)
      } else {
        stop('Specify a milepost.unit', call. = FALSE)
      }

      ols <- stats::lm(y ~ x, data = dat)
      edge.slope <- stats::coef(ols)[2]
      edge.intercept <- stats::coef(ols)[1]
      theta <- ifelse(is.na(edge.slope), pi / 2, atan(edge.slope))

      p.coords <- quandrantCoordinates(dat[2:1, ], h, theta)
      post.data <- data.frame(x = c(p.coords$x, origin.coords$x),
                              y = c(p.coords$y, origin.coords$y))

      a.data <- cbind(post.data[-nrow(post.data), ], post.data[-1, ])
      a.data <- stats::setNames(a.data, c("x1", "y1", "x2", "y2"))

      invisible(lapply(seq_len(nrow(a.data)), function(i) {
        dataB <- data.frame(x = c(a.data[i, "x1"], a.data[i, "x2"]),
                            y = c(a.data[i, "y1"], a.data[i, "y2"]))

        zero.length.x <- round(abs(dataB[1, "x"] - dataB[2, "x"]), 2) == 0
        zero.length.y <- round(abs(dataB[1, "y"] - dataB[2, "y"]), 2) == 0

        if (any(zero.length.x | zero.length.y)) {
          drawPath(dat, case.color, compute.coords = FALSE)
          text(dataB[1, c("x", "y")], labels = ">", srt = theta * 180L / pi,
            col = case.color, cex = 1.5)
        } else {
          arrows(a.data[i, "x1"], a.data[i, "y1"],
                 a.data[i, "x2"], a.data[i, "y2"],
                 length = 0.075, col = case.color, lwd = 3, code = 1)
        }
      }))
    }
  }
}
