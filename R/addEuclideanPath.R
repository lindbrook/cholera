#' Add the path for the Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or simulated expected data.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param unit.posts Character. "distance" for mileposts; "time" for timeposts; \code{NULL} for no posts.
#' @param unit.interval Numeric. Sets interval between \code{unit.posts}.
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @note The function uses a case's "address" (i.e., "anchor" case of a stack) to compute distance. Time is computed using \code{distanceTime()}.
#' @return An R list with 3 data frames: x-y coordinates for the origin and destination, and a summary of results.
#' @export

addEuclideanPath <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, vestry = FALSE, unit = "meter", time.unit = "second",
  walking.speed = 5, unit.posts = "distance", unit.interval = NULL,
  alpha.level = 1) {

  if (is.numeric(origin) == FALSE) {
    stop('origin must be numeric.')
  }

  if (is.null(destination) == FALSE) {
    if (is.numeric(destination) == FALSE) {
      stop('destination must be numeric.')
    }
  }

  arguments <- list(origin = origin,
                    destination = destination,
                    type = type,
                    observed = observed,
                    vestry = vestry,
                    unit = unit,
                    time.unit = "second",
                    walking.speed = 5)

  x <- do.call(euclideanPath, arguments)

  colors <- cholera::snowColors(x$vestry)

  origin.xy <- x$ego
  alter.xy <- x$alter
  dat <- stats::setNames(rbind(alter.xy, origin.xy), c("x", "y"))

  if (x$type == "case-pump") {
    destination.pump <- row.names(x$alter)
    case.color <- colors[paste0("p", destination.pump)]
    points(origin.xy, col = "red")
    pumpToken(x, case.color, destination.pump)
  } else if (x$type == "cases" | x$type == "pumps") {
    case.color <- "blue"
    destination.case <- row.names(x$alter)
    points(origin.xy, col = case.color)
    points(alter.xy, col = case.color)
    text(origin.xy, labels = x$origin, pos = 1, col = case.color)
    text(alter.xy, labels = destination.case, pos = 1, col = case.color)
  }

  if (x$time.unit == "hour") {
    nominal.time <- paste(round(x$t, 1), "hr")
  } else if (x$time.unit == "minute") {
    nominal.time <- paste(round(x$t, 1), "min")
  } else if (x$time.unit == "second") {
    nominal.time <- paste(round(x$t, 1), "sec")
  }

  if (x$unit == "native") {
    d.unit <- "units;"
  } else if (x$unit == "meter") {
    d.unit <- "m;"
  } else if (x$unit == "yard") {
    d.unit <- "yd;"
  }

  # mileposts #

  if (is.null(unit.posts)) {
    arrows(origin.xy$x.proj, origin.xy$y.proj,
           alter.xy$x.proj, alter.xy$y.proj,
           col = case.color, lwd = 3, length = 0.075)
  } else {
    if (unit.posts %in% c("distance", "time") == FALSE) {
      stop('If specified, unit.posts must be "distance" or "time".')
    } else {
      if (is.null(unit.interval)) {
        if (unit.posts == "distance")  {
          unit.interval <- 50
        } else if (unit.posts == "time") {
          unit.interval <- 60
        }
      } else {
        if (!is.numeric(unit.interval)) {
          stop('unit.interval must be numeric.')
        }
      }

      if (unit.posts == "distance") {
        tot <- cholera::unitMeter(stats::dist(dat))
        h <- seq(0, tot, unit.interval) / cholera::unitMeter(1)
      } else if (unit.posts == "time") {
        tot <- cholera::distanceTime(cholera::unitMeter(stats::dist(dat),
          unit = "native"), speed = x$speed)
        h <- seq(0, tot, unit.interval) * 1000 * x$speed / 60^2 /
          cholera::unitMeter(1)
      } else {
        stop('Specify a unit.posts')
      }

      ols <- stats::lm(y ~ x, data = dat)
      edge.slope <- stats::coef(ols)[2]
      edge.intercept <- stats::coef(ols)[1]
      theta <- ifelse(is.na(edge.slope), pi / 2, atan(edge.slope))

      p.coords <- quandrantCoordinates(dat, h, theta)
      post.data <- data.frame(x = c(p.coords$x, origin.xy$x.proj),
                              y = c(p.coords$y, origin.xy$y.proj))

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

    if (unit.posts == "distance") {
      post.info <- paste("posts @", unit.interval, "m intervals")
    } else if (unit.posts == "time") {
      post.info <- paste("posts @", unit.interval, "sec intervals")
    }
  }
}
