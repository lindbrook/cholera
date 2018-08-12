#' Compute path of the Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Default is 5 km/hr.
#' @note The function uses a case's "address" (i.e., "anchor" case of a stack) to compute distance. Time is computed using \code{distanceTime()}.
#' @return An R list with 3 data frames: x-y coordinates for the origin and destination, and a summary of results.
#' @export
#' @examples
#' # path from case 1 to nearest pump.
#' euclideanPath(1)
#'
#' # path from case 1 to pump 6.
#' euclideanPath(1, 6)
#'
#' # exclude pump 7 from consideration.
#' euclideanPath(1, -7)
#'
#' # path from case 1 to case 6.
#' euclideanPath(1, 6, type = "cases")
#'
#' # path from pump 1 to pump 6.
#' euclideanPath(1, 6, type = "pumps")
#'
#' # Plot result
#' plot(euclideanPath(1))

euclideanPath <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, vestry = FALSE, unit = "meter", time.unit = "second",
  walking.speed = 5) {

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('unit must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('time.unit must be "hour", "minute" or "second".')
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('type must be "case-pump", "cases" or "pumps".')
  }

  obs.ct <- nrow(cholera::fatalities)
  exp.ct <- nrow(cholera::regular.cases)

  if (observed) ct <- obs.ct else ct <- exp.ct

  if (vestry) {
    p.data <- cholera::ortho.proj.pump.vestry
    p.data$street <- cholera::pumps.vestry$street
  } else {
    p.data <- cholera::ortho.proj.pump
    p.data$street <- cholera::pumps$street
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  # ----- #

  if (type == "case-pump") {
    if (origin %in% seq_len(ct) == FALSE) {
      txt1 <- 'With type = "case-pump" and observed = '
      txt2 <- 'origin must be between 1 and '
      stop(txt1, observed, ", ", txt2, ct, ".")
    }

    if (is.null(destination)) {
      alters <- p.data
    } else {
      if (any(abs(destination) %in% p.ID == FALSE)) {
        stop('With vestry = ', vestry, ', 1 >= |destination| <= ', p.count)
      } else {
        if (vestry) {
          alters <- p.data[destination, ]
          alters$street <- cholera::pumps.vestry[destination, "street"]
        } else {
          alters <- p.data[destination, ]
          alters$street <- cholera::pumps[destination, "street"]
        }
      }
    }

    if (observed) {
      ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
        "anchor.case"]
      ego <- cholera::ortho.proj[cholera::ortho.proj$case == ego.id,
        c("x.proj", "y.proj")]
    } else {
      ego.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case == origin,
        "case"]
      ego <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case == origin,
        c("x.proj", "y.proj")]
    }

    d <- vapply(alters$pump.id, function(i) {
      c(stats::dist(rbind(alters[alters$pump.id == i, c("x.proj", "y.proj")],
        ego)))
    }, numeric(1L))

    sel <- which.min(d)
    out <- data.frame(case = origin,
                      anchor = ego.id,
                      pump = alters[sel, "pump.id"],
                      pump.name = alters[sel, "street"],
                      distance = d[sel],
                      stringsAsFactors = FALSE)

  } else if (type == "cases") {
    if (any(abs(c(origin, destination)) %in% seq_len(ct) == FALSE)) {
      txt1 <- 'With type = "cases" and observed = '
      txt2 <- ', the absolute values of origin and of destination must be '
      txt3 <- 'between 1 and '
      stop(txt1, observed, txt2, txt3, ct, ".")
    }

    if (observed) {
      ego.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
        origin, "anchor.case"])
      ego <- cholera::ortho.proj[cholera::ortho.proj$case == ego.id, ]

      if (is.null(destination)) {
        alters.id <- cholera::fatalities.address$anchor.case
      } else {
        if (all(destination > 0)) {
          alters.id <- unique(cholera::ortho.proj[cholera::ortho.proj$case
            %in% destination, "case"])
        } else if (all(destination < 0)) {
          alters.id <- unique(cholera::ortho.proj[cholera::ortho.proj$case
            %in% abs(destination) == FALSE, "case"])
        }
      }
    } else {
      ego.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case == origin,
        "case"]
      ego <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case == origin, ]

      if (is.null(destination)) {
        alters.id <- cholera::sim.ortho.proj$case
      } else {
        if (all(destination > 0)) {
          alters.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case %in%
            destination, "case"]
        } else if (all(destination < 0)) {
          alters.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case %in%
            abs(destination) == FALSE, "case"]
        }
      }
    }

    if (identical(all.equal(ego.id, alters.id), TRUE)) {
      out <- data.frame(caseA = origin,
                        caseB = destination,
                        anchorA = ego.id,
                        anchorB = alters.id,
                        distance = 0,
                        stringsAsFactors = FALSE)
    } else {
      vars <- c("x.proj", "y.proj")

      alters <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case %in%
        alters.id, ]
      alters <- alters[alters$case != ego.id, ]

      d <- vapply(alters$case, function(i) {
        dat <- rbind(ego[, vars], alters[alters$case == i, vars])
        c(stats::dist(dat))
      }, numeric(1L))

      sel <- which.min(d)

      out <- data.frame(caseA = origin,
                        caseB = alters$case[sel],
                        anchorA = ego$case,
                        anchorB = alters$case[sel],
                        distance = d[sel],
                        stringsAsFactors = FALSE)
    }

  } else if (type == "pumps") {
    if (origin %in% p.ID == FALSE) {
      stop('With vestry = ', vestry, ', 1 >= |origin| <= ', p.count, ".")
    }

    ego <- p.data[p.data$pump.id == origin, ]

    if (!is.null(destination)) {
      if (any(abs(destination) %in% p.ID == FALSE)) {
        stop('With vestry = ', vestry, ', 1 >= |destination| <= ', p.count, ".")
      } else {
        if (all(destination > 0)) {
          alters <- p.data[destination, ]
        } else if (all(destination < 0)) {
          alters <- p.data[p.data$pump.id %in% abs(destination) == FALSE, ]
        }
        alters <- alters[alters$pump.id != origin, ]
      }
    } else {
      alters <- p.data[p.data$pump.id != origin, ]
    }

    d <- vapply(alters$pump.id, function(i) {
      dat <- rbind(ego[, c("x.proj", "y.proj")], alters[alters$pump.id == i,
        c("x.proj", "y.proj")])
      c(stats::dist(dat))
    }, numeric(1L))

    sel <- which.min(d)
    out <- data.frame(pumpA = ego$pump.id,
                      pumpB = alters$pump.id[sel],
                      pump.nameA = ego$street,
                      pump.nameB = alters$street[sel],
                      distance = d[sel],
                      stringsAsFactors = FALSE)
  }

  out$time <- cholera::distanceTime(out$distance, unit = time.unit,
    speed = walking.speed)

  if (unit == "meter") {
    out$distance <- cholera::unitMeter(out$distance, "meter")
  } else if (unit == "yard") {
    out$distance <- cholera::unitMeter(out$distance, "yard")
  } else if (unit == "native") {
    out$distance <- cholera::unitMeter(out$distance, "native")
  }

  output <- list(ego = ego[, c("x.proj", "y.proj")],
                 alter = alters[sel, c("x.proj", "y.proj")],
                 origin = origin,
                 destination = destination,
                 type = type,
                 observed = observed,
                 alters = alters,
                 sel = sel,
                 vestry = vestry,
                 unit = unit,
                 time.unit = time.unit,
                 d = out$distance,
                 t = out$time,
                 speed = walking.speed,
                 data = out)

  class(output) <- "euclidean_path"
  output
}

#' Summary of euclideanPath().
#'
#' @param x An object of class "euclidean_path" created by \code{euclideanPath()}.
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export
#' @examples
#' euclideanPath(1)
#' print(euclideanPath(1))

print.euclidean_path <- function(x, ...) {
  if (class(x) != "euclidean_path") {
    stop('"x"\'s class must be "euclidean_path".')
  }

  print(x[c("ego", "alter", "data")])
}

#' Plot the path of the Euclidean distance between cases and/or pumps.
#'
#' @param x An object of class "euclidean_path" created by euclideanPath().
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param unit.posts Character. "distance" for mileposts; "time" for timeposts; \code{NULL} for no posts.
#' @param unit.interval Numeric. Set interval between posts. When \code{unit.posts} is "distance", \code{unit.interval} automatically defaults to 50 meters. When \code{unit.posts} is "time", \code{unit.interval} automatically defaults to 60 seconds.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @examples
#' plot(euclideanPath(15))
#' plot(euclideanPath(15), unit.posts = "time")

plot.euclidean_path <- function(x, zoom = TRUE, radius = 0.5,
  unit.posts = "distance", unit.interval = NULL, ...) {

  if (class(x) != "euclidean_path") {
    stop('"x"\'s class must be "euclidean_path".')
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  colors <- cholera::snowColors(x$vestry)

  origin.xy <- x$ego
  alter.xy <- x$alter
  dat <- stats::setNames(rbind(alter.xy, origin.xy), c("x", "y"))

  if (zoom) {
    x.rng <- c(min(dat$x) - radius, max(dat$x) + radius)
    y.rng <- c(min(dat$y) - radius, max(dat$y) + radius)
  } else {
    x.rng <- range(cholera::roads$x)
    y.rng <- range(cholera::roads$y)
  }

  plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
    xlab = "x", ylab = "y", pch = 15, cex = 0.5, col = "lightgray", asp = 1)
  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(border.list, lines))

  if (x$type == "case-pump") {
    destination.pump <- row.names(x$alter)
    case.color <- colors[paste0("p", destination.pump)]
    points(origin.xy, col = "red")
    pumpToken(x, case.color, destination.pump)
    title(main = paste("Case", x$origin, "to Pump", row.names(x$alter)))
  } else if (x$type == "cases" | x$type == "pumps") {
    case.color <- "blue"
    destination.case <- row.names(x$alter)
    points(origin.xy, col = case.color)
    points(alter.xy, col = case.color)
    text(origin.xy, labels = x$origin, pos = 1, col = case.color)
    text(alter.xy, labels = destination.case, pos = 1, col = case.color)
  }

  if (x$type == "cases") {
    title(main = paste("Case", x$origin, "to Case", destination.case))
  } else if (x$type == "pumps") {
    title(main = paste("Pump", x$origin, "to Pump", destination.case))
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

    title(sub = paste(round(x$d, 1), d.unit, nominal.time, "@", x$speed,
      "km/hr"))
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
        stop('Specify unit.posts.')
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

    title(sub = paste(round(x$d, 1), d.unit, nominal.time, "@", x$speed,
      "km/hr;", post.info))
  }
}

pumpToken <- function(x, case.color, destination.pump) {
  if (x$vestry) {
    obs.pump <- cholera::pumps.vestry
    ortho.pump <- cholera::ortho.proj.pump.vestry
  } else {
    obs.pump <- cholera::pumps
    ortho.pump <- cholera::ortho.proj.pump
  }

  obs.data <- obs.pump[obs.pump$id == as.numeric(destination.pump), ]
  ortho.data <- ortho.pump[ortho.pump$pump.id == as.numeric(destination.pump), ]
  points(obs.data[, c("x", "y")], pch = 24, cex = 1, col = case.color)
  text(obs.data[, c("x", "y")], label = paste0("p", destination.pump), pos = 1)
  points(ortho.data[, c("x.proj", "y.proj")], pch = 0, col = "red")
}
