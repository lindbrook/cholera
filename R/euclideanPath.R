#' Compute path of the Euclidean distance between cases and/or pumps (Beta).
#'
#' @param origin Numeric or Character. Numeric ID of case or pump. Character landmark name.
#' @param destination Numeric or Character. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}, which returns closest pump or "anchor" case. Character landmark name.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param case.location Character. For observed = FALSE: "address" or "nominal". "nominal" is the x-y coordinate of \code{regular.cases}.
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

euclideanPath <- function(origin = 1, destination = NULL, type = "case-pump",
  observed = TRUE, case.location = "address", vestry = FALSE, unit = "meter",
  time.unit = "second", walking.speed = 5) {

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('unit must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('time.unit must be "hour", "minute" or "second".')
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('type must be "case-pump", "cases" or "pumps".')
  }

  if (is.character(destination)) {
    if (type != "cases") stop('type must be "cases".')
  }

  if (case.location %in% c("address", "nominal") == FALSE) {
    stop('case.location must be "address" or "nominal".')
  }

  obs.ct <- nrow(cholera::fatalities)
  exp.ct <- nrow(cholera::regular.cases)
  if (observed) ct <- obs.ct else ct <- exp.ct

  if (case.location == "address") {
    if (vestry) {
      p.data <- cholera::ortho.proj.pump.vestry
      p.data$street <- cholera::pumps.vestry$street
    } else {
      p.data <- cholera::ortho.proj.pump
      p.data$street <- cholera::pumps$street
    }
  } else if (case.location == "nominal") {
    if (vestry) {
      p.data <- cholera::pumps.vestry
    } else {
      p.data <- cholera::pumps
    }
    names(p.data)[names(p.data) %in% c("id", "x", "y")] <-
      c("pump.id", "x.proj", "y.proj")
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  # ----- #

  if (type == "case-pump") {
    if (is.numeric(origin)) {
      if (abs(origin) %in% seq_len(ct) == FALSE) {
        txt1 <- 'With observed = '
        txt2 <- ', 1 >= |origin| <= '
        stop(txt1, observed, txt2, ct, ".")
      }
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
      if (is.numeric(origin)) {
        ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
          "anchor.case"]
        ego <- cholera::ortho.proj[cholera::ortho.proj$case == ego.id,
          c("x.proj", "y.proj")]
      } else if (is.character(origin)) {
        origin <- caseAndSpace(origin)

        if (grepl("Square", origin)) {
          sel <- cholera::landmarks.squares$name == origin
          ego.id <- cholera::landmarks.squares[sel, "case"]
          ego <- cholera::landmarks.squares[sel, c("x.proj", "y.proj")]
        } else if (origin %in% cholera::landmarks$name) {
          ego.id <- cholera::landmarks[cholera::landmarks$name == origin,
            "case"]
          ego <- cholera::landmarks[cholera::landmarks$case == ego.id,
            c("x.proj", "y.proj")]
        } else stop('Use a valid landmark name.')
      }

      d <- vapply(alters$pump.id, function(i) {
        c(stats::dist(rbind(alters[alters$pump.id == i, c("x.proj", "y.proj")],
          ego)))
      }, numeric(1L))

    } else {
      if (case.location == "address") {
        ego.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case ==
          origin, "case"]
        ego <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case == origin,
          c("x.proj", "y.proj")]

        d <- vapply(alters$pump.id, function(i) {
          c(stats::dist(rbind(alters[alters$pump.id == i, c("x.proj",
            "y.proj")], ego)))
        }, numeric(1L))

      } else if (case.location == "nominal") {
        ego.id <- origin
        ego <- cholera::regular.cases[origin, c("x", "y")]
        names(alters)[names(alters) %in% c("x.proj", "y.proj")] <- c("x", "y")

        d <- vapply(alters$pump.id, function(i) {
          c(stats::dist(rbind(alters[alters$pump.id == i, c("x", "y")],
            ego)))
        }, numeric(1L))

      } else stop('1 >= |origin| <= ', nrow(cholera::sim.ortho.proj), "!")
    }

    sel <- which.min(d)

    out <- data.frame(case = origin,
                      anchor = ego.id,
                      pump = alters[sel, "pump.id"],
                      pump.name = alters[sel, "street"],
                      distance = d[sel],
                      stringsAsFactors = FALSE)

  # ----- #

  } else if (type == "cases") {
    if (is.null(destination) == FALSE) {
      if (is.numeric(origin) & is.numeric(destination)) {
        if (any(abs(c(origin, destination)) %in% seq_len(ct) == FALSE)) {
          txt1 <- 'With type = "cases" and observed = '
          txt2 <- ', the absolute value of origin and destination must be '
          txt3 <- 'between 1 and '
          stop(txt1, observed, txt2, txt3, ct, ".")
        }
      }
    }

    if (observed) {
      if (is.numeric(origin)) {
        if (origin <= nrow(cholera::fatalities)) {
          ego.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
            origin, "anchor.case"])
          ego <- cholera::ortho.proj[cholera::ortho.proj$case == ego.id, ]
        } else stop('1 >= |origin| <= ', nrow(cholera::fatalities), "!")

      } else if (is.character(origin)) {
        origin <- caseAndSpace(origin)

        if (grepl("Square", origin)) {
          sel <- cholera::landmarks.squares$name == origin
          ego.id <- cholera::landmarks.squares[sel, "case"]
          ego <- cholera::landmarks.squares[sel, ]
        } else if (origin %in% cholera::landmarks$name) {
          ego.id <- cholera::landmarks[cholera::landmarks$name == origin,
            "case"]
          ego <- cholera::landmarks[cholera::landmarks$case == ego.id, ]
        } else stop('Use a valid landmark name for origin.')
      }

      if (is.null(destination)) {
        alters.id <- cholera::fatalities.address$anchor.case
        alters <- cholera::ortho.proj[cholera::ortho.proj$case %in% alters.id, ]
      } else {
        if (is.numeric(destination)) {
          if (all(destination > 0)) {
            alters.id <- unique(cholera::ortho.proj[cholera::ortho.proj$case
              %in% destination, "case"])
          } else if (all(destination < 0)) {
            alters.id <- unique(cholera::ortho.proj[cholera::ortho.proj$case
              %in% abs(destination) == FALSE, "case"])
          }

          alters <- cholera::ortho.proj[cholera::ortho.proj$case %in%
            alters.id, ]
        } else if (is.character(destination)) {
          destination <- caseAndSpace(destination)

          if (grepl("Square", destination)) {
            sel <- cholera::landmarks.squares$name == destination
            alters.id <- cholera::landmarks.squares[sel, "case"]
            alters <- cholera::landmarks.squares[sel, ]
          } else if (destination %in% cholera::landmarks$name) {
            alters.id <- cholera::landmarks[cholera::landmarks$name ==
              destination, "case"]
            alters <- cholera::landmarks[cholera::landmarks$case == alters.id, ]
          } else stop('Use a valid landmark name for destination.')
        }
      }

    } else {
      if (is.numeric(origin)) {
        if (case.location == "address") {
          ego.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case ==
            origin, "case"]
          ego <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case == origin,
            c("x.proj", "y.proj")]
        } else if (case.location == "nominal") {
          ego.id <- origin
          ego <- cholera::regular.cases[origin, c("x", "y")]
        } else stop('1 >= |origin| <= ', nrow(cholera::sim.ortho.proj), "!")

      } else if (is.character(origin)) {
        origin <- caseAndSpace(origin)

        if (grepl("Square", origin)) {
          sel <- cholera::landmarks.squares$name == origin
          ego.id <- cholera::landmarks.squares[sel, "case"]
          ego <- cholera::landmarks.squares[sel, ]
        } else if (origin %in% cholera::landmarks$name) {
          ego.id <- cholera::landmarks[cholera::landmarks$name == origin,
            "case"]
          ego <- cholera::landmarks[cholera::landmarks$case == ego.id, ]
        } else stop('Use a valid landmark name for origin.')
      }

      if (is.null(destination) | is.numeric(destination)) {
        if (case.location == "address") {
            alters <- cholera::sim.ortho.proj
            alters.id <- cholera::sim.ortho.proj$case
          if (is.numeric(destination)) {
            if (any(abs(destination) %in% alters.id) == FALSE) {
              stop('1 >= |destination| <= ', max(alters.id), "!")
            }

            if (all(destination > 0)) {
              alters.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case
                %in% destination, "case"]
            } else if (all(destination < 0)) {
              alters.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case
                %in% abs(destination) == FALSE, "case"]
            } else stop("Use all positive or all negative selection.")

            alters <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case %in%
              alters.id, ]
          }

        } else if (case.location == "nominal") {
          alters <- cholera::regular.cases
          alters.id <- seq_len(nrow(alters))

          if (is.numeric(destination)) {
            if (any(destination %in% alters.id) == FALSE) {
              stop('1 >= |destination| <= ', max(alters.id), "!")
            }

            if (all(destination > 0)) {
              alters.id <- alters.id[alters.id %in% destination]
            } else if (all(destination < 0)) {
              alters.id <- alters.id[alters.id %in% abs(destination) == FALSE]
            }

            alters <- alters[alters.id, ]
          }
        }

      } else if (is.character(destination)) {
        destination <- caseAndSpace(destination)

        if (grepl("Square", destination)) {
          sel <- cholera::landmarks.squares$name == destination
          alters.id <- cholera::landmarks.squares[sel, "case"]
          alters <- cholera::landmarks.squares[sel, ]
        } else if (destination %in% cholera::landmarks$name) {
          alters.id <- cholera::landmarks[cholera::landmarks$name ==
            destination, "case"]
          alters <- cholera::landmarks[cholera::landmarks$case == ego.id, ]
        } else stop('Use a valid landmark name for destination.')
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
      if (case.location == "address") {
        vars <- c("x.proj", "y.proj")
      } else if (case.location == "nominal") {
        vars <- c("x", "y")
      }

      alters <- alters[alters.id != ego.id, ]

      d <- vapply(seq_len(nrow(alters)), function(i) {
        dat <- rbind(ego[, vars], alters[i, vars])
        c(stats::dist(dat))
      }, numeric(1L))

      sel <- which.min(d)

      if (is.character(origin) & is.character(destination)) {
        out <- data.frame(caseA = origin,
                          caseB = destination,
                          anchorA = ego.id,
                          anchorB = alters$case[sel],
                          distance = d[sel],
                          stringsAsFactors = FALSE)
      } else if (!is.character(origin) & is.character(destination)) {
        out <- data.frame(caseA = origin,
                          caseB = destination,
                          anchorA = ego.id,
                          anchorB = alters.id,
                          distance = d,
                          stringsAsFactors = FALSE)
      } else if (is.character(origin) & !is.character(destination)) {
        out <- data.frame(caseA = origin,
                          caseB = alters$case[sel],
                          anchorA = ego.id,
                          anchorB = alters$case[sel],
                          distance = d[sel],
                          stringsAsFactors = FALSE)
      } else if (!is.character(origin) & !is.character(destination)) {
        if (observed) {
          out <- data.frame(caseA = origin,
                            caseB = alters$case[sel],
                            anchorA = ego$case,
                            anchorB = alters$case[sel],
                            distance = d[sel],
                            stringsAsFactors = FALSE)
        } else {
          out <- data.frame(caseA = origin,
                            caseB = row.names(alters[sel, ]),
                            anchorA = row.names(ego),
                            anchorB = row.names(alters[sel, ]),
                            distance = d[sel],
                            stringsAsFactors = FALSE)
        }
      }
    }

  # ----- #

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

  if (observed | (observed == FALSE & case.location == "address")) {
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
  } else {
    output <- list(ego = ego[, c("x", "y")],
                   alter = alters[sel, c("x", "y")],
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
  }

  class(output) <- "euclidean_path"
  output
}

#' Print method for euclideanPath().
#'
#' Summary output.
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

  ego.xy <- stats::setNames(x$ego, c("x", "y"))
  alter.xy <- stats::setNames(x$alter, c("x", "y"))
  dat <- rbind(alter.xy, ego.xy)

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
    points(ego.xy, col = "red")
    pumpTokensEuclidean(x, case.color, destination.pump)
    text(ego.xy, labels = x$origin, pos = 1)
  } else if (x$type == "cases" | x$type == "pumps") {
    case.color <- "blue"
    destination.case <- row.names(x$alter)
    points(ego.xy, col = case.color)
    points(alter.xy, col = case.color)
    text(ego.xy, labels = x$origin, pos = 1, col = case.color)

    if (is.character(x$destination)) {
      text(alter.xy, labels = x$destination, pos = 1, col = case.color)
    } else {
      text(alter.xy, labels = destination.case, pos = 1, col = case.color)
    }
  }

  if (x$type == "case-pump") {
    if (is.numeric(x$origin)) {
      title(main = paste("Case", x$origin, "to Pump", destination.pump))
    } else if (is.character(x$origin)) {
      title(main = paste(x$origin, "to Pump", destination.pump))
    }
  } else if (x$type == "cases") {
    if (is.numeric(x$origin) &
      (is.numeric(x$destination) | is.null(x$destination))) {
      title(main = paste("Case", x$origin, "to Case", destination.case))
    } else if (is.character(x$origin) & (is.numeric(x$destination) |
      is.null(x$destination))) {
      title(main = paste(x$origin, "to Case", destination.case))
    } else if (is.numeric(x$origin) & is.character(x$destination)) {
      title(main = paste("Case", x$origin, "to", x$destination))
    } else if (is.character(x$origin) & is.character(x$destination)) {
      title(main = paste(x$origin, "to", x$destination))
    }
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
    arrows(ego.xy$x, ego.xy$y, alter.xy$x, alter.xy$y, col = case.color,
      lwd = 3, length = 0.075)
    title(sub = paste(round(x$d, 1), d.unit, nominal.time, "@", x$speed,
      "km/hr"))
  } else {
    if (unit.posts %in% c("distance", "time") == FALSE) {
      stop('If specified, unit.posts must be "distance" or "time".')
    } else {
      if (is.null(unit.interval)) {
        if (unit.posts == "distance")  {
          unit.interval <- 50 * x$speed / 5
        } else if (unit.posts == "time") {
          unit.interval <- 60 * x$speed / 5
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

      post.coords <- quandrantCoordinates(dat, h, theta)

      arrow.data <- data.frame(x = c(post.coords$x, ego.xy$x),
                               y = c(post.coords$y, ego.xy$y))

      arrow.list <- lapply(seq_len(nrow(arrow.data) - 1), function(i) {
        a.data <- cbind(arrow.data[i, ], arrow.data[i + 1, ])
        stats::setNames(a.data, c("x1", "y1", "x2", "y2"))
      })

      invisible(lapply(arrow.list, function(seg) {
        zero.length.x <- round(abs(seg$x1 - seg$x2), 2) == 0
        zero.length.y <- round(abs(seg$y1 - seg$y2), 2) == 0

        if (any(zero.length.x | zero.length.y)) {
          drawPath(dat, case.color, compute.coords = FALSE)
          text(seg[, c("x1", "y1")], labels = ">", srt = theta * 180L / pi,
            col = case.color, cex = 1.5)
        } else {
          arrows(seg$x1, seg$y1, seg$x2, seg$y2, length = 0.075,
            col = case.color, lwd = 3, code = 1)
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

pumpTokensEuclidean <- function(x, case.color, destination.pump) {
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
