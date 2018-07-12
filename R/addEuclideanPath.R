#' Add the path for the Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is NULL: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Default is 5 km/hr.
#' @param unit.posts Character. "distance" for mileposts; "time" for timeposts.
#' @param unit.interval Numeric. Sets interval between posts: for "distance",
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @note The function uses a case's "address" (i.e., "anchor" case of a stack) to compute distance. Time is computed using distanceTime().
#' @return An R list with 3 data frames: x-y coordinates for the origin and destination, and a summary of results.
#' @export

addEuclideanPath <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, vestry = FALSE, unit = "meter", time.unit = "second",
  walking.speed = 5, unit.posts = "distance", unit.interval = NULL,
  alpha.level = 1) {

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('"unit" must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('"time.unit" must be "hour", "minute" or "second".')
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('"type" must be "case-pump", "cases" or "pumps".')
  }

  if (isFALSE(alpha.level > 0 | alpha.level <= 1)) {
    stop('"alpha.level" must be > 0 and <= 1')
  }

  n.sim.obs <- nrow(cholera::regular.cases)

  if (type == "case-pump") {
    if (observed) {
      if (origin %in% 1:578 == FALSE) {
        txt1 <- 'With type = "case-pump" and "observed" = TRUE,'
        txt2 <- '"origin" must be between 1 and 578.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (origin %in% 1:n.sim.obs == FALSE) {
        txt1 <- 'With type = "case-pump" and "observed" = FALSE,'
        txt2 <- '"origin" must be between 1 and'
        stop(paste(txt1, txt2, n.sim.obs, "."))
      }
    }

    if (!is.null(destination)) {
      if (vestry) {
        if (any(abs(destination) %in% 1:14 == FALSE)) {
          txt1 <- 'With type = "case-pump" and "vestry = TRUE",'
          txt2 <- '1 >= |destination| <= 14.'
          stop(paste(txt1, txt2))
        } else {
          alters <- cholera::pumps.vestry[destination, ]
        }
      } else {
        if (any(abs(destination) %in% 1:13 == FALSE)) {
          txt1 <- 'With type = "case-pump" and "vestry = FALSE",'
          txt2 <- '1 >= |destination| <= 13.'
          stop(paste(txt1, txt2))
        } else {
          alters <- cholera::pumps[destination, ]
        }
      }
    } else {
      if (vestry) {
        alters <- cholera::pumps.vestry
      } else {
        alters <- cholera::pumps
      }
    }

    if (observed) {
      ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
        "anchor.case"]
      ego <- cholera::fatalities[cholera::fatalities$case == ego.id,
        c("x", "y")]
    } else {
      ego <- cholera::regular.cases[origin, ]
      ego.id <- as.numeric(row.names(ego))
    }

    d <- vapply(alters$id, function(i) {
      c(stats::dist(rbind(alters[alters$id == i, c("x", "y")], ego)))
    }, numeric(1L))

    sel <- which.min(d)
    out <- data.frame(case = origin,
                      anchor = ego.id,
                      pump = alters[sel, "id"],
                      pump.name = alters[sel, "street"],
                      distance = d[sel],
                      stringsAsFactors = FALSE)

  } else if (type == "cases") {
    if (observed) {
      if (any(abs(c(origin, destination)) %in% 1:578 == FALSE)) {
        txt1 <- 'With type = "cases", the absolute value of both "origin"'
        txt2 <- 'and "destination" must be between 1 and 578.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (any(abs(c(origin, destination)) %in% 1:n.sim.obs == FALSE)) {
        txt1 <- 'With type = "cases", the absolute value of both "origin"'
        txt2 <- 'and "destination" must be between 1 and'
        stop(paste(txt1, txt2, n.sim.obs, "."))
      }
    }

    if (observed) {
      ego.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
        origin, "anchor.case"])
      ego <- cholera::fatalities[cholera::fatalities$case == ego.id, ]
    } else {
      ego <- cholera::regular.cases[origin, ]
      ego.id <- as.numeric(row.names(ego))
    }

    if (observed) {
      if (is.null(destination)) {
        alters.id <- cholera::fatalities.address$anchor.case
      } else {
        if (all(destination > 0)) {
          alters.id <- unique(cholera::anchor.case[cholera::anchor.case$case
            %in% destination, "anchor.case"])
        } else if (all(destination < 0)) {
          alters.id <- unique(cholera::anchor.case[cholera::anchor.case$case
            %in% abs(destination) == FALSE, "anchor.case"])
        }
      }
    } else {
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
      alters <- cholera::fatalities[cholera::fatalities$case %in% alters.id, ]
      alters <- alters[alters$case != ego.id, ]

      d <- vapply(alters$case, function(i) {
        dat <- rbind(ego[, c("x", "y")], alters[alters$case == i, c("x", "y")])
        c(stats::dist(dat))
      }, numeric(1L))

      sel <- which.min(d)

      if (is.null(destination) | all(destination < 0)) {
        out <- data.frame(caseA = origin,
                          caseB = alters$case[sel],
                          anchorA = ego$case,
                          anchorB = alters$case[sel],
                          distance = d[which.min(d)],
                          stringsAsFactors = FALSE)
      } else if (all(destination > 0)) {
        if (length(destination) == 1) {
          out <- data.frame(caseA = origin,
                            caseB = destination,
                            anchorA = ego$case,
                            anchorB = alters$case[sel],
                            distance = d[which.min(d)],
                            stringsAsFactors = FALSE)
        } else if (length(destination) > 1) {
          out <- data.frame(caseA = origin,
                            caseB = destination[sel],
                            anchorA = ego$case,
                            anchorB = alters$case[sel],
                            distance = d[which.min(d)],
                            stringsAsFactors = FALSE)
        }
      }
    }

  } else if (type == "pumps") {
    if (!is.null(destination)) {
      if (vestry) {
        if (any(abs(c(origin, destination)) %in% 1:14 == FALSE)) {
          txt1 <- 'With type = "pumps" and "vestry = TRUE",'
          txt2 <- 'origin and destination must be 1 >= |x| <= 14.'
          stop(paste(txt1, txt2))
        } else {
          ego <- cholera::pumps.vestry[cholera::pumps.vestry$id == origin, ]
          alters <- cholera::pumps.vestry[destination, ]
          alters <- alters[alters$id != origin, ]
        }
      } else {
        if (any(abs(c(origin, destination)) %in% 1:13 == FALSE)) {
          txt1 <- 'With type = "pumps" and "vestry = FALSE",'
          txt2 <- 'origin and destination must be 1 >= |x| <= 13.'
          stop(paste(txt1, txt2))
        } else {
          ego <- cholera::pumps[cholera::pumps$id == origin, ]
          alters  <- cholera::pumps[destination, ]
          alters <- alters[alters$id != origin, ]
        }
      }
    } else {
      if (vestry) {
        ego <- cholera::pumps.vestry[cholera::pumps.vestry$id == origin, ]
        alters <- cholera::pumps.vestry[cholera::pumps.vestry$id != origin, ]
      } else {
        ego <- cholera::pumps[cholera::pumps$id == origin, ]
        alters <- cholera::pumps[cholera::pumps$id != origin, ]
      }
    }

    d <- vapply(alters$id, function(i) {
      dat <- rbind(ego[, c("x", "y")], alters[alters$id == i, c("x", "y")])
      c(stats::dist(dat))
    }, numeric(1L))

    sel <- which.min(d)
    out <- data.frame(pumpA = ego$id,
                      pumpB = alters$id[sel],
                      pump.nameA = ego$street,
                      pump.nameB = alters$street[sel],
                      distance = d[which.min(d)],
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

  x <- list(ego = ego[, c("x", "y")],
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
                 summary = out)

  colors <- cholera::snowColors(x$vestry)

  if (x$vestry) {
    pmp <- cholera::pumps.vestry
  } else {
    pmp <- cholera::pumps
  }

  end.sel <- x$sel

  if (x$type == "case-pump" | x$type == "pumps") {
    end.pump <- x$alters$id[end.sel]
    alter.xy <- pmp[pmp$id == end.pump, c("x", "y")]
  } else if (x$type == "cases") {
    end.case <- x$alters$case[end.sel]

    if (x$observed) {
      alter.xy <- cholera::fatalities[cholera::fatalities$case == end.case,
        c("x", "y")]
    } else {
      alter.xy <- cholera::regular.cases[end.case, c("x", "y")]
    }
  }

  if (x$type == "case-pump" | x$type == "cases") {
    if (x$observed) {
      origin.xy <- cholera::fatalities[cholera::fatalities$case == x$origin,
        c("x", "y")]
      ego.id <- cholera::anchor.case[cholera::anchor.case$case == x$origin,
        "anchor.case"]
      addr <- cholera::fatalities.address
      ego.xy <- addr[addr$anchor.case == ego.id,  c("x", "y")]
    } else {
      origin.xy <- cholera::regular.cases[x$origin, c("x", "y")]
      ego.id <-  x$origin
      ego.xy <- origin.xy
    }

  } else if (x$type == "pumps") {
    origin.xy <- pmp[pmp$id == x$origin, c("x", "y")]
    ego.xy <- origin.xy
  }

  dat <- rbind(alter.xy, origin.xy)

  if (x$type == "case-pump") {
    case.color <- colors[end.pump]

    if (alpha.level != 1) {
      case.color <- grDevices::adjustcolor(case.color, alpha.f = alpha.level)
    }

    points(origin.xy, col = "red")
    title(main = paste("Case", x$origin, "to Pump", x$alters$id[end.sel]))
  } else if (x$type == "cases") {
    case.color <- "blue"
    points(origin.xy, col = case.color)
    points(alter.xy, col = case.color)
    text(origin.xy, labels = x$origin, pos = 1, col = case.color)
    text(alter.xy, labels = end.case, pos = 1, col = case.color)
    title(main = paste("Case", x$origin, "to Case", end.case))
  } else if (x$type == "pumps") {
    case.color <- "blue"
    title(main = paste("Pump", x$origin, "to Pump", x$alters$id[end.sel]))
  }

  if (x$vestry) {
    pump.names <- paste0("p", cholera::pumps.vestry$id)
    points(cholera::pumps.vestry[, c("x", "y")], pch = 24, cex = 1,
      col = colors)
    text(cholera::pumps.vestry[, c("x", "y")], label = pump.names, pos = 1)
  } else {
    pump.names <- paste0("p", cholera::pumps$id)
    points(cholera::pumps[, c("x", "y")], pch = 24, cex = 1, col = colors)
    text(cholera::pumps[, c("x", "y")], label = pump.names, pos = 1)
  }

  # mileposts #

  if (unit.posts %in% c("distance", "time") == FALSE) {
    stop('"unit.posts" must be "distance" or "time".')
  } else {
    if (is.null(unit.interval)) {
      if (unit.posts == "distance")  {
        unit.interval <- 50
      } else if (unit.posts == "time") {
        unit.interval <- 60
      }
    } else {
      if (!is.numeric(unit.interval)) {
        stop('"unit.interval" must be numeric.')
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
      stop('specify a "unit.posts"')
    }

    ols <- stats::lm(y ~ x, data = dat)
    edge.slope <- stats::coef(ols)[2]
    edge.intercept <- stats::coef(ols)[1]
    theta <- atan(edge.slope)

    p.coords <- quandrantCoordinates(dat, h, theta)
    post.data <- data.frame(x = c(p.coords$x, ego.xy$x),
                            y = c(p.coords$y, ego.xy$y))

    a.data <- cbind(post.data[-nrow(post.data), ], post.data[-1, ])
    a.data <- stats::setNames(a.data, c("x1", "y1", "x2", "y2"))

    invisible(lapply(seq_len(nrow(a.data)), function(i) {
      dataB <- data.frame(x = c(a.data[i, "x1"], a.data[i, "x2"]),
                          y = c(a.data[i, "y1"], a.data[i, "y2"]))

      zero.length.x <- round(abs(dataB[1, "x"] - dataB[2, "x"]), 2) == 0
      zero.length.y <- round(abs(dataB[1, "y"] - dataB[2, "y"]), 2) == 0

      if (any(zero.length.x | zero.length.y)) {
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
