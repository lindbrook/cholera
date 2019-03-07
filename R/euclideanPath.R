#' Compute path of the Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Character. Numeric ID of case or pump. Character landmark name.
#' @param destination Numeric or Character. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}, which returns the closest pump, "anchor" case or landmark.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param case.location Character. For \code{observed = FALSE}: "address" or "nominal". "nominal" is the x-y coordinates of \code{regular.cases}.
#' @param landmark.cases Logical. \code{TRUE} includes landmarks as cases.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Default is 5 km/hr.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @note The function uses a case's "address" (i.e., "anchor" case of a stack) to compute distance. Time is computed using \code{distanceTime()}.
#' @return An R list with 3 data frames: x-y coordinates for the origin and destination, and a summary of results.
#' @export
#' @examples
#' # path from case 1 to nearest pump.
#' euclideanPath(1)
#'
#' # path from pump 1 to nearest case.
#' euclideanPath(NULL, 1)
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
#' # compute multiple cases.
#' lapply(1:3, euclideanPath)
#'
#' # plot path
#' plot(euclideanPath(1))

euclideanPath <- function(origin = 1, destination = NULL, type = "case-pump",
  observed = TRUE, case.location = "nominal", landmark.cases = TRUE,
  vestry = FALSE, distance.unit = "meter", time.unit = "second",
  walking.speed = 5, multi.core = FALSE) {

  if (is.null(origin) & is.null(destination)) {
    stop("If origin = NULL, you must supply a destination.")
  }

  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
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

  cores <- multiCore(multi.core)

  obs.ct <- nrow(cholera::fatalities)
  exp.ct <- nrow(cholera::regular.cases)

  if (observed) {
    ct <- obs.ct
  } else {
    ct <- exp.ct
  }

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
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  if (case.location == "address") {
    coords <- c("x.proj", "y.proj")
    pump.var <- "pump.id"
  } else if (case.location == "nominal") {
    coords <- c("x", "y")
    pump.var <- "id"
  }

  case.coords <- c("case", coords)

  # ----- #

  if (type == "case-pump") {
    if (is.null(destination)) {
      if (is.null(origin)) {
        stop("If origin is set to NULL, you must provide a destination pump!")
      } else {
        alters <- p.data
      }
    } else {
      if (any(abs(destination) %in% p.ID)) {
        alters <- p.data[destination, ]
      } else {
        stop('With vestry = ', vestry, ', 1 >= |destination| <= ', p.count)
      }
    }

    if (is.null(origin)) {
      if (observed) {
        if (case.location == "address") {
          egos <- cholera::ortho.proj[, case.coords]
        } else if (case.location == "nominal") {
          egos <- cholera::fatalities[, case.coords]
        }
      } else {
        if (case.location == "address") {
          egos <- cholera::sim.ortho.proj[, case.coords]
        } else if (case.location == "nominal") {
          case.id <- seq_len(nrow(cholera::regular.cases))
          egos <- data.frame(case = case.id, cholera::regular.cases)
        }
      }

      if (landmark.cases) {
        egos <- rbind(egos, cholera::landmarks[, case.coords])
      }

      d <- vapply(seq_len(nrow(egos)), function(i) {
        c(stats::dist(rbind(egos[i, coords], alters[, coords])))
      }, numeric(1L))

      sel <- which.min(d)
      nearest.case <- egos$case[sel]

      if (nearest.case > 20000) {
        anchor.sel <- cholera::landmarks$case == nearest.case
        anchor <- cholera::landmarks[anchor.sel, "name"]
        ego <- cholera::landmarks[anchor.sel, coords]
      } else {
        anchor <- nearest.case
        if (observed) {
          if (case.location == "address") {
            ego.select <- cholera::ortho.proj$case == anchor
            ego <- cholera::ortho.proj[ego.select, ]
          } else if (case.location == "nominal") {
            ego.select <- cholera::fatalities$case == anchor
            ego <- cholera::fatalities[ego.select, ]
          }
        } else {
          if (case.location == "address") {
            ego.select <- cholera::sim.ortho.proj$case == anchor
            ego <- cholera::sim.ortho.proj[ego.select, ]
          } else if (case.location == "nominal") {
            ego <- cholera::regular.cases[anchor, ]
          }
        }
      }

      ego <- ego[, coords]
      alter <- alters[, coords]

      out <- data.frame(case = anchor,
                        anchor = nearest.case,
                        pump.name = alters[, "street"],
                        pump = alters[, pump.var],
                        distance = d[sel],
                        stringsAsFactors = FALSE)

    } else {
      if (is.numeric(origin)) {
        if (origin %in% seq_len(ct)) {
          if (observed) {
            if (case.location == "address") {
              ego.sel <- cholera::ortho.proj$case == origin
              ego.id <- cholera::ortho.proj[ego.sel, "case"]
              ego <- cholera::ortho.proj[ego.sel, coords]
            } else if (case.location == "nominal") {
              ego.id <- origin
              ego.sel <- cholera::fatalities$case == origin
              ego <- cholera::fatalities[ego.sel, coords]
            }
          } else {
            if (case.location == "address") {
              ego.sel <- cholera::sim.ortho.proj$case == origin
              ego.id <- cholera::sim.ortho.proj[ego.sel, "case"]
              ego <- cholera::sim.ortho.proj[ego.sel, coords]
            } else if (case.location == "nominal") {
              ego.id <- origin
              ego <- cholera::regular.cases[origin, coords]
            }
          }
        } else {
          txt1 <- 'With type = "case-pump" and observed = '
          txt2 <- 'origin must be between 1 and '
          stop(txt1, observed, ", ", txt2, ct, ".")
        }
      }

      if (is.character(origin)) {
        origin <- caseAndSpace(origin)

        if (origin %in% cholera::landmark.squares$name) {
          ego.sel <- grepl(origin, cholera::landmarks$name)
        } else if (origin %in% cholera::landmarks$name) {
          ego.sel <- cholera::landmarks$name == origin
        } else {
          stop('Use a valid landmark name.')
        }

        ego.id <- cholera::landmarks[ego.sel, "case"]
        coord.sel <- cholera::landmarks$case %in% ego.id
        ego <- cholera::landmarks[coord.sel, coords]
      }

      if (nrow(ego) == 1) {
        d <- vapply(alters[, pump.var], function(i) {
          c(stats::dist(rbind(alters[alters[, pump.var] == i, coords], ego)))
        }, numeric(1L))

        sel <- which.min(d)

        out <- data.frame(case = origin,
                          anchor = ego.id,
                          pump = alters[sel, pump.var],
                          pump.name = alters[sel, "street"],
                          distance = d[sel],
                          stringsAsFactors = FALSE)

        alter <- alters[sel, coords]

      } else if (nrow(ego) > 1) {
        ds <- lapply(seq_len(nrow(ego)), function(i) {
          vapply(alters[, pump.var], function(j) {
            dat <- rbind(alters[alters[, pump.var] == j, coords], ego[i, ])
            c(stats::dist(dat))
          }, numeric(1L))
        })

        exit.data <- expand.grid(alters[, pump.var], ego.id)
        exit.space <- stats::setNames(exit.data, c("pump", "exit"))
        exit.space$d <- unlist(ds)
        exit.soln <- exit.space[which.min(exit.space$d), ]
        sel <- cholera::landmarks$case == exit.soln$exit

        out <- data.frame(case = cholera::landmarks[sel, "name"],
                          anchor = exit.soln$exit,
                          pump = exit.soln$pump,
                          pump.name = alters[alters$id == exit.soln$pump,
                            "street"],
                          distance = exit.soln$d,
                          stringsAsFactors = FALSE)

        alter <- alters[alters[, pump.var] == exit.soln$pump, coords]
        ego <- cholera::landmarks[sel, coords]
      }
    }

  # ----- #

  } else if (type == "cases") {
    rev.flag <- is.null(origin) & is.null(destination) == FALSE

    if (rev.flag) {
      tmp <- origin
      origin <- destination
      destination <- tmp
    }

    if (observed) {
      if (case.location == "address") {
        case.data <- cholera::ortho.proj[, case.coords]
      } else if (case.location == "nominal") {
        case.data <- cholera::fatalities[, case.coords]
      }
    } else {
      if (case.location == "address") {
        case.data <- cholera::sim.ortho.proj[, case.coords]
      } else if (case.location == "nominal") {
        case.id <- seq_len(nrow(cholera::regular.cases))
        case.data <- data.frame(case = case.id, cholera::regular.cases)
      }
    }

    if (landmark.cases) {
      case.data <- rbind(case.data, cholera::landmarks[, case.coords])
    }

    if (is.null(destination)) {
      alters <- case.data
    } else {
      if (is.numeric(destination)) {
        if (all(destination > 0)) {
          alters.sel <- case.data$case %in% destination
        } else if (all(destination < 0)) {
          alters.sel <- case.data$case %in% abs(destination) == FALSE
        } else {
          stop("all positive or all negative.")
        }

        alters <- case.data[alters.sel, case.coords]
      }

      if (is.character(destination)) {
        destination <- caseAndSpace(destination)
        landmark.test1 <- destination %in% cholera::landmark.squares$name
        landmark.test2 <- destination %in% cholera::landmarks$name

        if (!landmark.test1 & !landmark.test2) {
          stop('Use a valid landmark name for the destination.')
        } else {
          alters.sel <- grepl(destination, cholera::landmarks$name)
          alters <- cholera::landmarks[alters.sel, case.coords]
        }
      }
    }

    if (is.numeric(origin)) {
      if (all(origin > 0)) {
        ego.sel <- case.data$case %in% origin
      } else if (all(origin < 0)) {
        ego.sel <- case.data$case %in% abs(origin) == FALSE
      } else {
        stop("all positive or all negative.")
      }

      ego <- case.data[ego.sel, case.coords]

    }

    if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      landmark.test1 <- origin %in% cholera::landmark.squares$name
      landmark.test2 <- origin %in% cholera::landmarks$name

      if (!landmark.test1 & !landmark.test2) {
        stop('Use a valid landmark name for the origin.')
      } else {
        ego.sel <- grepl(origin, cholera::landmarks$name)
        ego <- cholera::landmarks[ego.sel, case.coords]
      }
    }

    # remove ego from alters
    if (any(alters$case %in% ego$case)) {
      case.id <- cholera::anchor.case$case %in% ego$case
      stack.id <- cholera::anchor.case[case.id, "anchor"]
      stack.sel <- cholera::anchor.case$anchor %in% stack.id
      stack.cases <- cholera::anchor.case[stack.sel, "case"]
      landmarks.sel <- cholera::landmarks$case %in% ego$case
      landmarks.cases <- cholera::landmarks[landmarks.sel, "case"]
      alters <- alters[alters$case %in% c(stack.cases, landmarks.cases) ==
        FALSE, ]
    }

    if (nrow(ego) == 1) {
      d <- vapply(alters$case, function(i) {
        dat <- rbind(alters[alters$case == i, coords], ego[, coords])
        c(stats::dist(dat))
      }, numeric(1L))

      sel <- which.min(d)

      if (all(ego$case <= 20000)) {
        stack.sel <- cholera::anchor.case$case %in% ego$case
        ego.id <- cholera::anchor.case[stack.sel, "anchor"]
      } else {
        landmarks.sel <- cholera::landmarks$case %in% ego$case
        ego.id <- cholera::landmarks[landmarks.sel, "case"]
      }

      if (all(alters$case <= 20000)) {
        stack.sel <- cholera::anchor.case$case %in% ego$case
        alters.id <- cholera::anchor.case[stack.sel, "anchor"]
      } else {
        landmarks.sel <- cholera::landmarks$case %in% ego$case
        alters.id <- cholera::landmarks[landmarks.sel, "case"]
      }

      if (is.character(destination)) {
        nm.sel <- cholera::landmarks$case == alters[sel, "case"]
        out <- data.frame(caseA = origin,
                          anchorA = ego.id,
                          caseB = cholera::landmarks[nm.sel, "name"],
                          anchorB = alters[sel, "case"],
                          distance = d[sel],
                          stringsAsFactors = FALSE)
      } else {
        out <- data.frame(caseA = origin,
                          anchorA = ego.id,
                          caseB = alters[sel, "case"],
                          anchorB = alters[sel, "case"],
                          distance = d[sel],
                          stringsAsFactors = FALSE)
      }

    } else if (nrow(ego) > 1) {
      ds <- parallel::mclapply(ego$case, function(i) {
        vapply(alters$case, function(j) {
          dat <- rbind(alters[alters$case == j, coords],
                       ego[ego$case == i, coords])
          c(stats::dist(dat))
        }, numeric(1L))
      }, mc.cores = cores)

      exit.space <- stats::setNames(expand.grid(alters$case, ego$case),
        c("case", "exit"))
      exit.space$d <- unlist(ds)
      exit.soln <- exit.space[which.min(exit.space$d), ]

      case.a <- exit.soln$exit
      case.b <- exit.soln$case

      ego <- ego[ego$case == case.a, coords]

      if (case.a < 20000) {
        anchor.a <- cholera::anchor.case[cholera::anchor.case$case == case.a,
          "anchor"]
      } else if (case.a > 20000) {
        anchor.a <- case.a
        case.a <- cholera::landmarks[cholera::landmarks$case == case.a, "name"]
      }

      if (case.b < 20000) {
        anchor.b <- cholera::anchor.case[cholera::anchor.case$case == case.b,
          "anchor"]
      } else if (case.b > 20000) {
        anchor.b <- case.b
        case.b <- cholera::landmarks[cholera::landmarks$case == case.b, "name"]
      }

      out <- data.frame(caseA = case.a,
                        anchorA = anchor.a,
                        caseB = case.b,
                        anchorB = anchor.b,
                        distance = exit.soln$d,
                        stringsAsFactors = FALSE)

    }

    ego <- ego[, coords]
    alter <- case.data[case.data$case == out$anchorB, coords]

    if (rev.flag) {
      tmp.case <- out$caseA
      tmp.anchor <- out$anchorA
      out$caseA <- out$caseB
      out$anchorA <- out$anchorB
      out$caseB <- tmp.case
      out$anchorB <- tmp.anchor

      tmp <- ego
      ego <- alter
      alter <- tmp

      tmp <- origin
      origin <- destination
      destination <- tmp
    }

  # ----- #

  } else if (type == "pumps") {
    if (identical(all.equal(origin, destination), TRUE)) {
      stop("Origin must different from destination.")
    }

    rev.flag <- is.null(origin) & is.null(destination) == FALSE

    if (rev.flag) {
      tmp <- origin
      origin <- destination
      destination <- tmp
    }

    if (origin %in% p.ID == FALSE) {
      txt1 <- 'With type = "pumps", observed = '
      txt2 <- 'and vestry = '
      txt3 <- ', the origin must be between 1 and '
      stop(txt1, observed, ", ", txt2, vestry, txt3, ct, ".")
    } else {
      ego <- p.data[p.data[, pump.var] == origin, ]
    }

    if (!is.null(destination)) {
      if (any(abs(destination) %in% p.ID == FALSE)) {
        stop('With vestry = ', vestry, ', 1 >= |destination| <= ', p.count, ".")
      } else {
        if (all(destination > 0)) {
          alters <- p.data[destination, ]
        } else if (all(destination < 0)) {
          alters <- p.data[p.data[, pump.var] %in% abs(destination) == FALSE, ]
        }
        alters <- alters[alters[, pump.var] != origin, ]
      }
    } else {
      alters <- p.data[p.data[, pump.var] != origin, ]
    }

    d <- vapply(alters[, pump.var], function(i) {
      dat <- rbind(ego[, coords], alters[alters[, pump.var] == i, coords])
      c(stats::dist(dat))
    }, numeric(1L))

    sel <- which.min(d)

    if (rev.flag) {
      out <- data.frame(pumpA = alters[, pump.var][sel],
                        pumpB = ego[, pump.var],
                        pump.nameA = alters$street[sel],
                        pump.nameB = ego$street,
                        distance = d[sel],
                        stringsAsFactors = FALSE)
    } else {
      out <- data.frame(pumpA = ego[, pump.var],
                        pumpB = alters[, pump.var][sel],
                        pump.nameA = ego$street,
                        pump.nameB = alters$street[sel],
                        distance = d[sel],
                        stringsAsFactors = FALSE)
    }

    ego <- ego[, coords]
    alter <- alters[sel, coords]
  }

  # ----- #

  if (distance.unit == "meter") {
    out$distance <- unitMeter(out$distance, "meter")
  } else if (distance.unit == "yard") {
    out$distance <- unitMeter(out$distance, "yard")
  } else if (distance.unit == "native") {
    out$distance <- unitMeter(out$distance, "native")
  }

  out$time <- distanceTime(out$distance, distance.unit = distance.unit,
    time.unit = time.unit, walking.speed = walking.speed)

  output <- list(ego = ego,
                 alter = alter,
                 origin = origin,
                 destination = destination,
                 type = type,
                 observed = observed,
                 alters = alters,
                 vestry = vestry,
                 distance.unit = distance.unit,
                 time.unit = time.unit,
                 d = out$distance,
                 t = out$time,
                 walking.speed = walking.speed,
                 data = out)

  class(output) <- "euclidean_path"
  output
}

#' Plot the path of the Euclidean distance between cases and/or pumps.
#'
#' @param x An object of class "euclidean_path" created by euclideanPath().
#' @param zoom Logical or Numeric. A numeric value >= 0 controls the degree of zoom. The default is 0.5.
#' @param unit.posts Character. "distance" for mileposts; "time" for timeposts; \code{NULL} for no posts.
#' @param unit.interval Numeric. Set interval between posts. When \code{unit.posts} is "distance", \code{unit.interval} automatically defaults to 50 meters. When \code{unit.posts} is "time", \code{unit.interval} automatically defaults to 60 seconds.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @examples
#' plot(euclideanPath(15))
#' plot(euclideanPath(15), unit.posts = "time")

plot.euclidean_path <- function(x, zoom = 0.5, unit.posts = "distance",
  unit.interval = NULL, ...) {

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

  if (is.logical(zoom)) {
    if (zoom) {
      padding <- 0.1
      x.rng <- c(min(dat$x) - padding, max(dat$x) + padding)
      y.rng <- c(min(dat$y) - padding, max(dat$y) + padding)
    } else {
      x.rng <- range(cholera::roads$x)
      y.rng <- range(cholera::roads$y)
    }
  } else if (is.numeric(zoom)) {
    if (zoom >= 0) {
      x.rng <- c(min(dat$x) - zoom, max(dat$x) + zoom)
      y.rng <- c(min(dat$y) - zoom, max(dat$y) + zoom)
    } else stop("If numeric, zoom must be >= 0.")
  } else stop("zoom must either be logical or numeric.")

  plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
    xlab = "x", ylab = "y", pch = 15, cex = 0.5, col = "lightgray", asp = 1)
  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(border.list, lines))

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

  if (x$type == "case-pump") {
    case.color <- colors[paste0("p", x$data$pump)]
    points(ego.xy, col = "red")
    text(ego.xy, labels = x$data$case, pos = 1, col = "red")

  } else if (x$type == "cases") {
    case.color <- "blue"
    points(ego.xy, col = case.color)
    points(alter.xy, col = case.color)
    text(ego.xy, labels = x$data$caseA, pos = 1, col = case.color)
    text(alter.xy, labels = x$data$caseB, pos = 1, col = case.color)

  } else if (x$type == "pumps") {
    case.color <- "blue"
    points(ego.xy, col = case.color)
    points(alter.xy, col = case.color)
    text(ego.xy, labels = x$data$pumpA, pos = 1, col = case.color)
    text(alter.xy, labels = x$data$pumpB, pos = 1, col = case.color)
  }

  if (x$type == "case-pump") {
    if (is.numeric(x$data$case)) {
      title(main = paste("Case", x$data$anchor, "to Pump", x$data$pump))
    } else if (is.character(x$data$case)) {
      title(main = paste(x$data$case, "to Pump", x$data$pump))
    }

  } else if (x$type == "cases") {
    if (is.numeric(x$data$caseA) & is.numeric(x$data$caseB)) {
      title(main = paste("Case", x$data$anchorA, "to Case", x$data$anchorB))
    } else if (is.character(x$data$caseA) & is.numeric(x$data$caseB)) {
      title(main = paste(x$data$caseA, "to Case", x$data$anchorB))
    } else if (is.numeric(x$data$caseA) & is.character(x$data$caseB)) {
      title(main = paste("Case", x$data$anchorA, "to", x$data$caseB))
    } else if (is.character(x$data$caseA) & is.character(x$data$caseB)) {
      title(main = paste(x$data$caseA, "to", x$data$caseB))
    }

  } else if (x$type == "pumps") {
    title(main = paste("Pump", x$data$pumpA, "to Pump", x$data$pumpB))
  }

  if (x$time.unit == "hour") {
    nominal.time <- paste(round(x$t, 1), "hr")
  } else if (x$time.unit == "minute") {
    nominal.time <- paste(round(x$t, 1), "min")
  } else if (x$time.unit == "second") {
    nominal.time <- paste(round(x$t, 1), "sec")
  }

  if (x$distance.unit == "native") {
    d.unit <- "units;"
  } else if (x$distance.unit == "meter") {
    d.unit <- "m;"
  } else if (x$distance.unit == "yard") {
    d.unit <- "yd;"
  }

  # mileposts #

  if (is.null(unit.posts)) {
    arrows(ego.xy$x, ego.xy$y, alter.xy$x, alter.xy$y, col = case.color,
      lwd = 3, length = 0.075)
    title(sub = paste(round(x$d, 1), d.unit, nominal.time, "@", x$walking.speed,
      "km/hr"))
  } else {
    if (unit.posts %in% c("distance", "time") == FALSE) {
      stop('If specified, unit.posts must be "distance" or "time".')
    } else {
      if (is.null(unit.interval)) {
        if (unit.posts == "distance")  {
          unit.interval <- 50 * x$walking.speed / 5
        } else if (unit.posts == "time") {
          unit.interval <- 60 * x$walking.speed / 5
        }
      } else {
        if (!is.numeric(unit.interval)) {
          stop('unit.interval must be numeric.')
        }
      }

      if (unit.posts == "distance") {
        tot <- unitMeter(stats::dist(dat))
        h <- seq(0, tot, unit.interval) / cholera::unitMeter(1)
      } else if (unit.posts == "time") {
        tot <- cholera::distanceTime(unitMeter(stats::dist(dat),
          distance.unit = "native"), walking.speed = x$walking.speed)
        h <- seq(0, tot, unit.interval) * 1000 * x$walking.speed / 60^2 /
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

    title(sub = paste(round(x$d, 1), d.unit, nominal.time, "@", x$walking.speed,
      "km/hr;", post.info))
  }
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
