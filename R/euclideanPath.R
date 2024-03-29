#' Compute path of the Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Character. Numeric ID of case or pump. Character landmark name.
#' @param destination Numeric or Character. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}, which returns the closest pump, "anchor" case or landmark.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param case.location Character. "address" or "orthogonal". For \code{case.set = "observed"}: "address" uses \code{fatalities} and "orthogonal" uses \code{ortho.proj}. For \code{case.set = "expected"}: "address" uses \code{regular.cases} and "orthogonal" uses \code{sim.ortho.proj}.
#' @param case.set Character. "observed" or "expected".
#' @param landmark.cases Logical. \code{TRUE} includes landmarks as cases.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Default is 5 km/hr.
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
  case.location = "address", case.set = "observed", landmark.cases = TRUE,
  vestry = FALSE, distance.unit = "meter", time.unit = "second",
  walking.speed = 5) {

  if (is.null(origin) & is.null(destination)) {
    stop("If origin = NULL, you must supply a destination.", call. = FALSE)
  }

  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
    stop('unit must be "meter", "yard" or "native".', call. = FALSE)
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('time.unit must be "hour", "minute" or "second".', call. = FALSE)
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('type must be "case-pump", "cases" or "pumps".', call. = FALSE)
  }

  if (is.character(destination)) {
    if (type != "cases") stop('type must be "cases".', call. = FALSE)
  }

  if (!case.location %in% c("address", "orthogonal")) {
    stop('case.location must be "address" or "orthogonal".', call. = FALSE)
  }

  if (!case.set %in% c("observed", "expected")) {
    stop('case.set must be "observed" or "expected".', call. = FALSE)
  }

  if (case.set == "observed") {
    ct <- nrow(cholera::fatalities)
  } else if (case.set == "expected") {
    ct <- nrow(cholera::regular.cases)
  }

  if (case.location == "address") {
    if (vestry) {
      p.data <- cholera::pumps.vestry
    } else {
      p.data <- cholera::pumps
    }
  } else if (case.location == "orthogonal") {
    if (vestry) {
      p.data <- cholera::ortho.proj.pump.vestry
      p.data$street <- cholera::pumps.vestry$street
    } else {
      p.data <- cholera::ortho.proj.pump
      p.data$street <- cholera::pumps$street
    }
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  if (case.location == "address") {
    coords <- c("x", "y")
    pump.var <- "id"
  } else if (case.location == "orthogonal") {
    coords <- c("x.proj", "y.proj")
    pump.var <- "pump.id"
  }

  case.coords <- c("case", coords)

  # ----- #

  if (type == "case-pump") {
    if (is.null(destination)) {
      if (is.null(origin)) {
        stop("If origin is set to NULL, you must provide a destination pump!",
          call. = FALSE)
      } else {
        alters <- p.data
      }
    } else {
      if (any(abs(destination) %in% p.ID)) {
        alters <- p.data[destination, ]
      } else {
        stop('With vestry = ', vestry, ', 1 >= |destination| <= ', p.count,
          call. = FALSE)
      }
    }

    if (is.null(origin)) {
      if (case.set == "observed") {
        if (case.location == "address") {
          egos <- cholera::fatalities[, case.coords]
        } else if (case.location == "orthogonal") {
          egos <- cholera::ortho.proj[, case.coords]
        }
      } else if (case.set == "expected") {
        if (case.location == "address") {
          case.id <- seq_len(nrow(cholera::regular.cases))
          egos <- data.frame(case = case.id, cholera::regular.cases)
        } else if (case.location == "orthogonal") {
          egos <- cholera::sim.ortho.proj[, case.coords]
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
        if (case.set == "observed") {
          if (case.location == "address") {
            ego.select <- cholera::ortho.proj$case == anchor
            ego <- cholera::ortho.proj[ego.select, ]
          } else if (case.location == "nominal") {
            ego.select <- cholera::fatalities$case == anchor
            ego <- cholera::fatalities[ego.select, ]
          }
        } else if (case.set == "expected") {
          if (case.location == "address") {
            ego <- cholera::regular.cases[anchor, ]
          } else if (case.location == "orthogonal") {
            ego.select <- cholera::sim.ortho.proj$case == anchor
            ego <- cholera::sim.ortho.proj[ego.select, ]
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
          if (case.set == "observed") {
            if (case.location == "address") {
              ego.id <- origin
              ego.sel <- cholera::fatalities$case == origin
              ego <- cholera::fatalities[ego.sel, coords]
            } else if (case.location == "orthogonal") {
              ego.sel <- cholera::ortho.proj$case == origin
              ego.id <- cholera::ortho.proj[ego.sel, "case"]
              ego <- cholera::ortho.proj[ego.sel, coords]
            }
          } else if (case.set == "expected") {
            if (case.location == "address") {
              ego.id <- origin
              ego <- cholera::regular.cases[origin, coords]
            } else if (case.location == "orthogonal") {
              ego.sel <- cholera::sim.ortho.proj$case == origin
              ego.id <- cholera::sim.ortho.proj[ego.sel, "case"]
              ego <- cholera::sim.ortho.proj[ego.sel, coords]
            }
          }
        } else {
          txt1 <- 'With type = "case-pump" and case.set = '
          txt2 <- 'origin must be between 1 and '
          stop(txt1, case.set, ", ", txt2, ct, ".", call. = FALSE)
        }
      }

      if (is.character(origin)) {
        origin <- caseAndSpace(origin)

        if (origin %in% cholera::landmark.squares$name) {
          ego.sel <- grepl(origin, cholera::landmarks$name)
        } else if (origin %in% cholera::landmarks$name) {
          ego.sel <- cholera::landmarks$name == origin
        } else {
          stop('Use a valid landmark name.', call. = FALSE)
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
                          pump.name = alters[sel, "street"],
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

    if (case.set == "observed") {
      if (case.location == "address") {
        case.data <- cholera::fatalities[, case.coords]
      } else if (case.location == "orthogonal") {
        case.data <- cholera::ortho.proj[, case.coords]
      }
    } else if (case.set == "expected") {
      if (case.location == "address") {
        case.id <- seq_len(nrow(cholera::regular.cases))
        case.data <- data.frame(case = case.id, cholera::regular.cases)
      } else if (case.location == "orthogonal") {
        case.data <- cholera::sim.ortho.proj[, case.coords]
      }
    }

    if (landmark.cases) {
      case.data <- rbind(case.data, cholera::landmarks[, case.coords])
      st.james.cases <- cholera::anchor.case[cholera::anchor.case$anchor == 369,
        "case"]
      st.james.landmark <- cholera::landmarks[cholera::landmarks$name ==
        "St James Workhouse", "case"]
      st.james <- c(st.james.cases, st.james.landmark)
    }

    if (is.numeric(origin)) {
      origin.anchor <- cholera::anchor.case[cholera::anchor.case$case ==
        origin, "anchor"]
      origin.stack <- cholera::anchor.case[cholera::anchor.case$anchor ==
        origin.anchor, "case"]

      if (length(origin.stack) > 1 & is.numeric(destination)) {
        if (origin %in% origin.stack & destination %in% origin.stack) {
          stop("origin and destination are at same address!", call. = FALSE)
        }
      }

      if (origin.anchor %in% st.james.cases) {
        origin.stack <- st.james
      }
    }

    if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      landmark.test1 <- origin %in% cholera::landmark.squares$name
      landmark.test2 <- origin %in% cholera::landmarks$name

      ego.sel <- grepl(origin, cholera::landmarks$name)

      if (!landmark.test1 & !landmark.test2) {
        stop('Use a valid landmark name for the origin.', call. = FALSE)

      } else if (origin == "St James Workhouse") {
        origin.anchor <- st.james.landmark
        origin.stack <- st.james

      } else if (origin %in% cholera::landmark.squares$name) {
        origin.anchor <- cholera::landmarks[ego.sel, "case"]
        sq.segments <- cholera::landmarks[ego.sel, "road.segment"]

        if (any(cholera::ortho.proj$road.segment %in% sq.segments)) {
          sq.cases <- cholera::ortho.proj[cholera::ortho.proj$road.segment %in%
            sq.segments, "case"]
          origin.stack <- c(sq.cases, origin.anchor)
        } else origin.stack <- origin.anchor

      } else {
        origin.anchor <- cholera::landmarks[ego.sel, "case"]
        origin.stack <- origin.anchor
      }
    }

    if (is.null(destination)) {
      alters <- case.data[case.data$case %in% origin.stack == FALSE, ]
    } else {
      if (is.numeric(destination)) {
        if (all(destination > 0)) {
          alters.sel <- case.data$case %in% destination
        } else if (all(destination < 0)) {
          alters.sel <- case.data$case %in% abs(destination) == FALSE
        } else {
          stop("Destination must be all positive or all negative.",
            call. = FALSE)
        }
        alters <- case.data[alters.sel & case.data$case != origin, case.coords]
      }

      if (is.character(destination)) {
        destination <- caseAndSpace(destination)

        if (is.character(origin)) {
          if (origin == destination) {
            stop("origin and destination are at same address!", call. = FALSE)
          }
        }

        landmark.test1 <- destination %in% cholera::landmark.squares$name
        landmark.test2 <- destination %in% cholera::landmarks$name

        alter.sel <- grepl(destination, cholera::landmarks$name)

        if (!landmark.test1 & !landmark.test2) {
          stop('Use a valid landmark name for the destination.', call. = FALSE)

        } else if (destination == "St James Workhouse") {
          destination.anchor <- st.james.landmark
          destination.stack <- st.james

        } else if (destination %in% cholera::landmark.squares$name) {
          destination.anchor <- cholera::landmarks[alter.sel, "case"]
          sq.segments <- cholera::landmarks[alter.sel, "road.segment"]

          if (any(cholera::ortho.proj$road.segment %in% sq.segments)) {
            sq.cases <- cholera::ortho.proj[cholera::ortho.proj$road.segment
              %in% sq.segments, "case"]
            destination.stack <- c(sq.cases, destination.anchor)
          } else {
            destination.stack <- destination.anchor
          }

        } else {
          destination.anchor <- cholera::landmarks[alter.sel, "case"]
          destination.stack <- destination.anchor
        }

        alters <- case.data[case.data$case %in% destination.stack, case.coords]
      }
    }

    if (origin == "St James Workhouse") {
      ego <- case.data[case.data$case == st.james.landmark, case.coords]
    } else {
      ego <- case.data[case.data$case %in% origin.anchor, case.coords]
    }

    if (nrow(ego) == 1) {
      d <- vapply(alters$case, function(i) {
        dat <- rbind(alters[alters$case == i, coords], ego[, coords])
        c(stats::dist(dat))
      }, numeric(1L))

      sel <- which.min(d)

      if (is.character(destination) == FALSE) {
        b.case <- cholera::anchor.case$case == alters[sel, "case"]
        b.anchor <- cholera::anchor.case[b.case, "anchor"]
        out <- data.frame(caseA = origin,
                          anchorA = origin.anchor,
                          caseB = alters[sel, "case"],
                          anchorB = b.anchor,
                          distance = d[sel],
                          stringsAsFactors = FALSE)
      } else {
        b.anchor <- alters[sel, "case"]
        out <- data.frame(caseA = origin,
                          anchorA = origin.anchor,
                          caseB = destination,
                          anchorB = alters[sel, "case"],
                          distance = d[sel],
                          stringsAsFactors = FALSE)
      }

    } else if (nrow(ego) > 1) {
      ds <- lapply(ego$case, function(i) {
        vapply(alters$case, function(j) {
          dat <- rbind(alters[alters$case == j, coords],
                       ego[ego$case == i, coords])
          c(stats::dist(dat))
        }, numeric(1L))
      })

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
      stop("Origin must different from destination.", call. = FALSE)
    }

    rev.flag <- is.null(origin) & is.null(destination) == FALSE

    if (rev.flag) {
      tmp <- origin
      origin <- destination
      destination <- tmp
    }

    if (origin %in% p.ID == FALSE) {
      txt1 <- 'With type = "pumps", case.set = '
      txt2 <- 'and vestry = '
      txt3 <- ', the origin must be between 1 and '
      stop(txt1, case.set, ", ", txt2, vestry, txt3, ct, ".", call. = FALSE)
    } else {
      ego <- p.data[p.data[, pump.var] == origin, ]
    }

    if (!is.null(destination)) {
      if (any(abs(destination) %in% p.ID == FALSE)) {
        stop('With vestry = ', vestry, ', 1 >= |destination| <= ', p.count,
          ".", call. = FALSE)
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
                 case.set = case.set,
                 alters = alters,
                 vestry = vestry,
                 case.location = case.location,
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

  if (!inherits(x, "euclidean_path")) {
    stop('"x"\'s class must be "euclidean_path".', call. = FALSE)
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  colors <- cholera::snowColors(x$vestry)

  if (x$case.location == "address") {
    coords <- c("x", "y")
    names(x$ego) <- coords
    names(x$alter) <- coords
  }

  ego.xy <- x$ego
  alter.xy <- x$alter
  dat <- rbind(alter.xy, ego.xy) # alter before ego for arrow order

  ## city square data ##

  if (any(grepl("case", names(x$data)))) {
    ego <- unlist(x$data[, grepl("case", names(x$data))][1])
    alter <- unlist(x$data[, grepl("case", names(x$data))][2])
    if (is.character(ego)) {
      if (grepl("Square", ego)) {
        if (x$origin == "Soho Square") {
          sq.sel <- cholera::landmark.squares$name == "Soho Square"
        } else if (x$origin == "Golden Square") {
          sq.sel <- cholera::landmark.squares$name == "Golden Square"
        }
        sq.center.origin <- cholera::landmark.squares[sq.sel, c("x", "y")]
      }
    }
    if (is.character(alter)) {
      if (grepl("Square", alter)) {
        if (x$destination == "Soho Square") {
          sq.sel <- cholera::landmark.squares$name == "Soho Square"
        } else if (x$destination == "Golden Square") {
          sq.sel <- cholera::landmark.squares$name == "Golden Square"
        }
        sq.center.destination <- cholera::landmark.squares[sq.sel, c("x", "y")]
      }
    }
    if (grepl("Square", ego) & grepl("Square", alter)) {
      dat.plus <- rbind(dat, sq.center.origin, sq.center.destination)
    } else if (grepl("Square", ego) & !grepl("Square", alter)) {
      dat.plus <- rbind(dat, sq.center.origin)
    } else if (!grepl("Square", ego) & grepl("Square", alter)) {
      dat.plus <- rbind(dat, sq.center.destination)
    } else dat.plus <- dat
  }

  if (x$type %in% c("case-pump", "cases")) {
    if (is.logical(zoom)) {
      if (zoom) {
        x.rng <- c(min(dat.plus$x) - zoom, max(dat.plus$x) + zoom)
        y.rng <- c(min(dat.plus$y) - zoom, max(dat.plus$y) + zoom)
      } else {
        x.rng <- range(cholera::roads$x)
        y.rng <- range(cholera::roads$y)
      }
    } else if (is.numeric(zoom)) {
      if (zoom >= 0) {
        x.rng <- c(min(dat.plus$x) - zoom, max(dat.plus$x) + zoom)
        y.rng <- c(min(dat.plus$y) - zoom, max(dat.plus$y) + zoom)
      } else stop("If numeric, zoom must be >= 0.", call. = FALSE)
    } else stop("zoom must either be logical or numeric.", call. = FALSE)
  } else if (x$type == "pumps") {
    if (is.logical(zoom)) {
      if (zoom) {
        x.rng <- c(min(dat$x) - zoom, max(dat$x) + zoom)
        y.rng <- c(min(dat$y) - zoom, max(dat$y) + zoom)
      } else {
        x.rng <- range(cholera::roads$x)
        y.rng <- range(cholera::roads$y)
      }
    } else if (is.numeric(zoom)) {
      if (zoom >= 0) {
        x.rng <- c(min(dat$x) - zoom, max(dat$x) + zoom)
        y.rng <- c(min(dat$y) - zoom, max(dat$y) + zoom)
      } else stop("If numeric, zoom must be >= 0.", call. = FALSE)
    } else stop("zoom must either be logical or numeric.", call. = FALSE)
  }

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
    if (is.numeric(ego)) {
      text(ego.xy, labels = x$data$case, pos = 1, col = "red")
    } else if (is.character(ego)) {
      if (grepl("Soho Square", ego)) {
        text(sq.center.origin$x, sq.center.origin$y,
          labels = "Soho\nSquare", col = "red", cex = 0.8)
      } else if (grepl("Golden Square", ego)) {
        text(sq.center.origin$x, sq.center.origin$y,
          labels = "Golden\nSquare", col = "red", cex = 0.8)
      } else {
        text(cholera::landmarks[cholera::landmarks$name == ego,
          c("x.proj", "y.proj")], labels = ego, pos = 1, col = "red")
      }
    }

  } else if (x$type == "cases") {
    case.color <- "blue"
    points(ego.xy, col = case.color)
    points(alter.xy, col = case.color)
    if (is.numeric(ego)) {
      text(ego.xy, labels = x$data$caseA, pos = 1, col = "red")
    } else if (is.character(ego)) {
      if (grepl("Soho Square", ego)) {
        text(sq.center.origin$x, sq.center.origin$y,
          labels = "Soho\nSquare", col = "red", cex = 0.8)
      } else if (grepl("Golden Square", ego)) {
        text(sq.center.origin$x, sq.center.origin$y,
          labels = "Golden\nSquare", col = "red", cex = 0.8)
      } else {
        text(cholera::landmarks[cholera::landmarks$name == ego,
          c("x.proj", "y.proj")], labels = ego, pos = 1, col = "red")
      }
    }
    if (is.numeric(alter)) {
      text(alter.xy, labels = x$data$caseB, pos = 1, col = "red")
    } else if (is.character(alter)) {
      if (grepl("Soho Square", alter)) {
        text(sq.center.destination$x, sq.center.destination$y,
          labels = "Soho\nSquare", col = "red", cex = 0.8)
      } else if (grepl("Golden Square", alter)) {
        text(sq.center.destination$x, sq.center.destination$y,
          labels = "Golden\nSquare", col = "red", cex = 0.8)
      } else {
        text(cholera::landmarks[cholera::landmarks$name == alter,
          c("x.proj", "y.proj")], labels = alter, pos = 1, col = "red")
      }
    }

  } else if (x$type == "pumps") {
    case.color <- "blue"
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

  d.unit <- distanceUnit(x$distance.unit)
  nominal.time <- nominalTime(x$t, x$time.unit)

  # mileposts #

  if (is.null(unit.posts)) {
    arrows(ego.xy$x, ego.xy$y, alter.xy$x, alter.xy$y, col = case.color,
      lwd = 3, length = 0.075)
    title(sub = paste(round(x$d, 1), d.unit, nominal.time, "@", x$walking.speed,
      "km/hr"))
  } else {
    if (unit.posts %in% c("distance", "time") == FALSE) {
      stop('If specified, unit.posts must be "distance" or "time".',
        call. = FALSE)
    } else {
      if (is.null(unit.interval)) {
        if (unit.posts == "distance")  {
          unit.interval <- 50 * x$walking.speed / 5
        } else if (unit.posts == "time") {
          unit.interval <- 60 * x$walking.speed / 5
        }
      } else {
        if (!is.numeric(unit.interval)) {
          stop('unit.interval must be numeric.', call. = FALSE)
        }
      }

      if (unit.posts == "distance") {
        tot <- unitMeter(stats::dist(dat))
        h <- seq(0, tot, unit.interval) / cholera::unitMeter(1)
      } else if (unit.posts == "time") {
        tot <- distanceTime(unitMeter(stats::dist(dat)),
          walking.speed = x$walking.speed)
        h <- seq(0, tot, unit.interval) * 1000 * x$walking.speed / 60^2 /
          cholera::unitMeter(1)
      } else {
        stop('Specify unit.posts.', call. = FALSE)
      }

      ols <- stats::lm(y ~ x, data = dat)
      edge.slope <- stats::coef(ols)[2]
      edge.intercept <- stats::coef(ols)[1]
      theta <- ifelse(is.na(edge.slope), pi / 2, atan(edge.slope))

      post.coords <- quandrantCoordinates(dat[2:1, ], h, theta)

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
  if (!inherits(x, "euclidean_path")) {
    stop('"x"\'s class must be "euclidean_path".')
  }

  print(x[c("ego", "alter", "data")])
}
