#' Compute Euclidean path coordinates from observed case/landmark to nearest/selected pump.
#'
#' @param origin Numeric. Vector of origin(s) (numeric ID or character name landmark/pump ).
#' @param destination Numeric. Vector of destination(s) (numeric or landmark/pump name).
#' @param type Character. Path case to pump. FALSE is all other combinations of cases, landmarks and pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param latlong Logical.
#' @param case.set Character. "observed" or "expected".
#' @param location Character. For cases and pumps. "anchor, "fatality" or "orthogonal.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param distance.unit Character. Unit of distance: "meter" or "yard".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param include.landmarks Logical. Include landmarks as cases.
#' @importFrom geosphere distGeo
#' @export

euclideanPath <- function(origin = 1, destination = NULL, type = "case-pump",
  vestry = FALSE, latlong = FALSE, case.set = "observed", location = "nominal",
  weighted = TRUE, distance.unit = "meter", time.unit = "second",
  walking.speed = 5, include.landmarks = TRUE) {

  meter.to.yard <- 1.09361

  if (is.null(origin) & is.null(destination)) {
    stop("You must provide at least one origin or destination.", call. = FALSE)
  }

  if (!type %in% c("case-pump", "cases", "pumps")) {
    stop('type must be "case-pump", "cases" or "pumps".', call. = FALSE)
  }

  if (any(is.character(origin))) origin <- caseLandmarks(origin)
  if (any(is.character(destination))) destination <- caseLandmarks(destination)

  if (!include.landmarks & type %in% c("case-pump", "cases")) {
    msg <- 'landmarks not considered when include.landmarks = FALSE.'
    if (is.numeric(origin)) {
      if (origin > 1000L) stop(msg, call. = FALSE)
    } else if (is.character(origin)) {
      lndmrk.test <- origin %in% cholera::landmarksB$name |
                     origin %in% cholera::landmark.squaresB$name
      if (lndmrk.test) stop(msg, call. = FALSE)
    }
    if (is.numeric(destination)) {
      if (destination > 1000L) stop(msg, call. = FALSE)
    } else if (is.character(destination)) {
      lndmrk.test <- destination %in% cholera::landmarksB$name |
                     destination %in% cholera::landmark.squaresB$name
      if (lndmrk.test) stop(msg, call. = FALSE)
    }
  }

  # Change type to "cases" in presence of destination landmarks
  if (is.character(destination)) {
    dest.nm <- c(cholera::landmark.squaresB$name, cholera::landmarksB$name)
    if (any(destination %in% dest.nm) & type != "cases") type <- "cases"
  } else if (is.numeric(destination)) {
    dest.num <- c(cholera::landmark.squaresB$case, cholera::landmarksB$case)
    if (any(destination %in% dest.num) & type != "cases") type <- "cases"
  }

  if (location %in% c("nominal", "anchor")) {
    if (vestry) {
      pmp <- cholera::pumps.vestry
    } else {
      pmp <- cholera::pumps
    }
  } else if (location == "orthogonal") {
    if (latlong) {
      if (vestry) {
        pmp <- cholera::latlong.ortho.pump.vestry
        pmp$street <- cholera::pumps.vestry$street
      } else {
        pmp <- cholera::latlong.ortho.pump
        pmp$street <- cholera::pumps$street
      }
    } else {
      if (vestry) {
        pmp <- cholera::ortho.proj.pump.vestry
        pmp$street <- cholera::pumps.vestry$street
      } else {
        pmp <- cholera::ortho.proj.pump
        pmp$street <- cholera::pumps$street
      }
      newvars <- c("x", "y", "id")
      names(pmp)[names(pmp) %in% c("x.proj", "y.proj", "pump.id")] <- newvars
    }
  }

  if (type == "case-pump") {
    origin.chk <- validateCase(origin, case.set, location, include.landmarks)
    orgn <- origin.chk$out
    orgn.nm <- origin.chk$out.nm

    destination.chk <- validatePump(destination, pmp, vestry)
    dstn <- destination.chk$out
    dstn.nm <- destination.chk$out.nm

  } else if (type == "cases") {
    origin.chk <- validateCase(origin, case.set, location, include.landmarks)
    orgn <- origin.chk$out
    orgn.nm <- origin.chk$out.nm

    destination.chk <- validateCase(destination, case.set, location,
      include.landmarks)
    dstn <- destination.chk$out
    dstn.nm <- destination.chk$out.nm

  } else if (type == "pumps") {
    origin.chk <- validatePump(origin, pmp, vestry)
    orgn <- origin.chk$out
    orgn.nm <- origin.chk$out.nm

    destination.chk <- validatePump(destination, pmp, vestry)
    dstn <- destination.chk$out
    dstn.nm <- destination.chk$out.nm
  }

  if (type == "case-pump") {
    path.data <- casePumpEucl(orgn, orgn.nm, destination, dstn, latlong, pmp,
      vestry, case.set, location)
  } else if (type == "cases") {
    path.data <- caseCaseEucl(orgn, orgn.nm, destination, dstn,
      include.landmarks, latlong, origin, vestry, location)
  } else if (type == "pumps") {
    path.data <- pumpPumpEucl(orgn, orgn.nm, destination, dstn, latlong, origin,
      pmp, vestry)
  }

  if (length(orgn) > 1) orgn <- path.data$data$orgn
  if (length(orgn.nm) > 1) orgn.nm <- path.data$data$orgn.nm
  nearest.dest <- path.data$data$nearest.dest

  if (latlong) {
    p1 <- path.data$ego
    p2 <- path.data$alter
    d <- geosphere::distGeo(p1, p2)
  } else {
    d <- unitMeter(path.data$data$d, distance.unit = distance.unit)
  }

  if (latlong) {
    walking.time <- walkingTime(d, time.unit = time.unit,
      walking.speed = walking.speed)
  } else {
    walking.time <- distanceTime(d, distance.unit = distance.unit,
      time.unit = time.unit, walking.speed = walking.speed)
  }

  if (as.integer(nearest.dest) < 1000L) {
    if (type %in% c("case-pump", "pumps")) {
      dest.nm <- pmp[pmp$id == nearest.dest, ]$street
    } else if (type == "cases") {
      dest.nm <- nearest.dest
    }
  } else if (as.integer(nearest.dest) >= 1000L) {
    sel <- cholera::landmarksB$case == as.integer(nearest.dest)
    dest.nm <- cholera::landmarksB[sel, ]$name
    if (grepl("Square", dest.nm)) {
      sel <- cholera::landmarksB$case == nearest.dest
      tmp <- strsplit(cholera::landmarksB[sel, ]$name, "-")
      dest.nm <- unlist(tmp)[1]
    }
  }

  data.summary <- data.frame(orig = orgn,
                             dest = nearest.dest,
                             orig.nm = orgn.nm,
                             dest.nm = dest.nm,
                             distance = d,
                             time = walking.time,
                             type = type,
                             row.names = NULL)

  output <- list(ego = path.data$ego,
                 alter = path.data$alter,
                 data = data.summary,
                 destination = destination,
                 vestry = vestry,
                 distance.unit = distance.unit,
                 latlong = latlong,
                 case.set = case.set,
                 location = location,
                 pmp = pmp,
                 time.unit = time.unit,
                 walking.speed = walking.speed)

  class(output) <- "euclidean_path"
  output
}

#' Plot the path of the Euclidean distance between cases and/or pumps.
#'
#' @param x An object of class "euclidean_path" created by euclideanPath().
#' @param zoom Logical or Numeric. A numeric value >= 0 controls the degree of zoom. The default is 0.5.
#' @param long.title Logical. Tile with names.
#' @param mileposts Logical. Plot mile/time posts.
#' @param milepost.unit Character. "distance" or "time".
#' @param milepost.interval Numeric. Mile post interval unit of distance (yard or meter) or unit of time (seconds).
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.euclidean_path <- function(x, zoom = TRUE, long.title = TRUE,
  mileposts = TRUE, milepost.unit = "distance", milepost.interval = NULL,
  alpha.level = 1, ...) {

  path.data <- x$data
  type <- x$data$type
  ego.xy <- x$ego
  alter.xy <- x$alter
  dat <- rbind(alter.xy, ego.xy)
  pmp <- x$pmp
  orig <- path.data$orig
  dest <- path.data$dest

  colors <- snowColors(x$vestry)
  distance.unit <- x$distance.unit
  latlong <- x$latlong
  time.unit <- x$time.unit
  walking.speed <- x$walking.speed

  if (distance.unit == "meter") {
    d.unit <- "m"
  } else if (distance.unit == "yard") {
    d.unit <- "yd"
  }

  if (milepost.unit == "distance") {
    path.length <- path.data$distance
  } else if (milepost.unit == "time") {
    path.length <- (3600L * path.data$distance) / (1000L * walking.speed)
  }

  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  frame <- cholera::roads[cholera::roads$name == "Map Frame", ]

  fatality <- cholera::fatalities

  land <- cholera::landmarksB

  if (latlong) {
    ew <- "lon"
    ns <- "lat"
    asp <- 1.6
  } else {
    ew <- "x"
    ns <- "y"
    asp <- 1L
  }

  vars <- c(ew, ns)
  padding <- ifelse(latlong, 0.000125, 0.25)

  if (is.logical(zoom)) {
    if (zoom) {
      map.data <- mapDataRange(dat, land, path.data, vars, ew, ns)
      xlim <- c(min(map.data[, ew]) - padding, max(map.data[, ew]) + padding)
      ylim <- c(min(map.data[, ns]) - padding, max(map.data[, ns]) + padding)
    } else {
      map.data <- rbind(frame, rd)
      xlim <- range(map.data[, ew])
      ylim <- range(map.data[, ns])
    }
  } else if (is.numeric(zoom)) {
    if (zoom >= 0) {
      xlim <- c(min(dat[, ew]) - zoom * (padding),
                max(dat[, ew]) + zoom * (padding))
      ylim <- c(min(dat[, ns]) - zoom * (padding),
                max(dat[, ns]) + zoom * (padding))
    } else stop("If numeric, zoom must be >= 0.")
  }

  if (type == "case-pump") {
    p.sel <- paste0("p", path.data$dest)
    case.color <- grDevices::adjustcolor(colors[p.sel], alpha.f = alpha.level)
  } else {
    case.color <- "blue"
  }

  plot(rd[, vars], pch = NA, asp = asp, xlim = xlim, ylim = ylim)
  roads.list <- split(rd[, vars], rd$street)
  frame.list <- split(frame[, vars], frame$street)
  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(frame.list, lines))
  points(fatality[, vars], col = "lightgray", pch = 16, cex = 0.5)
  points(pmp[, vars], pch = 24, col = grDevices::adjustcolor(colors,
    alpha.f = alpha.level))
  text(pmp[, vars], pos = 1, labels = paste0("p", pmp$id))

  if (type %in% c("case-pump", "cases")) {
    if (orig < 1000L) {
      points(ego.xy, col = "red")
      text(ego.xy, pos = 1, labels = orig, col = "red")
    } else if (orig >= 1000L) {
      points(land[land$case == orig, vars], col = "red")
      land.tmp <- land[land$case == orig, ]

      if (grepl("Square", land.tmp$name)) {
        sq.label <- unlist(strsplit(land.tmp$name, "-"))[1]
        label.parse <- unlist(strsplit(sq.label, "[ ]"))
        sq.label <- paste0(label.parse[1], "\n", label.parse[2])
        obs.sq <- paste(label.parse, collapse = " ")
        sel <- cholera::landmark.squaresB$name == obs.sq
        text(cholera::landmark.squaresB[sel, c(ew, ns)], labels = sq.label,
          col = "red", cex = 0.8)
      } else {
        label.dat <- land.tmp[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
        names(label.dat) <- vars
        if (grepl("St", land.tmp$name)) {
          label.parse <- unlist(strsplit(land.tmp$name, "[ ]"))
          land.label <- paste0(paste(label.parse[1], label.parse[2]), "\n",
            label.parse[3])
        } else {
          label.parse <- unlist(strsplit(land.tmp$name, "[ ]"))
          if (length(label.parse) == 2) {
            land.label <- paste0(label.parse[1], "\n", label.parse[2])
          } else if (length(label.parse) == 3) {
            land.label <- paste0(label.parse[1], "\n", label.parse[2], "\n",
                                 label.parse[3])
          }
        }
        text(label.dat, labels = land.label, col = "red", cex = 0.8)
      }
    }

    if (type == "cases") {
      if (dest < 1000L) {
        points(alter.xy, col = "red")
        text(alter.xy, pos = 1, labels = dest, col = "red")
      } else if (dest >= 1000L) {
        points(land[land$case == dest, vars], col = "red")
        land.tmp <- land[land$case == dest, ]
        if (grepl("Square", land.tmp$name)) {
          sel <- cholera::landmark.squaresB$name == path.data$dest.nm
          label.dat <- cholera::landmark.squaresB[sel, ]
          label.parse <- unlist(strsplit(label.dat$name, "[ ]"))
          sq.label <- paste0(label.parse[1], "\n", label.parse[2])
          text(label.dat[, c(ew, ns)], labels = sq.label, col = "red",
            cex = 0.8)
        } else if (land.tmp[, ew] != land.tmp[, paste0(ew, ".lab")]) {
          label.dat <- land.tmp[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
          names(label.dat) <- vars
          if (grepl("St", land.tmp$name)) {
            label.parse <- unlist(strsplit(land.tmp$name, "[ ]"))
            land.label <- paste0(paste(label.parse[1], label.parse[2]), "\n",
                                       label.parse[3])
          } else {
            label.parse <- unlist(strsplit(land.tmp$name, "[ ]"))
            if (length(label.parse) == 2) {
              land.label <- paste0(label.parse[1], "\n", label.parse[2])
            } else if (length(label.parse) == 3) {
              land.label <- paste0(label.parse[1], "\n", label.parse[2], "\n",
                                   label.parse[3])
            }
          }
          text(label.dat, labels = land.label, col = "red", cex = 0.8)
        } else {
          label.dat <- land.tmp[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
          names(label.dat) <- vars
          label.parse <- unlist(strsplit(land.tmp$name, "[ ]"))
          land.label <- paste0(label.parse[1], "\n", label.parse[2])
          text(land[land$case == dest, vars], labels = land.label, col = "red",
            cex = 0.8)
        }
      }
    }
  }

  if (x$location == "orthogonal") points(ego.xy[, vars], pch = 0)

  arrows(ego.xy[, ew], ego.xy[, ns], alter.xy[, ew], alter.xy[, ns],
    col = case.color, lwd = 3, length = 0.075)

  d <- paste(round(path.data$distance, 1), d.unit)
  t <- paste(round(path.data$time, 1), paste0(time.unit, "s"), "@",
    walking.speed, "km/hr")

  if (mileposts) {
    if (is.null(milepost.interval)) {
      if (milepost.unit == "distance") {
        milepost.interval <- 50
      } else if (milepost.unit == "time") {
        milepost.interval <- 60
      }
    }

    if (milepost.unit == "distance") {
      h <- seq(0, path.data$distance, milepost.interval)
      if (isFALSE(latlong)) h <- h / unitMeter(1)
    } else if (milepost.unit == "time") {
      h <- seq(0, path.data$time, milepost.interval)
      if (isFALSE(latlong)) {
        h <- h * 1000 * x$walking.speed / 60^2 / unitMeter(1)
      }
    } else stop('Specify milepost.unit', call. = FALSE)

    if (latlong) ols <- stats::lm(lat ~ lon, data = dat)
    else ols <- stats::lm(y ~ x, data = dat)

    edge.slope <- stats::coef(ols)[2]
    theta <- ifelse(is.na(edge.slope), pi / 2, atan(edge.slope))

    if (latlong) {
      post.coords <- latlongEuclideanPosts(ego.xy, alter.xy, h, ew, ns)
    } else {
      post.coords <- quandrantCoordinates(dat[2:1, ], h, theta)
    }

    arrow.data <- data.frame(x = c(post.coords[, ew], ego.xy[, ew]),
                             y = c(post.coords[, ns], ego.xy[, ns]))

    arrow.list <- lapply(seq_len(nrow(arrow.data) - 1), function(i) {
      a.data <- cbind(arrow.data[i, ], arrow.data[i + 1, ])
      stats::setNames(a.data, c(paste0(c(ew, ns), 1), paste0(c(ew, ns), 2)))
    })

    invisible(lapply(arrow.list, function(seg) {
      arrows(seg[, paste0(ew, 1)], seg[, paste0(ns, 1)],
             seg[, paste0(ew, 2)], seg[, paste0(ns, 2)],
             length = 0.075, col = case.color, lwd = 3, code = 1)
    }))

    if (milepost.unit == "distance") {
      if (distance.unit == "meter") {
        post.info <- paste("posts at", milepost.interval, "m intervals")
      } else if (distance.unit == "yard") {
        post.info <- paste("posts at", milepost.interval, "yd intervals")
      }
    } else if (milepost.unit == "time") {
      post.info <- paste("posts at", milepost.interval, "sec intervals")
    } else {
      stop('"milepost.unit" muster either be "distance" or "time".')
    }
    title(sub = paste(d, t, post.info, sep = "; "))
  } else {
    title(sub = paste(d, t, sep = "; "))
  }

  longTitle(long.title, type, pmp, path.data, orig, land)
}

#' Print method for euclideanPath().
#'
#' Summary output.
#' @param x An object of class "euclidean_path" created by \code{euclideanPath()}.
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export

print.euclidean_path <- function(x, ...) {
  if (!inherits(x, "euclidean_path")) {
    stop('"x"\'s class must be "euclidean_path".')
  }
  print(x[c("ego", "alter", "data")])
}

casePumpEucl <- function(orgn, orgn.nm, destination, dstn, latlong, pmp,
  vestry, case.set, location) {

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  if (case.set == "observed") {
    if (location %in% c("nominal", "anchor")) {
      if (location == "anchor") {
        if (orgn %in% cholera::anchor.case$anchor == FALSE) {
          sel <- cholera::anchor.case$case %in% orgn
          orgn <- cholera::anchor.case[sel, "anchor"]
        }
      }

      fatal <- cholera::fatalities$case %in% orgn
      land <- cholera::landmarksB$case %in% orgn
      # sq <- cholera::landmark.squaresB$case %in% orgn  # via validateCase()!

      if (any(fatal) & any(land)) {
        a <- cholera::fatalities[fatal, vars]
        b <- cholera::landmarksB[land, vars]
        ego.coords <- rbind(a, b)
      } else if (all(!fatal) & any(land)) {
        ego.coords <- cholera::landmarksB[land, vars]
      } else if (any(fatal) & all(!land)) {
        ego.coords <- cholera::fatalities[fatal, vars]
      }
    } else if (location == "orthogonal") {
      if (latlong) {
        ortho <- cholera::latlong.ortho.addr

        sel <- cholera::anchor.case$case %in% orgn
        orgn <- cholera::anchor.case[sel, "anchor"]

        fatal <- ortho$case %in% orgn
        land <- cholera::landmarksB$case %in% orgn
      } else {
        ortho <- cholera::ortho.proj
        fatal <- ortho$case %in% orgn
        land <- cholera::landmarksB$case %in% orgn
      }

      if (any(fatal) & any(land)) {
        a <- ortho[fatal, vars]
        b <- cholera::landmarksB[land, vars]
        ego.coords <- rbind(a, b)
      } else if (all(!fatal) & any(land)) {
        ego.coords <- cholera::landmarksB[land, vars]
      } else if (any(fatal) & all(!land)) {
        ego.coords <- ortho[fatal, vars]
      }
    }

  } else if (case.set == "expected") {
    if (latlong) {
      if (location %in% c("anchor", "nominal")) {
        ego.coords <- cholera::latlong.regular.cases[, vars]
      } else if (location == "orthogonal") {
        ego.coords <- cholera::latlong.sim.ortho.proj[, vars]
      }
    } else {
      if (location %in% c("anchor", "nominal")) {
        ego.coords <- cholera::regular.cases[, vars]
      } else if (location == "orthogonal") {
        vars.ortho <- paste0(vars, ".proj")
        ego.coords <- cholera::sim.ortho.proj[, vars.ortho]
      }
    }
  }

  alter.coords <- pmp[pmp$id %in% dstn, vars]

  sel <- seq_len(nrow(alter.coords))

  if (nrow(ego.coords) == 1) {
    d <- stats::dist(rbind(ego.coords, alter.coords))[sel]
    d.sel <- which.min(d)

    nearest.pump <- pmp[d.sel, "id"]
    nearest.d <- min(d)

    ego <- ego.coords
    alter <- alter.coords[d.sel, ]
  } else if (length(ego.coords) > 1) {
    d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
      stats::dist(rbind(ego.coords[i, ], alter.coords))[sel]
    })
    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    orgn <- orgn[ego.id]
    orgn.nm <- orgn.nm[ego.id]
    d <- d.multi.ego[[ego.id]]
    nearest.pump <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords[ego.id, ]
    alter <- alter.coords[which.min(d), ]
  }

  data.summary <- data.frame(orgn = orgn, orgn.nm = orgn.nm,
    nearest.dest = nearest.pump, d = nearest.d)

  list(ego = ego, alter = alter, data = data.summary)
}

caseCaseEucl <- function(orgn, orgn.nm, destination, dstn, include.landmarks,
  latlong, origin, vestry, location) {

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  sq.cases <- sort(c(sqCases("Golden"), sqCases("Soho")))

  if (is.null(origin) & !is.null(destination)) {
    sq.destination <- (grepl("Square", destination) |
                       destination %in% sq.cases) &
                      is.null(origin)

    if (sq.destination) {
      if (is.character(orgn)) variable <- "name"
      else if (is.numeric(orgn)) variable <- "case"
      gold <- sqCases("Golden", variable)
      soho <- sqCases("Soho", variable)

       if (any(dstn %in% gold)) {
        sel <- !orgn %in% gold
        orgn <- orgn[sel]
        orgn.nm <- orgn.nm[sel]
      }

      if (any(dstn %in% soho)) {
        sel <- !orgn %in% soho
        orgn <- orgn[sel]
        orgn.nm <- orgn.nm[sel]
      }
    }

    if (any(dstn %in% orgn)) {
      sel <- !orgn %in% dstn
      orgn <- orgn[sel]
      orgn.nm <- orgn.nm[sel]
    }
  }

  # Origin (egos) #

  if (location %in% c("nominal", "anchor")) {
    if (location == "anchor") {
      if (orgn %in% cholera::anchor.case$anchor == FALSE) {
        sel <- cholera::anchor.case$case %in% orgn
        orgn <- cholera::anchor.case[sel, "anchor"]
      }
    }

    fatal <- cholera::fatalities$case %in% orgn
    land <- cholera::landmarksB$case %in% orgn

    if (any(fatal) & any(land)) {
      a <- cholera::fatalities[fatal, vars]
      b <- cholera::landmarksB[land, vars]
      ego.coords <- rbind(a, b)
      orgn <- c(a$case, b$case)

    } else if (all(!fatal) & any(land)) {
      ego.coords <- cholera::landmarksB[land, vars]
      orgn <- cholera::landmarksB[land, "case"]

    } else if (any(fatal) & all(!land)) {
      ego.coords <- cholera::fatalities[fatal, vars]
      orgn <- cholera::fatalities[fatal, "case"]
    }

    if (!is.null(origin) & is.null(destination)) {
      sq.origin <- (grepl("Square", origin) |
                    origin %in% sq.cases) &
                   is.null(destination)

      if (any(sq.origin)) {
        if (is.character(dstn)) variable <- "name"
        else if (is.numeric(dstn)) variable <- "case"
        gold <- sqCases("Golden", variable)
        soho <- sqCases("Soho", variable)
        if (any(orgn %in% gold)) dstn <- dstn[!dstn %in% gold]
        if (any(orgn %in% soho)) dstn <- dstn[!dstn %in% soho]
      }
    }

    # Destination (alters) #

    if (location == "anchor") {
      if (dstn %in% cholera::anchor.case$anchor == FALSE) {
        sel <- cholera::anchor.case$case %in% dstn
        dstn <- cholera::anchor.case[sel, "anchor"]
      }
    }

    if (any(orgn %in% dstn)) dstn <- dstn[!dstn %in% orgn]

    fatal <- cholera::fatalities$case %in% dstn
    land <- cholera::landmarksB$case %in% dstn

    if (any(fatal) & any(land)) {
      a <- cholera::fatalities[fatal, vars]
      b <- cholera::landmarksB[land, vars]
      alter.coords <- rbind(a, b)
      dstn <- c(a$case, b$case)

    } else if (all(!fatal) & any(land)) {
      alter.coords <- cholera::landmarksB[land, vars]
      dstn <- cholera::landmarksB[land, "case"]

    } else if (any(fatal) & all(!land)) {
      alter.coords <- cholera::fatalities[fatal, vars]
      dstn <- cholera::fatalities[fatal, "case"]
    }
  }

  sel <- seq_len(nrow(alter.coords))

  if (nrow(ego.coords) == 1) {
    d <- stats::dist(rbind(ego.coords, alter.coords))[sel]
    nearest.dest <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords
    alter <- alter.coords[which.min(d), ]
  } else if (length(ego.coords) > 1) {
    d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
      stats::dist(rbind(ego.coords[i, ], alter.coords))[sel]
    })
    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    orgn <- orgn[ego.id]
    orgn.nm <- orgn.nm[ego.id]
    d <- d.multi.ego[[ego.id]]
    nearest.dest <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords[ego.id, ]
    alter <- alter.coords[which.min(d), ]
  }

  data.summary <- data.frame(orgn = orgn, orgn.nm = orgn.nm,
    nearest.dest = nearest.dest, d = nearest.d)

  list(ego = ego, alter = alter, data = data.summary)
}

pumpPumpEucl <- function(orgn, orgn.nm, destination, dstn, latlong, origin, pmp,
  vestry, location) {

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  if (!is.null(origin) & is.null(destination)) {
    if (any(orgn %in% dstn)) dstn <- dstn[!dstn %in% orgn]
  }

  if (is.null(origin) & !is.null(destination)) {
    if (any(dstn %in% orgn)) orgn <- orgn[!orgn %in% dstn]
  }

  ego.coords <- pmp[pmp$id %in% orgn, vars]
  alter.coords <- pmp[pmp$id %in% dstn, vars]

  sel <- seq_len(nrow(alter.coords))

  if (nrow(ego.coords) == 1) {
    d <- stats::dist(rbind(ego.coords, alter.coords))[sel]
    nearest.pump <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords
    alter <- alter.coords[which.min(d), ]
  } else if (length(ego.coords) > 1) {
    d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
      stats::dist(rbind(ego.coords[i, ], alter.coords))[sel]
    })
    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    orgn <- orgn[ego.id]
    orgn.nm <- orgn.nm[ego.id]
    d <- d.multi.ego[[ego.id]]
    nearest.pump <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords[ego.id, ]
    alter <- alter.coords[which.min(d), ]
  }

  data.summary <- data.frame(orgn = orgn, orgn.nm = orgn.nm,
    nearest.dest = nearest.pump, d = nearest.d)

  list(ego = ego, alter = alter, data = data.summary)
}

latlongEuclideanPosts <- function(ego.xy, alter.xy, h, ew, ns) {
  origin <- data.frame(lon = min(cholera::roads[, ew]),
                       lat = min(cholera::roads[, ns]))

  ego.cartesian <- latlongCartesian(ego.xy, origin)
  alter.cartesian <- latlongCartesian(alter.xy, origin)
  meter.coords <- rbind(ego.cartesian, alter.cartesian)

  ols <- stats::lm(y ~ x, data = meter.coords)
  path.slope <- stats::coef(ols)[2]
  theta <- ifelse(is.na(path.slope), pi / 2, atan(path.slope))

  cartesian.posts <- quandrantCoordinates(meter.coords, h, theta)

  conversion <- lapply(seq_len(nrow(cartesian.posts)), function(i) {
    coords.tmp <- cartesian.posts[i, c("x", "y")]
    meterLatLong(coords.tmp)[, c("lon", "lat")]
  })

  data.frame(post = h, do.call(rbind, conversion))
}

latlongCartesian <- function(xy, origin) {
  x.proj <- c(xy$lon, origin$lat)
  y.proj <- c(origin$lon, xy$lat)
  m.lon <- geosphere::distGeo(y.proj, xy)
  m.lat <- geosphere::distGeo(x.proj, xy)
  data.frame(x = m.lon, y = m.lat)
}
