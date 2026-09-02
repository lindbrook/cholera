#' Compute Euclidean path coordinates from observed case/landmark to nearest/selected pump.
#'
#' @param origin Numeric. Vector of origin(s) (numeric ID or character name landmark/pump ).
#' @param destination Numeric. Vector of destination(s) (numeric or landmark/pump name).
#' @param type Character. Path case to pump. FALSE is all other combinations of cases, landmarks and pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param latlong Logical.
#' @param case.set Character. "observed" or "expected".
#' @param location Character. For cases and pumps. "nominal", "anchor" or "orthogonal".
#' @param distance.unit Character. Unit of distance: "meter" or "yard".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param include.landmarks Logical. Include landmarks as cases.
#' @param anchor.anchor Logical. Use anchor cases.
#' @importFrom geosphere distGeo
#' @export

euclideanPath <- function(origin = 1, destination = NULL, type = "case-pump",
  vestry = FALSE, latlong = FALSE, case.set = "observed", location = "nominal",
  distance.unit = "meter", time.unit = "second", walking.speed = 5, 
  include.landmarks = TRUE, anchor.anchor = TRUE) {

  if (is.null(origin) & is.null(destination)) {
    stop("You must provide at least one origin or destination.", call. = FALSE)
  }

  if (!type %in% c("case-pump", "cases", "pumps")) {
    stop('type must be "case-pump", "cases" or "pumps".', call. = FALSE)
  }

  if (!case.set %in% c("observed", "expected")) {
    stop('case.set must be "observed" or "expected".', call. = FALSE)
  }

  if (!location %in% c("nominal", "anchor", "orthogonal")) {
    stop('type must be "anchor", "nominal" or "orthogonal".', call. = FALSE)
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
    origin.chk <- validateCase(origin, case.set)
    orgn <- origin.chk$out
    orgn.nm <- origin.chk$out.nm

    destination.chk <- validatePump(destination, pmp, vestry)
    dstn <- destination.chk$out
    dstn.nm <- destination.chk$out.nm

  } else if (type == "cases") {
    origin.chk <- validateCase(origin, case.set)
    destination.chk <- validateCase(destination, case.set)
    
    OD <- list(orgn = orgnDstn(origin.chk$out,case.set = case.set),
               dstn = orgnDstn(destination.chk$out, case.set = case.set))
    
    orgn.all <- OD$orgn
    dstn.all <- OD$dstn
    
    landmarks.test <- case.set == "expected" &
                      all(OD$orgn$id >= 1000L & OD$orgn$id < 2000L) &
                      all(OD$dstn$id >= 1000L & OD$dstn$id < 2000L)
    
    if (landmarks.test) case.set <- "observed"

  } else if (type == "pumps") {
    origin.chk <- validatePump(origin, pmp, vestry)
    orgn <- origin.chk$out
    orgn.nm <- origin.chk$out.nm

    destination.chk <- validatePump(destination, pmp, vestry)
    dstn <- destination.chk$out
    dstn.nm <- destination.chk$out.nm
  }

  if (type == "case-pump") {
    path.data <- casePumpEucl(orgn, orgn.nm, destination, dstn, dstn.nm,
      latlong, pmp, vestry, case.set, location)
  } else if (type == "cases") {
    path.data <- caseCaseEucl(origin, destination, latlong, vestry, case.set, 
      OD, location, anchor.anchor)
  } else if (type == "pumps") {
    path.data <- pumpPumpEucl(orgn, orgn.nm, dstn, dstn.nm, origin, destination,
      latlong, pmp, vestry, location)
  }

  d <- path.data$data$d

  walking.time <- distanceTime(d, distance.unit = distance.unit,
    time.unit = time.unit, walking.speed = walking.speed)

  d <- unitMeter(d, distance.unit = distance.unit)

  if (type == "cases") {
    if (is.null(destination)) {
      alter.case <-  OD$dstn[OD$dstn$id == path.data$data$dstn, "id"]
    } else {
      alter.case <-  OD$dstn$id
    }
        
    data.summary <- data.frame(origin = path.data$data$orgn,
                               destination = path.data$data$dstn,
                               origin.nm = path.data$data$orgn.nm,
                               destination.nm = path.data$data$dstn.nm,
                               distance = d,
                               time = walking.time,
                               type = type,
                               ego.case = OD$orgn$id,
                               alter.case = alter.case,
                               row.names = NULL)
  } else {
    data.summary <- data.frame(origin = path.data$data$orgn,
                               destination = path.data$data$dstn,
                               origin.nm = path.data$data$orgn.nm,
                               destination.nm = path.data$data$dstn.nm,
                               distance = d,
                               time = walking.time,
                               type = type,
                               row.names = NULL)
  }

  output <- list(ego = path.data$ego,
                 alter = path.data$alter,
                 data.summary = data.summary,
                 origin = origin,
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
#' @param zoom Logical or Numeric. Positive numbers zoom in; negative numbers zoom out.
#' @param add Logical. Add graphic to plot.
#' @param long.title Logical. Tile with names.
#' @param mileposts Logical. Plot mile/time posts.
#' @param milepost.unit Character. "distance" or "time".
#' @param milepost.interval Numeric. Mile post interval unit of distance (yard or meter) or unit of time (seconds).
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @importFrom shape Arrowhead
#' @export

plot.euclidean_path <- function(x, zoom = TRUE, add = FALSE, long.title = TRUE,
  mileposts = TRUE, milepost.unit = "distance", milepost.interval = NULL,
  alpha.level = 1, ...) {

  data.summary <- x$data.summary
  type <- x$data$type
  ego.xy <- x$ego
  alter.xy <- x$alter
  latlong <- x$latlong

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

  if (x$location %in% c("anchor", "orthogonal")) {
    orgn.xy <- cholera::fatalities[cholera::fatalities$case %in% x$origin, vars]
  }

  if (exists("orgn.xy")) {
    dat <- rbind(alter.xy, ego.xy, orgn.xy)
  } else {
    dat <- rbind(alter.xy, ego.xy)
  }

  pmp <- x$pmp
  orig <- data.summary$origin
  dest <- data.summary$destination
  colors <- snowColors(x$vestry)
  distance.unit <- x$distance.unit
  time.unit <- x$time.unit
  walking.speed <- x$walking.speed

  if (distance.unit == "meter") {
    d.unit <- "m"
  } else if (distance.unit == "yard") {
    d.unit <- "yd"
  }

  if (milepost.unit == "distance") {
    path.length <- data.summary$distance
  } else if (milepost.unit == "time") {
    path.length <- (3600L * data.summary$distance) / (1000L * walking.speed)
  }

  rds <- cholera::roads
  frame <- cholera::roads[cholera::roads$name == "Map Frame", ]

  fatality <- cholera::fatalities

  sqB <- cholera::landmark.squares
  sqB$road.segment <- NA
  sqB$x.lab <- sqB$x
  sqB$y.lab <- sqB$y
  sqB$lon.lab <- sqB$lon
  sqB$lat.lab <- sqB$lat
  sqB <- sqB[, names(cholera::landmarks)]
  land <- rbind(sqB, cholera::landmarks)

  if (isFALSE(zoom)) {
    xlim <- range(rds[, ew])
    ylim <- range(rds[, ns])
  } else if (isTRUE(zoom) | is.numeric(zoom)) {
    if (is.logical(zoom) | zoom == 0) {
      padding <- ifelse(latlong, 0.0000125, 0.05)
      xlim <- c(min(dat[, ew]) - padding, max(dat[, ew]) + padding)
      ylim <- c(min(dat[, ns]) - padding, max(dat[, ns]) + padding)

    } else if (zoom != 0) {
      if (latlong) {
        cartesian.data <- lapply(seq_len(nrow(dat)), function(i) {
          geoCartesianCoord(dat[i, ])
        })

        cartesian.data <- do.call(rbind, cartesian.data)
        cart.x.range <- range(cartesian.data$x)
        cart.y.range <- range(cartesian.data$y)

        pad <- c(zoom, -zoom)
        xlim <- cart.x.range + pad
        ylim <- cart.y.range + pad

        xlim.delta <- xlim[2] - xlim[1]
        ylim.delta <- ylim[2] - ylim[1]

        if (xlim.delta <= 0 | ylim.delta <= 0) {
          xlim <- cart.x.range
          ylim <- cart.y.range
          message("Note: zoom = ", zoom, " too far! Use smaller.")
        }

        range.data <- meterLatLong(data.frame(x = xlim, y = ylim))
        xlim <- range.data$lon
        ylim <- range.data$lat

      } else {
        xlim <- range(dat[, ew])
        ylim <- range(dat[, ns])

        ols <- stats::lm(y ~ x, data = data.frame(x = xlim, y = ylim))
        slope <- stats::coef(ols)[2]
        theta <- atan(slope)

        pad <- abs(zoom) / unitMeter(1)
        delta.x <- abs(pad * cos(theta))
        delta.y <- abs(pad * sin(theta))

        if (zoom < 0) {
          xlim <- c(xlim[1] - delta.x, xlim[2] + delta.x)
          ylim <- c(ylim[1] - delta.y, ylim[2] + delta.y)
        } else if (zoom > 0) {
          xlim <- c(xlim[1] + delta.x, xlim[2] - delta.x)
          ylim <- c(ylim[1] + delta.y, ylim[2] - delta.y)
        }

        xlim.delta <- xlim[2] - xlim[1]
        ylim.delta <- ylim[2] - ylim[1]

        if (xlim.delta <= 0 | ylim.delta <= 0) {
          xlim <- range(dat[, ew])
          ylim <- range(dat[, ns])
          message("Note: zoom = ", zoom, " too far! Use smaller.")
        }
      }
    }
  }

  if (type == "case-pump") {
    p.sel <- paste0("p", data.summary$destination)
    case.color <- grDevices::adjustcolor(colors[p.sel], alpha.f = alpha.level)
  } else {
    case.color <- "blue"
  }

  if (!add) {
    plot(rds[, vars], pch = NA, asp = asp, xlim = xlim, ylim = ylim)
    roads.list <- split(rds[, vars], rds$street)
    frame.list <- split(frame[, vars], frame$street)
    invisible(lapply(roads.list, lines, col = "lightgray"))
    invisible(lapply(frame.list, lines))
    points(fatality[, vars], col = "lightgray", pch = 16, cex = 0.5)
    points(pmp[, vars], pch = 24, col = grDevices::adjustcolor(colors,
      alpha.f = alpha.level))
    text(pmp[, vars], pos = 1, labels = paste0("p", pmp$id))
  }

  if (type %in% c("case-pump", "cases")) {
    if (orig < 1000L | orig > 2000L) {
      points(ego.xy, col = "red")
      text(ego.xy, pos = 1, labels = orig, col = "red")
      if (x$location %in% c("anchor", "orthogonal")) {
        if (exists("orgn.xy")) {
          points(orgn.xy)
          text(orgn.xy, pos = 1, labels = x$origin)
        }
      }
    } else if (orig >= 1000L & orig < 2000L) {
      points(land[land$case == orig, vars], col = "red")
      land.tmp <- land[land$case == orig, ]

      if (grepl("Square", land.tmp$name)) {
        sq.label <- unlist(strsplit(land.tmp$name, "-"))[1]
        label.parse <- unlist(strsplit(sq.label, "[ ]"))
        sq.label <- paste0(label.parse[1], "\n", label.parse[2])
        obs.sq <- paste(label.parse, collapse = " ")
        sel <- cholera::landmark.squares$name == obs.sq
        text(cholera::landmark.squares[sel, c(ew, ns)], labels = sq.label,
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
      if (dest < 1000L | dest > 2000L) {
        points(alter.xy, col = "red")
        text(alter.xy, pos = 1, labels = dest, col = "red")
      } else if (dest >= 1000L & dest < 2000L) {
        points(land[land$case == dest, vars], col = "red")
        land.tmp <- land[land$case == dest, ]
        if (grepl("Square", land.tmp$name) & !grepl("Square-", land.tmp$name)) {
          sel <- cholera::landmark.squares$name == data.summary$destination.nm
          label.dat <- cholera::landmark.squares[sel, ]
          label.parse <- unlist(strsplit(label.dat$name, "[ ]"))
          sq.label <- paste0(label.parse[1], "\n", label.parse[2])
          text(label.dat[, c(ew, ns)], labels = sq.label, col = "red",
            cex = 0.8)
        } else if (grepl("Square-", land.tmp$name)) {
          sel <- land$case == data.summary$destination
          label.dat <- land[sel, ]
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

  d <- paste(round(data.summary$distance, 1), d.unit)
  t <- paste(round(data.summary$time, 1), paste0(time.unit, "s"), "@",
    walking.speed, "km/hr")

  segments(alter.xy[, ew], alter.xy[, ns], ego.xy[, ew], ego.xy[, ns], lwd = 2, 
    col = case.color)

  if (mileposts) {
    if (is.null(milepost.interval)) {
      if (milepost.unit == "distance") {
        milepost.interval <- 50
      } else if (milepost.unit == "time") {
        milepost.interval <- 60
      }
    }

    if (milepost.unit == "distance") {
      h <- seq(0, data.summary$distance, milepost.interval)
      if (isFALSE(latlong)) h <- h / unitMeter(1)
    } else if (milepost.unit == "time") {
      h <- seq(0, data.summary$time, milepost.interval)
      if (isFALSE(latlong)) {
        h <- h * 1000 * x$walking.speed / 60^2 / unitMeter(1)
      }
    } else stop('Specify milepost.unit', call. = FALSE)

    ptp <- rbind(alter.xy, ego.xy)

    if (latlong) ols <- stats::lm(lat ~ lon, data = ptp)
    else ols <- stats::lm(y ~ x, data = ptp)

    seg.slope <- stats::coef(ols)[2]
    theta <- ifelse(is.na(seg.slope), pi / 2, atan(seg.slope))
    angle <- theta * 180L / pi

    second.quadrant <- all(sign(alter.xy - ego.xy) == data.frame(-1,  1))
    fourth.quadrant <- all(sign(alter.xy - ego.xy) == data.frame(-1, -1))

    if (second.quadrant) angle <- angle + 180L
    if (fourth.quadrant) angle <- angle - 180L

    if (mileposts) {
      if (latlong) {
        arrow.head <- latlongEuclideanPosts(ego.xy, alter.xy, h, ew, ns)
      } else {
        arrow.head <- quadrantCoordinates(ptp[2:1, ], h, theta)
      }

      shape::Arrowhead(arrow.head[, ew], arrow.head[, ns], angle = angle, 
        arr.adj = -1, arr.col = case.color, arr.length = 0.1, 
        arr.type = "triangle", arr.width = 0.125, lcol = case.color)
  }

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
    if (!add) title(sub = paste(d, t, post.info, sep = "; "))
  } else {
    if (!add) title(sub = paste(d, t, sep = "; "))
  }

  if (!add) longTitle(long.title, type, pmp, orig, dest, land, x,
    rbind(ego.xy, alter.xy))
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
  print(x[c("ego", "alter", "data.summary")])
}

casePumpEucl <- function(orgn, orgn.nm, destination, dstn, dstn.nm, latlong,
  pmp, vestry, case.set, location) {

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  if (case.set == "observed") {
    vars.lndmrk <- c("case", vars, "name")
    lndmrk <- rbind(cholera::landmark.squares[, vars.lndmrk],
                    cholera::landmarks[, vars.lndmrk])

    if (any(orgn < 1000L)) {
      if (location %in% c("anchor", "orthogonal")) {
        if (any(!orgn %in% cholera::anchor.case$anchor)) {
          sel <- cholera::anchor.case$case %in% orgn
          orgn <- cholera::anchor.case[sel, "anchor"]
          orgn.nm <- paste(orgn)
        }
      }
    }

    if (location %in% c("nominal", "anchor")) {
      fatal <- cholera::fatalities$case %in% orgn
      land <- lndmrk$case %in% orgn

      if (any(fatal) & any(land)) {
        a <- cholera::fatalities[fatal, vars]
        b <- lndmrk[land, vars]
        ego.coords <- rbind(a, b)
      } else if (all(!fatal) & any(land)) {
        ego.coords <- lndmrk[land, vars]
      } else if (any(fatal) & all(!land)) {
        ego.coords <- cholera::fatalities[fatal, vars]
      }
    } else if (location == "orthogonal") {
      if (latlong) {
        ortho <- cholera::latlong.ortho.anchor

        sel <- cholera::anchor.case$case %in% orgn
        orgn <- cholera::anchor.case[sel, "anchor"]

        fatal <- ortho$case %in% orgn
        land <- lndmrk$case %in% orgn
      } else {
        ortho <- cholera::ortho.proj
        names(ortho)[names(ortho) %in% paste0(vars, ".proj")] <- vars
        fatal <- ortho$case %in% orgn
        land <- lndmrk$case %in% orgn
      }

      if (any(fatal) & any(land)) {
        a <- ortho[fatal, vars]
        b <- lndmrk[land, vars]
        ego.coords <- rbind(a, b)
      } else if (all(!fatal) & any(land)) {
        ego.coords <- lndmrk[land, vars]
      } else if (any(fatal) & all(!land)) {
        ego.coords <- ortho[fatal, vars]
      }
    }

  } else if (case.set == "expected") {
    if (latlong) {
      if (location %in% c("anchor", "nominal")) {
        ego.coords <- cholera::latlong.regular.cases[orgn, vars]
      } else if (location == "orthogonal") {
        ego.coords <- cholera::latlong.sim.ortho.proj[orgn, vars]
      }
    } else {
      if (location %in% c("anchor", "nominal")) {
        ego.coords <- cholera::regular.cases[orgn, vars]
      } else if (location == "orthogonal") {
        vars.ortho <- paste0(vars, ".proj")
        ego.coords <- cholera::sim.ortho.proj[orgn, vars.ortho]
        names(ego.coords) <- vars
      }
    }
  }

  alter.coords <- pmp[pmp$id %in% dstn, vars]

  if (latlong) {
    if (nrow(ego.coords) == 1) {
      ds <- geosphere::distGeo(ego.coords, alter.coords) / unitMeter(1)
    } else if (nrow(ego.coords) > 1) {
      d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
        geosphere::distGeo(ego.coords[i, ], alter.coords) / unitMeter(1)
      })
      ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
      orgn <- orgn[ego.id]
      orgn.nm <- orgn.nm[ego.id]
      ds <- d.multi.ego[[ego.id]]
    }

    d <- min(ds)
    alter.id <- which.min(ds)
    dstn <- dstn[alter.id]
    dstn.nm <- dstn.nm[alter.id]

    if (nrow(ego.coords) == 1) ego <- ego.coords
    else if (nrow(ego.coords) > 1) ego <- ego.coords[ego.id, ]

    alter <- alter.coords[alter.id, ]

  } else {
    if (nrow(ego.coords) == 1 & nrow(alter.coords) == 1) {
      d <- stats::dist(rbind(ego.coords, alter.coords))
      ego <- ego.coords
      alter <- alter.coords

    } else if (nrow(ego.coords) == 1 & nrow(alter.coords) > 1) {
      d.sel <- seq_len(nrow(alter.coords))
      ds <- stats::dist(rbind(ego.coords, alter.coords))[d.sel]

      d <- min(ds)
      alter.id <- which.min(ds)
      dstn <- dstn[alter.id]
      dstn.nm <- dstn.nm[alter.id]

      ego <- ego.coords
      alter <- alter.coords[alter.id, ]

    } else if (nrow(ego.coords) > 1 & nrow(alter.coords) == 1) {
      ds <- vapply(seq_len(nrow(ego.coords)), function(i) {
        stats::dist(rbind(ego.coords[i, ], alter.coords))
      }, numeric(1L))

      d <- min(ds)

      ego.id <- which.min(ds)
      orgn <- orgn[ego.id]
      orgn.nm <- orgn.nm[ego.id]

      ego <- ego.coords[ego.id, ]
      alter <- alter.coords

    } else if (nrow(ego.coords) > 1 & nrow(alter.coords) > 1) {
      d.sel <- seq_len(nrow(alter.coords))
      d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
        stats::dist(rbind(ego.coords[i, ], alter.coords))[d.sel]
      })

      d <- min(unlist(d.multi.ego))

      ego.dist <- vapply(d.multi.ego, min, numeric(1L))
      ego.id <- which.min(ego.dist)
      orgn <- orgn[ego.id]
      orgn.nm <- orgn.nm[ego.id]

      alter.dist <- d.multi.ego[[ego.id]]
      alter.id <- which.min(alter.dist)
      dstn <- dstn[alter.id]
      dstn.nm <- dstn.nm[alter.id]

      ego <- ego.coords[ego.id, ]
      alter <- alter.coords[alter.id, ]
    }
  }

  data.summary <- data.frame(orgn = orgn, orgn.nm = orgn.nm,
    dstn = dstn, dstn.nm = dstn.nm, d = d)

  list(ego = ego, alter = alter, data = data.summary)
}

caseCaseEucl <- function(origin, destination, latlong, vestry, case.set, OD, 
  location, anchor.anchor) {

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  orgn <- OD$orgn
  dstn <- OD$dstn
  
  vars.lndmrk <- c("case", vars, "name")
  lndmrk <- rbind(cholera::landmark.squares[, vars.lndmrk],
                  cholera::landmarks[, vars.lndmrk])

  if (case.set == "observed") {
    if (anchor.anchor) {
      anchr <- unique(cholera::anchor.case$anchor)
      
      stack.cases <- lapply(anchr, function(x) {
        sel <- cholera::anchor.case$anchor == x
        cases <- cholera::anchor.case[sel, "case"]
        cases[cases != x]
      })
      
      names(stack.cases) <- anchr
      
      # Origins
      
      sel <- OD$orgn$id %in% lndmrk$case
      orgn.case <- OD$orgn[!sel, ]
      orgn.land <- OD$orgn[sel, ]
      
      if (nrow(orgn.case) > 0 & nrow(orgn.land) > 0) {
        orgn <- OD$orgn[OD$orgn$id %in% c(anchr, orgn.land$id), ]
        
      } else if (nrow(orgn.case) > 0 & nrow(orgn.land) == 0) {
        if (any(orgn.case$id %in% anchr)) {
          orgnA <- orgn.case[orgn.case$id %in% anchr, ]
        }
        
        if (any(!orgn.case$id %in% anchr)) {
          orgnB <- orgn.case[!orgn.case$id %in% anchr, ]
          stk <- vapply(stack.cases, function(x) orgnB$id %in% x, logical(1L))
          stack.anchor <- as.numeric(names(which(stk)))
          orgnB$id.nm <- stack.anchor
          orgnB$id <- as.numeric(stack.anchor)
        }
        
        if (exists("orgnA") & exists("orgnB")) {
          tmp <- rbind(orgnA, orgnB)
          tmp <- tmp[order(tmp$id), ]
          row.names(tmp) <- NULL
          orgn <- tmp
        } else if (exists("orgnA") & !exists("orgnB")) {
          orgn <- orgnA
        } else if (!exists("orgnA") & exists("orgnB")) {
          orgn <- orgnB
        }
        
      } else if (nrow(orgn.case) == 0 & nrow(orgn.land) > 0) {
        orgn <- orgn.land
      }
      
      # Destinations

      sel <- OD$dstn$id %in% lndmrk$case
      dstn.case <- OD$dstn[!sel, ]
      dstn.land <- OD$dstn[sel, ]
      
      if (nrow(dstn.case) > 0 & nrow(dstn.land) > 0) {
        dstn <- OD$dstn[OD$dstn$id %in% c(anchr, dstn.land$id), ]
      } else if (nrow(dstn.case) > 0 & nrow(dstn.land) == 0) {  
        if (any(dstn.case$id %in% anchr)) {
          dstnA <- dstn.case[dstn.case$id %in% anchr, ]
        }
        
        if (any(!dstn.case$id %in% anchr)) {
          dstnB <- dstn.case[!dstn.case$id %in% anchr, ]
          stk <- vapply(stack.cases, function(x) OD$dstn$id %in% x, logical(1L))
          stack.anchor <- as.numeric(names(which(stk)))
          dstnB$id.nm <- stack.anchor
          dstnB$id <- as.numeric(stack.anchor)
        }
        
        if (exists("dstnA") & exists("dstnB")) {
          tmp <- rbind(dstnA, dstnB)
          tmp <- tmp[order(tmp$id), ]
          row.names(tmp) <- NULL
          dstn <- tmp
        } else if (exists("dstnA") & !exists("dstnB")) {
          dstn <- dstnA
        } else if (!exists("dstnA") & exists("dstnB")) {
          dstn <- dstnB
        }
        
      } else if (nrow(dstn.case) == 0 & nrow(dstn.land) > 0) {
        dstn <- dstn.land
      }
    }
  }

  # exclude Square street exits #

  if (all(orgn$id < 2000L)) {
    fatal <- orgn$id[orgn$id < 1000L]
    if (is.null(destination)) {
      sel <- orgn$id >= 1000L & orgn$id < 1002L | orgn$id > 1011L
    } else {
      sel <- orgn$id >= 1000L
    }
    land <- orgn$id[sel]
    orgn <- orgn[orgn$id %in% c(fatal, land), ]
  }

  if (all(dstn$id < 2000L)) {
    fatal <- dstn$id[dstn$id < 1000L]
    if (is.null(destination)) {
      sel <- dstn$id >= 1000L & dstn$id < 1002L | dstn$id > 1011L
    } else {
      sel <- dstn$id >= 1000L
    }
    land <- dstn$id[sel]
    dstn <- dstn[dstn$id %in% c(fatal, land), ]
  }
    
  ## St James Workhouse: identical single origin and destination
  
  workhouse <- c(369, 1019)
  if (isFALSE(anchor.anchor)) workhouse <- c(workhouse, stack.cases$`369`)
  
  if (any(workhouse %in% orgn$id) & any(workhouse %in% dstn$id)) {
    if (any(workhouse %in% orgn$id)) {
      dstn <- dstn[!dstn$id %in% workhouse, ]
      if (all(workhouse %in% orgn$id)) {
        if (workhouse[1] %in% orgn$id) {
          orgn <- orgn[orgn$id != workhouse[2], ]
        } else if (workhouse[2] %in% orgn$id) {
          orgn <- orgn[orgn$id != workhouse[1], ]
        }
      }
    }
  }

  ## origin-destination intersection overlap

  if (length(intersect(orgn$id, dstn$id)) != 0) {
    if (!is.null(origin) & is.null(destination) | all(destination < 0)) {
      dstn <- dstn[dstn$id %in% setdiff(dstn$id, orgn$id), ]
    } else if (is.null(origin) & !is.null(destination)) {
      orgn <- orgn[orgn$id %in% setdiff(orgn$id, dstn$id), ]
    }
  }
  
  # data #

  if (case.set == "observed") {
    if (location == "orthogonal") {
      if (latlong) {
        fatality <- cholera::latlong.ortho.anchor
      } else {
        fatality <- cholera::ortho.proj
      }
      names(fatality)[names(fatality) %in% c("x.proj", "y.proj")] <- vars
    } else {
      fatality <- cholera::fatalities
    }
  } else if (case.set == "expected") {
    if (location == "orthogonal") {
      if (latlong) {
        fatality <- cholera::latlong.sim.ortho.proj
      } else {
        fatality <- cholera::sim.ortho.proj
      }
      names(fatality)[names(fatality) %in% c("x.proj", "y.proj")] <- vars
    } else {
      if (latlong) fatality <- cholera::latlong.regular.cases  
      else fatality <- cholera::regular.cases
      fatality$case <- seq_along(fatality$x) + 2000L
    }
  }

  # coordinates #

  if (!is.null(origin) & !is.null(destination)) {
    # Origin (egos) #
    fatal.sel <- fatality$case %in% orgn$id
    land.sel <- lndmrk$case %in% orgn$id

    if (any(fatal.sel) & any(land.sel)) {
      a <- fatality[fatal.sel, vars]
      b <- lndmrk[land.sel, vars]
      ego.coords <- rbind(a, b)
    } else if (all(!fatal.sel) & any(land.sel)) {
      ego.coords <- lndmrk[land.sel, vars]
    } else if (any(fatal.sel) & all(!land.sel)) {
      ego.coords <- fatality[fatal.sel, vars]
    }
    
    # Destination (alters) #
    fatal.sel <- fatality$case %in% dstn$id
    land.sel <- lndmrk$case %in% dstn$id

    if (any(fatal.sel) & any(land.sel)) {
      a <- fatality[fatal.sel, vars]
      b <- lndmrk[land.sel, vars]
      alter.coords <- rbind(a, b)
    } else if (all(!fatal.sel) & any(land.sel)) {
      alter.coords <- lndmrk[land.sel, vars]
    } else if (any(fatal.sel) & all(!land.sel)) {
      alter.coords <- fatality[fatal.sel, vars]
    }

  } else if (is.null(origin) & !is.null(destination)) {
    sel <- orgn$id %in% cholera::landmark.squares$case |
           orgn$id %in% cholera::landmarks$case
    orgn <- orgn[!sel, ]
    ego.coords <- fatality[fatality$case %in% orgn$id, vars]
    alter.coords <- rbind(fatality[fatality$case %in% dstn$id, vars],
                          lndmrk[lndmrk$case %in% dstn$id, vars])

  } else if (!is.null(origin) & is.null(destination)) {
    ego.coords <- rbind(fatality[fatality$case %in% orgn$id, vars],
                        lndmrk[lndmrk$case %in% orgn$id, vars])

    sel <- dstn$id %in% cholera::landmark.squares$case |
           dstn$id %in% cholera::landmarks$case
    dstn <- dstn[!sel, ]
    alter.coords <- fatality[fatality$case %in% dstn$id, vars]
  }

  if (latlong) {
    if (nrow(ego.coords) == 1) {
      d <- geosphere::distGeo(ego.coords, alter.coords) / unitMeter(1)
    } else if (nrow(ego.coords) > 1) {
      d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
        geosphere::distGeo(ego.coords[i, ], alter.coords) / unitMeter(1)
      })
      ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
      orgn <- orgn[ego.id]
      orgn.nm <- orgn$nm[ego.id]
      d <- d.multi.ego[[ego.id]]
    }
  } else {
    d.sel <- seq_len(nrow(alter.coords))

    if (nrow(ego.coords) == 1) {
      d <- stats::dist(rbind(ego.coords, alter.coords))[d.sel]
    } else if (nrow(ego.coords) > 1) {
      d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
        stats::dist(rbind(ego.coords[i, ], alter.coords))[d.sel]
      })
      ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
      orgn <- orgn[ego.id]
      orgn.nm <- orgn.nm[ego.id]
      d <- d.multi.ego[[ego.id]]
    }
  }

  nearest.dest <- dstn[which.min(d), "id"]
  nearest.d <- min(d)

  if (nrow(ego.coords) == 1) {
    ego <- ego.coords
  } else if (nrow(ego.coords) > 1) {
    ego <- ego.coords[ego.id, ]
  }

  alter <- alter.coords[which.min(d), ]

  data.summary <- data.frame(orgn = orgn$id,
    orgn.nm = ifelse(is.na(orgn$name), orgn$id.nm, orgn$name),
    dstn = nearest.dest, dstn.nm = dstn[which.min(d), "id.nm"], d = nearest.d,
    ego.case = orgn$id, alter.case = nearest.dest)
  
  list(ego = ego, alter = alter, data.summary = data.summary)
}

pumpPumpEucl <- function(orgn, orgn.nm, dstn, dstn.nm, origin, destination,
  latlong, pmp, vestry, location) {

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  if (!is.null(origin) & is.null(destination)) {
    if (any(orgn %in% dstn)) dstn <- dstn[!dstn %in% orgn]
  }

  if (is.null(origin) & !is.null(destination)) {
    if (any(dstn %in% orgn)) orgn <- orgn[!orgn %in% dstn]
  }

  dstn <- setdiff(dstn, orgn)

  ego.coords <- pmp[pmp$id %in% orgn, vars]
  alter.coords <- pmp[pmp$id %in% dstn, vars]

  if (latlong) {
    if (nrow(ego.coords) == 1) {
      d <- geosphere::distGeo(ego.coords, alter.coords) / unitMeter(1)
    } else if (nrow(ego.coords) > 1) {
      d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
        geosphere::distGeo(ego.coords[i, ], alter.coords) / unitMeter(1)
      })
      ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
      orgn <- orgn[ego.id]
      orgn.nm <- orgn.nm[ego.id]
      d <- d.multi.ego[[ego.id]]
    }
  } else {
    d.sel <- seq_len(nrow(alter.coords))

    if (nrow(ego.coords) == 1) {
      d <- stats::dist(rbind(ego.coords, alter.coords))[d.sel]
    } else if (nrow(ego.coords) > 1) {
      d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
        stats::dist(rbind(ego.coords[i, ], alter.coords))[d.sel]
      })
      ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
      orgn <- orgn[ego.id]
      orgn.nm <- orgn.nm[ego.id]
      d <- d.multi.ego[[ego.id]]
    }
  }

  nearest.pump <- dstn[which.min(d)]
  nearest.d <- min(d)

  if (nrow(ego.coords) == 1) {
    ego <- ego.coords
  } else if (nrow(ego.coords) > 1) {
    ego <- ego.coords[ego.id, ]
  }

  alter <- alter.coords[which.min(d), ]

  data.summary <- data.frame(orgn = orgn, orgn.nm = orgn.nm,
    dstn = nearest.pump, dstn.nm = dstn.nm[which.min(d)], d = nearest.d)

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

  cartesian.posts <- quadrantCoordinates(meter.coords, h, theta)

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
