#' Compute walking path from case/landmark to nearest or selected pump.
#'
#' @param origin Numeric. Vector of origin(s) (numeric or case/landmark name).
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

walkingPath <- function(origin = 1, destination = NULL, type = "case-pump",
  vestry = FALSE, latlong = FALSE, case.set = "observed", location = "nominal",
  weighted = TRUE, distance.unit = "meter", time.unit = "second",
  walking.speed = 5, include.landmarks = TRUE) {

  if (is.null(origin) & is.null(destination)) {
    stop("You must provide at least one origin or destination.", call. = FALSE)
  }

  if (!type %in% c("case-pump", "cases", "pumps")) {
    stop('type must be "case-pump", "cases" or "pumps".', call. = FALSE)
  }

  if (vestry) {
    pmp <- cholera::pumps.vestry
  } else {
    pmp <- cholera::pumps
  }

  if (type == "case-pump") {
    origin.chk <- validateCase(origin, case.set, include.landmarks)
    orgn <- origin.chk$out
    orgn.nm <- origin.chk$out.nm

    destination.chk <- validatePump(destination, pmp, vestry)
    dstn <- destination.chk$out
    dstn.nm <- destination.chk$out.nm

  } else if (type == "cases") {
    origin.chk <- validateCase(origin, case.set, include.landmarks)
    orgn <- origin.chk$out
    orgn.nm <- origin.chk$out.nm

    destination.chk <- validateCase(destination, case.set, include.landmarks)
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

  network.data <- neighborhoodData(vestry = vestry, case.set = case.set,
    latlong = latlong)

  if (type == "case-pump") {
    path.data <- casePump(orgn, orgn.nm, dstn, dstn.nm, destination,
      network.data, vestry, weighted)
  } else if (type == "cases") {
    path.data <- caseCase(orgn, orgn.nm, origin, dstn, dstn.nm, destination,
      network.data, vestry, weighted)
  } else if (type == "pumps") {
    path.data <- pumpPump(orgn, orgn.nm, origin, dstn, dstn.nm, destination,
      network.data, vestry, weighted)
  }

  p <- names(unlist(path.data$p))
  p.data <- do.call(rbind, strsplit(p, "_&_"))
  path <- data.frame(id = seq_len(nrow(p.data)),
                     lon = as.numeric(p.data[, 1]),
                     lat = as.numeric(p.data[, 2]))

  if (!latlong) names(path)[-1] <- c("x", "y")

  endpts <- do.call(rbind, lapply(seq_len(length(p[-1])), function(i) {
    data.frame(ep1 = p[i], ep2 = p[i + 1])
  }))

  edges <- network.data$edges

  ds <- vapply(seq_len(nrow(endpts)), function(i) {
    tmp <- endpts[i, ]
    edge.sel <- tmp$ep1 == edges$node1 & tmp$ep2 == edges$node2 |
                tmp$ep1 == edges$node2 & tmp$ep2 == edges$node1
    edges[edge.sel, ]$d
  }, numeric(1L))

  if (latlong) ds <- ds / unitMeter(1)

  walking.time <- distanceTime(sum(ds), distance.unit = distance.unit,
    time.unit = time.unit, walking.speed = walking.speed)

  ds <- unitMeter(ds, distance.unit = distance.unit)

  data.summary <- data.frame(origin = path.data$orgn,
                             destination = path.data$dstn,
                             origin.nm = path.data$orgn.nm,
                             destination.nm = path.data$dstn.nm,
                             distance = sum(ds),
                             time = walking.time,
                             type = type)

  output <- list(path = path,
                 data = data.summary,
                 origin = origin,
                 destination = destination,
                 vestry = vestry,
                 ds = ds,
                 distance.unit = distance.unit,
                 latlong = latlong,
                 location = location,
                 edges = edges,
                 pmp = pmp,
                 time.unit = time.unit,
                 walking.speed = walking.speed,
                 case.set = case.set)

  class(output) <- "walking_path"
  output
}

#' Plot the walking path between selected cases and/or pumps.
#'
#' @param x An object of class "walking_path" created by walkingPath().
#' @param zoom Logical or Numeric. Positive numbers zoom in; negative numbers zoom out.
#' @param add Logical. Add graphic to plot.
#' @param long.title Logical. Tile with names.
#' @param mileposts Logical. Plot mile/time posts.
#' @param milepost.unit Character. "distance" or "time".
#' @param milepost.interval Numeric. Mile post interval unit of distance (yard or meter) or unit of time (seconds).
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.walking_path <- function(x, zoom = TRUE, add = FALSE, long.title = TRUE,
  mileposts = TRUE, milepost.unit = "distance", milepost.interval = NULL,
  alpha.level = 1, ...) {

  path.data <- x$data
  type <- x$data$type
  orig <- path.data$origin
  dest <- path.data$destination
  destination <- x$destination
  colors <- snowColors(x$vestry)
  dat <- x$path
  ds <- x$ds
  distance.unit <- x$distance.unit
  latlong <- x$latlong
  time.unit <- x$time.unit
  walking.speed <- x$walking.speed
  pmp <- x$pmp
  edges <- x$edges

  if (distance.unit == "meter") {
    d.unit <- "m"
  } else if (distance.unit == "yard") {
    d.unit <- "yd"
  }

  if (milepost.unit == "distance") {
    path.length <- sum(ds)
  } else if (milepost.unit == "time") {
    path.length <- (3600L * sum(ds)) / (1000L * walking.speed)
  }

  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  frame <- cholera::roads[cholera::roads$name == "Map Frame", ]

  if (x$case.set == "observed") {
    fatality <- cholera::fatalities
    fatality.ortho <- cholera::latlong.ortho.addr
  } else if (x$case.set == "expected") {
    fatality <- cholera::regular.cases
    fatality.ortho <- cholera::sim.ortho.proj
  }

  land <- cholera::landmarks

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

  if (isFALSE(zoom)) {
    map.data <- rbind(frame, rd)
    xlim <- range(map.data[, ew])
    ylim <- range(map.data[, ns])
  } else if (isTRUE(zoom) | is.numeric(zoom)) {
    map.data <- mapDataRange(dat, land, path.data, vars, ew, ns)

    if (is.logical(zoom) | zoom == 0) {
      padding <- ifelse(latlong, 0.0000125, 0.05)
      xlim <- c(min(map.data[, ew]) - padding, max(map.data[, ew]) + padding)
      ylim <- c(min(map.data[, ns]) - padding, max(map.data[, ns]) + padding)

    } else if (zoom != 0) {
      if (latlong) {
        cartesian.data <- lapply(seq_len(nrow(map.data)), function(i) {
          geoCartesianCoord(map.data[i, ])
        })

        cartesian.data <- do.call(rbind, cartesian.data)
        cart.x.range <- range(cartesian.data$x)
        cart.y.range <- range(cartesian.data$y)

        padding <- c(zoom, -zoom)
        xlim <- cart.x.range + padding
        ylim <- cart.y.range + padding

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
        xlim <- range(map.data[, ew])
        ylim <- range(map.data[, ns])

        ols <- stats::lm(y ~ x, data = data.frame(x = xlim, y = ylim))
        slope <- stats::coef(ols)[2]
        theta <- atan(slope)

        padding <- abs(zoom) / unitMeter(1)
        delta.x <- abs(padding * cos(theta))
        delta.y <- abs(padding * sin(theta))

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
    p.sel <- paste0("p", path.data$destination)
    case.color <- grDevices::adjustcolor(colors[p.sel], alpha.f = alpha.level)
  } else {
    case.color <- "blue"
  }

  if (!add) {
    plot(rd[, vars], pch = NA, asp = asp, xlim = xlim, ylim = ylim)
    roads.list <- split(rd[, vars], rd$street)
    frame.list <- split(frame[, vars], frame$street)
    invisible(lapply(roads.list, lines, col = "lightgray"))
    invisible(lapply(frame.list, lines))
    if (x$case.set == "observed") {
      points(fatality[, vars], col = "lightgray", pch = 16, cex = 0.5)
    }
    points(pmp[, vars], pch = 24, col = grDevices::adjustcolor(colors,
      alpha.f = alpha.level))
    text(pmp[, vars], pos = 1, labels = paste0("p", pmp$id))
  }

  if (type %in% c("case-pump", "cases")) {
    if (orig < 1000L | orig > max(land$case)) {
      if (x$case.set == "observed") {
        sel <- fatality$case == orig
      } else if (x$case == "expected") {
        sel <- fatality.ortho$case == orig
      }
      points(fatality[sel, vars], col = "red")
      text(fatality[sel, vars], pos = 1, labels = orig, col = "red")

    } else if (orig >= 1000L & orig <= max(land$case)) {
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
      if (dest < 1000L) {
        points(fatality[fatality$case == dest, vars], col = "red")
        text(fatality[fatality$case == dest, vars], pos = 1, labels = dest,
          col = "red")
      } else if (dest >= 1000L) {
        points(land[land$case == dest, vars], col = "red")
        land.tmp <- land[land$case == dest, ]
        if (grepl("Square", land.tmp$name)) {
          dstn.sq <- unlist(strsplit(land.tmp$name, "-"))[1]
          sel <- cholera::landmark.squares$name == dstn.sq
          label.dat <- cholera::landmark.squares[sel, ]
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

  points(dat[1, vars], pch = 0)
  points(dat[nrow(dat), vars], pch = 0)

  drawPath(dat, case.color, latlong)

  d <- paste(round(path.length, 1), d.unit)
  t <- paste(round(x$data$time, 1), paste0(time.unit, "s"), "@", walking.speed,
             "km/hr")

  if (is.null(milepost.interval)) {
    if (milepost.unit == "distance") {
      milepost.interval <- 50
    } else if (milepost.unit == "time") {
      milepost.interval <- 60
    }
  }

  milepost.data <- milePosts(path.data, dat, destination, distance.unit, ds,
    latlong, milepost.unit, milepost.interval, time.unit, walking.speed)

  seg.data <- milepost.data$seg.data

  # last/final arrow ("last mile")
  arrows(seg.data[1, paste0(ew, 2)], seg.data[1, paste0(ns, 2)],
         seg.data[1, paste0(ew, 1)], seg.data[1, paste0(ns, 1)],
         length = 0.0875, lwd = 3, col = case.color)

  if (mileposts) {
    if (path.length > milepost.interval) {
      arrow.head <- milepost.data$arrow.head
      arrow.tail <- milepost.data$arrow.tail
    }

    # intermediate arrows (mileposts)
    if (path.length >= milepost.interval) {
      # diagnostic #
      # dotchart(log(abs(arrow.tail$lon - arrow.head$lon)))
      # dotchart(log(abs(arrow.tail$lat - arrow.head$lat)))

      cutpoint <- ifelse(latlong, -13L, -6L)
      zero.length.ew <- log(abs(arrow.tail[, ew] - arrow.head[, ew])) < cutpoint
      zero.length.ns <- log(abs(arrow.tail[, ns] - arrow.head[, ns])) < cutpoint

      if (any(zero.length.ew | zero.length.ns)) {
        zero.id <- unique(row.names(arrow.head[zero.length.ew, ]),
                          row.names(arrow.head[zero.length.ns, ]))

        angle <- vapply(zero.id, function(id) {
          zero.arrow <- rbind(arrow.tail[id, vars], arrow.head[id, vars])
          if (latlong) ols <- stats::lm(lat ~ lon, data = zero.arrow)
          else ols <- stats::lm(y ~ x, data = zero.arrow)
          slope <- stats::coef(ols)[2]
          theta <- atan(slope)
          theta * 180L / pi
        }, numeric(1L))

        invisible(lapply(seq_along(zero.id), function(i) {
          text(arrow.head[zero.id[i], vars], labels = "<", srt = angle[i],
               col = case.color, cex = 1.25)
        }))

        arrow.head <- arrow.head[!row.names(arrow.head) %in% zero.id, ]
        arrow.tail <- arrow.tail[!row.names(arrow.tail) %in% zero.id, ]
      }

      arrows(arrow.tail[, ew], arrow.tail[, ns],
             arrow.head[, ew], arrow.head[, ns],
             length = 0.0875, lwd = 3, col = case.color)
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

  if (!add) longTitle(long.title, type, pmp, path.data, orig, land, x)
}

#' Print method for walkingPath().
#'
#' Summary output.
#' @param x An object of class "latlong_walking_path" created by latlongWalkingPath().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export

print.walking_path <- function(x, ...) {
  if (!inherits(x, "walking_path")) {
    stop('"x"\'s class must be "walking_path".')
  }
  print(x[c("path", "data")])
}

drawPath <- function(dat, case.color, latlong) {
  n1 <- dat[1:(nrow(dat) - 1), ]
  n2 <- dat[2:nrow(dat), ]
  if (latlong) {
    segments(n1$lon, n1$lat, n2$lon, n2$lat, lwd = 3, col = case.color)
  } else {
    segments(n1$x, n1$y, n2$x, n2$y, lwd = 3, col = case.color)
  }
}

milePosts <- function(path.data, dat, destination, distance.unit, ds, latlong,
  milepost.unit, milepost.interval, time.unit, walking.speed) {

  rev.data <- dat[order(dat$id, decreasing = TRUE), ]

  if (latlong) {
    ew <- "lon"
    ns <- "lat"
  } else {
    ew <- "x"
    ns <- "y"
  }

  vars <- c(ew, ns)
  seg.vars <- c(paste0(vars, 1), paste0(vars, 2))

  seg.data <- do.call(rbind, lapply(seq_len(nrow(rev.data) - 1), function(i) {
    endpts <- cbind(rev.data[i, vars], rev.data[i + 1, vars])
    names(endpts) <- seg.vars
    data.frame(id = i, endpts)
  }))

  seg.data$d <- rev(ds)
  seg.data$cumulative.d <- cumsum(seg.data$d)

  if (milepost.unit == "distance") {
    path.length <- sum(ds)
    cumulative <- seg.data$cumulative.d
  } else if (milepost.unit == "time") {
    path.length <- path.data$time
    seg.data$t <- (3600L * seg.data$d) / (1000L * walking.speed)
    seg.data$cumulative.t <- cumsum(seg.data$t)
    cumulative <- seg.data$cumulative.t
  }

  posts <- seq(0, path.length, milepost.interval)
  if (max(posts) > path.length) posts <- posts[-length(posts)]

  bins <- data.frame(lo = c(0, cumulative[-length(cumulative)]),
                     hi = cumulative)

  seg.select <- vapply(posts, function(x) {
    which(vapply(seq_len(nrow(bins)), function(i) {
      x >= bins[i, "lo"] & x < bins[i, "hi"]
    }, logical(1L)))
  }, integer(1L))

  if (all(seg.select == 1) & length(seg.select) > 1) {
    milepost.seg.id <- unique(seg.select)
  } else {
    if (sum(seg.select == 1) > 1) {
      milepost.seg.id <- c(1, seg.select[seg.select != 1L])
    } else {
      milepost.seg.id <- seg.select[seg.select != 1L]
    }
  }

  segment.census <- table(milepost.seg.id)

  if (any(segment.census > 1)) {
    single.post.seg <- as.numeric(names(segment.census[segment.census == 1]))
    multi.post.seg <- as.numeric(names(segment.census[segment.census > 1]))
  } else {
    single.post.seg <- milepost.seg.id
  }

  if (path.length > milepost.interval) {
    milepost.values <- seq_along(milepost.seg.id) * milepost.interval
    census <- data.frame(seg = milepost.seg.id, post = milepost.values)

    if (latlong) {
      origin <- data.frame(lon = min(cholera::roads[, ew]),
                           lat = min(cholera::roads[, ns]))
      if (any(segment.census > 1)) {
        single.arrow.data <- arrowData(single.post.seg, census, distance.unit,
          latlong, milepost.unit, seg.data, origin)
        multi.arrow.data <- arrowData(multi.post.seg, census,
          distance.unit, latlong, milepost.unit, seg.data, origin,
          multi.arrow.seg = TRUE)
        arrow.data <- rbind(single.arrow.data, multi.arrow.data)
      } else {
        arrow.data <- arrowData(single.post.seg, census, distance.unit,
          latlong, milepost.unit, seg.data, origin)
      }
    } else {
      if (any(segment.census > 1)) {
        single.arrow.data <- arrowData(single.post.seg, census, distance.unit,
          latlong, milepost.unit, seg.data)
        multi.arrow.data <- arrowData(multi.post.seg, census, distance.unit,
          latlong, milepost.unit, seg.data, multi.arrow.seg = TRUE)
        arrow.data <- rbind(single.arrow.data, multi.arrow.data)
      } else {
        arrow.data <- arrowData(single.post.seg, census, distance.unit,
          latlong, milepost.unit, seg.data)
      }
    }

    arrow.tail <- arrow.data[, paste0(c("x", "y"), 1)]
    arrow.head <- arrow.data[, paste0(c("x", "y"), 2)]

    if (latlong) {
      arrow.tail <- meterLatLong(arrow.tail)
      arrow.head <- meterLatLong(arrow.head)
    } else {
      arrow.tail <- stats::setNames(arrow.data[, paste0(c("x", "y"), 1)], vars)
      arrow.head <- stats::setNames(arrow.data[, paste0(c("x", "y"), 2)], vars)
    }

    if (nrow(arrow.tail) > 1) {
      arrow.tail <- arrow.tail[order(row.names(arrow.tail)), ]
      arrow.head <- arrow.head[order(row.names(arrow.head)), ]
    }

    out <- list(seg.data = seg.data, arrow.head = arrow.head,
                arrow.tail = arrow.tail)
  } else {
    out <- list(seg.data = seg.data)
  }
  out
}

arrowData <- function(segs, census, distance.unit, latlong, milepost.unit,
  seg.data, origin, multi.arrow.seg = FALSE) {

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  out <- lapply(segs, function(s) {
    tmp <- seg.data[seg.data$id == s, ]
    endpt1 <- stats::setNames(tmp[, grep("1", names(tmp))], vars)
    endpt2 <- stats::setNames(tmp[, grep("2", names(tmp))], vars)
    data.tmp <- rbind(endpt1, endpt2)

    if (latlong) {
      idx <- seq_along(data.tmp$lon)
      meter.coords <- do.call(rbind, lapply(idx, function(i) {
        tmp <- data.tmp[i, vars]
        x.proj <- c(tmp$lon, origin$lat)
        y.proj <- c(origin$lon, tmp$lat)
        m.lon <- geosphere::distGeo(y.proj, tmp)
        m.lat <- geosphere::distGeo(x.proj, tmp)
        data.frame(x = m.lon, y = m.lat)
      }))
      ols <- stats::lm(y ~ x, data = meter.coords)
    } else {
      ols <- stats::lm(y ~ x, data = data.tmp)
    }

    seg.slope <- stats::coef(ols)[2]
    theta <- atan(seg.slope)

    if (multi.arrow.seg) {
      posts <- census[census$seg %in% s, "post"]
      if (latlong) {
         multi.out <- lapply(posts, function(p) {
          if (milepost.unit == "distance") {
            h <- tmp$cumulative.d - p
          } else if (milepost.unit == "time") {
            h <- tmp$cumulative.t - p
          }
          arrow.point <- quandrantCoordinates(meter.coords, h, theta)
          data.frame(x1 = meter.coords[2, "x"],
                     y1 = meter.coords[2, "y"],
                     x2 = arrow.point$x,
                     y2 = arrow.point$y)
        })
      } else {
        multi.out <- lapply(posts, function(p) {
          if (milepost.unit == "distance") {
            h <- (tmp$cumulative.d - p) / unitMeter(1, distance.unit)
          } else if (milepost.unit == "time") {
            h <- tmp$cumulative.t - p
          }
          arrow.point <- quandrantCoordinates(data.tmp, h, theta)
          data.frame(x1 = data.tmp[2, "x"],
                     y1 = data.tmp[2, "y"],
                     x2 = arrow.point$x,
                     y2 = arrow.point$y)
        })
      }
      do.call(rbind, multi.out)
    } else {
      post <- census[census$seg == s, "post"]

      if (latlong) {
        if (milepost.unit == "distance") {
          h <- tmp$cumulative.d - post
        } else if (milepost.unit == "time") {
          h <- tmp$cumulative.t - post
        }
        arrow.point <- quandrantCoordinates(meter.coords, h, theta)
        data.frame(x1 = meter.coords[2, "x"],
                   y1 = meter.coords[2, "y"],
                   x2 = arrow.point$x,
                   y2 = arrow.point$y)
      } else {
        if (milepost.unit == "distance") {
          h <- (tmp$cumulative.d - post) / unitMeter(1, distance.unit)
        } else if (milepost.unit == "time") {
          h <- tmp$cumulative.t - post
        }
        arrow.point <- quandrantCoordinates(data.tmp, h, theta)
        data.frame(x1 = data.tmp[2, "x"],
                   y1 = data.tmp[2, "y"],
                   x2 = arrow.point$x,
                   y2 = arrow.point$y)
      }
    }
  })
  do.call(rbind, out)
}

casePump <- function(orgn, orgn.nm, dstn, dstn.nm, destination, network.data,
  vestry, weighted) {

  g <- network.data$g
  edges <- network.data$edges
  nodes <- network.data$nodes

  if (any(orgn < 1000L)) {
    fatal <- orgn[orgn < 1000L]
    land <- orgn[orgn >= 1000L]
    land.nm <- orgn.nm[orgn >= 1000L]

    if (any(!fatal %in% cholera::anchor.case$anchor)) {
      sel <- cholera::anchor.case$case %in% fatal
      orgn <- c(unique(cholera::anchor.case[sel, "anchor"]), land)
      orgn.nm <- c(paste(orgn), land.nm)
    }
  }

  if (any(grepl("Square", orgn.nm))) {
    if (length(orgn) > 1) {
      if (any(cholera::landmark.squares$case %in% orgn)) {
        orgn <- orgn[!orgn %in% cholera::landmark.squares$case]
        orgn.nm <- orgn.nm[!orgn.nm %in% cholera::landmark.squares$name]
      }
    } else if (length(orgn) == 1) {
      if (orgn.nm %in% cholera::landmark.squares$name) {
        sel <- grepl(orgn.nm, cholera::landmarks$name)
        sq.tmp <- cholera::landmarks[sel, ]
        orgn <- sq.tmp$case
        orgn.nm <- sq.tmp$name
      }
    }
  }

  ego.node <- c(nodes[nodes$case %in% orgn, ]$node,
                nodes[nodes$land %in% orgn, ]$node)

  if (any(dstn == 2L)) {
    # message("Note: Pump 2 excluded because it's a technical isolate.")
    dstn <- dstn[dstn != 2L]
  }

  alters <- nodes[nodes$pump %in% dstn, ]

  alter.node <- alters$node
  names(alter.node) <- alters$pump

  if (length(ego.node) == 1) {
    if (weighted) {
      d <- igraph::distances(graph = g, v = ego.node, to = alter.node,
        weights = edges$d)
    } else {
      d <- igraph::distances(graph = g, v = ego.node, to = alter.node)
    }

    nearest.node <- dimnames(d)[[2]][which.min(d)]
    nearest.dstn <- as.character(alters[alters$node == nearest.node, ]$pump)

    if (weighted) {
      p <- igraph::shortest_paths(graph = g, from = ego.node,
        to = alter.node[nearest.dstn], weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(graph = g, from = ego.node,
        to = alter.node[nearest.dstn])
    }
  } else if (length(ego.node) > 1) {
    d.multi.ego <- lapply(ego.node, function(x) {
      if (weighted) {
        igraph::distances(graph = g, v = x, to = alter.node,
          weights = edges$d)
      } else {
        igraph::distances(graph = g, v = x, to = alter.node)
      }
    })

    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    d <- min(d.multi.ego[[ego.id]])

    sel <- (nodes$case != 0 | nodes$land != 0) &
            nodes$node %in% ego.node[ego.id]

    orgn <- nodes[sel, ]$case + nodes[sel, ]$land
    orgn.nm <- orgn.nm[ego.id]

    nr.ego.node <- nodes[sel, ]$node

    alter.id <- which.min(d.multi.ego[[ego.id]])
    nr.alter.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]
    nearest.dstn <- nodes[nodes$node == nr.alter.node, ]$pump

    if (weighted) {
      p <- igraph::shortest_paths(graph = g, from = nr.ego.node,
        to = nr.alter.node, weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(graph = g, from = nr.ego.node,
        to = nr.alter.node)
    }
  }

  list(orgn = orgn, orgn.nm = orgn.nm, dstn = dstn[dstn == nearest.dstn],
       dstn.nm = dstn.nm[dstn == nearest.dstn], p = p[[1]])
}

caseCase <- function(orgn, orgn.nm, origin, dstn, dstn.nm, destination,
  network.data, vestry, weighted) {

  g <- network.data$g
  edges <- network.data$edges
  nodes <- network.data$nodes

  if (any(orgn < 1000L)) {
    fatal <- orgn[orgn < 1000L]
    land <- orgn[orgn >= 1000L]
    land.nm <- orgn.nm[orgn >= 1000L]

    if (any(!fatal %in% cholera::anchor.case$anchor)) {
      sel <- cholera::anchor.case$case %in% fatal
      orgn <- c(unique(cholera::anchor.case[sel, "anchor"]), land)
      orgn.nm <- c(paste(orgn), land.nm)
    }
  }

  if (any(grepl("Square", orgn.nm))) {
    if (length(orgn) > 1) {
      if (any(cholera::landmark.squares$case %in% orgn)) {
        orgn <- orgn[!orgn %in% cholera::landmark.squares$case]
        orgn.nm <- orgn.nm[!orgn.nm %in% cholera::landmark.squares$name]
      }
    } else if (length(orgn) == 1) {
      if (orgn.nm %in% cholera::landmark.squares$name) {
        sel <- grepl(orgn.nm, cholera::landmarks$name)
        sq.tmp <- cholera::landmarks[sel, ]
        orgn <- sq.tmp$case
        orgn.nm <- sq.tmp$name
      }
    }
  }

  if (any(dstn < 1000L)) {
    fatal <- dstn[dstn < 1000L]
    land <- dstn[dstn >= 1000L]
    land.nm <- dstn.nm[dstn >= 1000L]

    if (any(!fatal %in% cholera::anchor.case$anchor)) {
      sel <- cholera::anchor.case$case %in% fatal
      dstn <- c(unique(cholera::anchor.case[sel, "anchor"]), land)
      dstn.nm <- c(paste(dstn), land.nm)
    }
  }

  if (any(grepl("Square", dstn.nm))) {
    if (length(dstn) > 1) {
      if (any(cholera::landmark.squares$case %in% dstn)) {
        dstn <- dstn[!dstn %in% cholera::landmark.squares$case]
        dstn.nm <- dstn.nm[!dstn.nm %in% cholera::landmark.squares$name]
      }
    } else if (length(dstn) == 1) {
      if (dstn.nm %in% cholera::landmark.squares$name) {
        sel <- grepl(dstn.nm, cholera::landmarks$name)
        sq.tmp <- cholera::landmarks[sel, ]
        dstn <- sq.tmp$case
        dstn.nm <- sq.tmp$name
      }
    }
  }

  if (length(intersect(orgn, dstn)) != 0) {
    if (!is.null(origin) & is.null(destination) | all(destination < 0)) {
      dstn <- setdiff(dstn, orgn)
      dstn.nm <- setdiff(dstn.nm, orgn.nm)
    } else if (is.null(origin) & !is.null(destination)) {
      orgn <- setdiff(orgn, dstn)
      orgn.nm <- setdiff(orgn.nm, dstn.nm)
    }
  }

  ego.node <- nodes[nodes$case %in% orgn | nodes$land %in% orgn, ]$node

  alters <- nodes[nodes$case %in% dstn | nodes$land %in% dstn, ]
  # Anchor case 369 and landmark St James Workhouse
  if (is.null(destination)) alters <- alters[alters$land != 1019, ]
  alter.node <- alters$node
  names(alter.node) <- alters$case + alters$land

  if (length(ego.node) == 1) {
    if (weighted) {
      d <- igraph::distances(graph = g, v = ego.node, to = alter.node,
        weights = edges$d)
    } else {
      d <- igraph::distances(raph = g, v = ego.node, to = alter.node)
    }

    if (length(d) == 1) nearest.node <- dimnames(d)[[2]]
    else if (length(d) > 1) nearest.node <- dimnames(d)[[2]][which.min(d)]

    sel <- (nodes$case != 0 | nodes$land != 0) & nodes$node == nearest.node
    nearest.dstn <- nodes[sel, ]$case + nodes[sel, ]$land

    if (weighted) {
      p <- igraph::shortest_paths(graph = g, from = ego.node, to = nearest.node,
        weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(graph = g, from = ego.node, to = nearest.node)
    }
  } else if (length(ego.node) > 1) {
    d.multi.ego <- lapply(ego.node, function(x) {
      if (weighted) {
        igraph::distances(graph = g, v = x, to = alter.node,
          weights = edges$d)
      } else {
        igraph::distances(graph = g, v = x, to = alter.node)
      }
    })

    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    d <- min(d.multi.ego[[ego.id]])

    sel <- (nodes$case != 0 | nodes$land != 0) & nodes$node == ego.node[ego.id]
    orgn <- nodes[sel, ]$case + nodes[sel, ]$land
    orgn.nm <- orgn.nm[ego.id]

    nr.ego.node <- nodes[sel, ]$node

    alter.id <- which.min(d.multi.ego[[ego.id]])
    nr.alter.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]

    sel <- (nodes$case != 0 | nodes$land != 0) & nodes$node == nr.alter.node
    nearest.dstn <- nodes[sel, ]$case + nodes[sel, ]$land

    if (weighted) {
      p <- igraph::shortest_paths(graph = g, from = nr.ego.node,
        to = nr.alter.node, weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(graph = g, from = nr.ego.node,
        to = nr.alter.node)
    }
  }

  list(orgn = orgn, orgn.nm = orgn.nm, dstn = dstn[dstn == nearest.dstn],
       dstn.nm = dstn.nm[dstn == nearest.dstn], p = p[[1]])
}

pumpPump <- function(orgn, orgn.nm, origin, dstn, dstn.nm, destination,
  network.data, vestry, weighted) {

  g <- network.data$g
  edges <- network.data$edges
  nodes <- network.data$nodes

  if (length(intersect(orgn, dstn)) != 0) {
    if (!is.null(origin) & is.null(destination) | all(destination < 0)) {
      dstn <- setdiff(dstn, orgn)
      dstn.nm <- setdiff(dstn.nm, orgn.nm)
    } else if (is.null(origin) & !is.null(destination)) {
      orgn <- setdiff(orgn, dstn)
      orgn.nm <- setdiff(orgn.nm, dstn.nm)
    }
  }

  egos <- nodes[nodes$pump %in% orgn, ]
  if (nrow(egos) > 1) egos <- egos[order(egos$pump), ]

  alters <- nodes[nodes$pump %in% dstn, ]
  if (nrow(alters) > 1) alters <- alters[order(alters$pump), ]

  if (2L %in% egos$pump) {
    egos <- egos[egos$pump != 2, ]
    if (nrow(egos) == 0) {
      msg1 <- "No valid origins: "
      msg2 <- "Pump 2 excluded because it's a technical isolate."
      stop(msg1, msg2, call. = FALSE)
    }
  }

  if (2L %in% alters$pump) {
    alters <- alters[alters$pump != 2, ]
    if (nrow(alters) == 0) {
      msg1 <- "No valid destinations: "
      msg2 <- "Pump 2 excluded because it's a technical isolate."
      stop(msg1, msg2, call. = FALSE)
    }
  }

  if (is.null(origin)) {
    if (any(egos$pump %in% alters$pump)) {
      egosB <- egos[!egos$pump %in% alters$pump, ]
    } else egosB <- egos
  } else egosB <- egos

  if (is.null(destination)) {
    if (any(alters$pump %in% egos$pump)) {
      altersB <- alters[!alters$pump %in% egos$pump, ]
    } else altersB <- alters
  } else altersB <- alters

  ego.node <- egosB$node
  names(ego.node) <- egosB$pump

  alter.node <- altersB$node
  names(alter.node) <- altersB$pump

  if (length(ego.node) == 1) {
    if (weighted) {
      d <- igraph::distances(graph = g, v = ego.node, to = alter.node,
        weights = edges$d)
    } else {
      d <- igraph::distances(graph = g, v = ego.node, to = alter.node)
    }

    nearest.node <- dimnames(d)[[2]][which.min(d)]
    nearest.dstn <- alters[alters$node == nearest.node, ]$pump

    if (weighted) {
      p <- igraph::shortest_paths(graph = g, from = ego.node, to = nearest.node,
        weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(graph = g, from = ego.node, to = nearest.node)
    }

  } else if (length(ego.node) > 1) {
    d.multi.ego <- lapply(ego.node, function(x) {
      if (weighted) {
        igraph::distances(graph = g, v = x, to = alter.node,
          weights = edges$d)
      } else {
        igraph::distances(graph = g, v = x, to = alter.node)
      }
    })

    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    d <- min(d.multi.ego[[ego.id]])

    nr.ego.node <- nodes[nodes$node == ego.node[ego.id], ]$node

    alter.id <- which.min(d.multi.ego[[ego.id]])
    nr.alter.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]
    nearest.dstn <- nodes[nodes$node == nr.alter.node, ]$pump

    if (weighted) {
      p <- igraph::shortest_paths(graph = g, from = nr.ego.node,
        to = nr.alter.node, weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(raph = g, from = nr.ego.node,
        to = nr.alter.node)
    }

    orgn <- orgn[ego.id]
    orgn.nm <- orgn.nm[ego.id]
  }

  list(orgn = orgn, orgn.nm = orgn.nm, dstn = nearest.dstn,
       dstn.nm = dstn.nm[dstn == nearest.dstn], p = p[[1]])
}
