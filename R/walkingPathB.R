#' Plot walking path to nearest pump (prototype).
#'
#' @param origin Numeric. Vector of origin(s) (numeric or landmark/pump name).
#' @param destination Numeric. Vector of destination(s) (numeric or landmark/pump name).
#' @param type Character. Path case to pump. FALSE is all other combinations of cases, landmarks and pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param latlong Logical.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param distance.unit Character. Unit of distance: "meter" or "yard".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @importFrom geosphere distGeo
#' @export

walkingPathB <- function(origin = 1, destination = NULL,
  type = "case-pump", vestry = FALSE, latlong = FALSE, weighted = TRUE,
  distance.unit = "meter", time.unit = "second", walking.speed = 5) {

  meter.to.yard <- 1.09361

  # Change type to "cases" in presence of landmarks
  if (is.character(destination)) {
    destination <- caseAndSpace(destination)
    if (any(grepl(destination, cholera::landmarksB$name)) & type != "cases") {
      type <- "cases"
    }
  } else if (is.numeric(destination)) {
    if (any(destination %in% cholera::landmarksB$case) & type != "cases") {
      type <- "cases"
    }
  }

  if (type %in% c("case-pump", "cases")) {
    if (is.numeric(origin)) {
      if (any(!origin %in% cholera::fatalities$case &
              !origin %in% cholera::landmarksB$case)) {
        message("Cases range from 1 to 578; Landmarks from 1000 to 1021.")
      } else {
        if (any(origin < 1000L)) {
          sel <- cholera::anchor.case$case %in% origin
          anchor <- cholera::anchor.case[sel, "anchor"]
        } else {
          anchor <- origin
        }
        anchor.nm <- anchor
      }
    } else if (is.character(origin)) {
      if (length(origin) == 1) {
        origin <- caseAndSpace(origin)
      } else if (length(origin) > 1) {
        origin <- vapply(origin, caseAndSpace, character(1L))
      }

      if (all(!origin %in% cholera::landmarksB$name &
              !origin %in% cholera::landmark.squaresB$name)) {
        stop("Landmark not found. Check spelling or cholera::landmarksB.",
          call. = FALSE)
      } else {
        if (any(origin %in% cholera::landmarksB$name)) {
          sel <- cholera::landmarksB$name %in% origin
          anchor <- cholera::landmarksB[sel, ]$case
          anchor.nm <- cholera::landmarksB[sel, ]$name
        } else if (any(origin %in% cholera::landmark.squaresB$name)) {
          sel <- grep(origin, cholera::landmarksB$name)
          anchor <- cholera::landmarksB[sel, ]$case
          anchor.nm <- origin
        }
      }
    }
  }

  if (vestry) pmp <- cholera::pumps.vestry
  else pmp <- cholera::pumps

  if (type == "pumps") {
    if (is.numeric(origin)) {
      if (all(!origin %in% pmp$id)) {
        stop("For vestry = ", vestry, ", pump IDs range from 1 to ", nrow(pmp),
          "." , call. = FALSE)
      } else if (any(!origin %in% pmp$id)) {
        message("For vestry = ", vestry, ", pump IDs range from 1 to ",
          nrow(pmp), ".")
        origin <- origin[origin %in% pmp$id]
        anchor.nm <- pmp[pmp$id %in% origin, ]$street
      } else {
        anchor <- origin
        anchor.nm <- pmp[pmp$id %in% origin, ]$street
      }
    } else if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      if (all(!origin %in% pmp$street)) {
        stop("For vestry = ", vestry,
          ", pump (street) name not found. Check spelling or cholera::pumps.",
          call. = FALSE)
      } else if (any(!origin %in% pmp$street)) {
        message("For vestry = ", vestry,
          ", pump (street) name not found. Check spelling or cholera::pumps.")
        origin <- origin[origin %in% pmp$street]
        anchor.nm <- pmp[pmp$id %in% origin, ]$street
      } else {
        anchor <- pmp[pmp$street %in% origin, ]$id
        anchor.nm <- origin
      }
    }
  }

  network.data <- neighborhoodDataB(vestry = vestry, latlong = latlong)
  edges <- network.data$edges

  if (type == "case-pump") {
    path.data <- casePump(anchor, anchor.nm, destination, network.data, pmp,
      vestry, weighted)
  } else if (type == "cases") {
    path.data <- caseCase(anchor, anchor.nm, destination, network.data,
      vestry, weighted)
  } else if (type == "pumps") {
    path.data <- pumpPump(anchor, anchor.nm, destination, network.data,
      origin, pmp, vestry, weighted)
  }

  if (length(anchor) > 1) anchor <- path.data$anchor
  if (length(anchor.nm) > 1) anchor.nm <- path.data$anchor.nm
  nearest.dest <- path.data$nearest.dest

  p <- names(unlist(path.data$p))
  p.data <- do.call(rbind, strsplit(p, "_&_"))
  path <- data.frame(id = seq_len(nrow(p.data)),
                     lon = as.numeric(p.data[, 1]),
                     lat = as.numeric(p.data[, 2]))

  if (!latlong) names(path)[-1] <- c("x", "y")

  endpts <- do.call(rbind, lapply(seq_len(length(p[-1])), function(i) {
    data.frame(ep1 = p[i], ep2 = p[i + 1])
  }))

  ds <- vapply(seq_len(nrow(endpts)), function(i) {
    tmp <- endpts[i, ]
    edge.sel <- tmp$ep1 == edges$node1 & tmp$ep2 == edges$node2 |
                tmp$ep1 == edges$node2 & tmp$ep2 == edges$node1
    edges[edge.sel, ]$d
  }, numeric(1L))

  if (latlong) {
    walking.time <- walkingTime(sum(ds), time.unit = time.unit,
      walking.speed = walking.speed)
  } else {
    ds <- unitMeter(ds, distance.unit = distance.unit)
    walking.time <- distanceTime(sum(ds), distance.unit = distance.unit,
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

  data.summary <- data.frame(orig.anchor = anchor,
                             dest.anchor = as.integer(nearest.dest),
                             orig.nm = anchor.nm,
                             dest.nm = dest.nm,
                             distance = sum(ds),
                             time = walking.time,
                             type = type,
                             row.names = NULL)

  output <- list(path = path,
                 data = data.summary,
                 destination = destination,
                 vestry = vestry,
                 ds = ds,
                 distance.unit = distance.unit,
                 latlong = latlong,
                 edges = edges,
                 pmp = pmp,
                 time.unit = time.unit,
                 walking.speed = walking.speed)

  class(output) <- "walking_path_B"
  output
}

#' Plot the walking path between selected cases and/or pumps.
#'
#' @param x An object of class "latlong_walking_path" created by latlongWalkingPath().
#' @param zoom Logical or Numeric. A numeric value >= 0 controls the degree of zoom. A value of 1 equals zoom = TRUE.
#' @param long.title Logical. Tile with names.
#' @param mileposts Logical. Plot mile/time posts.
#' @param milepost.unit Character. "distance" or "time".
#' @param milepost.interval Numeric. Mile post interval unit of distance (yard or meter) or unit of time (seconds).
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.walking_path_B <- function(x, zoom = TRUE, long.title = TRUE,
  mileposts = TRUE, milepost.unit = "distance", milepost.interval = NULL,
  alpha.level = 1, ...) {

  path.data <- x$data
  type <- x$data$type
  orig <- path.data$orig.anchor
  dest <- path.data$dest.anchor
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
  fatality <- cholera::fatalities
  fatality.ortho <- cholera::latlong.ortho.addr
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
  } else stop("zoom must either be logical or numeric.")

  if (type == "case-pump") {
    p.sel <- paste0("p", path.data$dest.anchor)
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
      points(fatality[fatality$case == orig, vars], col = "red")
      text(fatality[fatality$case == orig, vars], pos = 1, labels = orig,
           col = "red")
    } else if (orig >= 1000L) {
      points(land[land$case == orig, vars], col = "red")
      # text(land[land$case == orig, vars], pos = 1, labels = orig, col = "red")
    }
  }

  if (type %in% c("case-pump", "cases")) {
    if (orig < 1000L) {
      points(fatality[fatality$case == orig, vars], col = "red")
      text(fatality[fatality$case == orig, vars], pos = 1, labels = orig,
        col = "red")
    } else if (orig >= 1000L) {
      points(land[land$case == orig, vars], col = "red")
      land.tmp <- land[land$case == orig, ]

      if (grepl("Square", land.tmp$name)) {
        sel <- cholera::landmark.squaresB$name == path.data$orig.nm
        label.dat <- cholera::landmark.squaresB[sel, ]
        label.parse <- unlist(strsplit(label.dat$name, "[ ]"))
        sq.label <- paste0(label.parse[1], "\n", label.parse[2])
        text(label.dat[, c(ew, ns)], labels = sq.label, col = "red", cex = 0.8)
        # text(land[land$case == orig, vars], pos = 1, labels = orig, col = "red")
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
        # text(land[land$case == orig, vars], pos = 1, labels = orig, col = "red")
      }
    }

    if (dest < 1000L) {
      points(fatality[fatality$case == dest, vars], col = "red")
      text(fatality[fatality$case == dest, vars], pos = 1, labels = dest,
        col = "red")
    } else if (dest >= 1000L) {
      points(land[land$case == dest, vars], col = "red")
      land.tmp <- land[land$case == dest, ]
      if (grepl("Square", land.tmp$name)) {
        sel <- cholera::landmark.squaresB$name == path.data$dest.nm
        label.dat <- cholera::landmark.squaresB[sel, ]
        label.parse <- unlist(strsplit(label.dat$name, "[ ]"))
        sq.label <- paste0(label.parse[1], "\n", label.parse[2])
        text(label.dat[, c(ew, ns)], labels = sq.label, col = "red", cex = 0.8)
        # text(land[land$case == dest, vars], pos = 1, labels = dest, col = "red")
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
        # text(land[land$case == dest, vars], pos = 1, labels = dest, col = "red")
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

  points(dat[1, vars], pch = 0)
  points(dat[nrow(dat), vars], pch = 0)

  drawPathB(dat, case.color, latlong)

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

  milepost.data <- milePostsB(path.data, dat, destination, distance.unit,
    ds, latlong, milepost.unit, milepost.interval, time.unit, walking.speed)

  seg.data <- milepost.data$seg.data

  # last arrow (last mile)
  arrows(seg.data[1, paste0(ew, 2)], seg.data[1, paste0(ns, 2)],
         seg.data[1, paste0(ew, 1)], seg.data[1, paste0(ns, 1)],
         length = 0.0875, lwd = 3, col = case.color)

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

  }
  longTitle(long.title, type, pmp, path.data, orig, land)
  title(sub = paste(d, t, post.info, sep = "; "))
}

#' Print method for walkingPathB().
#'
#' Summary output.
#' @param x An object of class "latlong_walking_path" created by latlongWalkingPath().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export

print.walking_path_B <- function(x, ...) {
  if (!inherits(x, "walking_path_B")) {
    stop('"x"\'s class must be "walking_path_B".')
  }
  print(x[c("path", "data")])
}

drawPathB <- function(dat, case.color, latlong) {
  n1 <- dat[1:(nrow(dat) - 1), ]
  n2 <- dat[2:nrow(dat), ]
  if (latlong) {
    segments(n1$lon, n1$lat, n2$lon, n2$lat, lwd = 3, col = case.color)
  } else {
    segments(n1$x, n1$y, n2$x, n2$y, lwd = 3, col = case.color)
  }
}

milePostsB <- function(path.data, dat, destination, distance.unit, ds, latlong,
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
      topleft <- data.frame(lon = min(cholera::roads[, ew]),
                            lat = max(cholera::roads[, ns]))
      bottomright <- data.frame(lon = max(cholera::roads[, ew]),
                                lat = min(cholera::roads[, ns]))
      if (any(segment.census > 1)) {
        single.arrow.data <- arrowDataB(single.post.seg, census, distance.unit,
          latlong, milepost.unit, seg.data, origin)
        multi.arrow.data <- arrowDataB(multi.post.seg, census,
          distance.unit, latlong, milepost.unit, seg.data, origin,
          multi.arrow.seg = TRUE)
        arrow.data <- rbind(single.arrow.data, multi.arrow.data)
      } else {
        arrow.data <- arrowDataB(single.post.seg, census, distance.unit,
          latlong, milepost.unit, seg.data, origin)
      }
    } else {
      if (any(segment.census > 1)) {
        single.arrow.data <- arrowDataB(single.post.seg, census, distance.unit,
          latlong, milepost.unit, seg.data)
        multi.arrow.data <- arrowDataB(multi.post.seg, census, distance.unit,
          latlong, milepost.unit, seg.data, multi.arrow.seg = TRUE)
        arrow.data <- rbind(single.arrow.data, multi.arrow.data)
      } else {
        arrow.data <- arrowDataB(single.post.seg, census, distance.unit,
          latlong, milepost.unit, seg.data)
      }
    }

    arrow.tail <- arrow.data[, paste0(c("x", "y"), 1)]
    arrow.head <- arrow.data[, paste0(c("x", "y"), 2)]

    if (latlong) {
      arrow.tail <- meterLatLong(arrow.tail, origin, topleft, bottomright)
      arrow.head <- meterLatLong(arrow.head, origin, topleft, bottomright)
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

arrowDataB <- function(segs, census, distance.unit, latlong, milepost.unit,
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

longTitle <- function(long.title, type, pmp, path.data, orig, land) {
  if (long.title) {
    if (type == "case-pump") {
      p.nm <- pmp[pmp$id == path.data$dest.anchor, ]$street
      if (orig < 1000L) {
        alpha <- paste("Case", orig)
        omega <- paste(p.nm, "Pump", paste0("(#", path.data$dest.anchor, ")"))
      } else if (orig >= 1000L) {
        c.nm <- land[land$case == orig, ]$name
        alpha <- paste(c.nm, paste0("(#", orig, ")"))
        omega <- paste(p.nm, "Pump", paste0("(#", path.data$dest.anchor, ")"))
      }
    } else if (type == "cases") {
      if (orig >= 1000L & path.data$dest.anchor >= 1000L) {
        c.orig.nm <- land[land$case == orig, ]$name
        c.dest.nm <- land[land$case == path.data$dest.anchor, ]$name
        alpha <- paste(c.orig.nm, paste0("(#", orig, ")"))
        omega <- paste(c.dest.nm, paste0("(#", path.data$dest.anchor, ")"))
      } else if (orig < 1000L & path.data$dest.anchor >= 1000L) {
        c.dest.nm <- land[land$case == path.data$dest.anchor, ]$name
        alpha <- paste("Case", orig)
        omega <- paste(c.dest.nm, paste0("(#", path.data$dest.anchor, ")"))
      } else if (orig >= 1000L & path.data$dest.anchor < 1000L) {
        c.orig.nm <- land[land$case == orig, ]$name
        alpha <- paste(c.orig.nm, paste0("(#", orig, ")"))
        omega <- paste("to Case", path.data$dest.anchor)
      } else {
        alpha <- paste("Case", orig)
        omega <- paste("Case", path.data$dest.anchor)
      }
    } else if (type == "pumps") {
      orig.nm <- pmp[pmp$id == path.data$orig.anchor, ]$street
      dest.nm <- pmp[pmp$id == path.data$dest.anchor, ]$street
      alpha <- paste(orig.nm, paste0("(p", path.data$orig.anchor, ")"))
      omega <- paste(dest.nm, paste0("(p", path.data$dest.anchor, ")"))
    }
    title(main = paste(alpha, "to", omega))
  } else {
    if (type == "case-pump") {
      title(main = paste("Case", orig, "to Pump", path.data$dest.anchor))
    } else if (type == "cases") {
      title(main = paste("Case", orig, "to Case", path.data$dest.anchor))
    } else if (type == "pumps") {
      title(main = paste("Pump", orig, "to Pump", path.data$dest.anchor))
    }
  }
}

mapDataRange <- function(dat, land, path.data, vars, ew, ns) {
  if (any(path.data$orig.anchor >= 1000L)) {
    land.orig <- land[land$case %in% path.data$orig.anchor, ]
    if (grepl("Square", land.orig$name)) {
      sel <- grepl(path.data$orig.nm, cholera::landmarksB$name)
      label.orig <- cholera::landmarksB[sel, vars]
    } else {
      label.orig <- land.orig[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
      names(label.orig) <- vars
    }
  }

  if (any(path.data$dest.anchor >= 1000L)) {
    land.dest <- land[land$case %in% path.data$dest.anchor, ]
    if (grepl("Square", land.dest$name)) {
      sel <- grepl(path.data$dest.nm, cholera::landmarksB$name)
      label.dest <- cholera::landmarksB[sel, vars]
    } else {
      label.dest <- land.dest[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
      names(label.dest) <- vars
    }
  }

  if (exists("label.orig") & exists("label.dest")) {
    rbind(dat[, vars], label.orig, label.dest)
  } else if (exists("label.orig") & !exists("label.dest")) {
    rbind(dat[, vars], label.orig)
  } else if (!exists("label.orig") & exists("label.dest")) {
    rbind(dat[, vars], label.dest)
  } else {
    dat[, vars]
  }
}

casePump <- function(anchor, anchor.nm, destination, network.data, pmp, vestry,
  weighted) {

  edges <- network.data$edges
  g <- network.data$g
  nodes <- network.data$nodes

  ego.node <- c(nodes[nodes$case %in% anchor, ]$node,
                nodes[nodes$land %in% anchor, ]$node)

  pump.id <- selectPump(pmp, pump.select = destination, vestry = vestry)

  if (any(pump.id == 2L)) {
    # message("Note: Pump 2 excluded because it's a technical isolate.")
    pump.id <- pump.id[pump.id != 2L]
  }

  alters <- nodes[nodes$pump %in% pump.id, ]

  alter.node <- alters$node
  names(alter.node) <- alters$pump

  if (length(ego.node) == 1) {
    if (weighted) {
      d <- igraph::distances(g, ego.node, alter.node, weights = edges$d)
    } else {
      d <- igraph::distances(g, ego.node, alter.node)
    }

    nearest.node <- dimnames(d)[[2]][which.min(d)]
    nearest.dest <- as.character(alters[alters$node == nearest.node, ]$pump)

    if (weighted) {
      p <- igraph::shortest_paths(g, ego.node, alter.node[nearest.dest],
                                  weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(g, ego.node, alter.node[nearest.dest])
    }
  } else if (length(ego.node) > 1) {
    d.multi.ego <- lapply(ego.node, function(x) {
      if (weighted) {
        igraph::distances(g, x, alter.node, weights = edges$d)
      } else {
        igraph::distances(g, x, alter.node)
      }
    })

    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    d <- min(d.multi.ego[[ego.id]])

    sel <- (nodes$case != 0 | nodes$land != 0) &
            nodes$node %in% ego.node[ego.id]

    anchor <- nodes[sel, ]$case + nodes[sel, ]$land
    anchor.nm <- anchor.nm[ego.id]

    nr.ego.node <- nodes[sel, ]$node

    alter.id <- which.min(d.multi.ego[[ego.id]])
    nr.alter.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]
    nearest.dest <- as.character(nodes[nodes$node == nr.alter.node, ]$pump)

    if (weighted) {
      p <- igraph::shortest_paths(g, nr.ego.node, nr.alter.node,
                                  weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(g, nr.ego.node, nr.alter.node)
    }
  }

  list(anchor = anchor, anchor.nm = anchor.nm, nearest.dest = nearest.dest,
       p = p[[1]])
}

caseCase <- function(anchor, anchor.nm, destination, network.data, vestry,
  weighted) {

  edges <- network.data$edges
  g <- network.data$g
  nodes <- network.data$nodes

  ego.node <- c(nodes[nodes$case %in% anchor, ]$node,
                nodes[nodes$land %in% anchor, ]$node)

  if (is.null(destination)) {
    destination <- c(cholera::fatalities$case, cholera::landmarksB$case)
  }

  dest <- validateDestinationCases(destination)

  if (any(anchor %in% dest$anchor)) {
    if (length(anchor) == 1) {  # single origin
      dest <- dest[!dest$anchor %in% anchor, ]
      # message("Note: 'origin' anchor cases excluded from 'destination'.")
    } else if (length(anchor) > 1) {  # multiple origins | NULL origin
      anchor <- anchor[!anchor %in% dest$anchor]
    }
  }

  sel <- nodes$case %in% dest$anchor | nodes$land %in% dest$anchor
  alters <- nodes[sel, ]

  alter.node <- alters$node
  names(alter.node) <- alters$case + alters$land

  if (any(alter.node %in% ego.node)) {
    ego.node <- ego.node[!ego.node %in% alter.node]
  }

  if (length(ego.node) == 1) {
    if (weighted) {
      d <- igraph::distances(g, ego.node, alter.node, weights = edges$d)
    } else {
      d <- igraph::distances(g, ego.node, alter.node)
    }

    if (length(d) == 1) nearest.node <- dimnames(d)[[2]]
    else if (length(d) > 1) nearest.node <- dimnames(d)[[2]][which.min(d)]

    sel <- (nodes$case != 0 | nodes$land != 0) & nodes$node == nearest.node
    nearest.dest <- nodes[sel, ]$case + nodes[sel, ]$land

    if (weighted) {
      p <- igraph::shortest_paths(g, ego.node, nearest.node,
                                  weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(g, ego.node, nearest.node)
    }
  } else if (length(ego.node) > 1) {
    d.multi.ego <- lapply(ego.node, function(x) {
      if (weighted) {
        igraph::distances(g, x, alter.node, weights = edges$d)
      } else {
        igraph::distances(g, x, alter.node)
      }
    })

    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    d <- min(d.multi.ego[[ego.id]])

    sel <- (nodes$case != 0 | nodes$land != 0) &
            nodes$node == ego.node[ego.id]

    anchor <- nodes[sel, ]$case + nodes[sel, ]$land
    anchor.nm <- anchor.nm[ego.id]

    nr.ego.node <- nodes[sel, ]$node

    alter.id <- which.min(d.multi.ego[[ego.id]])
    nr.alter.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]

    sel <- (nodes$case != 0 | nodes$land != 0) & nodes$node == nr.alter.node
    nearest.dest <- nodes[sel, ]$case + nodes[sel, ]$land

    if (weighted) {
      p <- igraph::shortest_paths(g, nr.ego.node, nr.alter.node,
                                  weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(g, nr.ego.node, nr.alter.node)
    }
  }

  list(anchor = anchor, anchor.nm = anchor.nm, nearest.dest = nearest.dest,
       p = p[[1]])
}

pumpPump <- function(anchor, anchor.nm, destination, network.data, origin, pmp,
  vestry, weighted) {

  edges <- network.data$edges
  g <- network.data$g
  nodes <- network.data$nodes

  egos <- nodes[nodes$pump %in% anchor, ]
  if (nrow(egos) > 1) egos <- egos[order(egos$pump), ]

  pump.id <- selectPump(pmp, pump.select = destination, vestry = vestry)
  alters <- nodes[nodes$pump %in% pump.id, ]
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

  if (length(setdiff(anchor, egosB$pump) != 0)) {
    sel <- anchor %in% setdiff(anchor, egosB$pump)
    anchor <- anchor[!sel]
    anchor.nm <- anchor.nm[!sel]
  }

  if (length(ego.node) == 1) {
    if (weighted) {
      d <- igraph::distances(g, ego.node, alter.node, weights = edges$d)
    } else {
      d <- igraph::distances(g, ego.node, alter.node)
    }

    nearest.node <- dimnames(d)[[2]][which.min(d)]
    nearest.dest <- as.character(alters[alters$node == nearest.node, ]$pump)

    if (weighted) {
      p <- igraph::shortest_paths(g, ego.node, nearest.node,
        weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(g, ego.node, nearest.node)
    }

  } else if (length(ego.node) > 1) {
    d.multi.ego <- lapply(ego.node, function(x) {
      if (weighted) igraph::distances(g, x, alter.node, weights = edges$d)
      else igraph::distances(g, x, alter.node)
    })

    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    d <- min(d.multi.ego[[ego.id]])

    nr.ego.node <- nodes[nodes$node == ego.node[ego.id], ]$node

    alter.id <- which.min(d.multi.ego[[ego.id]])
    nr.alter.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]
    nearest.dest <- as.character(nodes[nodes$node == nr.alter.node, ]$pump)

    if (weighted) {
      p <- igraph::shortest_paths(g, nr.ego.node, nr.alter.node,
        weights = edges$d)$vpath
    } else {
      p <- igraph::shortest_paths(g, nr.ego.node, nr.alter.node)
    }

    anchor <- anchor[ego.id]
    anchor.nm <- anchor.nm[ego.id]
  }

  list(anchor = anchor, anchor.nm = anchor.nm, nearest.dest = nearest.dest,
       p = p[[1]])
}
