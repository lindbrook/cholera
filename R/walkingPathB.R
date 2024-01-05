#' Plot walking path to nearest pump (prototype).
#'
#' @param origin Numeric. A single numeric value (vector of length one).
#' @param destination Numeric. Vector of destinations (numeric or landmark names.
#' @param type Character. Path case to pump. FALSE is all other combinations of cases, landmarks and pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param latlong Logical.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param distance.unit Character. Unit of distance: "meter" or "yard".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @importFrom geosphere distGeo
#' @export

walkingPathB <- function(origin = 1, destination = NULL,
  type = "case-pump", vestry = FALSE, latlong = FALSE, weighted = TRUE,
  distance.unit = "meter", time.unit = "second", walking.speed = 5,
  multi.core = TRUE) {

  meter.to.yard <- 1.09361
  cores <- multiCore(multi.core)

  if (length(origin) != 1) {
    stop('"origin" must be a character/numeric vector of length one.',
      call. = FALSE)
  }

  if (is.character(destination)) {
    destination <- caseAndSpace(destination)
    if (any(grepl(destination, cholera::landmarksB$name)) & type != "cases") {
      type <- "cases"
    }
  } else if (is.numeric(destination)) {
    if (destination %in% cholera::landmarksB$case & type != "cases") {
      type <- "cases"
    }
  }

  if (type %in% c("case-pump", "cases")) {
    if (is.numeric(origin)) {
      if (!origin %in% cholera::fatalities$case &
          !origin %in% cholera::landmarksB$case) {
        stop("Cases range from 1 to 578; Landmarks from 1000 to 1021.",
          call. = FALSE)
      } else {
        if (origin < 1000L) {
          sel <- cholera::anchor.case$case == origin
          anchor <- cholera::anchor.case[sel, "anchor"]
        } else {
          anchor <- origin
        }
        anchor.nm <- anchor
      }
    } else if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      if (!origin %in% cholera::landmarksB$name &
          !origin %in% cholera::landmark.squaresB$name) {
        stop("Landmark not found. Check spelling or cholera::landmarksB.",
          call. = FALSE)
      } else {
        if (origin %in% cholera::landmarksB$name) {
          sel <- cholera::landmarksB$name == origin
          anchor <- cholera::landmarksB[sel, ]$case
          anchor.nm <- cholera::landmarksB[sel, ]$name
        } else if (origin %in% cholera::landmark.squaresB$name) {
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
      if (!origin %in% pmp$id) {
        stop("For vestry = ", vestry, ", pump IDs range from 1 to ", nrow(pmp),
          "." , call. = FALSE)
      } else {
        anchor <- origin
        anchor.nm <- pmp[pmp$id == origin, ]$street
      }
    } else if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      if (!origin %in% pmp$street) {
        stop("For vestry = ", vestry,
          ", pump (street) name not found. Check spelling or cholera::pumps.",
          call. = FALSE)
      } else {
        anchor <- pmp[pmp$street == origin, ]$id
        anchor.nm <- origin
      }
    }
  }

  network.data <- neighborhoodDataB(vestry = vestry, latlong = latlong)
  edges <- network.data$edges
  g <- network.data$g
  nodes <- network.data$nodes

  if (!is.null(destination)) {
    if (type == "case-pump") {
      ego.node <- c(nodes[nodes$case %in% anchor, ]$node,
                    nodes[nodes$land %in% anchor, ]$node)

      pump.id <- selectPump(pmp, pump.select = destination, vestry = vestry)

      if (any(pump.id == 2L)) {
        # message("Note: Pump 2 excluded because it's a technical isolate.")
        pump.id <- pump.id[pump.id != 2L]
      }

      alters <- nodes[nodes$pump %in% pump.id, ]
      if (nrow(alters) > 1) alters <- alters[order(alters$pump), ]
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

        min_dist.multi.ego <- vapply(d.multi.ego, min, numeric(1L))
        ego.id <- which.min(min_dist.multi.ego)
        d <- d.multi.ego[[ego.id]]

        sel <- nodes$node == ego.node[ego.id] & nodes$land != 0
        anchor <- nodes[sel, ]$land
        nearest.ego.node <- nodes[sel, ]$node

        alter.id <- which.min(d.multi.ego[[ego.id]])
        nearest.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]
        nearest.dest <- as.character(nodes[nodes$node == nearest.node, ]$pump)

        if (weighted) {
          p <- igraph::shortest_paths(g, nearest.ego.node, nearest.node,
                                      weights = edges$d)$vpath
        } else {
          p <- igraph::shortest_paths(g, nearest.ego.node, nearest.node)
        }
      }

    } else if (type == "cases") {
      ego.node <- c(nodes[nodes$case %in% anchor, ]$node,
                    nodes[nodes$land %in% anchor, ]$node)

      dest <- validateDestinationCases(destination)

      if (any(anchor %in% dest$anchor)) {
        dest <- dest[!dest$anchor %in% anchor, ]
        # message("Note: 'origin' anchor cases excluded from 'destination'.")
      }

      alter.node <- c(nodes[nodes$case %in% dest$anchor, ]$node,
                      nodes[nodes$land %in% dest$anchor, ]$node)

      if (length(ego.node) == 1) {
        if (weighted) {
          d <- igraph::distances(g, ego.node, alter.node, weights = edges$d)
        } else {
          d <- igraph::distances(g, ego.node, alter.node)
        }

        nearest.node <- dimnames(d)[[2]][which.min(d)]

        sel <- nodes$node == nearest.node & (nodes$case != 0 | nodes$land != 0)
        nearest.candidate <- nodes[sel, ]

        nearest.dest <- nearest.candidate$case + nearest.candidate$land

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

        min_dist.multi.ego <- vapply(d.multi.ego, min, numeric(1L))
        ego.id <- which.min(min_dist.multi.ego)
        d <- d.multi.ego[[ego.id]]

        sel <- nodes$node == ego.node[ego.id] & nodes$land != 0
        anchor <- nodes[sel, ]$land
        nearest.ego.node <- nodes[sel, ]$node

        alter.id <- which.min(d.multi.ego[[ego.id]])
        nearest.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]

        sel <- nodes$node == nearest.node & (nodes$case != 0 | nodes$land != 0)
        nearest.candidate <- nodes[sel, ]

        nearest.dest <- nearest.candidate$case + nearest.candidate$land

        if (weighted) {
          p <- igraph::shortest_paths(g, nearest.ego.node, nearest.node,
                                      weights = edges$d)$vpath
        } else {
          p <- igraph::shortest_paths(g, nearest.ego.node, nearest.node)
        }
      }

    } else if (type == "pumps") {
      ego.node <- nodes[nodes$pump == anchor, ]$node
      pump.id <- selectPump(pmp, pump.select = destination, vestry = vestry)
      alters <- nodes[nodes$pump %in% pump.id & nodes$pump != 0, ]

      if (origin %in% pump.id) {
        # message("Note: 'origin' pumps excluded from 'destination'.")
        alters <- alters[alters$pump %in% setdiff(pump.id, origin), ]
      }

      alters <- alters[alters$pump != 2, ]
      # message("Note: Pump 2 excluded because it's a technical isolate.")
      alter.node <- alters$node
      names(alter.node) <- alters$pump

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
    }

  } else {
    if (type == "case-pump") {
      ego.node <- c(nodes[nodes$case %in% anchor, ]$node,
                    nodes[nodes$land %in% anchor, ]$node)

      alters <- nodes[!nodes$pump %in% anchor & nodes$pump != 0, ]
      alters <- alters[order(alters$pump), ]
      # message("Note: Pump 2 excluded because it's a technical isolate.")
      alters <- alters[alters$pump != 2, ]

      alter.node <- alters$node
      names(alter.node) <- alters$pump

      if (length(ego.node) == 1) {
        if (weighted) {
          d <- igraph::distances(g, ego.node, alter.node, weights = edges$d)
        } else {
          d <- igraph::distances(g, ego.node, alter.node)
        }

        nearest.node <- dimnames(d)[[2]][which.min(d)]
        sel <- alters$node == nearest.node
        nearest.dest <- as.character(alters[sel, ]$pump)

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

        min_dist.multi.ego <- vapply(d.multi.ego, min, numeric(1L))
        ego.id <- which.min(min_dist.multi.ego)
        d <- d.multi.ego[[ego.id]]

        sel <- nodes$node == ego.node[ego.id] & nodes$land != 0
        anchor <- nodes[sel, ]$land
        nearest.ego.node <- nodes[sel, ]$node

        alter.id <- which.min(d.multi.ego[[ego.id]])
        nearest.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]
        nearest.dest <- as.character(nodes[nodes$node == nearest.node, ]$pump)

        if (weighted) {
          p <- igraph::shortest_paths(g, nearest.ego.node, nearest.node,
                                      weights = edges$d)$vpath
        } else {
          p <- igraph::shortest_paths(g, nearest.ego.node, nearest.node)
        }
      }

    } else if (type == "cases") {
      ego.node <- c(nodes[nodes$case %in% anchor, ]$node,
                    nodes[nodes$land %in% anchor, ]$node)

      destination <- c(cholera::fatalities$case, cholera::landmarksB$case)
      dest <- validateDestinationCases(destination)

      if (any(anchor %in% dest$anchor)) {
        dest <- dest[!dest$anchor %in% anchor, ]
        # message("Note: 'origin' anchor cases excluded from 'destination'.")
      }

      sel <- nodes$case %in% dest$anchor | nodes$land %in% dest$anchor
      alters <- nodes[sel, ]

      if (nrow(alters) > 1) {
        case.land <- alters$case + alters$land
        alters <- alters[order(case.land), ]
      }

      alter.node <- alters$node
      names(alter.node) <- alters$case + alters$land

      if (length(ego.node) == 1) {
        if (weighted) {
          d <- igraph::distances(g, ego.node, alter.node, weights = edges$d)
        } else {
          d <- igraph::distances(g, ego.node, alter.node)
        }

        nearest.node <- dimnames(d)[[2]][which.min(d)]
        nearest.dest <- nodes[nodes$node == nearest.node, ]$case +
                        nodes[nodes$node == nearest.node, ]$land

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

        min_dist.multi.ego <- vapply(d.multi.ego, min, numeric(1L))
        ego.id <- which.min(min_dist.multi.ego)
        d <- d.multi.ego[[ego.id]]

        sel <- nodes$node == ego.node[ego.id] & nodes$land != 0
        anchor <- nodes[sel, ]$land
        nearest.ego.node <- nodes[sel, ]$node

        alter.id <- which.min(d.multi.ego[[ego.id]])
        nearest.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]
        nearest.dest <- nodes[nodes$node == nearest.node, ]$case +
                        nodes[nodes$node == nearest.node, ]$land

        if (weighted) {
          p <- igraph::shortest_paths(g, nearest.ego.node, nearest.node,
                                      weights = edges$d)$vpath
        } else {
          p <- igraph::shortest_paths(g, nearest.ego.node, nearest.node)
        }
      }

    } else if (type == "pumps") {
      ego.node <- nodes[nodes$pump == anchor, "node"]

      alters <- nodes[!nodes$pump %in% anchor & nodes$pump != 0, ]
      # message("Note: 'origin' pumps excluded from 'destination'.")
      alters <- alters[order(alters$pump), ]

      alters <- alters[alters$pump != 2, ]
      # message("Note: Pump 2 excluded because it's a technical isolate.")

      alter.node <- alters$node
      names(alter.node) <- alters$pump

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
    }
  }

  p <- names(unlist(p))
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
                             distance = round(sum(ds)),
                             time = round(walking.time),
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

  if (type == "cases") {
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
  t <- paste(round(x$data$time), paste0(time.unit, "s"), "@", walking.speed,
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
      alpha <- paste(orig.nm, "Pump", paste0("(#", path.data$orig.anchor, ")"))
      omega <- paste(dest.nm, "Pump", paste0("(#", path.data$dest.anchor, ")"))
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
  if (path.data$orig.anchor >= 1000L) {
    land.orig <- land[land$case == path.data$orig.anchor, ]
    if (grepl("Square", land.orig$name)) {
      sel <- grepl(path.data$orig.nm, cholera::landmarksB$name)
      label.orig <- cholera::landmarksB[sel, vars]
    } else {
      label.orig <- land.orig[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
      names(label.orig) <- vars
    }
  }

  if (path.data$dest.anchor >= 1000L) {
    land.dest <- land[land$case == path.data$dest.anchor, ]
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
