#' Plot walking path to nearest pump (prototype).
#'
#' @param origin Numeric. A single numeric value (vector of length one).
#' @param destination Numeric. Vector of destinations (numeric or landmark names.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param type Character. Path case to pump. FALSE is all other combinations of cases, landmarks and pumps.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param distance.unit Character. Unit of distance: "meter" or "yard".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

latlongWalkingPath <- function(origin = 1, destination = NULL,
  type = "case-pump", vestry = FALSE, weighted = TRUE, distance.unit = "meter",
  time.unit = "second", walking.speed = 5, multi.core = TRUE) {

  meter.to.yard <- 1.09361
  cores <- multiCore(multi.core)

  if (length(origin) != 1) {
    stop('"origin" must be a character/numeric vector of length one.',
      call. = FALSE)
  }

  if (type %in% c("case-pump", "cases")) {
    if (is.numeric(origin)) {
      if (!origin %in% cholera::fatalities$case &
          !origin %in% cholera::landmarks$case) {
        stop("Cases range from 1 to 578; Landmarks from 20001 to 20019.",
          call. = FALSE)
      } else {
        if (origin < 20000L) {
          sel <- cholera::anchor.case$case == origin
          anchor <- cholera::anchor.case[sel, "anchor"]
        } else {
          anchor <- origin
        }
      }
    } else if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      if (!origin %in% cholera::landmarks$name &
          !origin %in% cholera::landmark.squares$name) {
        stop("Landmarks not found. Check spelling or cholera::landmarks.",
          call. = FALSE)
      } else {
        if (origin %in% cholera::landmarks$name) {
          sel <- cholera::landmarks$name == origin
          anchor <- cholera::landmarks[sel, ]$case
        } else if (origin %in% cholera::landmark.squares$name) {
          sel <- grep(origin, cholera::landmarks$name)
          anchor <- cholera::landmarks[sel, ]$case
        }
      }
    }
  }

  if (type %in% c("case-pump", "pumps")) {
    if (vestry) pmp <- cholera::pumps.vestry
    else pmp <- cholera::pumps
  }

  if (type == "pumps") {
    if (is.numeric(origin)) {
      if (!origin %in% pmp$id) {
        stop("For vestry = ", vestry, ", pump IDs range from 1 to ", nrow(pmp),
          "." , call. = FALSE)
      } else anchor <- origin
    } else if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      if (!origin %in% pmp$street) {
        stop("For vestry = ", vestry,
          ", pump (street) name not found. Check spelling or cholera::pumps.",
          call. = FALSE)
      } else anchor <- pmp[pmp$street == origin, ]$id
    }
  }

  network.data <- neighborhoodDataB(vestry = vestry, latlong = TRUE,
    multi.core = cores)
  edges <- network.data$edges
  g <- network.data$g
  nodes <- network.data$nodes

  if (!is.null(destination)) {
    if (type == "case-pump") {
      ego.node <- c(nodes[nodes$case %in% anchor, ]$node,
                    nodes[nodes$land %in% anchor, ]$node)

      pump.id <- selectPump(pmp, pump.select = destination, vestry = vestry)

      if (any(pump.id == 2L)) {
        message("Note: Pump 2 excluded because it's a technical isolate.")
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
        alter.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]
        nearest.dest <- as.character(nodes[nodes$node == alter.node, ]$pump)

        if (weighted) {
          p <- igraph::shortest_paths(g, nearest.ego.node, alter.node,
                                      weights = edges$d)$vpath
        } else {
          p <- igraph::shortest_paths(g, nearest.ego.node, alter.node)
        }
      }

    } else if (type == "cases") {
      ego.node <- c(nodes[nodes$case %in% anchor, ]$node,
                    nodes[nodes$land %in% anchor, ]$node)

      dest <- validateDestinationCases(destination)


      if (any(anchor %in% dest$anchor)) {
        dest <- dest[!dest$anchor %in% anchor, ]
        message("Note: 'origin' anchor cases excluded from 'destination'.")
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
        message("Note: 'origin' pumps excluded from 'destination'.")
        alters <- alters[alters$pump %in% setdiff(pump.id, origin), ]
      }

      alters <- alters[alters$pump != 2, ]

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

    p <- names(unlist(p))
    p.data <- do.call(rbind, strsplit(p, "_&_"))
    path <- data.frame(id = seq_len(nrow(p.data)),
                       lon = as.numeric(p.data[, 1]),
                       lat = as.numeric(p.data[, 2]))

    endpts <- do.call(rbind, lapply(seq_len(length(p[-1])), function(i) {
      data.frame(ep1 = p[i], ep2 = p[i + 1])
    }))

    ds <- vapply(seq_len(nrow(endpts)), function(i) {
      tmp <- endpts[i, ]
      edge.sel <- tmp$ep1 == edges$node1 & tmp$ep2 == edges$node2 |
                  tmp$ep1 == edges$node2 & tmp$ep2 == edges$node1
      edges[edge.sel, ]$d
    }, numeric(1L))

    walking.time <- walkingTime(d[colnames(d) == nearest.node],
      time.unit = time.unit, walking.speed = walking.speed)

    if (as.integer(nearest.dest) < 20000L) {
      if (type %in% c("case-pump", "pumps")) {
        dest.nm <- pmp[pmp$id == nearest.dest, ]$street
      } else if (type == "cases") {
        dest.nm <- nearest.dest
      }
    } else if (as.integer(nearest.dest) >= 20000L) {
      sel <- cholera::landmarks$case == as.integer(nearest.dest)
      dest.nm <- cholera::landmarks[sel, ]$name
      if (grepl("Square", dest.nm)) {
        sel <- cholera::landmarks$case == nearest.dest
        tmp <- strsplit(cholera::landmarks[sel, ]$name, "-")
        dest.nm <- unlist(tmp)[1]
      }
    }

    data.summary <- data.frame(origin = origin, anchor = anchor,
      dest.name = dest.nm, destination = as.integer(nearest.dest),
      distance = d[colnames(d) == nearest.node], time = walking.time,
      row.names = NULL)

    output <- list(path = path,
                   data = data.summary,
                   destination = destination,
                   vestry = vestry,
                   ds = ds,
                   distance.unit = distance.unit,
                   time.unit = time.unit,
                   walking.speed = walking.speed)
  } else {
    if (type == "case-pump") {
      if (any(anchor >= 20000L)) {
        ego.land.node <- nodes[nodes$land %in% anchor, ]$node
      } else if (anchor < 20000L) {
        ego.case.node <- nodes[nodes$case == anchor, ]$node
      }

      if (exists("ego.case.node") & exists("ego.land.node")) {
        ego.node <- c(ego.case.node, ego.land.node)
      } else if (exists("ego.case.node") & !exists("ego.land.node")) {
        ego.node <- ego.case.node
      } else if (!exists("ego.case.node") & exists("ego.land.node")) {
        ego.node <- ego.land.node
      }

      alters <- nodes[!nodes$pump %in% anchor & nodes$pump != 0, ]
      alters <- alters[order(alters$pump), ]
      message("Note: Pump 2 excluded because it's a technical isolate.")
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

        sel <- nodes$node == ego.node[ego.id] & nodes$land != 0
        anchor <- nodes[sel, ]$land
        nearest.ego.node <- nodes[sel, ]$node

        alter.id <- which.min(d.multi.ego[[ego.id]])
        alter.node <- dimnames(d.multi.ego[[ego.id]])[[2]][alter.id]
        nearest.dest <- as.character(nodes[nodes$node == alter.node, ]$pump)

        if (weighted) {
          p <- igraph::shortest_paths(g, nearest.ego.node, alter.node,
                                      weights = edges$d)$vpath
        } else {
          p <- igraph::shortest_paths(g, nearest.ego.node, alter.node)
        }
      }

      p <- names(unlist(p))
      p.data <- do.call(rbind, strsplit(p, "_&_"))
      path <- data.frame(id = seq_len(nrow(p.data)),
                         lon = as.numeric(p.data[, 1]),
                         lat = as.numeric(p.data[, 2]))

      endpts <- do.call(rbind, lapply(seq_len(length(p[-1])), function(i) {
        data.frame(ep1 = p[i], ep2 = p[i + 1])
      }))

      ds <- vapply(seq_len(nrow(endpts)), function(i) {
        tmp <- endpts[i, ]
        edge.sel <- tmp$ep1 == edges$node1 & tmp$ep2 == edges$node2 |
                    tmp$ep1 == edges$node2 & tmp$ep2 == edges$node1
        edges[edge.sel, ]$d
      }, numeric(1L))

      nodes <- do.call(rbind, strsplit(p, "_&_"))
      dat <- data.frame(lon = as.numeric(nodes[, 1]),
                        lat = as.numeric(nodes[, 2]))

      if (distance.unit == "meter") {
        speed <- walking.speed
      } else if (distance.unit == "yard") {
        speed <- walking.speed * meter.to.yard
        ds <- ds * meter.to.yard
      } else {
        stop('distance.unit must be "meter" or "yard".')
      }

      path.length <- sum(ds)

      walking.time <- walkingTime(path.length, time.unit = time.unit,
        walking.speed = walking.speed)

      path <- data.frame(id = seq_along(dat$lon), dat)

      data.summary <- data.frame(origin = origin, anchor = anchor,
        pump.name = pmp[pmp$id == nearest.dest, "street"],
        pump = nearest.dest, distance = path.length, time = walking.time,
        row.names = NULL)

      output <- list(path = path,
                     data = data.summary,
                     destination = destination,
                     vestry = vestry,
                     ds = ds,
                     distance.unit = distance.unit,
                     pmp = pmp,
                     time.unit = time.unit,
                     walking.speed = walking.speed)

    } else if (type == "cases") {
      ego.node <- c(nodes[nodes$case %in% anchor, ]$node,
                    nodes[nodes$land %in% anchor, ]$node)

      destination <- c(cholera::fatalities$case, cholera::landmarks$case)
      dest <- validateDestinationCases(destination)

      if (any(anchor %in% dest$anchor)) {
        dest <- dest[dest$anchor != anchor, ]
        message("Note: 'origin' anchor cases excluded from 'destination'.")
      }

      sel <- nodes$case %in% dest$anchor | nodes$land %in% dest$anchor
      alters <- nodes[sel, ]

      if (nrow(alters) > 1) {
        case.land <- alters$case + alters$land
        alters <- alters[order(case.land), ]
      }

      alter.node <- alters$node
      names(alter.node) <- alters$case + alters$land

      if (weighted) {
        d <- c(igraph::distances(g, ego.node, alter.node, weights = edges$d))
      } else {
        d <- c(igraph::distances(g, ego.node, alter.node))
      }

      names(d) <- names(alter.node)

      if (nrow(alters) > 1) {
        nearest.dest <- names(which.min(d))
      } else {
        nearest.dest <- names(d)
      }

      if (weighted) {
        p <- igraph::shortest_paths(g, ego.node, alter.node[nearest.dest],
                                    weights = edges$d)$vpath
      } else {
        p <- igraph::shortest_paths(g, ego.node, alter.node[nearest.dest])
      }

      p <- names(unlist(p))
      p.data <- do.call(rbind, strsplit(p, "_&_"))
      path <- data.frame(id = seq_len(nrow(p.data)),
                         lon = as.numeric(p.data[, 1]),
                         lat = as.numeric(p.data[, 2]))

      endpts <- do.call(rbind, lapply(seq_len(length(p[-1])), function(i) {
        data.frame(ep1 = p[i], ep2 = p[i + 1])
      }))

      ds <- vapply(seq_len(nrow(endpts)), function(i) {
        tmp <- endpts[i, ]
        edge.sel <- tmp$ep1 == edges$node1 & tmp$ep2 == edges$node2 |
                    tmp$ep1 == edges$node2 & tmp$ep2 == edges$node1
        edges[edge.sel, ]$d
      }, numeric(1L))

      walking.time <- walkingTime(d[nearest.dest], time.unit = time.unit,
                                  walking.speed = walking.speed)

      if (as.integer(nearest.dest) < 20000L) {
        dest.nm <- as.integer(nearest.dest)
      } else if (as.integer(nearest.dest) >= 20000L) {
        sel <- cholera::landmarks$case == as.integer(nearest.dest)
        dest.nm <- cholera::landmarks[sel, ]$name
        if (grepl("Square", dest.nm)) {
          sel <- cholera::landmarks$case == nearest.dest
          tmp <- strsplit(cholera::landmarks[sel, ]$name, "-")
          dest.nm <- unlist(tmp)[1]
        }
      }

      data.summary <- data.frame(origin = origin,
                                 anchor = anchor,
                                 dest.name = dest.nm,
                                 destination = as.integer(nearest.dest),
                                 distance = d[nearest.dest],
                                 time = walking.time,
                                 row.names = NULL)

      output <- list(path = path,
                     data = data.summary,
                     destination = destination,
                     vestry = vestry,
                     ds = ds,
                     distance.unit = distance.unit,
                     # pmp = pmp,
                     time.unit = time.unit,
                     walking.speed = walking.speed)

    } else if (type == "pumps") {
      ego.node <- nodes[nodes$pump == anchor, "node"]

      alters <- nodes[!nodes$pump %in% anchor & nodes$pump != 0, ]
      alters <- alters[order(alters$pump), ]

      if (any(pump.id == 2L)) {
        message("Note: Pump 2 excluded because it's a technical isolate.")
        alters <- alters[alters$pump != 2, ]
      }

      if (origin %in% pump.id) {
        message("Note: 'origin' pumps excluded from 'destination'.")
        alters <- alters[alters$pump %in% setdiff(pump.id, origin), ]
      }

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
        p <- igraph::shortest_paths(g, ego.node, alter.node[nearest.dest],
                                    weights = edges$d)$vpath
      } else {
        p <- igraph::shortest_paths(g, ego.node, alter.node[nearest.dest])
      }

      p <- names(unlist(p))
      p.data <- do.call(rbind, strsplit(p, "_&_"))
      path <- data.frame(id = seq_len(nrow(p.data)),
                         lon = as.numeric(p.data[, 1]),
                         lat = as.numeric(p.data[, 2]))

      endpts <- do.call(rbind, lapply(seq_len(length(p[-1])), function(i) {
        data.frame(ep1 = p[i], ep2 = p[i + 1])
      }))

      ds <- vapply(seq_len(nrow(endpts)), function(i) {
        tmp <- endpts[i, ]
        edge.sel <- tmp$ep1 == edges$node1 & tmp$ep2 == edges$node2 |
                    tmp$ep1 == edges$node2 & tmp$ep2 == edges$node1
        edges[edge.sel, ]$d
      }, numeric(1L))

      walking.time <- walkingTime(sum(ds), time.unit = time.unit,
                                  walking.speed = walking.speed)

      p.nm <- pmp[pmp$id == nearest.dest, ]$street

      data.summary <- data.frame(origin = origin,
                                 anchor = anchor,
                                 dest.name = p.nm,
                                 destination = as.integer(nearest.dest),
                                 distance = d[which.min(d)],
                                 time = walking.time,
                                 row.names = NULL)

      output <- list(path = path,
                     data = data.summary,
                     destination = destination,
                     vestry = vestry,
                     ds = ds,
                     distance.unit = distance.unit,
                     pmp = pmp,
                     time.unit = time.unit,
                     walking.speed = walking.speed)
    }
  }

  class(output) <- "latlong_walking_path"
  output
}

#' Plot the walking path between selected cases and/or pumps.
#'
#' @param x An object of class "latlong_walking_path" created by latlongWalkingPath().
#' @param zoom Logical or Numeric. A numeric value >= 0 that controls the degree of zoom.
#' @param mileposts Logical. Plot mile/time posts.
#' @param milepost.unit Character. "distance" or "time".
#' @param milepost.interval Numeric. Mile post interval unit of distance (yard or meter) or unit of time (seconds).
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.latlong_walking_path <- function(x, zoom = TRUE, mileposts = TRUE,
  milepost.unit = "distance", milepost.interval = NULL, alpha.level = 1, ...) {

  path.data <- x$data
  case <- path.data$case
  destination <- x$destination
  colors <- snowColors(x$vestry)
  dat <- x$path
  ds <- x$ds
  distance.unit <- x$distance.unit
  pmp <- x$pmp
  time.unit <- x$time.unit
  walking.speed <- x$walking.speed

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

  case.address <- cholera::latlong.ortho.addr
  addr <- cholera::anchor.case[cholera::anchor.case$case == case, "anchor"]
  if (x$vestry) pump.address <- cholera::latlong.ortho.pump.vestry
  else pump.address <- cholera::latlong.ortho.pump

  if (is.logical(zoom)) {
    if (zoom) {
      padding <- 0.00026
      xlim <- c(min(dat$x) - padding, max(dat$x) + padding)
      ylim <- c(min(dat$y) - padding, max(dat$y) + padding)
    } else {
      map.data <- rbind(frame, rd)
      xlim <- range(map.data$lon)
      ylim <- range(map.data$lat)
    }
  } else if (is.numeric(zoom)) {
    if (zoom >= 0) {
      xlim <- c(min(dat$x) - zoom, max(dat$x) + zoom)
      ylim <- c(min(dat$y) - zoom, max(dat$y) + zoom)
    } else stop("If numeric, zoom must be >= 0.")
  } else stop("zoom must either be logical or numeric.")

  vars <- c("lon", "lat")

  p.sel <- paste0("p", path.data$pump)
  case.color <- grDevices::adjustcolor(colors[p.sel], alpha.f = alpha.level)

  plot(rd[, vars], pch = NA, asp = 1.6, xlim = xlim, ylim = ylim)
  roads.list <- split(rd[, vars], rd$street)
  frame.list <- split(frame[, vars], frame$street)
  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(frame.list, lines))
  points(fatality[, vars], col = "lightgray", pch = 16, cex = 0.5)
  points(fatality[fatality$case == case, vars], col = "red")
  text(fatality[fatality$case == case, vars], pos = 1, labels = case,
    col = "red")
  points(pmp[, vars], pch = 24, col = grDevices::adjustcolor(colors,
    alpha.f = alpha.level))
  text(pmp[, vars], pos = 1, labels = paste0("p", pmp$id))
  points(case.address[case.address$case == addr, vars], pch = 0,
    col = case.color)
  points(pump.address[pump.address$id == path.data$pump, vars], pch = 0,
    col = case.color)
  points(dat[1, c("x", "y")], col = "dodgerblue", pch = 0)
  points(dat[nrow(dat), c("x", "y")], col = "dodgerblue", pch = 0)

  drawPathLatLong(dat, case.color)

  d <- paste(round(path.length, 1), d.unit)
  t <- paste(round(x$data$time), paste0(time.unit, "s"), "@", walking.speed,
    "km/hr")

  if (mileposts) {
    if (is.null(milepost.interval)) {
      if (milepost.unit == "distance") {
        milepost.interval <- 50
      } else if (milepost.unit == "time") {
        milepost.interval <- 60
      }
    }

    milepost.data <- milePostsLatLong(path.data, dat, ds, milepost.unit,
      milepost.interval, distance.unit, time.unit, walking.speed, destination)
    seg.data <- milepost.data$seg.data

    if (path.length > milepost.interval) {
      arrow.head <- milepost.data$arrow.head
      arrow.tail <- milepost.data$arrow.tail
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

    arrows(seg.data[1, "x2"], seg.data[1, "y2"],
           seg.data[1, "x1"], seg.data[1, "y1"],
           length = 0.0875, lwd = 3, col = case.color)
    if (path.length >= milepost.interval) {
      # dotchart(log(abs(arrow.tail$lon - arrow.head$lon)))
      # dotchart(log(abs(arrow.tail$lat - arrow.head$lat)))
      cutpoint <- -13

      zero.length.lon <- log(abs(arrow.tail$lon - arrow.head$lon)) < cutpoint
      zero.length.lat <- log(abs(arrow.tail$lat - arrow.head$lat)) < cutpoint

      if (any(zero.length.lon | zero.length.lat)) {
        zero.id <- unique(row.names(arrow.head[zero.length.lon, ]),
                          row.names(arrow.head[zero.length.lat, ]))

        angle <- vapply(zero.id, function(id) {
          zero.arrow <- rbind(arrow.tail[id, vars], arrow.head[id, vars])
          ols <- stats::lm(lat ~ lon, data = zero.arrow)
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

      arrows(arrow.tail$lon, arrow.tail$lat, arrow.head$lon, arrow.head$lat,
        length = 0.0875, lwd = 3, col = case.color)
    }

    title(main = paste("Case", case, "to Pump", path.data$pump),
          sub = paste(d, t, post.info, sep = "; "))

  } else {
    title(main = paste("Case", case, "to Pump", path.data$pump),
          sub = paste(d, t, sep = "; "))
  }
}

#' Print method for latlongWalkingPath().
#'
#' Summary output.
#' @param x An object of class "latlong_walking_path" created by latlongWalkingPath().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export

print.latlong_walking_path <- function(x, ...) {
  if (!inherits(x, "latlong_walking_path")) {
    stop('"x"\'s class must be "latlong_walking_path".')
  }
  print(x[c("path", "data")])
}

drawPathLatLong <- function(x, case.color) {
  path.data <- x
  n1 <- path.data[1:(nrow(path.data) - 1), ]
  n2 <- path.data[2:nrow(path.data), ]
  segments(n1$x, n1$y, n2$x, n2$y, lwd = 3, col = case.color)
}

milePostsLatLong <- function(path.data, dat, ds, milepost.unit,
  milepost.interval, distance.unit, time.unit, walking.speed, destination) {

  rev.data <- dat[order(dat$id, decreasing = TRUE), ]

  vars <- c("x", "y")
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
    origin <- data.frame(lon = min(cholera::roads$lon),
                         lat = min(cholera::roads$lat))
    topleft <- data.frame(lon = min(cholera::roads$lon),
                          lat = max(cholera::roads$lat))
    bottomright <- data.frame(lon = max(cholera::roads$lon),
                              lat = min(cholera::roads$lat))

    milepost.values <- seq_along(milepost.seg.id) * milepost.interval
    census <- data.frame(seg = milepost.seg.id, post = milepost.values)

    if (any(segment.census > 1)) {
      single.arrow.data <- arrowData(single.post.seg, census, seg.data, origin,
        milepost.unit)
      multi.arrow.data <- arrowData(multi.post.seg, census, seg.data, origin,
        milepost.unit, multi.arrow.seg = TRUE)
      arrow.data <- rbind(single.arrow.data, multi.arrow.data)
    } else {
      arrow.data <- arrowData(single.post.seg, census, seg.data, origin,
        milepost.unit)
    }

    arrow.tail <- stats::setNames(arrow.data[, paste0(vars, 1)], vars)
    arrow.head <- stats::setNames(arrow.data[, paste0(vars, 2)], vars)
    arrow.tail <- meterLatLong(arrow.tail, origin, topleft, bottomright)
    arrow.head <- meterLatLong(arrow.head, origin, topleft, bottomright)
    arrow.tail <- arrow.tail[order(row.names(arrow.tail)), ]
    arrow.head <- arrow.head[order(row.names(arrow.head)), ]
    out <- list(seg.data = seg.data, arrow.head = arrow.head,
      arrow.tail = arrow.tail)
  } else {
    out <- list(seg.data = seg.data)
  }
  out
}

arrowData <- function(segs, census, seg.data, origin, milepost.unit,
  multi.arrow.seg = FALSE, vars = c("lon", "lat")) {

  out <- lapply(segs, function(s) {
    tmp <- seg.data[seg.data$id == s, ]
    endpt1 <- stats::setNames(tmp[, grep("1", names(tmp))], vars)
    endpt2 <- stats::setNames(tmp[, grep("2", names(tmp))], vars)
    latlong.tmp <- rbind(endpt1, endpt2)

    idx <- seq_along(latlong.tmp$lon)

    meter.coords <- do.call(rbind, lapply(idx, function(i) {
      tmp <- latlong.tmp[i, vars]
      x.proj <- c(tmp$lon, origin$lat)
      y.proj <- c(origin$lon, tmp$lat)
      m.lon <- geosphere::distGeo(y.proj, tmp)
      m.lat <- geosphere::distGeo(x.proj, tmp)
      data.frame(x = m.lon, y = m.lat)
    }))

    ols <- stats::lm(y ~ x, data = meter.coords)
    seg.slope <- stats::coef(ols)[2]
    theta <- atan(seg.slope)

    if (multi.arrow.seg) {
      posts <- census[census$seg %in% s, "post"]
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
      do.call(rbind, multi.out)

    } else {
      post <- census[census$seg == s, "post"]
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
    }
  })

  do.call(rbind, out)
}
