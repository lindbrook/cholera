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
        anchor.nm <- anchor
      }
    } else if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      if (!origin %in% cholera::landmarks$name &
          !origin %in% cholera::landmark.squares$name) {
        stop("Landmark not found. Check spelling or cholera::landmarks.",
          call. = FALSE)
      } else {
        if (origin %in% cholera::landmarks$name) {
          sel <- cholera::landmarks$name == origin
          anchor <- cholera::landmarks[sel, ]$case
          anchor.nm <- cholera::landmarks[sel, ]$name
        } else if (origin %in% cholera::landmark.squares$name) {
          sel <- grep(origin, cholera::landmarks$name)
          anchor <- cholera::landmarks[sel, ]$case
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

  network.data <- neighborhoodDataB(vestry = vestry, latlong = latlong,
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

      destination <- c(cholera::fatalities$case, cholera::landmarks$case)
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

  data.summary <- data.frame(orig.anchor = anchor,
                             dest.anchor = as.integer(nearest.dest),
                             orig.nm = anchor.nm,
                             dest.name = dest.nm,
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
