#' Compute the shortest walking path between cases and/or pumps.
#'
#' @param origin Numeric or Character. Numeric ID of case or pump. Character landmark name.
#' @param destination Numeric or Character. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}: this returns closest pump or "anchor" case. Character landmark name (case insensitive).
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of nodes.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. "unit" is meaningful only when "weighted" is TRUE. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @note The function uses a case's "address" (i.e., a stack's "anchor" case) to compute distance. Time is computed using \code{distanceTime()}. Adam and Eve Court, and Falconberg Court and Falconberg Mews, are disconnected from the larger road network; they form two isolated subgraphs. This has two consequences: first, only cases on Adam and Eve Court can reach pump 2 and those cases cannot reach any other pump; second, cases on Falconberg Court and Mews cannot reach any pump. Unreachable pumps will return distances of "Inf".
#' @return An R list with two elements: a character vector of path nodes and a data frame summary.
#' @export
#' @examples
#' \donttest{
#' # path from case 1 to nearest pump.
#' walkingPath(1)
#'
#' # path from pump 1 to nearest case.
#' walkingPath(NULL, 1)
#'
#' # path from case 1 to pump 6.
#' walkingPath(1, 6)
#'
#' # exclude pump 7 from consideration.
#' walkingPath(1, -7)
#'
#' # path from case 1 to case 6.
#' walkingPath(1, 6, type = "cases")
#'
#' # path from pump 1 to pump 6.
#' walkingPath(1, 6, type = "pumps")
#'
#' # for multiple cases.
#' lapply(1:3, walkingPath)
#'
#' # path from case 1 to nearest pump.
#' plot(walkingPath(1))
#'
#' # path from John Snow's residence to Broad Street pump.
#' plot(walkingPath("John Snow", 7))
#' }

walkingPath <- function(origin = 1, destination = NULL, type = "case-pump",
  observed = TRUE, weighted = TRUE, vestry = FALSE, distance.unit = "meter",
  time.unit = "second", walking.speed = 5) {

  if (is.null(origin) & is.null(destination)) {
    stop("If origin = NULL, you must supply a destination.")
  }

  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
    stop('distance.unit must be "meter", "yard" or "native".')
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

  if (type %in% c("case-pump", "pumps")) {
    if (is.null(destination) == FALSE) {
      if (length(destination) == 1) {
        if (destination == 2) {
          stop('Pump 2 is a technical isolate. Choose another.')
        }
      }
      if (any(abs(destination) == 2)) {
        message('Pump 2 is a technical isolate. Already not considered.')
      }
    }
  }

  if (type == "pumps") {
    if (length(origin) == 1) {
      if (origin == 2) {
        stop('Pump 2 is a technical isolate. Choose another.')
      }
    }

    if (is.null(origin) == FALSE) {
      if (any(abs(origin) == 2)) {
        message('Pump 2 is a technical isolate. Already not considered.')
      }
    }
  }

  if (type %in% c("cases", "pumps")) {
    if (is.null(origin) == FALSE & is.null(destination) == FALSE) {
      alpha.omega <- c(origin, destination)
      if (all(is.numeric(alpha.omega)) | all(is.character(alpha.omega))) {
        if (origin == destination) {
          stop("origin and destination are at same address!")
        }
      }
    }
  }

  if (observed) {
    node.data <- neighborhoodData(vestry)
  } else {
    node.data <- neighborhoodData(vestry, case.set = "expected")
  }

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g

  obs.ct <- nrow(cholera::fatalities)
  exp.ct <- nrow(cholera::regular.cases)

  if (observed) {
    ct <- obs.ct
  } else {
    ct <- exp.ct
  }

  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  # ----- #

  if (type == "case-pump") {
    if (!is.null(origin)) {
      if (observed) {
        if (is.numeric(origin)) {
          if (origin %in% seq_len(ct)) {
            ego.sel <- cholera::anchor.case$case == origin
            ego.id <- cholera::anchor.case[ego.sel, "anchor"]
          } else {
            txt1 <- 'With type = "case-pump" and observed = '
            txt2 <- ', origin must be between 1 and '
            stop(txt1, observed, txt2, ct, ".")
          }
        }
      } else {
        if (origin %in% seq_len(ct)) {
          ego.id <- origin
        } else {
          txt1 <- 'With type = "case-pump" and observed = '
          txt2 <- ', origin must be between 1 and '
          stop(txt1, observed, txt2, ct, ".")
        }
      }

      if (is.character(origin)) {
        origin <- caseAndSpace(origin)
        if (origin %in% cholera::landmark.squares$name) {
          ego.sel <- grepl(origin, cholera::landmarks$name)
          ego.id <- cholera::landmarks[ego.sel, "case"]
        } else if (origin %in% cholera::landmarks$name) {
          ego.sel <- cholera::landmarks$name == origin
          ego.id <- cholera::landmarks[ego.sel, "case"]
        } else stop('Use a valid landmark name.')
      }

      ego.node <- nodes[nodes$anchor %in% ego.id, "node"]

      if (!is.null(destination)) {
        if (is.numeric(destination)) {
          if (any(abs(destination) %in% p.ID == FALSE)) {
            txt1 <- 'With type = "case-pump" and vestry = '
            txt2 <- ', 1 >= |destination| <= '
            stop(txt1, vestry, txt2, p.count, ".")
          } else {
            if (all(destination < 0)) {
              p.nodes <- nodes[nodes$pump != 0, ]
              alter.sel <- p.nodes$pump %in% abs(destination) == FALSE
              alters <- p.nodes[alter.sel, "node"]
            } else if (all(destination > 0)) {
              alters <- nodes[nodes$pump %in% destination, "node"]
            }
          }
        }
      } else alters <- nodes[nodes$pump != 0, "node"]

      if (length(ego.node) > 1) {
        c.square <- citySquare(ego.node, alters, g, nodes, edges, weighted,
          type)
        nr.pair <- which.min(c.square$distance)

        sel <- nodes$node == c.square[nr.pair, "origin"] & nodes$anchor != 0
        ego.node <- nodes[sel, "node"]

        alter.id <- c.square[nr.pair, "pump"]
        p.name <- p.data[p.data$id == alter.id, "street"]
        alter.node <- nodes[nodes$pump == alter.id, "node"]

      } else if (length(ego.id) == 1) {
        if (weighted) {
          d <- vapply(alters, function(x) {
            igraph::distances(g, ego.node, x, weights = edges$d)
          }, numeric(1L))
        } else {
          d <- vapply(alters, function(x) {
            igraph::distances(g, ego.node, x)
          }, numeric(1L))
        }

        if (all(is.infinite(d))) {
          sel <- which.min(d)
          alter.id <- NA
          p.name <- NA
          alter.node <- NA
        } else {
          sel <- which.min(d)
          node.sel <- nodes$node %in% names(sel) & nodes$pump != 0
          alter.id <- nodes[node.sel, "pump"]
          p.name <- p.data[p.data$id == alter.id, "street"]
        }

        alter.node <- names(sel)
      }

    } else {
      if (is.null(destination)) {
        stop("You must provide a destination!")
      } else if (length(destination) != 1) {
        stop("If origin is NULL, select only one destination pump.")
      } else if (destination < 0) {
        stop("If origin is NULL, no negative selection for destination.")
      } else {
        if (is.numeric(destination)) {
          if (any(abs(destination) %in% p.ID == FALSE)) {
            txt1 <- 'With type = "case-pump" and vestry = '
            txt2 <- ', 1 >= |destination| <= '
            stop(txt1, vestry, txt2, p.count, ".")
          } else {
            if (all(destination < 0)) {
              p.nodes <- nodes[nodes$pump != 0, ]
              alter.node <- p.nodes[p.nodes$pump %in% abs(destination) == FALSE,
                "node"]
            } else if (all(destination > 0)) {
              alter.node <- nodes[nodes$pump %in% destination, "node"]
            }
          }
        }

        egos <- nodes[nodes$anchor > 0, "node"]

        if (weighted) {
          d <- vapply(egos, function(x) {
            igraph::distances(g, x, alter.node, weights = edges$d)
          }, numeric(1L))
        } else {
          d <- vapply(egos, function(x) {
            igraph::distances(g, x, alter.node)
          }, numeric(1L))
        }

        sel <- which.min(d)
        node.sel <- nodes$node %in% names(sel) & nodes$anchor != 0
        ego.node <- nodes[node.sel, "node"]
      }
    }

    if (weighted) {
      path <- names(unlist(igraph::shortest_paths(g, ego.node, alter.node,
        weights = edges$d)$vpath))
    } else {
      path <- names(unlist(igraph::shortest_paths(g, ego.node,
        alter.node)$vpath))
    }

  if (is.null(origin)) {
    case <- nodes[nodes$node == ego.node & nodes$anchor > 0, "anchor"]
    if (case > 20000) {
      case.nm <- cholera::landmarks[cholera::landmarks$case == case, "name"]
      if (grepl("Square", case.nm)) {
        case.nm <- unlist(strsplit(case.nm, "-"))[1]
      }

      out <- list(path = rev(path),
                  data = data.frame(case = case.nm,
                                    anchor = case,
                                    pump.name = p.data[p.data$id ==
                                      destination, "street"],
                                    pump = destination,
                                    distance = d[sel],
                                    stringsAsFactors = FALSE,
                                    row.names = NULL))
    } else {
      out <- list(path = rev(path),
                  data = data.frame(case = case,
                                    anchor = case,
                                    pump.name = p.data[p.data$id ==
                                      destination, "street"],
                                    pump = destination,
                                    distance = d[sel],
                                    stringsAsFactors = FALSE,
                                    row.names = NULL))
    }
  } else {
    if (grepl("Square", origin)) {
      out <- list(path = path,
                  data = data.frame(case = origin,
                                    anchor = nodes[nodes$node == ego.node &
                                      nodes$anchor != 0, "anchor"],
                                    pump.name = p.name,
                                    pump = alter.id,
                                    distance = c.square[nr.pair, "distance"],
                                    stringsAsFactors = FALSE,
                                    row.names = NULL))
    } else {
      out <- list(path = path,
                  data = data.frame(case = origin,
                                    anchor = ego.id,
                                    pump.name = p.name,
                                    pump = alter.id,
                                    distance = d[sel],
                                    stringsAsFactors = FALSE,
                                    row.names = NULL))
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
      if (is.numeric(origin)) {
        if (origin %in% seq_len(ct) ) {
          ego.id <- origin
          ego.sel <- cholera::anchor.case$case == origin
          ego.anchor <- cholera::anchor.case[ego.sel, "anchor"]
        } else {
          txt1 <- 'With type = "cases" and observed = '
          txt2 <- ', origin must be between 1 and '
          stop(txt1, observed, ", ", txt2, ct, ".")
        }
      }
    } else {
      if (is.numeric(origin)) {
        if (origin %in% seq_len(ct)) {
          ego.id <- origin
          ego.anchor <- origin
        } else {
          txt1 <- 'With type = "cases" and observed = '
          txt2 <- ', origin must be between 1 and '
          stop(txt1, observed, ", ", txt2, ct, ".")
        }
      }
    }

    if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      if (origin %in% cholera::landmark.squares$name) {
        sq.test <- grepl(origin, cholera::landmarks$name)
        if (any(sq.test)) {
          ego.data <- cholera::landmarks[sq.test, ]
        } else stop('Use a valid landmark square name for origin.')
      } else if (origin %in% cholera::landmarks$name) {
        ego.data <- cholera::landmarks[cholera::landmarks$name == origin, ]
      } else stop('Use a valid landmark name for origin.')

      ego.id <- ego.data$name
      ego.anchor <- ego.data$case
    }

    ego.node <- nodes[nodes$anchor %in% ego.anchor, "node"]

    if (is.null(destination)) {
      node.sel <- nodes$anchor != 0 & nodes$node %in% ego.node == FALSE
      alters <- nodes[node.sel, "node"]

    } else if (is.numeric(destination)) {
      if (observed) {
        if (all(destination > 0)) {
          alter.sel <- cholera::anchor.case$case%in% destination
        } else if (all(destination < 0)) {
          alter.sel <- cholera::anchor.case$case %in% abs(destination) ==
            FALSE
        } else stop("all positive or all negative.")
        alter.case <- unique(cholera::anchor.case[alter.sel, "anchor"])
      } else {
        if (all(destination > 0)) {
          alter.case <- nodes$anchor[nodes$anchor %in% destination]
        } else if (all(destination < 0)) {
          alter.case <- nodes$anchor[nodes$anchor %in% destination == FALSE]
        } else stop("all positive or all negative.")
      }

      if (observed) {
        # same stack test
        if (all(is.numeric(c(ego.id, alter.case)))) {
          ego.anchor <- cholera::anchor.case[cholera::anchor.case$case %in%
            ego.id, "anchor"]
          alter.anchor <- cholera::anchor.case[cholera::anchor.case$case %in%
            alter.case, "anchor"]
          stack.test <- vapply(c(ego.anchor, alter.anchor), length, numeric(1L))

          if (all(stack.test== 1)) {
            if (ego.anchor == alter.anchor) {
              stop("origin and destination are at same address!")
            }
          }
        }
      }

      alters <- nodes$node[nodes$anchor %in% alter.case &
                           nodes$node %in% ego.node == FALSE]

    } else if (is.character(destination)) {
      destination <- caseAndSpace(destination)
      if (destination %in% cholera::landmark.squares$name) {
        sq.test <- grepl(destination, cholera::landmarks$name)
        if (any(sq.test)) {
          alter.case <- cholera::landmarks[sq.test, "case"]
        } else stop('Use a valid landmark square name for destination.')


      } else if (destination == "St James Workhouse") {
        st.james.node <- nodes[nodes$anchor == 369, "node"]
        if (ego.node == st.james.node) {
          stop("origin and destination are at same address!")
        } else {
          sel <- cholera::landmarks$name == destination
          alter.case <- cholera::landmarks[sel, "case"]
        }

      } else if (destination %in% cholera::landmarks$name) {
        sel <- cholera::landmarks$name == destination
        alter.case <- cholera::landmarks[sel, "case"]

      } else stop('Use a valid landmark name for destination.')

      # post caseAndSpace()
      if (is.character(origin) & is.character(destination)) {
        if (origin == destination) {
          stop("origin and destination are at same address!")
        }
      }

      alters <- nodes$node[nodes$anchor %in% alter.case &
                           nodes$node %in% ego.node == FALSE]
    }

    if (length(ego.node) > 1) {
      c.square <- citySquare(ego.node, alters, g, nodes, edges, weighted, type)
      nr.pair <- which.min(c.square$distance)

      sel <- nodes$node == c.square[nr.pair, "origin"] & nodes$anchor != 0
      ego.node <- nodes[sel, "node"]

      alter.anchor <- c.square[nr.pair, "destination"]
      alter.node <- nodes[nodes$anchor == alter.anchor, "node"]

      if (is.character(destination)) {
        alter.id <- cholera::landmarks[cholera::landmarks$case %in%
          alter.anchor, "name"]
      } else {
        alter.id <- alter.anchor
      }

      if (weighted) {
        d <- igraph::distances(g, ego.node, alter.node, weights = edges$d)
      } else {
        d <- igraph::distances(g, ego.node, alter.node)
      }

      if (length(d) == 1) {
        alter.node <- colnames(d)
      } else {
        alter.node <- names(which.min(d))
      }

    } else if (length(ego.id) == 1) {
      if (weighted) {
        d <- vapply(alters, function(x) {
          igraph::distances(g, ego.node, x, weights = edges$d)
        }, numeric(1L))
      } else {
        d <- vapply(alters, function(x) {
          igraph::distances(g, ego.node, x)
        }, numeric(1L))
      }

      if (all(is.infinite(d))) {
        alter.node <- NA
        alter.id <- NA
        alter.anchor <- NA
      } else {
        alter.node <- names(which.min(d))
        node.sel <- nodes$node %in% alter.node & nodes$anchor != 0
        alter.id <- nodes[node.sel, "anchor"]
        alter.anchor <- alter.id
      }
    }

    if (weighted) {
      pth <- igraph::shortest_paths(g, ego.node, alter.node, weights = edges$d)
    } else {
      pth <- igraph::shortest_paths(g, ego.node, alter.node)
    }

    if (is.character(origin)) {
      if (grepl("Square", origin)) {
        sel <- nodes$node %in% c.square[nr.pair, "origin"] & nodes$anchor != 0
        sq.origin.id <- nodes[sel, "anchor"]
        sq.sel <- cholera::landmarks$case == sq.origin.id
        ego.data <- cholera::landmarks[sq.sel, ]
      } else {
        origin.sel <- cholera::landmarks$name == origin
        ego.data <- cholera::landmarks[origin.sel, ]
      }
      ego.id <- ego.data$name
      ego.anchor <- ego.data$case
    }

    if (is.character(destination)) {
      landmark.sel <- cholera::landmarks$case %in% alter.anchor
      landmark.data <- cholera::landmarks[landmark.sel, ]
      alter.id <- landmark.data$name
      alter.anchor <- landmark.data$case
    }

    out <- list(path = names(unlist(pth$vpath)),
                data = data.frame(caseA = ego.id,
                                  caseB = alter.id,
                                  anchorA = ego.anchor,
                                  anchorB = alter.anchor,
                                  distance = d[which.min(d)],
                                  stringsAsFactors = FALSE,
                                  row.names = NULL))

    if (rev.flag) {
      tmp.case <- out$data$caseA
      tmp.anchor <- out$data$anchorA
      out$path <- rev(out$path)
      out$data$caseA <- out$data$caseB
      out$data$anchorA <- out$data$anchorB
      out$data$caseB <- tmp.case
      out$data$anchorB <- tmp.anchor
      tmp <- origin
      origin <- destination
      destination <- tmp
      tmp <- ego.node
      ego.node <- alter.node
      alter.node <- tmp
    }

  # ----- #

  } else if (type == "pumps") {
    rev.flag <- is.null(origin) & is.null(destination) == FALSE

    if (rev.flag) {
      tmp <- origin
      origin <- destination
      destination <- tmp
    }

    ego.node <- nodes[nodes$pump == origin, "node"]
    p.nodes <- nodes[nodes$pump > 0, ]

    if (origin %in% p.ID == FALSE) {
      txt1 <- 'With type = "pumps" and vestry = '
      txt2 <- ', origin must be whole numbers 1 <= x <= '
      stop(txt1, vestry, txt2, p.count, ".")
    }

    if (is.null(destination)) {
      alters <- p.nodes[p.nodes$pump != origin, "node"]
    } else {
      if (all(destination > 0)) {
        alters <- p.nodes[p.nodes$pump %in% destination &
                          p.nodes$pump != origin, "node"]
      } else if (all(destination < 0)) {
        alters <- p.nodes[p.nodes$pump %in% abs(destination) == FALSE &
                          p.nodes$pump != origin, "node"]
      } else if (any(abs(destination) %in% p.ID == FALSE)) {
        txt1 <- 'With type = "pumps" and vestry = '
        txt2 <- ', destination must be whole numbers 1 <= |x| <= '
        stop(txt1, vestry, txt2, p.count, ".")
      }
    }

    if (weighted) {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x, weights = edges$d)
      }, numeric(1L))
    } else {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x)
      }, numeric(1L))
    }

    A <- p.nodes[p.nodes$node == ego.node, "pump"]
    ego.node <- p.nodes[p.nodes$node == ego.node, "node"]

    if (all(is.infinite(d))) {
      B <- NA
      alter.node <- NA
    } else {
      sel <- which.min(d)
      B <- p.nodes[p.nodes$node == names(sel), "pump"]
      alter.node <- p.nodes[p.nodes$node == names(sel), "node"]
    }

    if (weighted) {
      path <- names(unlist(igraph::shortest_paths(g, ego.node, alter.node,
        weights = edges$d)$vpath))
    } else {
      path <- names(unlist(igraph::shortest_paths(g, ego.node,
        alter.node)$vpath))
    }

    out <- list(path = path,
                data = data.frame(pumpA = A,
                                  nameA = p.data[p.data$id == A, "street"],
                                  pumpB = B,
                                  nameB = p.data[p.data$id == B, "street"],
                                  distance = d[sel],
                                  stringsAsFactors = FALSE,
                                  row.names = NULL))

    if (rev.flag) {
      tmp.a <- out$data$pumpA
      tmp.b <- out$data$nameA
      out$path <- rev(out$path)
      out$data$caseA <- out$data$pumpB
      out$data$anchorA <- out$data$nameB
      out$data$caseB <- tmp.a
      out$data$anchorB <- tmp.b
      tmp <- origin
      origin <- destination
      destination <- tmp
      tmp <- ego.node
      ego.node <- alter.node
      alter.node <- tmp
    }
  }

  # ----- #

  if (distance.unit == "meter") {
    out$data$distance <- unitMeter(out$data$distance, "meter")
  } else if (distance.unit == "yard") {
    out$data$distance <- unitMeter(out$data$distance, "yard")
  } else if (distance.unit == "native") {
    out$data$distance <- unitMeter(out$data$distance, "native")
  }

  out$data$time <- distanceTime(out$data$distance,
    distance.unit = distance.unit, time.unit = time.unit,
    walking.speed = walking.speed)

  output <- list(path = out$path,
                 data = out$data,
                 origin = origin,
                 destination = destination,
                 type = type,
                 observed = observed,
                 weighted = weighted,
                 vestry = vestry,
                 distance.unit = distance.unit,
                 time.unit = time.unit,
                 nodes = nodes,
                 edges = edges,
                 g = g,
                 ego.node = ego.node,
                 alter.node = alter.node,
                 walking.speed = walking.speed)

  class(output) <- "walking_path"
  output
}

#' Plot the walking path between selected cases and/or pumps.
#'
#' @param x An object of class "walking_path" created by walkingPath().
#' @param zoom Logical or Numeric. A numeric value >= 0 controls the degree of zoom. The default is 0.5.
#' @param unit.posts Character. "distance" for mileposts; "time" for timeposts; NULL for no posts.
#' @param unit.interval Numeric. Set interval between posts. When \code{unit.posts = "distance"}, \code{unit.interval} defaults to 50 meters. When \code{unit.posts = "time"}, \code{unit.interval} defaults to 60 seconds.
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @section Note: Arrows represent mileposts or timeposts to the destination.
#' @examples
#' \donttest{
#' plot(walkingPath(15))
#' plot(walkingPath(15), unit.posts = "time")
#' }

plot.walking_path <- function(x, zoom = 0.5, unit.posts = "distance",
  unit.interval = NULL, alpha.level = 1, ...) {

  if (class(x) != "walking_path") {
    stop('x\'s class must be "walking_path".')
  }

  if (is.na(x$alter.node)) {
    txt1 <- paste("Case", x$origin, "is part of an isolated subgraph.")
    txt2 <- "It (technically) has no nearest pump."
    stop(txt1, txt2)
  }

  if ((alpha.level > 0 & alpha.level <= 1) == FALSE) {
    stop('alpha.level must be > 0 and <= 1.')
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  colors <- snowColors(x$vestry)

  nodes <- x$nodes
  edges <- x$edges
  g <- x$g
  ego.node <- x$ego.node
  alter.node <- x$alter.node
  node.filter <- nodes$anchor + nodes$pump > 0
  ego.data <- nodes[nodes$node == ego.node & node.filter, ]
  alter.data <- nodes[nodes$node == alter.node & node.filter, ]
  dat <- numericNodeCoordinates(x$path)

  # St James Workhouse fix
  if (x$type == "case-pump") {
    if (nrow(ego.data) > 1) {
      ego.data <- ego.data[ego.data$anchor > 20000, ]
    }
  } else if (x$type == "cases") {
    if (nrow(ego.data) > 1) {
      if (is.numeric(x$origin)) {
        ego.data <- ego.data[ego.data$anchor < 20000, ]
      } else if (is.character(x$origin)) {
        ego.data <- ego.data[ego.data$anchor > 20000, ]
      }
    }

    if (nrow(alter.data) > 1) {
      if (is.numeric(x$destination)) {
        alter.data <- alter.data[alter.data$anchor < 20000, ]
      } else if (is.character(x$destination)) {
        alter.data <- alter.data[alter.data$anchor > 20000, ]
      }
    }
  }

  if (x$observed) {
    case.data <- cholera::fatalities
  } else {
    case.data <- cholera::regular.cases
  }

  if (x$type %in% c("case-pump", "pumps")) {
    if (x$vestry) {
      p.data <- cholera::pumps.vestry
    } else {
      p.data <- cholera::pumps
    }
  }

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
  }

  ## case/landmark as ego ##

  if (x$type %in% c("case-pump", "cases")) {
    ego.anchor <- ego.data$anchor

    if (is.numeric(unlist(x$data[1]))) {
      if (x$observed) {
        sel <- cholera::fatalities$case == ego.anchor
        origin.obs <- cholera::fatalities[sel, c("x", "y")]
      } else {
        origin.obs <- cholera::regular.cases[ego.anchor, ]
      }
    } else if (is.character(unlist(x$data[1]))) {
      sel <- cholera::landmarks$case == ego.anchor
      origin.obs <- cholera::landmarks[sel, c("x.proj", "y.proj")]
      names(origin.obs) <- c("x", "y")
    } else {
      stop("case/landmark as ego error.")
    }
  }

  ## case/landmark as alter ##

  if (x$type == "cases") {
    alter.anchor <- alter.data$anchor

    if (is.numeric(unlist(x$data[2]))) {
      if (x$observed) {
        sel <- cholera::fatalities$case == alter.anchor
        destination.obs <- cholera::fatalities[sel, c("x", "y")]
      } else {
        destination.obs <- cholera::regular.cases[alter.anchor, ]
      }
    } else if (is.character(unlist(x$data[2]))) {
      sel <- cholera::landmarks$case %in% alter.anchor
      destination.obs <- cholera::landmarks[sel, c("x.proj", "y.proj")]
      names(destination.obs) <- c("x", "y")
    } else {
      stop("case/landmark as alter error.")
    }
  }

  ## pump as ego ##

  if (x$type == "pumps") {
    origin.obs <- p.data[p.data == ego.data$pump, c("x", "y")]
  }

  ## pump as alter ##

  if (x$type %in% c("case-pump", "pumps")) {
    alter.p <- alter.data$pump
    destination.obs <- p.data[p.data == alter.p, c("x", "y")]

    if (alpha.level != 1) {
      case.color <- grDevices::adjustcolor(colors[alter.p],
        alpha.f = alpha.level)
    } else {
      case.color <- colors[alter.p]
    }
  } else {
    case.color <- "blue"
  }

  dat.plus.test <- signif(origin.obs$x) %in% signif(dat$x) &
                   signif(origin.obs$y) %in% signif(dat$y)

  if (dat.plus.test) {
    dat.plus.origin <- dat
  } else {
    dat.plus.origin <- rbind(dat, origin.obs)
  }

  sq.case <- cholera::landmarks[grepl("Square", cholera::landmarks$name),
    "case"]
  sq.ego <- ego.data$anchor %in% sq.case
  sq.alter <- alter.data$anchor %in% sq.case

  if (sq.ego & !sq.alter) {
    dat.plus.origin <- rbind(dat.plus.origin, sq.center.origin)
  } else if (!sq.ego & sq.alter) {
    dat.plus.origin <- rbind(dat.plus.origin, sq.center.destination)
  } else if (sq.ego & sq.alter) {
    dat.plus.origin <- rbind(dat.plus.origin, sq.center.origin,
      sq.center.destination)
  }

  if (is.logical(zoom)) {
    if (zoom) {
      padding <- 0.1
      x.rng <- c(min(dat.plus.origin$x) - padding,
                 max(dat.plus.origin$x) + padding)
      y.rng <- c(min(dat.plus.origin$y) - padding,
                 max(dat.plus.origin$y) + padding)
    } else {
      x.rng <- range(cholera::roads$x)
      y.rng <- range(cholera::roads$y)
    }
  } else if (is.numeric(zoom)) {
    if (zoom >= 0) {
      x.rng <- c(min(dat.plus.origin$x) - zoom, max(dat.plus.origin$x) + zoom)
      y.rng <- c(min(dat.plus.origin$y) - zoom, max(dat.plus.origin$y) + zoom)
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

  if (x$type == "case-pump" | x$type == "cases") {
    points(origin.obs, col = "red")
    if (is.numeric(x$data$case)) {
      text(origin.obs, col = "red", pos = 1, labels = ego.data$anchor)
    }
  }

  if (x$type == "case-pump") {
    if (is.numeric(x$data$case)) {
      title(main = paste("Case", x$data$case, "to Pump", x$data$pump))
    } else if (is.character(x$data$case)) {
      nm <- cholera::landmarks[cholera::landmarks$case == x$data$anchor, "name"]
      title(main = paste(nm, "to Pump", x$data$pump))
    }
  } else if (x$type == "cases") {
    points(destination.obs, col = "red")
    if (is.numeric(x$data$caseA) & is.numeric(x$data$caseB)) {
      title(main = paste("Case", x$data$caseA, "to Case", x$data$caseB))
    } else if (is.character(x$data$caseA) & is.numeric(x$data$caseB)) {
      title(main = paste(x$data$caseA, "to Case", x$data$caseB))
    } else if (is.numeric(x$data$caseA) & is.character(x$data$caseB)) {
      title(main = paste("Case", x$data$caseA, "to", x$data$caseB))
    } else if (is.character(x$data$caseA) & is.character(x$data$caseB)) {
      title(main = paste(x$data$caseA, "to", x$data$caseB))
    }
  } else if (x$type == "pumps") {
    title(main = paste("Pump", x$data$pumpA, "to Pump", x$data$pumpB))
  }

  points(dat[1, c("x", "y")], col = case.color, pch = 0)
  points(dat[nrow(dat), c("x", "y")], col = case.color, pch = 0)

  if ((is.logical(zoom) & zoom == TRUE) | is.numeric(zoom)) {
    if (x$type %in% c("case-pump", "cases")) {
      if (is.numeric(ego)) {
        if (x$observed) sel <- case.data$case == ego.data$anchor
        else sel <- row.names(case.data) == ego.data$anchor
        text(case.data[sel, c("x", "y")], labels = ego.data$anchor, pos = 1,
          col = "red")
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
    }

    if (x$type == "cases") {
      if (is.numeric(alter)) {
        if (x$observed) sel <- case.data$case == alter.data$anchor
        else sel <- row.names(case.data) == alter.data$anchor
        text(case.data[sel, c("x", "y")],
          labels = alter.data$anchor, pos = 1, col = "red")
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
    }
  }

  drawPath(dat, case.color, compute.coords = FALSE)

  if (x$time.unit == "hour") {
    nominal.time <- paste(round(x$data$time, 1), "hr")
  } else if (x$time.unit == "minute") {
    nominal.time <- paste(round(x$data$time, 1), "min")
  } else if (x$time.unit == "second") {
    nominal.time <- paste(round(x$data$time), "sec")
  }

  if (x$distance.unit == "native") {
    d.unit <- "units;"
  } else if (x$distance.unit == "meter") {
    d.unit <- "m;"
  } else if (x$distance.unit == "yard") {
    d.unit <- "yd;"
  }

  # mileposts #

  if (is.null(unit.posts) == FALSE) {
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

      path <- rev(x$path)
      path.edge <- data.frame(node1 = path[1:(length(path) - 1)],
                              node2 = path[2:length(path)],
                              stringsAsFactors = FALSE)

      edge.data <- identifyEdges(path.edge, edges)

      if (unit.posts == "distance") {
        cumulative <- unitMeter(cumsum(edge.data$d))
      } else if (unit.posts == "time") {
        cumulative <- distanceTime(unitMeter(cumsum(edge.data$d)),
          walking.speed = x$walking.speed)
      }

      total <- cumulative[length(cumulative)]
      posts <- seq(0, total, unit.interval)

      if (max(posts) > total) posts <- posts[-length(posts)]

      bins <- data.frame(lo = c(0, cumulative[-length(cumulative)]),
                         hi = cumulative)

      edge.select <- vapply(posts, function(x) {
        which(vapply(seq_len(nrow(bins)), function(i) {
          x >= bins[i, "lo"] & x < bins[i, "hi"]
        }, logical(1L)))
      }, integer(1L))

      post.nodes <- path.edge[edge.select, ]

      post.coordinates <- lapply(seq_along(edge.select), function(i) {
        node1.node2 <-
        edge.data[edge.select[i], "node1"] == post.nodes[i, "node1"] &
        edge.data[edge.select[i], "node2"] == post.nodes[i, "node2"]

        node2.node1 <-
        edge.data[edge.select[i], "node1"] == post.nodes[i, "node2"] &
        edge.data[edge.select[i], "node2"] == post.nodes[i, "node1"]

        sel.data <- edge.data[edge.select[i], ]

        if (any(node1.node2)) {
          e.data <- data.frame(x = c(sel.data$x1, sel.data$x2),
                               y = c(sel.data$y1, sel.data$y2))
        } else if (any(node2.node1)) {
          e.data <- data.frame(x = c(sel.data$x2, sel.data$x1),
                               y = c(sel.data$y2, sel.data$y1))
        } else stop("Post error.")

        ols <- stats::lm(y ~ x, data = e.data)
        edge.slope <- stats::coef(ols)[2]
        edge.intercept <- stats::coef(ols)[1]
        theta <- atan(edge.slope)

        if (unit.posts == "distance") {
          h <- (posts[i] - bins[edge.select[i], "lo"]) / unitMeter(1)
        } else if (unit.posts == "time") {
          h <- (posts[i] - bins[edge.select[i], "lo"]) * 1000 *
            x$walking.speed / 60^2 / unitMeter(1)
        }

        p.coords <- quandrantCoordinates(e.data, h, theta)

        data.frame(post = i,
                   x = p.coords$x,
                   y = p.coords$y,
                   angle = theta * 180L / pi,
                   start = ifelse(node1.node2, 1, ifelse(node2.node1, 2, 0)),
                   row.names = NULL)
      })

      coords <- do.call(rbind, post.coordinates)
      arrow.data <- edge.data[edge.select, ]
      start <- coords$start

      invisible(lapply(seq_len(nrow(arrow.data)), function(i) {
        if (start[i] == 1) {
          dataB <- data.frame(x = c(arrow.data[i, "x2"], coords[i, "x"]),
                              y = c(arrow.data[i, "y2"], coords[i, "y"]))
        } else if (start[i] == 2) {
          dataB <- data.frame(x = c(arrow.data[i, "x1"], coords[i, "x"]),
                              y = c(arrow.data[i, "y1"], coords[i, "y"]))
        }

        zero.length.x <- round(abs(dataB[1, "x"] - dataB[2, "x"]), 2) == 0
        zero.length.y <- round(abs(dataB[1, "y"] - dataB[2, "y"]), 2) == 0

        if (any(zero.length.x | zero.length.y)) {
          if (start[i] == 1) {
            text(dataB[1, c("x", "y")], labels = "<", srt = coords[i, "angle"],
              col = case.color, cex = 1.25)
          } else if (start[i] == 2) {
            text(dataB[1, c("x", "y")], labels = ">", srt = coords[i, "angle"],
              col = case.color, cex = 1.25)
          } else stop("Draw error.")
        } else {
          arrows(dataB[1, "x"], dataB[1, "y"],
                 dataB[2, "x"], dataB[2, "y"],
                 lwd = 3, length = 0.075, col = case.color, code = 2)
        }
      }))
    }

    if (unit.posts == "distance") {
      post.info <- paste("posts @", unit.interval, "m intervals")
    } else if (unit.posts == "time") {
      post.info <- paste("posts @", unit.interval, "sec intervals")
    }

    title(sub = paste(round(x$data$distance, 1), d.unit, nominal.time, "@",
      x$walking.speed, "km/hr;", post.info))
  } else {
    title(sub = paste(round(x$data$distance, 1), d.unit, nominal.time, "@",
      x$walking.speed, "km/hr"))
  }
}

#' Print method for walkingPath().
#'
#' Summary output.
#' @param x An object of class "walking_path" created by walkingPath().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export
#' @examples
#' \donttest{
#' walkingPath()
#' print(walkingPath())
#' }

print.walking_path <- function(x, ...) {
  if (class(x) != "walking_path") {
    stop('"x"\'s class must be "walking_path".')
  }

  print(x[c("path", "data")])
}

citySquare <- function(ego.node, alters, g, nodes, edges, weighted, type) {
  if (type == "case-pump") {
    if (weighted) {
      c.square <- lapply(ego.node, function(e) {
        d <- vapply(alters, function(x) {
          igraph::distances(g, e, x, weights = edges$d)
        }, numeric(1L))
        out <- data.frame(
          origin = e,
          pump = nodes[nodes$node == names(which.min(d)), "pump"],
          distance = d[which.min(d)],
          stringsAsFactors = FALSE)
        row.names(out) <- NULL
        out
      })
    } else {
      c.square <- lapply(ego.node, function(e) {
        d <- vapply(alters, function(x) {
          igraph::distances(g, e, x)
        }, numeric(1L))
        out <- data.frame(
          origin = e,
          pump = nodes[nodes$node == names(which.min(d)), "pump"],
          distance = d[which.min(d)],
          stringsAsFactors = FALSE)
        row.names(out) <- NULL
        out
      })
    }
  } else if (type == "cases") {
    if (weighted) {
      c.square <- lapply(ego.node, function(e) {
        d <- vapply(alters, function(x) {
          igraph::distances(g, e, x, weights = edges$d)
        }, numeric(1L))
        sel <- nodes$node == names(which.min(d)) & nodes$anchor != 0
        out <- data.frame(origin = e,
                          destination = nodes[sel, "anchor"],
                          distance = d[which.min(d)],
                          stringsAsFactors = FALSE)
        row.names(out) <- NULL
        out
      })
    } else {
      c.square <- lapply(ego.node, function(e) {
        d <- vapply(alters, function(x) {
          igraph::distances(g, e, x)
        }, numeric(1L))
        sel <- nodes$node == names(which.min(d)) & nodes$anchor != 0
        out <- data.frame(origin = e,
                          destination = nodes[sel, "anchor"],
                          distance = d[which.min(d)],
                          stringsAsFactors = FALSE)
        row.names(out) <- NULL
        out
      })
    }
  }
  do.call(rbind, c.square)
}
