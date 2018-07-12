#' Compute the shortest walking path between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is NULL: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of nodes.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. "unit" is meaningful only when "weighted" is TRUE. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Default walking speed is 5 km/hr.
#' @note The function uses a case's "address" (i.e., a stack's "anchor" case) to compute distance. Time is computed using distanceTime(). Adam and Eve Court, and Falconberg Court and Falconberg Mews, are disconnected from the larger road network; they form two isolated subgraphs. This has two consequences: first, only cases on Adam and Eve Court can reach pump 2 and those cases cannot reach any other pump; second, cases on Falconberg Court and Mews cannot reach any pump. Unreachable pumps will return distances of "Inf".
#' @return An R list with two elements: a character vector of path nodes and a data frame summary.
#' @seealso \code{\link{fatalities}}, \code{vignette("pump.neighborhoods")}
#' @export
#' @examples
#' \dontrun{
#' # path from case 1 to nearest pump.
#' walkingPath(1)
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
#' # path from case 1 to nearest pump.
#' plot(walkingPath(1))
#' }

walkingPath <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, weighted = TRUE, vestry = FALSE, unit = "meter",
  time.unit = "second", walking.speed = 5) {

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('"unit" must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('"time.unit" must be "hour", "minute" or "second".')
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('"type" must be "case-pump", "cases" or "pumps".')
  }

  if (observed) {
    node.data <- cholera::neighborhoodData(vestry)
  } else {
    node.data <- cholera::neighborhoodData(vestry, case.set = "expected")
  }

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g
  n.sim.obs <- nrow(cholera::regular.cases)

  if (type == "case-pump") {
    if (observed) {
      if (origin %in% 1:578 == FALSE) {
        txt1 <- 'With type = "case-pump" and "observed" = TRUE,'
        txt2 <- '"origin" must be between 1 and 578.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (origin %in% 1:n.sim.obs == FALSE) {
        txt1 <- 'With type = "case-pump" and "observed" = FALSE,'
        txt2 <- paste('"origin" must be between 1 and', paste0(n.sim.obs, "."))
        stop(paste(txt1, txt2))
      }
    }

    if (!is.null(destination)) {
      if (vestry) {
        if (any(abs(destination) %in% 1:14 == FALSE)) {
          txt1 <- 'With type = "case-pump" and "vestry = TRUE",'
          txt2 <- '1 >= |destination| <= 14.'
          stop(paste(txt1, txt2))
        } else {
          pumps <- cholera::pumps.vestry[destination, ]
        }
      } else {
        if (any(abs(destination) %in% 1:13 == FALSE)) {
          txt1 <- 'With type = "case-pump" and "vestry = FALSE",'
          txt2 <- '1 >= |destination| <= 13.'
          stop(paste(txt1, txt2))
        } else {
          pumps <- cholera::pumps[destination, ]
        }
      }
    } else {
      if (vestry) {
        pumps <- cholera::pumps.vestry
      } else {
        pumps <- cholera::pumps
      }
    }

    if (observed) {
      ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
        "anchor.case"]
      ego.node <- nodes[nodes$anchor == ego.id, "node"]
    } else {
      ego.id <- origin
      ego.node <- nodes[nodes$anchor == ego.id, "node"]
    }

    if (!is.null(destination)) {
      if (all(destination < 0)) {
        p.nodes <- nodes[nodes$pump != 0, ]
        alters <- p.nodes[p.nodes$pump %in% abs(destination) == FALSE, "node"]
      } else {
        alters <- nodes[nodes$pump %in% destination, "node"]
      }
    } else {
      alters <- nodes[nodes$pump != 0, "node"]
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

    if (all(is.infinite(d))) {
      sel <- which.min(d)
      alter.id <- NA
      p.name <- NA
      alter.node <- NA
    } else {
      sel <- which.min(d)
      alter.id <- nodes[nodes$node %in% names(sel), "pump"]
      p.name <- pumps[pumps$id == alter.id, "street"]
      alter.node <- names(sel)
    }

    if (weighted) {
      path <- names(unlist(igraph::shortest_paths(g, ego.node, alter.node,
                                                  weights = edges$d)$vpath))
    } else {
      path <- names(unlist(igraph::shortest_paths(g, ego.node,
                                                  alter.node)$vpath))
    }

    out <- list(path = path,
                data = data.frame(case = origin,
                                  anchor = ego.id,
                                  pump = alter.id,
                                  pump.name = p.name,
                                  distance = d[sel],
                                  stringsAsFactors = FALSE,
                                  row.names = NULL))

  } else if (type == "cases") {
    if (observed) {
      if (any(abs(c(origin, destination)) %in% 1:578 == FALSE)) {
        txt1 <- 'With type = "cases", the absolute value of both "origin"'
        txt2 <- 'and "destination" must be a whole number between 1 and 578.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (any(abs(c(origin, destination)) %in% 1:n.sim.obs == FALSE)) {
        txt1 <- 'With type = "case-pump" and "observed" = FALSE,'
        txt2 <- 'both "origin" and "destination" must be whole numbers between'
        txt3 <- paste('1 and', paste0(n.sim.obs, "."))
        stop(paste(txt1, txt2, txt3))
      }
    }

    if (observed) {
      ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
        "anchor.case"]
      ego.node <- nodes[nodes$anchor == ego.id, "node"]
    } else {
      ego.id <- origin
      ego.node <- nodes[nodes$anchor == ego.id, "node"]
    }

    if (is.null(destination)) {
      alters <- nodes[nodes$anchor != 0 & nodes$node != ego.node, "node"]
    } else {
      if (observed) {
        if (all(destination > 0)) {
          alter.case <- unique(cholera::anchor.case[cholera::anchor.case$case
            %in% destination, "anchor.case"])
        } else if (all(destination < 0)) {
          alter.case <- unique(cholera::anchor.case[cholera::anchor.case$case
            %in% abs(destination) == FALSE, "anchor.case"])
        }
      } else {
        if (all(destination > 0)) {
          alter.case <- nodes$anchor[nodes$anchor %in% destination]
        } else if (all(destination < 0)) {
          alter.case <- nodes$anchor[nodes$anchor %in% destination == FALSE]
        }
      }

      alters <- nodes$node[nodes$anchor %in% alter.case &
                           nodes$node != ego.node]
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

    if (all(is.infinite(d))) {
      alter.id <- NA
      alter.node <- NA
    } else {
      sel <- which.min(d)
      alter.id <- nodes[nodes$node %in% names(sel), "anchor"]
      alter.node <- names(sel)
    }

    if (weighted) {
      path <- names(unlist(igraph::shortest_paths(g, ego.node, alter.node,
                                                  weights = edges$d)$vpath))
    } else {
      path <- names(unlist(igraph::shortest_paths(g, ego.node,
                                                  alter.node)$vpath))
    }

    out <- list(path = path,
                data = data.frame(caseA = origin,
                                  caseB = alter.id,
                                  anchorA = ego.id,
                                  anchorB = alter.id,
                                  distance = d[which.min(d)],
                                  stringsAsFactors = FALSE,
                                  row.names = NULL))

    if (is.null(destination) | all(destination < 0)) {
      out
    } else if (all(destination > 0)) {
      if (length(destination) == 1) {
        out$data$caseB <- destination
      } else if (length(destination) > 1) {
        out$data$caseB <- destination[sel]
      }
      out
    }

  } else if (type == "pumps") {
    if (vestry) {
      pumps <- cholera::pumps.vestry

      if (any(abs(c(origin, destination)) %in% 1:14 == FALSE)) {
        txt1 <- 'With type = "pumps" and "vestry = TRUE",'
        txt2 <- 'origin and destination must whole numbers 1 >= |x| <= 14.'
        stop(paste(txt1, txt2))
      }
    } else {
      pumps <- cholera::pumps

      if (any(abs(c(origin, destination)) %in% 1:13 == FALSE)) {
        txt1 <- 'With type = "pumps" and "vestry = FALSE",'
        txt2 <- 'origin and destination must be whole numbers 1 >= |x| <= 13.'
        stop(paste(txt1, txt2))
      }
    }

    ego.node <- nodes[nodes$pump == origin, "node"]
    p.nodes <- nodes[nodes$pump > 0, ]

    if (is.null(destination)) {
      alters  <- p.nodes[p.nodes$pump != origin, "node"]
    } else {
      if (all(destination > 0)) {
        alters  <- p.nodes[p.nodes$pump %in% destination &
                           p.nodes$pump != origin, "node"]
      } else if (all(destination < 0)) {
        alters  <- p.nodes[p.nodes$pump %in% abs(destination) == FALSE &
                           p.nodes$pump != origin, "node"]
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
                                  nameA = pumps[pumps$id == A, "street"],
                                  pumpB = B,
                                  nameB = pumps[pumps$id == B, "street"],
                                  distance = d[sel],
                                  stringsAsFactors = FALSE,
                                  row.names = NULL))
  }

  out$data$time <- cholera::distanceTime(out$data$distance, unit = time.unit,
    speed = walking.speed)

  if (unit == "meter") {
    out$data$distance <- cholera::unitMeter(out$data$distance, "meter")
  } else if (unit == "yard") {
    out$data$distance <- cholera::unitMeter(out$data$distance, "yard")
  } else if (unit == "native") {
    out$data$distance <- cholera::unitMeter(out$data$distance, "native")
  }

  output <- list(path = out$path,
                 data = out$data,
                 origin = origin,
                 destination = destination,
                 type = type,
                 observed = observed,
                 weighted = weighted,
                 vestry = vestry,
                 unit = unit,
                 time.unit = time.unit,
                 nodes = nodes,
                 edges = edges,
                 g = g,
                 ego.node = ego.node,
                 alter.node = alter.node,
                 speed = walking.speed)

  class(output) <- "walking_path"
  output
}

#' Print summary output for walkingPath().
#'
#' @param x An object of class "walking_path" created by walkingPath().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export
#' @examples
#' walkingPath(1)
#' print(walkingPath(1))

print.walking_path <- function(x, ...) {
  if (class(x) != "walking_path") {
    stop('"x"\'s class needs to be "walking_path".')
  }

  print(x[c("path", "data")])
}

#' Plot the walking path between selected cases and/or pumps.
#'
#' @param x An object of class "walking_path" created by walkingPath().
#' @param zoom Logical.
#' @param radius Numeric. Control the degree of zoom.
#' @param unit.posts Character. "distance" for mileposts; "time" for timeposts; NULL for no posts.
#' @param unit.interval Numeric. Set interval between posts. When "unit.posts" is "distance", "unit.interval" automatically defaults to 50 meters. When "unit.posts" is "time", "unit.interval" automatically defaults to 60 seconds.
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @section Note: Arrow represent mileposts or timeposts to the destination.
#' @examples
#' plot(walkingPath(15))
#' plot(walkingPath(15), unit.posts = "time")

plot.walking_path <- function(x, zoom = TRUE, radius = 0.5,
  unit.posts = "distance", unit.interval = NULL, alpha.level = 1, ...) {

  if (class(x) != "walking_path") {
    stop('"x"\'s class needs to be "walking_path".')
  }

  if (is.na(x$alter.node)) {
    txt1 <- paste("Case", x$origin, "is part of an isolated subgraph.")
    txt2 <- "It (technically) has no neareast pump."
    stop(paste(txt1, txt2))
  }

  if (isFALSE(alpha.level > 0 | alpha.level <= 1)) {
    stop('"alpha.level" must be > 0 and <= 1')
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  colors <- cholera::snowColors(x$vestry)

  nodes <- x$nodes
  edges <- x$edges
  g <- x$g
  ego.node <- x$ego.node
  alter.node <- x$alter.node

  dat <- numericNodeCoordinates(x$path)

  if (x$type == "case-pump") {
    alter <- nodes[nodes$node == alter.node, "pump"]

    if (alpha.level != 1) {
      case.color <- grDevices::adjustcolor(colors[alter], alpha.f = alpha.level)
    } else {
      case.color <- colors[alter]
    }

    if (x$observed) {
      origin.obs <- cholera::fatalities[cholera::fatalities$case == x$origin,
        c("x", "y")]
    } else {
      origin.obs <- cholera::regular.cases[x$origin, ]
    }

  } else if (x$type == "cases") {
    alter <- nodes[nodes$node == alter.node, "anchor"]
    case.color <- "blue"

    if (x$observed) {
      origin.obs <- cholera::fatalities[cholera::fatalities$case == x$origin,
        c("x", "y")]
    } else {
      origin.obs <- cholera::regular.cases[x$origin, ]
    }

    if (is.null(x$destination)) {
      if (x$observed) {
        destination.obs <- cholera::fatalities[cholera::fatalities$case ==
          alter, c("x", "y")]
      } else {
        destination.obs <-  cholera::regular.cases[alter, ]
      }
    } else {
      id <- x$destination[x$sel]

      if (x$observed) {
        destination.obs <- cholera::fatalities[cholera::fatalities$case == id,
          c("x", "y")]
      } else {
        destination.obs <- cholera::regular.cases[id, ]
      }
    }

  } else if (x$type == "pumps") {
    if (x$vestry) {
      origin.obs <- cholera::pumps.vestry[cholera::pumps.vestry$id ==
        x$origin, c("x", "y")]
    } else {
      origin.obs <- cholera::pumps[cholera::pumps$id == x$origin, c("x", "y")]
    }

    alter <- nodes[nodes$node == alter.node, "pump"]
    case.color <- "blue"
  }

  dat.plus.origin <- rbind(dat, origin.obs)

  if (zoom) {
    x.rng <- c(min(dat.plus.origin$x) - radius, max(dat.plus.origin$x) + radius)
    y.rng <- c(min(dat.plus.origin$y) - radius, max(dat.plus.origin$y) + radius)
  } else {
    x.rng <- range(cholera::roads$x)
    y.rng <- range(cholera::roads$y)
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

  if (x$type == "case-pump" | x$type == "cases") {
    points(origin.obs, col = "red")
  }

  if (x$type == "case-pump") {
    title(main = paste("Case", x$origin, "to Pump", alter))
  } else if (x$type == "cases") {
    points(destination.obs, col = "red")
    title(main = paste("Case", x$origin, "to Case", alter))
  } else if (x$type == "pumps") {
    title(main = paste("Pump", x$origin, "to Pump", alter))
  }

  points(dat[1, c("x", "y")], col = case.color, pch = 0)
  points(dat[nrow(dat), c("x", "y")], col = case.color, pch = 0)

  if (x$type %in% c("case-pump", "cases")) {
    if (zoom) {
      if (x$observed) {
        text(cholera::fatalities[cholera::fatalities$case == x$origin,
          c("x", "y")], labels = x$origin, pos = 1, col = "red")
        if (x$type == "cases") {
          text(cholera::fatalities[cholera::fatalities$case == x$data$caseB,
            c("x", "y")], labels = x$data$caseB, pos = 1, col = "red")
        }
      } else {
        text(cholera::regular.cases[x$origin, ], labels = x$origin, pos = 1,
          col = "red")
        if (x$type == "cases") {
          text(cholera::regular.cases[x$data$caseB, ], labels = x$data$caseB,
            pos = 1, col = "red")
        }
      }
    }
  }

  drawPath(x$path, case.color)

  if (x$time.unit == "hour") {
    nominal.time <- paste(round(x$data$time, 1), "hr")
  } else if (x$time.unit == "minute") {
    nominal.time <- paste(round(x$data$time, 1), "min")
  } else if (x$time.unit == "second") {
    nominal.time <- paste(round(x$data$time), "sec")
  }

  if (x$unit == "native") {
    d.unit <- "units;"
  } else if (x$unit == "meter") {
    d.unit <- "m;"
  } else if (x$unit == "yard") {
    d.unit <- "yd;"
  }

  # mileposts #

  if (is.null(unit.posts) == FALSE) {
    if (unit.posts %in% c("distance", "time") == FALSE) {
      stop('If specified, "unit.posts" must be "distance" or "time".')
    } else {
      if (is.null(unit.interval)) {
        if (unit.posts == "distance")  {
          unit.interval <- 50
        } else if (unit.posts == "time") {
          unit.interval <- 60
        }
      } else {
        if (!is.numeric(unit.interval)) {
          stop('"unit.interval" must be numeric.')
        }
      }

      path <- rev(x$path)
      path.edge <- data.frame(node1 = path[1:(length(path) - 1)],
                              node2 = path[2:length(path)],
                              stringsAsFactors = FALSE)

      edge.data <- identifyEdges(path.edge, edges)

      if (unit.posts == "distance") {
        cumulative <- cholera::unitMeter(cumsum(edge.data$d), "meter")
      } else if (unit.posts == "time") {
        cumulative <- cholera::distanceTime(cumsum(edge.data$d),
          speed = x$speed)
      }

      total <- cumulative[length(cumulative)]
      posts <- seq(0, total, unit.interval)

      if (max(posts) > max(cumulative)) {
        posts <- posts[-length(posts)]
      }

      bins <- data.frame(lo = c(0, cumulative[-length(cumulative)]),
                         hi = cumulative)

      edge.select <- vapply(posts, function(x) {
        which(vapply(seq_len(nrow(bins)), function(i) {
          x >= bins[i, "lo"] & x < bins[i, "hi"]
        }, logical(1L)))
      }, integer(1L))

      start.node <- edgeOrder(edge.data, path.edge)

      post.coordinates <- lapply(seq_along(edge.select), function(i) {
        sel.data <- edge.data[edge.select[i], ]

        if (start.node[edge.select[i]] == 1) {
          e.data <- data.frame(x = c(sel.data$x1, sel.data$x2),
                               y = c(sel.data$y1, sel.data$y2))
        } else if (start.node[edge.select[i]] == 2) {
          e.data <- data.frame(x = c(sel.data$x2, sel.data$x1),
                               y = c(sel.data$y2, sel.data$y1))
        }

        ols <- stats::lm(y ~ x, data = e.data)
        edge.slope <- stats::coef(ols)[2]
        edge.intercept <- stats::coef(ols)[1]
        theta <- atan(edge.slope)

        if (unit.posts == "distance") {
          h <- (posts[i] - bins[edge.select[i], "lo"]) / cholera::unitMeter(1)
        } else if (unit.posts == "time") {
          h <- (posts[i] - bins[edge.select[i], "lo"]) * 1000 * x$speed / 60^2 /
            cholera::unitMeter(1)
        }

        p.coords <- quandrantCoordinates(e.data, h, theta)

        data.frame(post = posts[i],
                   x = p.coords$x,
                   y = p.coords$y,
                   angle = theta * 180L / pi,
                   row.names = NULL)
      })

      coords <- do.call(rbind, post.coordinates)
      arrow.data <- edge.data[edge.select, ]
      start <- start.node[edge.select]

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
          text(dataB[1, c("x", "y")], labels = ">", srt = coords[i, "angle"],
            col = case.color, cex = 1.5)
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
      x$speed, "km/hr;", post.info))
  } else {
    title(sub = paste(round(x$data$distance, 1), d.unit, nominal.time, "@",
      x$speed, "km/hr"))
  }
}
