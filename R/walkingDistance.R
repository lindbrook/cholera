#' Compute the shortest walking distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is NULL: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of nodes.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. Meaningful only when "weighted" is TRUE. See \code{vignette("roads")} for information on unit distances.
#' @note The function uses a case's "address" or "anchor" case to compute distance.
#'
#' Adam and Eve Court, and Falconberg Court and Falconberg Mews, are disconnected from the larger road network and form two isolated subgraphs. This has two consequences: first, only cases on Adam and Eve Court can reach pump 2 and those cases cannot reach any other pump; second, cases on Falconberg Court and Mews cannot reach any pump. Unreachable pumps will return distances of "Inf".
#' @return An R list.
#' @seealso \code{\link{fatalities}}, \code{vignette("pump.neighborhoods")}
#' @export
#' @examples
#' # path from case 1 to nearest pump.
#' # walkingDistance(1)
#'
#' # path from case 1 to nearest pump in meters (appox).
#' # walkingDistance(1, unit = "meter")
#'
#' # path from case 1 to pump 6.
#' # walkingDistance(1, 6)
#'
#' # exclude pump 7 from consideration.
#' # walkingDistance(1, -7)
#'
#' # path from case 1 to case 6.
#' # walkingDistance(1, 6, type = "cases")
#'
#' # path from pump 1 to pump 6.
#' # walkingDistance(1, 6, type = "pumps")
#'
#' # Plot result
#' # plot(walkingDistance(1, unit = "meter"))

walkingDistance <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, weighted = TRUE, vestry = FALSE, unit = NULL) {

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('"type" must be "case-pump", "cases" or "pumps".')
  }

  if (observed) {
    if (vestry) {
      node.data <- cholera::neighborhoodData(vestry = TRUE)
    } else {
      node.data <- cholera::neighborhoodData()
    }
  } else {
    if (vestry) {
      node.data <- cholera::neighborhoodData(vestry = TRUE,
        case.set = "expected")
    } else {
      node.data <- cholera::neighborhoodData(case.set = "expected")
    }
  }

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g

  if (type == "case-pump") {
    if (observed) {
      if (origin %in% 1:578 == FALSE) {
        txt1 <- 'With type = "case-pump" and "observed" = TRUE,'
        txt2 <- '"origin" must be between 1 and 578.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (origin %in% 1:4993 == FALSE) {
        txt1 <- 'With type = "case-pump" and "observed" = FALSE,'
        txt2 <- '"origin" must be between 1 and 4993.'
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

    out <- data.frame(case = origin,
                      anchor = ego.id,
                      pump = alter.id,
                      pump.name = p.name,
                      distance = d[sel],
                      stringsAsFactors = FALSE,
                      row.names = NULL)

  } else if (type == "cases") {
    if (observed) {
      if (any(abs(c(origin, destination)) %in% 1:578 == FALSE)) {
        txt1 <- 'With type = "cases", the absolute value of both "origin"'
        txt2 <- 'and "destination" must be a whole number between 1 and 578.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (any(abs(c(origin, destination)) %in% 1:4993 == FALSE)) {
        txt1 <- 'With type = "case-pump" and "observed" = FALSE,'
        txt2 <- 'both "origin" and "destination" must be whole numbers between'
        txt3 <- '1 and 4993.'
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

    if (is.null(destination) | all(destination < 0)) {
      out <- data.frame(caseA = origin,
                        caseB = alter.id,
                        anchorA = ego.id,
                        anchorB = alter.id,
                        distance = d[which.min(d)],
                        stringsAsFactors = FALSE,
                        row.names = NULL)
    } else if (all(destination > 0)) {
      if (length(destination) == 1) {
        out <- data.frame(caseA = origin,
                          caseB = destination,
                          anchorA = ego.id,
                          anchorB = alter.id,
                          distance = d[which.min(d)],
                          stringsAsFactors = FALSE,
                          row.names = NULL)
      } else if (length(destination) > 1) {
        out <- data.frame(caseA = origin,
                          caseB = destination[sel],
                          anchorA = ego.id,
                          anchorB = alter.id,
                          distance = d[which.min(d)],
                          stringsAsFactors = FALSE,
                          row.names = NULL)
      }
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

    out <- data.frame(pumpA = A,
                      nameA = pumps[pumps$id == A, "street"],
                      pumpB = B,
                      nameB = pumps[pumps$id == B, "street"],
                      distance = d[sel],
                      stringsAsFactors = FALSE,
                      row.names = NULL)
  }

  if (!is.null(unit)) {
    if (unit == "meter") {
      out$distance <- cholera::unitMeter(out$distance, "meter")
    } else if (unit == "yard") {
      out$distance <- cholera::unitMeter(out$distance, "yard")
    }
  }

  output <- list(origin = origin,
                 destination = destination,
                 type = type,
                 observed = observed,
                 weighted = weighted,
                 vestry = vestry,
                 unit = unit,
                 nodes = nodes,
                 edges = edges,
                 g = g,
                 ego.node = ego.node,
                 alter.node = alter.node,
                 d = out$distance,
                 summary = out)

  class(output) <- "walking_distance"
  output
}

#' Print method for walkingDistance().
#'
#' Return summary results.
#' @param x An object of class "walking_distance" created by walkingDistance().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export
#' @examples
#' # walkingDistance(1)
#' # print(walkingDistance(1))

print.walking_distance <- function(x, ...) {
  if (class(x) != "walking_distance") {
    stop('"x"\'s class needs to be "walking_distance".')
  }

  print(x$summary)
}

#' Plot the walking distance between cases and/or pumps.
#'
#' @param x An object of class "walking_distance" created by walkingDistance().
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @examples
#' # plot(walkingDistance(1))

plot.walking_distance <- function(x, zoom = TRUE, radius = 0.5, ...) {
  if (class(x) != "walking_distance") {
    stop('"x"\'s class needs to be "walking_distance".')
  }

  if (is.na(x$alter.node)) {
    txt1 <- paste("Case", x$origin, "is part of an isolated subgraph.")
    txt2 <- "It (technically) has no neareast pump."
    stop(paste(txt1, txt2))
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  if (x$vestry) {
    colors <- cholera::snowColors(vestry = TRUE)
  } else {
    colors <- cholera::snowColors()
  }

  nodes <- x$nodes
  edges <- x$edges
  g <- x$g
  ego.node <- x$ego.node
  alter.node <- x$alter.node

  if (x$weighted) {
    path <- names(unlist(igraph::shortest_paths(g, ego.node, alter.node,
      weights = edges$d)$vpath))
  } else {
    path <- names(unlist(igraph::shortest_paths(g, ego.node,
      alter.node)$vpath))
  }

  null.nodes <- nodes[nodes$anchor == 0 & nodes$pump == 0, ]
  intermediate.roads <- path %in% null.nodes$node
  road.path <- path[intermediate.roads]

  first <- c(1, which(path %in% road.path[1]))
  last <- c(which(path %in% road.path[length(road.path)]), length(path))

  road.path <- path[intermediate.roads]
  first.mile <- path[first]
  last.mile <- path[last]

  dat <- numericNodeCoordinates(path)

  if (zoom) {
    x.rng <- c(min(dat$x) - radius, max(dat$x) + radius)
    y.rng <- c(min(dat$y) - radius, max(dat$y) + radius)
  } else {
    x.rng <- range(cholera::roads$x)
    y.rng <- range(cholera::roads$y)
  }

  if (x$type == "case-pump") {
    alter <- nodes[nodes$node == alter.node, "pump"]
    case.color <- colors[alter]

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
    alter <- nodes[nodes$node == alter.node, "pump"]
    case.color <- "blue"
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

  if (zoom) {
    if (x$observed) {
      text(cholera::fatalities[cholera::fatalities$case == x$origin,
        c("x", "y")], labels = x$origin, pos = 1, col = "red")
      if (x$type == "cases") {
        text(cholera::fatalities[cholera::fatalities$case == x$summary$caseB,
          c("x", "y")], labels = x$summary$caseB, pos = 1, col = "red")
      }
    } else {
      text(cholera::regular.cases[x$origin, ], labels = x$origin, pos = 1,
        col = "red")
      if (x$type == "cases") {
        text(cholera::regular.cases[x$summary$caseB, ],
          labels = x$summary$caseB, pos = 1, col = "red")
      }
    }
  }

  if (sum(intermediate.roads) >= 2) {
    drawPath(first.mile, case.color)
    drawPath(road.path, case.color)
    drawPath(last.mile, case.color)
  } else if (sum(intermediate.roads) == 1) {
    drawPath(first.mile, case.color)
    drawPath(last.mile, case.color)
  } else if (all(intermediate.roads == FALSE)) {
    drawPath(c(first.mile, last.mile), case.color)
  }

  nominal.distance <- round(x$d, 1)

  if (is.null(x$unit)) {
    title(sub = paste(nominal.distance, "units"))
  } else if (x$unit == "meter") {
    title(sub = paste(nominal.distance, "meters"))
  } else if (x$unit == "yard") {
    title(sub = paste(nominal.distance, "yards"))
  }
}

numericNodeCoordinates <- function(x) {
  nodes <- do.call(rbind, (strsplit(x, "-")))
  data.frame(x = as.numeric(nodes[, 1]), y = as.numeric(nodes[, 2]))
}

drawPath <- function(x, case.color) {
  dat <- numericNodeCoordinates(x)
  n1 <- dat[1:(nrow(dat) - 1), ]
  n2 <- dat[2:nrow(dat), ]
  arrows(n1$x, n1$y, n2$x, n2$y, col = case.color, lwd = 2, length = 0.05)
}
