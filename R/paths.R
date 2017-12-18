#' Compute walking distance from anchor to nearest pump (or among selected pumps).
#'
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @export
#' @return An R data frame.

nearestPump <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE) {
  if (weighted) {
    if (vestry) {
      dat <- neighborhoodData(vestry = TRUE)
      distances <- distanceData(vestry = TRUE)
    } else {
      dat <- neighborhoodData()
      distances <- distanceData()
    }
  } else {
    if (vestry) {
      dat <- neighborhoodData(vestry = TRUE)
      distances <- distanceData(vestry = TRUE, weighted = FALSE)
    } else {
      dat <- neighborhoodData()
      distances <- distanceData( weighted = FALSE)
    }
  }

  pumps <- dat$pumps

  if (is.null(pump.select)) {
    dat <- lapply(distances, function(x) {
      data.frame(pump = as.numeric(names(which.min(x))),
                 distance = x[which.min(x)])
    })

  } else {
    if (all(pump.select > 0)) {
      dat <- lapply(distances, function(x) {
        candidates <- x[names(x) %in% pump.select]
        dat <- candidates[which.min(candidates)]
        data.frame(pump = as.numeric(names(dat)),
                   distance = dat)
      })
    } else if (all(pump.select < 0)) {
      dat <- lapply(distances, function(x) {
        candidates <- x[names(x) %in% abs(pump.select) == FALSE]
        dat <- candidates[which.min(candidates)]
        data.frame(pump = as.numeric(names(dat)),
                   distance = dat)
      })
    }
  }

  out <- data.frame(anchor = cholera::fatalities.address$anchor.case,
    do.call(rbind, dat), row.names = NULL)

  out$pump.name <- NA

  if (vestry) {
    for (p in unique(out$pump)) {
      out[out$pump == p, "pump.name"] <-
        cholera::pumps.vestry[cholera::pumps.vestry$id == p, "street"]
    }
  } else {
    for (p in unique(out$pump)) {
      out[out$pump == p, "pump.name"] <-
        cholera::pumps[cholera::pumps$id == p, "street"]
    }
  }

  out[, c("anchor", "pump", "pump.name", "distance")]
}

#' Compute walking path from anchor to nearest pump (or among selected pumps).
#'
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @export
#' @return A R list of vectors of nodes.

nearestPath <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE) {
  if (weighted) {
    if (vestry) {
      dat <- neighborhoodData(vestry = TRUE)
      distances <- distanceData(vestry = TRUE)
      paths <- pathData(vestry = TRUE)
    } else {
      dat <- neighborhoodData()
      distances <- distanceData()
      paths <- pathData()
    }
  } else {
    if (vestry) {
      dat <- neighborhoodData(vestry = TRUE)
      distances <- distanceData(vestry = TRUE, weighted = FALSE)
      paths <- pathData(vestry = TRUE, weighted = FALSE)
    } else {
      dat <- neighborhoodData()
      distances <- distanceData(weighted = FALSE)
      paths <- pathData(weighted = FALSE)
    }
  }

  pumps <- dat$pumps

  if (is.null(pump.select)) {
    nearest <- vapply(distances, function(x) names(which.min(x)), character(1L))
  } else {
    if (all(pump.select > 0)) {
      nearest <- vapply(distances, function(x) {
        candidates <- x[names(x) %in% pump.select]
        sel <- which.min(candidates)
        names(sel)
      }, character(1L))
    } else if (all(pump.select < 0)) {
      nearest <- vapply(distances, function(x) {
        candidates <- x[names(x) %in% abs(pump.select) == FALSE]
        sel <- which.min(candidates)
        names(sel)
      }, character(1L))
    }
  }

  lapply(seq_along(paths), function(i) {
    out <- names(paths[[i]][[nearest[i]]])
  })
}

neighborhoodData <- function(vestry = FALSE) {
  if (vestry) {
    node.data <- cholera::nodeData(vestry = TRUE)
  } else {
    node.data <- cholera::nodeData()
  }

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g
  pumps <- nodes[nodes$pump != 0, ]
  pumps <- pumps[order(pumps$pump), c("pump", "node")]
  pumps <- pumps[pumps$pump != 2, ] # P2 is a technical isolate
  list(g = g, nodes = nodes, edges = edges, pumps = pumps)
}

pathData <- function(weighted = TRUE, vestry = FALSE) {
  if (vestry) {
    dat <- neighborhoodData(vestry = TRUE)
  } else {
    dat <- neighborhoodData()
  }

  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges
  pumps <- dat$pumps

  lapply(cholera::fatalities.address$anchor.case, function(x) {
    case.node <- nodes[nodes$anchor == x, "node"]

    if (weighted) {
      stats::setNames(igraph::shortest_paths(g, case.node, pumps$node,
        weights = edges$d)$vpath, pumps$pump)
    } else {
      stats::setNames(igraph::shortest_paths(g, case.node,
        pumps$node)$vpath, pumps$pump)
    }
  })
}

distanceData <- function(weighted = TRUE, vestry = FALSE) {
  if (vestry) {
    dat <- neighborhoodData(vestry = TRUE)
  } else {
    dat <- neighborhoodData()
  }

  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges
  pumps <- dat$pumps

  lapply(cholera::fatalities.address$anchor.case, function(x) {
    case.node <- nodes[nodes$anchor == x, "node"]
    if (weighted) {
      stats::setNames(c(igraph::distances(g, case.node, pumps$node,
        weights = edges$d)), pumps$pump)
    } else {
      stats::setNames(c(igraph::distances(g, case.node, pumps$node)),
        pumps$pump)
    }
  })
}

#' Compute shortest walking paths by pump neighborhood.
#'
#' Group walking paths by neighborhood.
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @export

neighborhoodPaths <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE) {

  args <- list(pump.select = pump.select,
               vestry = vestry,
               weighted = weighted)

  nearest.pump <- do.call("nearestPump", args)
  nearest.path <- do.call("nearestPath", args)

  pumpID <- sort(unique(nearest.pump$pump))

  neighborhood.cases <- lapply(pumpID, function(p) {
    which(nearest.pump$pump == p)
  })

  names(neighborhood.cases) <- pumpID

  neighborhood.paths <- lapply(pumpID, function(p) {
    sel <- neighborhood.cases[[paste(p)]]
    nearest.path[sel]
  })

  names(neighborhood.paths) <- pumpID

  out <- list(paths = neighborhood.paths,
              cases = neighborhood.cases,
              vestry = vestry,
              pump.select = pump.select)

  class(out) <- "walkingB"
  out
}

#' Print method for neighborhoodPaths().
#'
#' Return count of paths (anchor cases) by pump neighborhood.
#' @param x An object of class "walkingB" created by neighborhoodPaths().
#' @param ... Additional parameters.
#' @return An R vector.
#' @export
#' @examples
#' # neighborhoodPaths()
#' # print(neighborhoodPaths())

print.walkingB <- function(x, ...) {
  if (class(x) != "walkingB") {
    stop('"x"\'s class needs to be "walkingB".')
  }
   out <- vapply(x$paths, length, numeric(1L))
   print(out)
}

#' Plot method for neighborhoodPaths().
#'
#' @param x An object of class "walkingB" created by neighborhoodPaths().
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @examples
#' # plot(neighborhoodPaths())

plot.walkingB <- function(x, ...) {
  if (class(x) != "walkingB") {
    stop('"x"\'s class needs to be "walkingB".')
  }

  if (x$vestry) {
    dat <- neighborhoodData(vestry = TRUE)
  } else {
    dat <- neighborhoodData()
  }

  edges <- dat$edges

  edgeAudit <- function(x) {
    vapply(seq_along(x[-1]), function(i) {
      ab <- edges$node1 %in% x[i] &
            edges$node2 %in% x[i + 1]
      ba <- edges$node2 %in% x[i] &
            edges$node1 %in% x[i + 1]
      which(ab | ba)
    }, numeric(1L))
  }

  n.paths <- lapply(x$paths, function(neighborhood) {
    dat <- lapply(neighborhood, edgeAudit)
  })

  edge.data <- lapply(n.paths, function(x) unique(unlist(x)))

  # Plot #

  n.sel <- as.numeric(names(x$paths))
  snow.colors <- cholera::snowColors()[n.sel]

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  plot(cholera::fatalities[, c("x", "y")], xlim = x.range, ylim = y.range,
    pch = NA, asp = 1)

  invisible(lapply(roads.list, lines, col = "gray"))
  invisible(lapply(border.list, lines))

  invisible(lapply(seq_along(n.sel), function(i) {
    points(cholera::fatalities.address[x$cases[[i]], c("x", "y")],
      pch = 20, cex = 0.75, col = snow.colors[i])
  }))

if (is.null(x$pump.select)) {
  if (x$vestry) {
    points(cholera::pumps.vestry[, c("x", "y")], pch = 24,
      col = cholera::snowColors(vestry = TRUE))
    text(cholera::pumps.vestry[, c("x", "y")], pos = 1, cex = 0.9,
      labels = paste0("p", cholera::pumps.vestry$id))
  } else {
    points(cholera::pumps[, c("x", "y")], pch = 24,
      col = cholera::snowColors())
    text(cholera::pumps[, c("x", "y")], pos = 1, cex = 0.9,
      labels = paste0("p", cholera::pumps$id))
  }
} else {
  if (x$vestry) {
    points(cholera::pumps.vestry[n.sel, c("x", "y")], pch = 24,
      col = snow.colors)
    text(cholera::pumps.vestry[n.sel, c("x", "y")], pos = 1, cex = 0.9,
      labels = paste0("p", cholera::pumps.vestry$id[n.sel]))
  } else {
    points(cholera::pumps[n.sel, c("x", "y")], pch = 24,
      col = snow.colors)
    text(cholera::pumps[n.sel, c("x", "y")], pos = 1, cex = 0.9,
      labels = paste0("p", cholera::pumps$id[n.sel]))
  }
}

  invisible(lapply(seq_along(edge.data), function(i) {
   n.edges <- edges[edge.data[[i]], ]
   segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 2,
     col = snow.colors[i])
  }))
}
