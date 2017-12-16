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
