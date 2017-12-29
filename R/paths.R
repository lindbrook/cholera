#' Compute walking path pump neighborhoods.
#'
#' Compute and group walking paths by neighborhood.
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @param observed Logical. Observed or expected walking path pump neighborhoods.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. With Numeric, you can specify the number logical cores. On Windows, only "multi.core = FALSE" is available.
#' @export

neighborhoodPaths <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, observed = TRUE, multi.core = FALSE) {

  if (is.logical(multi.core)) {
    if (multi.core == TRUE) {
      cores <- parallel::detectCores()
    } else {
      if (is.numeric(multi.core)) {
        if (is.integer(multi.core)) {
          cores <- multi.core
        } else {
          cores <- as.integer(multi.core)
        }
      } else {
        cores <- 1L
      }
    }
  } else if (is.numeric(multi.core)) {
    if (is.integer(multi.core)) {
      cores <- multi.core
    } else {
      cores <- as.integer(multi.core)
    }
  }

  args <- list(pump.select = pump.select,
               vestry = vestry,
               weighted = weighted,
               observed = observed,
               cores = cores)

  nearest.path <- do.call("nearestPath", args)
  nearest.pump <- do.call("nearestPump", args)
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
              observed = observed,
              pump.select = pump.select,
              cores = cores,
              metric = 1 / cholera::unitMeter(1, "meter"))

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

  if (x$observed == FALSE) {
    p.data <- dat$nodes.pump

    if (is.null(x$pump.select)) {
      p.node <- p.data$node
      p.name <- p.data$pump
    } else {
      if (all(x$pump.select > 0)) {
        p.data <- p.data[p.data$pump %in% x$pump.select, ]
      } else if (all(x$pump.select < 0)) {
        p.data <- p.data[p.data$pump %in% abs(x$pump.select) == FALSE, ]
      }
      p.node <- p.data$node
      p.name <- p.data$pump
    }

    edgesID <- seq_len(nrow(edges))
    isolates <- which(edges$name %in%
      c("Adam and Eve Court", "Falconberg Court", "Falconberg Mews"))

    drawn.segments <- sort(unname(unlist(edge.data)))
    missing.segments <- setdiff(edgesID[-isolates], drawn.segments)
    missing.segments <- unique(edges[missing.segments, "id"])

    # Exclude Portland Mews last segment (zero length).
    missing.segments <- missing.segments[missing.segments != "160-3"]

    nearest.pump <- parallel::mclapply(missing.segments, function(s) {
      seg.data <- cholera::road.segments[cholera::road.segments$id == s,
        c("x1", "y1", "x2", "y2")]

      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      ols <- stats::lm(y ~ x, data = seg.df)
      segment.slope <- stats::coef(ols)[2]

      theta <- atan(segment.slope)
      hypotenuse <- c(stats::dist(seg.df))
      hypotenuse.breaks <- seq(0, hypotenuse, x$metric)[-1]

      distances <- lapply(hypotenuse.breaks, function(h) {
        delta.x <- h * cos(theta)
        delta.y <- h * sin(theta)

        EW <- which.min(seg.data[, c("x1", "x2")])
        if (EW == 1) {
          test.x <- seg.data$x1 + delta.x
          test.y <- seg.data$y1 + delta.y
        } else {
          test.x <- seg.data$x2 + delta.x
          test.y <- seg.data$y2 + delta.y
        }

        case.node <- paste0(test.x, "-", test.y)
        seg.edge <- edges[edges$id == s, ]
        seg.edge <- seg.edge[c(1, nrow(seg.edge)), ]
        seg.edge[1, c("x2", "y2")] <- c(test.x, test.y)
        seg.edge[2, c("x1", "y1")] <- c(test.x, test.y)
        seg.edge[1, "node2"] <- case.node
        seg.edge[2, "node1"] <- case.node
        seg.edge[2, "id2"] <- paste0(s, "b")
        seg.edge$d <- sqrt((seg.edge$x1 - seg.edge$x2)^2 +
                           (seg.edge$y1 - seg.edge$y2)^2)

        edges2 <- rbind(seg.edge, edges[edges$id != s, ])
        edge.list <- edges2[, c("node1", "node2")]
        g2 <- igraph::graph_from_data_frame(edge.list, directed = FALSE)
        stats::setNames(c(igraph::distances(g2, case.node, p.node,
          weights = edges2$d)), p.name)
      })

      p <- vapply(distances, function(x) {
        as.numeric(names(which.min((x))))
      }, numeric(1L))

      data.frame(pump = p, cutpoint = hypotenuse.breaks)
    }, mc.cores = x$cores)

    rle.audit <- lapply(nearest.pump, function(x) rle(x$pump))
    rle.ct <- vapply(rle.audit, function(x) length(x$values), numeric(1L))

    whole.missing.segments <- missing.segments[rle.ct == 1]

    whole.missing.pumps <- vapply(rle.audit[rle.ct == 1], function(x) {
      x$values
    }, numeric(1L))

    split.missing.segs <- missing.segments[rle.ct != 1]

    split.missing.id <- vapply(rle.audit[rle.ct != 1], function(x) {
      x$lengths[1]
    }, numeric(1L))

    split.missing.data <- lapply(seq_along(split.missing.id), function(i) {
      dat <- nearest.pump[rle.ct != 1][[i]]
      dat[c(split.missing.id[i], split.missing.id[i] + 1), ]
    })

    split.missing.segments <- parallel::mclapply(seq_along(split.missing.segs),
      function(i) {

      seg.data <- cholera::road.segments[cholera::road.segments$id ==
        split.missing.segs[i], ]

      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      ols <- stats::lm(y ~ x, data = seg.df)
      segment.slope <- stats::coef(ols)[2]
      theta <- atan(segment.slope)

      split.data <- split.missing.data[[i]]

      h <- split.data$cutpoint
      delta.x <- h * cos(theta)
      delta.y <- h * sin(theta)

      EW <- which.min(seg.data[, c("x1", "x2")])

      if (EW == 1) {
        seg1 <- data.frame(seg.data[, c("x1", "y1")],
                           x2 = seg.data$x1 + delta.x[1],
                           y2 = seg.data$y1 + delta.y[1],
                           row.names = NULL)

        seg2 <- data.frame(x1 = seg.data$x1 + delta.x[2],
                           y1 = seg.data$y1 + delta.y[2],
                           seg.data[, c("x2", "y2")],
                           row.names = NULL)
     } else if (EW == 2) {
       seg1 <- data.frame(seg.data[, c("x2", "y2")],
                          x1 = seg.data$x2 + delta.x[1],
                          y1 = seg.data$y2 + delta.y[1],
                          row.names = NULL)

       seg2 <- data.frame(x2 = seg.data$x2 + delta.x[2],
                          y2 = seg.data$y2 + delta.y[2],
                          seg.data[, c("x1", "y1")],
                          row.names = NULL)
      }

      data.frame(rbind(seg1, seg2), pump = split.data$pump)
    }, mc.cores = x$cores)
  }

  # Plot #

  n.sel <- as.numeric(names(x$paths))
  snow.colors <- cholera::snowColors()[n.sel]

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  road.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  plot(cholera::fatalities[, c("x", "y")], xlim = x.range, ylim = y.range,
    pch = NA, asp = 1)
  invisible(lapply(road.list, lines, col = "gray"))
  invisible(lapply(border.list, lines))

  invisible(lapply(seq_along(edge.data), function(i) {
    n.edges <- edges[edge.data[[i]], ]
    segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 2,
     col = snow.colors[i])
  }))

  if (x$observed == FALSE) {
    invisible(lapply(seq_along(whole.missing.segments), function(i) {
      dat <- cholera::road.segments[cholera::road.segments$id ==
        whole.missing.segments[i], ]
      color <- snow.colors[names(snow.colors) %in%
        paste0("p", whole.missing.pumps[i])]
      segments(dat$x1, dat$y1, dat$x2, dat$y2, lwd = 2, col = color)
    }))

    invisible(lapply(split.missing.segments, function(dat) {
      colors <- vapply(dat$pump, function(x) {
        snow.colors[names(snow.colors) == paste0("p", x)]
      }, character(1L))

      segments(dat$x1[1], dat$y1[1], dat$x2[1], dat$y2[1], lwd = 2,
         col = colors[1])
      segments(dat$x1[2], dat$y1[2], dat$x2[2], dat$y2[2], lwd = 2,
        col = colors[2])
    }))
  }

  if (x$observed) {
    invisible(lapply(seq_along(n.sel), function(i) {
      points(cholera::fatalities.address[x$cases[[i]], c("x", "y")],
             pch = 20, cex = 0.75, col = snow.colors[i])
    }))
  }

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

  title(main = "Pump Neighborhoods: Walking")
}

#' Compute walking path from anchor cases to nearest pump (or from among selected pumps).
#'
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @param observed Logical. Observed or expected walking path pump neighborhoods.
#' @param cores  Numeric. The number logical cores (truncates with as.integer()). Default is 1, which is the only possible value for Windows.
#' @export
#' @return A R list of vectors of nodes.

nearestPath <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE,
  observed = TRUE, cores = 1L) {

  dat <- neighborhoodData(vestry)
  path.data <- pathData(dat, weighted, observed, cores)
  distances <- path.data$distances
  paths <- path.data$paths
  nodes.pump <- dat$nodes.pump

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

#' Compute walking distance from anchor cases to nearest pump (or from among selected pumps).
#'
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @param observed Logical. Observed or expected walking path pump neighborhoods.
#' @param cores  Numeric. The number logical cores (truncates with as.integer()). Default is 1, which is the only possible value for Windows.
#' @export
#' @return An R data frame.

nearestPump <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE,
  observed = TRUE, cores = 1L) {

  dat <- neighborhoodData(vestry)
  path.data <- pathData(dat, weighted, observed, cores)
  distances <- path.data$distances
  nodes.pump <- dat$nodes.pump

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

  if (observed) {
    out <- data.frame(anchor = cholera::fatalities.address$anchor.case,
      do.call(rbind, dat), row.names = NULL)
  } else {
    out <- data.frame(anchor = seq_along(dat), do.call(rbind, dat),
      row.names = NULL)
  }

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

neighborhoodData <- function(vestry = FALSE) {
  if (vestry) {
    node.data <- cholera::nodeData(vestry = TRUE)
  } else {
    node.data <- cholera::nodeData()
  }

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g
  nodes.pump <- nodes[nodes$pump != 0, ]
  nodes.pump <- nodes.pump[order(nodes.pump$pump), c("pump", "node")]
  nodes.pump <- nodes.pump[nodes.pump$pump != 2, ] # P2 is a technical isolate
  list(g = g, nodes = nodes, edges = edges, nodes.pump = nodes.pump)
}

pathData <- function(dat, weighted, observed, cores) {
  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges
  nodes.pump <- dat$nodes.pump

  if (observed) {
    anchor <- cholera::fatalities.address$anchor.case

    paths <- parallel::mclapply(anchor, function(x) {
      case.node <- nodes[nodes$anchor == x, "node"]
      if (weighted) {
        stats::setNames(igraph::shortest_paths(g, case.node, nodes.pump$node,
          weights = edges$d)$vpath, nodes.pump$pump)
      } else {
        stats::setNames(igraph::shortest_paths(g, case.node,
          nodes.pump$node)$vpath, nodes.pump$pump)
      }
    }, mc.cores = cores)

    distances <- parallel::mclapply(anchor, function(x) {
      case.node <- nodes[nodes$anchor == x, "node"]
      if (weighted) {
        stats::setNames(c(igraph::distances(g, case.node, nodes.pump$node,
          weights = edges$d)), nodes.pump$pump)
      } else {
        stats::setNames(c(igraph::distances(g, case.node, nodes.pump$node)),
          nodes.pump$pump)
      }
    }, mc.cores = cores)

    list(distances = distances, paths = paths)
  } else {
    road.nodes <- nodes[nodes$anchor == 0 & nodes$pump == 0, ]

    AE <- cholera::road.segments[cholera::road.segments$name ==
      "Adam and Eve Court", ]
    FC <- cholera::road.segments[cholera::road.segments$name ==
      "Falconberg Court", ]
    FM <- cholera::road.segments[cholera::road.segments$name ==
      "Falconberg Mews", ]

    ep1 <- which(road.nodes$x.proj == AE$x1 & road.nodes$y.proj == AE$y1)
    ep2 <- which(road.nodes$x.proj == AE$x2 & road.nodes$y.proj == AE$y2)
    ep3 <- which(road.nodes$x.proj == FC$x1 & road.nodes$y.proj == FC$y1)
    ep4 <- which(road.nodes$x.proj == FC$x2 & road.nodes$y.proj == FC$y2)

    ep5 <- vapply(seq_len(nrow(FM)), function(i) {
      which(road.nodes$x.proj == FM$x1[i] & road.nodes$y.proj == FM$y1[i])
    }, numeric(1L))

    ep6 <- vapply(seq_len(nrow(FM)), function(i) {
      which(road.nodes$x.proj == FM$x2[i] & road.nodes$y.proj == FM$y2[i])
    }, numeric(1L))

    exclude <- unique(c(ep1, ep2, ep3, ep4, ep5, ep6))
    road.nodes <- road.nodes[-exclude, "node"]

    distances <- parallel::mclapply(road.nodes, function(x) {
      if (weighted) {
        stats::setNames(c(igraph::distances(g, x, nodes.pump$node,
          weights = edges$d)), nodes.pump$pump)
      } else {
        stats::setNames(c(igraph::distances(g, x, nodes.pump$node)),
          nodes.pump$pump)
      }
    }, mc.cores = cores)

    paths <- parallel::mclapply(seq_along(road.nodes), function(i) {
      if (weighted) {
        stats::setNames(igraph::shortest_paths(g, road.nodes[i],
          nodes.pump$node, weights = edges$d)$vpath, nodes.pump$pump)
      } else {
        stats::setNames(igraph::shortest_paths(g, road.nodes[i],
          nodes.pump$node)$vpath, nodes.pump$pump)
      }
    }, mc.cores = cores)

    list(distances = distances, paths = paths)
  }
}
