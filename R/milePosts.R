#' Compute walking path mileposts.
#'
#' Return coordinates, post labels and post angles.
#' @param pump.select Numeric.
#' @param milepost.interval Numeric. Milepost interval in meters.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @return A list of data frames.
#' @export

milePosts <- function(pump.select, milepost.interval = 50, multi.core = FALSE) {

  cores <- multiCore(multi.core)
  x <- cholera::neighborhoodWalking(multi.core = cores)

  auditEdge <- function(p) {
    vapply(seq_along(p[-1]), function(i) {
      ab <- edges$node1 %in% p[i] &
            edges$node2 %in% p[i + 1]
      ba <- edges$node2 %in% p[i] &
            edges$node1 %in% p[i + 1]
      which(ab | ba)
    }, numeric(1L))
  }

  checkSegment <- function(s, sub.edge = FALSE) {
    if (sub.edge) {
      s.data <- edges[edges$id2 == s, ]
    } else {
      s.data <- edges[edges$id == s, ]
    }
    case.node <- c(s.data[1, "node1"], s.data[nrow(s.data), "node2"])
    lapply(case.node, function(node) {
      igraph::distances(dat$g, node, p.node, weights = edges$d)
    })
  }

  wholeSegments <- function(segs) {
    distances <- parallel::mclapply(segs, checkSegment, mc.cores = x$cores)
    audit <- lapply(distances, function(d) {
      unique(vapply(d, which.min, integer(1L)))
    })

    id <- vapply(audit, function(x) length(x) == 1, logical(1L))
    out <- segs[id]
    out.pump <- p.name[unlist(audit[id])]
    pump <- p.name[sort(unique(unlist(audit[id])))]
    out <- lapply(pump, function(p) out[out.pump %in% p])
    names(out) <- pump
    out
  }

  splitSegments <- function(seg) {
    s.data <- edges[edges$id == seg, ]
    seg.df <- data.frame(x = c(s.data[1, "x1"], s.data[nrow(s.data), "x2"]),
                         y = c(s.data[1, "y1"], s.data[nrow(s.data), "y2"]))

    ols <- stats::lm(y ~ x, data = seg.df)
    segment.slope <- stats::coef(ols)[2]
    theta <- atan(segment.slope)
    hypotenuse <- c(stats::dist(seg.df))
    hypotenuse.breaks <- seq(0, hypotenuse, x$metric)

    distances <- lapply(hypotenuse.breaks, function(h) {
      candidates.x <- h * cos(theta)
      candidates.y <- h * sin(theta)

      EW <- which.min(c(s.data[1, "x1"], s.data[nrow(s.data), "x2"]))

      if (EW == 1) {
        test.x <- seg.df[1, "x"] + candidates.x
        test.y <- seg.df[1, "y"] + candidates.y
      } else {
        test.x <- seg.df[2, "x"] + candidates.x
        test.y <- seg.df[2, "y"] + candidates.y
      }

      case.node <- paste0(test.x, "-", test.y)
      seg.edge <- data.frame(x1 = c(s.data[1, "x1"], test.x),
                             y1 = c(s.data[1, "y1"], test.y),
                             x2 = c(test.x, s.data[nrow(s.data), "x2"]),
                             y2 = c(test.y, s.data[nrow(s.data), "y2"]),
                             node1 = c(s.data[1, "node1"], case.node),
                             node2 = c(case.node, s.data[nrow(s.data),
                               "node2"]),
                             id2 = c(s.data$id2[1], paste0(seg, "b")),
                             row.names = NULL)

      seg.info <- s.data[rep(1, each = nrow(seg.edge)),
        c("street", "id", "name")]
      seg.edge <- cbind(seg.info, seg.edge, row.names = NULL)
      seg.edge$d <- sqrt((seg.edge$x1 - seg.edge$x2)^2 +
                         (seg.edge$y1 - seg.edge$y2)^2)

      edges2 <- rbind(seg.edge, edges[edges$id != seg, ])
      edge.list <- edges2[, c("node1", "node2")]
      g2 <- igraph::graph_from_data_frame(edge.list, directed = FALSE)
      stats::setNames(c(igraph::distances(g2, case.node, p.node,
        weights = edges2$d)), p.name)
    })

    p <- vapply(distances, function(x) {
      as.numeric(names(which.min((x))))
    }, numeric(1L))

    data.frame(id = seg, cutpoint = hypotenuse.breaks, pump = p,
      stringsAsFactors = FALSE)
  }

  cutpointValues <- function(dat) {
    rle.audit <- lapply(dat, function(x) rle(x$pump))
    lapply(seq_along(rle.audit), function(i) {
      rle.obs <- rle.audit[[i]]
      cutpoint.obs <- dat[[i]]
      sel <- rle.obs$lengths[1]
      if (length(rle.obs$lengths) != 1) {
        c(cutpoint.obs$cutpoint[sel], cutpoint.obs$cutpoint[sel + 1])
      } else {
        c(cutpoint.obs$cutpoint[sel], cutpoint.obs$cutpoint[sel])
      }
    })
  }

  splitData <- function(dat, cutpoints) {
    lapply(seq_along(dat), function(i) {
      s.data <- edges[edges$id == dat[i], ]
      seg.df <- data.frame(x = c(s.data[1, "x1"], s.data[nrow(s.data), "x2"]),
                           y = c(s.data[1, "y1"], s.data[nrow(s.data), "y2"]))

      ols <- stats::lm(y ~ x, data = seg.df)
      segment.slope <- stats::coef(ols)[2]
      theta <- atan(segment.slope)
      h <- cutpoints[[i]]
      candidates.x <- h * cos(theta)
      candidates.y <- h * sin(theta)

      EW <- which.min(seg.df$x)

      if (EW == 1) {
        x.cut <- seg.df$x[1] + candidates.x
        y.cut <- seg.df$y[1] + candidates.y
        data.frame(x = c(seg.df$x[1], x.cut, seg.df$x[2]),
                   y = c(seg.df$y[1], y.cut, seg.df$y[2]))
      } else {
        x.cut <- seg.df$x[2] + candidates.x
        y.cut <- seg.df$y[2] + candidates.y
        data.frame(x = c(seg.df$x[2], x.cut, seg.df$x[1]),
                   y = c(seg.df$y[2], y.cut, seg.df$y[1]))
      }
    })
  }

  ## Data ##

  dat <- cholera::neighborhoodData(vestry = x$vestry, case.set = "observed")
  edges <- dat$edges
  nodes <- dat$nodes
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

  n.path.edges <- parallel::mclapply(x$paths, function(neighborhood) {
    lapply(neighborhood, auditEdge)
  }, mc.cores = x$cores)

  # neighborhood paths by edge
  n.path.edges.select <- n.path.edges[[paste(pump.select)]]

  # path's case edge and path's other, remaining edges
  case.edge <- vapply(n.path.edges.select, function(x) x[1], numeric(1L))
  noncase.edges <- lapply(n.path.edges.select, function(x) x[-1])

  # b/c paths can overlap; some case.edges will not be dead ends
  candidates.id <- which(case.edge %in% unlist(noncase.edges) == FALSE)
  candidates <- edges[setdiff(case.edge, unlist(noncase.edges)), ]

  # Pump 5 Marlborough Mews exception
  if (length(unlist(noncase.edges)) > 0) {
    # unique edges in neighborhood
    n.path.data <- edges[unique(unlist(n.path.edges.select)), ]

    # presence of just one end point indicates a dead end, periphery case
    audit <- lapply(seq_len(nrow(candidates)), function(i) {
      test1 <- candidates[i, "node1"] %in% n.path.data$node1 &
               candidates[i, "node1"] %in% n.path.data$node2
      test2 <- candidates[i, "node2"] %in% n.path.data$node1 &
               candidates[i, "node2"] %in% n.path.data$node2
      # direct path exception
      if (all(c(test1, test2) == FALSE)) {
        test1A <- sum(candidates[i, "node1"] == n.path.data$node1)
        test1B <- sum(candidates[i, "node1"] == n.path.data$node2)
        test2A <- sum(candidates[i, "node2"] == n.path.data$node1)
        test2B <- sum(candidates[i, "node2"] == n.path.data$node2)
        c(any(c(test1A, test1B) == 2), any(c(test2A, test2B) == 2))
      } else c(test1, test2)
    })

    # case is (x1, y1) or (x2, y2)
    endptID <- vapply(audit, function(x) which(x == FALSE), integer(1L))

    endpt.paths <- lapply(candidates.id, function(x) {
      edges[n.path.edges.select[[x]], ]
    })

    path.order.check <- vapply(seq_along(endpt.paths), function(i) {
      pathOrderCheck(endpt.paths[[i]], endptID[i])
    }, logical(1L))

    out.of.order <- which(path.order.check == FALSE)

    endpt.pathsB <- lapply(out.of.order, function(x) {
      pathOrder(endpt.paths[[x]], endptID[x])
    })

    endpt.paths[out.of.order] <- endpt.pathsB
  } else {
    endpt.paths <- list(candidates)
  }

  parallel::mclapply(seq_along(endpt.paths), function(i) {
    milePostCoordinates(endpt.paths[[i]], pump.select, milepost.interval)
  }, mc.cores = cores)
}

## Check path order ##

pathOrderCheck <- function(dat, ep) {
  all(vapply(seq_len(nrow(dat) - 1), function(i) {
    if (ep == 1) {
      dat[i, "node2"] == dat[i + 1, "node1"]
    } else if (ep == 2) {
      dat[i, "node1"] == dat[i + 1, "node2"]
    }
  }, logical(1L)))
}

## Order path ##

pathOrder <- function(dat, ep) {
  if (ep == 1) {
    init.connector <- 2
  } else if (ep == 2) {
    init.connector <- 1
  }

  node.connector <- vector(mode = "integer", length = nrow(dat))
  node.connector[1] <- init.connector

  for (j in seq_along(node.connector[-1])) {
    previous <- node.connector[j]
    connector <- unlist(dat[j, c("node1", "node2")][previous])
    connected <- which(connector == dat[j + 1, c("node1", "node2")])
    node.connector[j + 1] <- ifelse(connected == 2, 1, 2)
  }

  tmp <- dat[node.connector == 2, ]
  ptA <- tmp[, c("street", "id", "name")]
  ptB <- tmp[, c("x2", "y2", "x1", "y1", "node2", "node1")]
  ptC <- tmp[, c("id2", "d")]
  ptB <- stats::setNames(ptB, c("x1", "y1", "x2", "y2", "node1", "node2"))
  dat[node.connector == 2, ] <- cbind(ptA, ptB, ptC)
  dat
}

## Compute mileposts ##

milePostCoordinates <- function(dat, pump.select, milepost.interval) {
  case.distance <- cholera::unitMeter(cumsum(rev(dat$d)), "meter")
  total.distance <- case.distance[length(case.distance)]
  mile.post <- seq(0, total.distance, milepost.interval)

  if (max(mile.post) > max(case.distance)) {
    mile.post <- mile.post[-length(mile.post)]
  }

  bins <- data.frame(lo = c(0, case.distance[-length(case.distance)]),
                     hi = case.distance)

  edge.select <- vapply(mile.post[-1], function(x) {
    which(vapply(seq_len(nrow(bins)), function(i) {
      x >= bins[i, "lo"] & x < bins[i, "hi"]
    }, logical(1L)))
  }, integer(1L))

  dat.rev <- do.call(rbind, lapply(rev(dat$id2), function(x) {
    dat[dat$id2 == x, ]
  }))

  post.coordinates <- lapply(seq_along(edge.select), function(i) {
    sel.data <- dat.rev[edge.select[i], ]
    edge.data <- data.frame(x = c(sel.data$x1, sel.data$x2),
                            y = c(sel.data$y1, sel.data$y2))

    ols <- stats::lm(y ~ x, data = edge.data)
    edge.slope <- stats::coef(ols)[2]
    edge.intercept <- stats::coef(ols)[1]
    theta <- atan(edge.slope)
    h <- (mile.post[-1][i] - bins[edge.select[i], "lo"]) /
      cholera::unitMeter(1, "meter")

    delta <- edge.data[2, ] - edge.data[1, ]

    # Quadrant I
    if (all(delta > 0)) {
      post.x <- edge.data[1, "x"] + abs(h * cos(theta))
      post.y <- edge.data[1, "y"] + abs(h * sin(theta))

    # Quadrant II
    } else if (delta[1] < 0 & delta[2] > 0) {
      post.x <- edge.data[1, "x"] - abs(h * cos(theta))
      post.y <- edge.data[1, "y"] + abs(h * sin(theta))

    # Quadrant III
    } else if (all(delta < 0)) {
      post.x <- edge.data[1, "x"] - abs(h * cos(theta))
      post.y <- edge.data[1, "y"] - abs(h * sin(theta))

    # Quadrant IV
    } else if (delta[1] > 0 & delta[2] < 0) {
      post.x <- edge.data[1, "x"] + abs(h * cos(theta))
      post.y <- edge.data[1, "y"] - abs(h * sin(theta))

    # I:IV
    } else if (delta[1] > 0 & delta[2] == 0) {
      post.x <- edge.data[1, "x"] + abs(h * cos(theta))
      post.y <- edge.data[1, "y"]

    # I:II
    } else if (delta[1] == 0 & delta[2] > 0) {
      post.x <- edge.data[1, "x"]
      post.y <- edge.data[1, "y"] + abs(h * sin(theta))

    # II:III
    } else if (delta[1] < 0 & delta[2] == 0) {
      post.x <- edge.data[1, "x"] - abs(h * cos(theta))
      post.y <- edge.data[1, "y"]

    # III:IV
    } else if (delta[1] == 0 & delta[2] < 0) {
      post.x <- edge.data[1, "x"]
      post.y <- edge.data[1, "y"] - abs(h * sin(theta))
    }

    data.frame(pump = pump.select, post = mile.post[-1], x = post.x,
      y = post.y, angle = theta * 180L / pi, row.names = NULL)
  })

  do.call(rbind, post.coordinates)
}
