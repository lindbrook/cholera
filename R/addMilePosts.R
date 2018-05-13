#' Add walking path mileposts.
#'
#' Return coordinates, post labels and post angles: in-progress prototype.
#' @param pump.subset Numeric. Vector of pumps to select (subset) from neighborhoods defined by "pump.select". Negative selection possible. NULL selects all pumps in "pump.select".
#' @param pump.select Numeric. Numeric vector of pumps to define pump neighborhoods (i.e. the "population"). Negative selection possible. NULL selects all pumps.
#' @param milepost.interval Numeric. Milepost interval in meters.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @return A list of data frames.
#' @export

addMilePosts <- function(pump.subset = NULL, pump.select = NULL,
  milepost.interval = 50, multi.core = FALSE) {

  cores <- multiCore(multi.core)
  x <- cholera::neighborhoodWalking(pump.select, multi.core = cores)

  auditEdge <- function(p) {
    vapply(seq_along(p[-1]), function(i) {
      ab <- edges$node1 %in% p[i] &
            edges$node2 %in% p[i + 1]
      ba <- edges$node2 %in% p[i] &
            edges$node1 %in% p[i + 1]
      edges[which(ab | ba), "id2"]
    }, character(1L))
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

  # vector of nodes for the 321 observed anchor cases
  n.path.edges <- parallel::mclapply(x$paths, function(neighborhood) {
    lapply(neighborhood, auditEdge)
  }, mc.cores = x$cores)

  if (!is.null(pump.subset)) {
    # neighborhood paths by edge
    n.path.edges.subset <- n.path.edges[paste(pump.subset)]

    # path's case edge and path's other, remaining edges
    case.edge <- lapply(n.path.edges.subset, function(n) {
      vapply(n, function(x) x[1], numeric(1L))
    })

    noncase.edges <- lapply(n.path.edges.subset, function(n) {
      lapply(n, function(x) x[-1])
    })

  } else {
    # neighborhood paths by edge
    case.edge <- lapply(n.path.edges, function(n) {
      vapply(n, function(x) x[1], character(1L))
    })

    # path's case edge and path's other, remaining edges
    noncase.edges <- lapply(n.path.edges, function(n) {
      lapply(n, function(x) x[-1])
    })
  }

  # potential neighborhood periphery edges
  candidate.case.edge <- lapply(seq_along(case.edge), function(i) {
    case.edge[[i]][case.edge[[i]] %in% unlist(noncase.edges[[i]]) == FALSE]
  })

  candidate.data <- lapply(seq_along(candidate.case.edge), function(i) {
    sel <- setdiff(candidate.case.edge[[i]], unlist(noncase.edges[[i]]))
    edges[edges$id2 %in% sel, ]
  })

  if (!is.null(pump.subset)) {
    candidate.case.edge <- stats::setNames(candidate.case.edge,
      paste(pump.subset))
    candidate.data <- stats::setNames(candidate.data, paste(pump.subset))
  } else {
    candidate.case.edge <- stats::setNames(candidate.case.edge, names(x$paths))
    candidate.data <- stats::setNames(candidate.data, names(x$paths))
  }

  # all unique observed edges
  n.path.data <- edges[edges$id2 %in% unique(unlist(n.path.edges)), ]

  #

  candidateID <- lapply(seq_along(case.edge), function(i) {
    case.edge[[i]] %in% candidate.case.edge[[i]]
  })

  candidateID <- stats::setNames(candidateID, names(x$paths))

  #

  endpt.paths <- lapply(names(candidateID), function(nm) {
    n.path <- x$paths[[nm]]
    c.id <- candidateID[[nm]]
    n.path[c.id]
  })

  endpt.paths <- stats::setNames(endpt.paths, names(x$paths))

  #

  identifyEdges <- function(dat) {
    out <- lapply(seq_len(nrow(dat)), function(i) {
      test1 <- dat[i, "node1"] == edges$node1 &
               dat[i, "node2"] == edges$node2
      test2 <- dat[i, "node2"] == edges$node1 &
               dat[i, "node1"] == edges$node2
      if (any(test1)) {
        edges[test1, ]
      } else if (any(test2)) {
        edges[test2, ]
      } else {
       stop("Error!")
      }
    })
    do.call(rbind, out)
  }

  edgeData <- function(endpt.paths) {
    lapply(endpt.paths, function(p.vectors) {
      out <- lapply(p.vectors, function(p) {
        path <- rev(p)
        path.edge <- data.frame(node1 = path[1:(length(path) - 1)],
                                node2 = path[2:length(path)],
                                stringsAsFactors = FALSE)

        edge.data <- identifyEdges(path.edge)
        audit1 <- path.edge$node1 == edge.data$node1
        audit2 <- path.edge$node2 == edge.data$node2

        if (!all(audit1 == audit2)) {
          stop("Error!")
        } else {
          out.of.order <- which(audit1 == FALSE)
          tmp.sel <- c("x2", "y2", "x1", "y1", "node2", "node1")
          tmp <- edge.data[out.of.order, tmp.sel]
          out.sel <- c("x1", "y1", "x2", "y2", "node1", "node2")
          edge.data[out.of.order, out.sel] <- tmp
          edge.data
        }
      })
      out
    })
  }

  edge.data <- edgeData(endpt.paths)

  out <- parallel::mclapply(names(endpt.paths), function(nm) {
    dat <- edge.data[[nm]]
    lapply(dat, milePostCoordinates, milepost.interval)
  }, mc.cores = cores)

  stats::setNames(out, names(endpt.paths))
}

milePostCoordinates <- function(dat, milepost.interval) {
  case.distance <- cholera::unitMeter(cumsum(dat$d), "meter")
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

  post.coordinates <- lapply(seq_along(edge.select), function(i) {
    sel.data <- dat[edge.select[i], ]
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

    data.frame(post = mile.post[-1][i], x = post.x,
      y = post.y, angle = theta * 180L / pi, row.names = NULL)
  })

  do.call(rbind, post.coordinates)
}
