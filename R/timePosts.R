#' Compute the coordinates of walking path timeposts.
#'
#' In-progress prototype.
#' @param pump.select Numeric.
#' @param timepost.interval Numeric. Seconds.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @return An R list.
#' @export

timePosts <- function(pump.select, timepost.interval = 60, multi.core = FALSE) {
  cores <- multiCore(multi.core)
  x <- neighborhoodWalking(multi.core = cores)

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

  # unique edges in neighborhood
  n.path.data <- edges[unique(unlist(n.path.edges.select)), ]

  # presence of just one end point indicates a dead end, periphery case
  audit <- lapply(seq_len(nrow(candidates)), function(i) {
    t1 <- candidates[i, "node1"] %in% n.path.data$node1 &
          candidates[i, "node1"] %in% n.path.data$node2
    t2 <- candidates[i, "node2"] %in% n.path.data$node1 &
          candidates[i, "node2"] %in% n.path.data$node2
    c(t1, t2)
  })

  endptID <- vapply(audit, function(x) which(x == FALSE), integer(1L))

  endpt.paths <- lapply(candidates.id, function(x) {
    edges[n.path.edges.select[[x]], ]
  })

  parallel::mclapply(endpt.paths, function(x) {
    timePostCoordinates(x, timepost.interval)
  }, mc.cores = cores)
}

## Compute timeposts ##

timePostCoordinates <- function(dat, timepost.interval) {
  # case.distance <- cholera::unitMeter(cumsum(rev(dat$d)), "meter")
  # total.distance <- cholera::unitMeter(sum(dat$d), "meter")

  case.time <- cholera::distanceTime(cumsum(rev(dat$d)))
  total.time <- cholera::distanceTime(sum(dat$d))
  time.post <- seq(0, total.time, timepost.interval)

  if (max(time.post) > max(case.time)) {
    time.post <- time.post[-length(time.post)]
  }

  bins <- data.frame(lo = c(0, case.time[-length(case.time)]),
                     hi = case.time)

  edge.select <- vapply(time.post[-1], function(x) {
    which(vapply(seq_len(nrow(bins)), function(i) {
      x >= bins[i, "lo"] & x < bins[i, "hi"]
    }, logical(1L)))
  }, integer(1L))

  dat.rev <- lapply(rev(dat$id2), function(x) {
    dat[dat$id2 == x, ]
  })

  dat.rev <- do.call(rbind, dat.rev)

  post.coordinates <- lapply(seq_along(edge.select), function(i) {
    dat <- dat.rev[edge.select[i], ]
    edge.data <- data.frame(x = c(dat$x1, dat$x2), y = c(dat$y1, dat$y2))
    ols <- stats::lm(y ~ x, data = edge.data)
    edge.slope <- stats::coef(ols)[2]
    edge.intercept <- stats::coef(ols)[1]
    theta <- atan(edge.slope)
    h <- (time.post[-1][i] - case.time[edge.select[i] - 1]) / distanceTime(1)
    post.x <- h * cos(theta) + edge.data[1, "x"]
    post.y <- h * sin(theta) + edge.data[1, "y"]
    data.frame(x = post.x, y = post.y, row.names = NULL)
  })

  do.call(rbind, post.coordinates)
}
