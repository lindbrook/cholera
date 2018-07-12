#' Common auxillary functions for walking path functions.
#' walking(), walkingPath(), addMilePosts(), milePosts(), addNeighborhood().
#' @noRd

walkingAuxillaryFunctions <- function() NULL

auditEdge <- function(p, edges, output = "logical") {
  if (output == "logical") {
    vapply(seq_along(p[-1]), function(i) {
      ab <- edges$node1 %in% p[i] &
            edges$node2 %in% p[i + 1]
      ba <- edges$node2 %in% p[i] &
            edges$node1 %in% p[i + 1]
      which(ab | ba)
    }, numeric(1L))
  } else if (output == "id2") {
    vapply(seq_along(p[-1]), function(i) {
      ab <- edges$node1 %in% p[i] &
            edges$node2 %in% p[i + 1]
      ba <- edges$node2 %in% p[i] &
            edges$node1 %in% p[i + 1]
      edges[which(ab | ba), "id2"]
    }, character(1L))
  } else {
    stop('"output" must either "logical" or "id2".')
  }
}

checkSegment <- function(s, dat, edges, p.node, sub.edge = FALSE) {
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

wholeSegments <- function(segs, dat, edges, p.name, p.node, x) {
  distances <- parallel::mclapply(segs, checkSegment, dat, edges, p.node,
    mc.cores = x$cores)
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

splitSegments <- function(seg, edges, p.name, p.node, x) {
  s.data <- edges[edges$id == seg, ]
  seg.df <- data.frame(x = c(s.data[1, "x1"], s.data[nrow(s.data), "x2"]),
                       y = c(s.data[1, "y1"], s.data[nrow(s.data), "y2"]))

  ols <- stats::lm(y ~ x, data = seg.df)
  segment.slope <- stats::coef(ols)[2]
  theta <- atan(segment.slope)
  hypotenuse <- c(stats::dist(seg.df))
  hypotenuse.breaks <- seq(0, hypotenuse, x$metric)

  distances <- lapply(hypotenuse.breaks, function(h) {
    delta.x <- h * cos(theta)
    delta.y <- h * sin(theta)

    EW <- which.min(c(s.data[1, "x1"], s.data[nrow(s.data), "x2"]))

    if (EW == 1) {
      test.x <- seg.df[1, "x"] + delta.x
      test.y <- seg.df[1, "y"] + delta.y
    } else {
      test.x <- seg.df[2, "x"] + delta.x
      test.y <- seg.df[2, "y"] + delta.y
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

cutpointValues <- function(dat, x) {
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

splitData <- function(dat, cutpoints, edges) {
  lapply(seq_along(dat), function(i) {
    s.data <- edges[edges$id == dat[i], ]
    seg.df <- data.frame(x = c(s.data[1, "x1"], s.data[nrow(s.data), "x2"]),
                         y = c(s.data[1, "y1"], s.data[nrow(s.data), "y2"]))

    ols <- stats::lm(y ~ x, data = seg.df)
    segment.slope <- stats::coef(ols)[2]
    theta <- atan(segment.slope)
    h <- cutpoints[[i]]
    delta.x <- h * cos(theta)
    delta.y <- h * sin(theta)

    EW <- which.min(seg.df$x)

    if (EW == 1) {
      x.cut <- seg.df$x[1] + delta.x
      y.cut <- seg.df$y[1] + delta.y
      data.frame(x = c(seg.df$x[1], x.cut, seg.df$x[2]),
                 y = c(seg.df$y[1], y.cut, seg.df$y[2]))
    } else {
      x.cut <- seg.df$x[2] + delta.x
      y.cut <- seg.df$y[2] + delta.y
      data.frame(x = c(seg.df$x[2], x.cut, seg.df$x[1]),
                 y = c(seg.df$y[2], y.cut, seg.df$y[1]))
    }
  })
}

## mileposts and timeposts ##

identifyEdges <- function(dat, edges) {
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

edgeData <- function(endpt.paths, edges) {
  lapply(endpt.paths, function(p.vectors) {
    out <- lapply(p.vectors, function(p) {
      path <- rev(p)
      path.edge <- data.frame(node1 = path[1:(length(path) - 1)],
                              node2 = path[2:length(path)],
                              stringsAsFactors = FALSE)

      edge.data <- identifyEdges(path.edge, edges)
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

postCoordinates <- function(dat, unit, interval, walking.speed,
  arrow.data = FALSE) {

  if (unit == "distance") {
    cumulative <- cholera::unitMeter(cumsum(dat$d), "meter")
  } else if (unit == "time") {
    cumulative <- cholera::distanceTime(cumsum(dat$d), speed = walking.speed)
  }

  total <- cumulative[length(cumulative)]
  posts <- seq(0, total, interval)

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

  post.coordinates <- lapply(seq_along(edge.select), function(i) {
    sel.data <- dat[edge.select[i], ]
    edge.data <- data.frame(x = c(sel.data$x1, sel.data$x2),
                            y = c(sel.data$y1, sel.data$y2))

    ols <- stats::lm(y ~ x, data = edge.data)
    edge.slope <- stats::coef(ols)[2]
    edge.intercept <- stats::coef(ols)[1]
    theta <- atan(edge.slope)

    if (unit == "distance") {
      h <- (posts[i] - bins[edge.select[i], "lo"]) /
        cholera::unitMeter(1, "meter")
    } else if (unit == "time") {
      h <- (posts[i] - bins[edge.select[i], "lo"]) * 1000 * walking.speed /
        60^2 / cholera::unitMeter(1, "meter")
    }

    p.coords <- quandrantCoordinates(edge.data, h, theta)

    if (arrow.data) {
      data.frame(post = posts[i],
                 x0 = edge.data[2, "x"],
                 y0 = edge.data[2, "y"],
                 x = p.coords$x,
                 y = p.coords$y,
                 angle = theta * 180L / pi,
                 row.names = NULL)
    } else {
      data.frame(post = posts[i],
                 x = p.coords$x,
                 y = p.coords$y,
                 angle = theta * 180L / pi,
                 row.names = NULL)
    }
  })

  do.call(rbind, post.coordinates)
}

quandrantCoordinates <- function(dat, h, theta) {
  delta <- dat[2, ] - dat[1, ]

  # Quadrant I
  if (all(delta > 0)) {
    post.x <- dat[1, "x"] + abs(h * cos(theta))
    post.y <- dat[1, "y"] + abs(h * sin(theta))

  # Quadrant II
  } else if (delta[1] < 0 & delta[2] > 0) {
    post.x <- dat[1, "x"] - abs(h * cos(theta))
    post.y <- dat[1, "y"] + abs(h * sin(theta))

  # Quadrant III
  } else if (all(delta < 0)) {
    post.x <- dat[1, "x"] - abs(h * cos(theta))
    post.y <- dat[1, "y"] - abs(h * sin(theta))

  # Quadrant IV
  } else if (delta[1] > 0 & delta[2] < 0) {
    post.x <- dat[1, "x"] + abs(h * cos(theta))
    post.y <- dat[1, "y"] - abs(h * sin(theta))

  # I:IV
  } else if (delta[1] > 0 & delta[2] == 0) {
    post.x <- dat[1, "x"] + abs(h * cos(theta))
    post.y <- dat[1, "y"]

  # I:II
  } else if (delta[1] == 0 & delta[2] > 0) {
    post.x <- dat[1, "x"]
    post.y <- dat[1, "y"] + abs(h * sin(theta))

  # II:III
  } else if (delta[1] < 0 & delta[2] == 0) {
    post.x <- dat[1, "x"] - abs(h * cos(theta))
    post.y <- dat[1, "y"]

  # III:IV
  } else if (delta[1] == 0 & delta[2] < 0) {
    post.x <- dat[1, "x"]
    post.y <- dat[1, "y"] - abs(h * sin(theta))
  }

  data.frame(x = post.x, y = post.y)
}

numericNodeCoordinates <- function(x) {
  nodes <- do.call(rbind, (strsplit(x, "-")))
  data.frame(x = as.numeric(nodes[, 1]), y = as.numeric(nodes[, 2]))
}

drawPath <- function(x, case.color) {
  dat <- numericNodeCoordinates(x)
  n1 <- dat[1:(nrow(dat) - 1), ]
  n2 <- dat[2:nrow(dat), ]
  segments(n1$x, n1$y, n2$x, n2$y, col = case.color, lwd = 3)
}

edgeOrder <- function(dat, path.edge) {
  vapply(seq_len(nrow(dat)), function(i) {
    test1 <- dat[i, "node1"] == path.edge[i, "node1"] &
             dat[i, "node2"] == path.edge[i, "node2"]
    test2 <- dat[i, "node1"] == path.edge[i, "node2"] &
             dat[i, "node2"] == path.edge[i, "node1"]
    ifelse(any(test1), 1, ifelse(any(test2), 2, 0))
  }, numeric(1L))
}

areaPointsData <- function(sim.proj.segs, wholes, snow.colors, sim.proj,
  split.cases) {

  wholes.id <- sim.proj.segs[sim.proj.segs %in% unlist(wholes)]
  sim.proj.wholes <- sim.proj[sim.proj$road.segment %in% wholes.id, ]
  sim.proj.wholes$pump <- NA
  sim.proj.wholes$color <- NA

  for (nm in names(wholes)) {
    sel <- sim.proj.wholes$road.segment %in% wholes[[nm]]
    sim.proj.wholes[sel, "pump"] <- as.numeric(nm)
    sim.proj.wholes[sel, "color"] <- snow.colors[paste0("p", nm)]
  }

  sim.proj.splits <- sim.proj[sim.proj$case %in% unlist(split.cases), ]
  sim.proj.splits$pump <- NA
  sim.proj.splits$color <- NA

  for (nm in names(split.cases)) {
    sel <- sim.proj.splits$case %in% split.cases[[nm]]
    sim.proj.splits[sel, "pump"] <- as.numeric(nm)
    sim.proj.splits[sel, "color"] <- snow.colors[paste0("p", nm)]
  }

  list(sim.proj.wholes = sim.proj.wholes,
       sim.proj.splits = sim.proj.splits)
}
