#' Common auxiliary functions for walking path functions.
#' walking(), walkingPath(), addMilePosts(), milePosts(), addNeighborhood().
#' @noRd

walkingAuxiliaryFunctions <- function() NULL

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
  if (x$dev.mode) {
    cl <- parallel::makeCluster(x$cores)

    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = c("dat", "edges", "p.node"))

    distances <- parallel::parLapply(cl, segs, function(s) {
      checkSegment(s, dat, edges, p.node)
    })

    parallel::stopCluster(cl)
  } else {
    distances <- parallel::mclapply(segs, checkSegment, dat, edges, p.node,
      mc.cores = x$cores)
  }

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
    cumulative <- unitMeter(cumsum(dat$d))
  } else if (unit == "time") {
    cumulative <- distanceTime(cumsum(dat$d), walking.speed = walking.speed)
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
      h <- (posts[i] - bins[edge.select[i], "lo"]) / unitMeter(1)
    } else if (unit == "time") {
      h <- (posts[i] - bins[edge.select[i], "lo"]) * 1000 * walking.speed /
        60^2 / unitMeter(1)
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

drawPath <- function(x, case.color, compute.coords = TRUE) {
  if (compute.coords) {
    path.data <- numericNodeCoordinates(x)
  } else {
    path.data <- x
  }
  n1 <- path.data[1:(nrow(path.data) - 1), ]
  n2 <- path.data[2:nrow(path.data), ]
  segments(n1$x, n1$y, n2$x, n2$y, col = case.color, lwd = 3)
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

  if (is.null(split.cases) == FALSE) {
    sim.proj.splits <- sim.proj[sim.proj$case %in% unlist(split.cases), ]
    sim.proj.splits$pump <- NA
    sim.proj.splits$color <- NA

    for (nm in names(split.cases)) {
      sel <- sim.proj.splits$case %in% split.cases[[nm]]
      sim.proj.splits[sel, "pump"] <- as.numeric(nm)
      sim.proj.splits[sel, "color"] <- snow.colors[paste0("p", nm)]
    }
  } else sim.proj.splits <- NULL

  list(sim.proj.wholes = sim.proj.wholes,
       sim.proj.splits = sim.proj.splits)
}

# for neighborhoodWalking(), expectedCount() and pearsonResiduals.walking()
observedExpected <- function(x, n.data) {
  if (class(x) != "walking") {
    stop('"x"\'s class needs to be "walking".')
  }

  dat <- n.data$dat
  edges <- n.data$edges
  neighborhood.path.edges <- n.data$neighborhood.path.edges
  p.node <- n.data$p.node
  p.name <- n.data$p.name

  ## ----- segment audit ----- ##

  obs.segment.count <- lapply(neighborhood.path.edges, function(x) {
    table(edges[unique(unlist(x)), "id"])
  })

  edge.count <- table(edges$id)

  segment.audit <- lapply(obs.segment.count, function(neighborhood) {
    whole.id <- vapply(names(neighborhood), function(nm) {
      identical(neighborhood[nm], edge.count[nm])
    }, logical(1L))

    list(whole = names(neighborhood[whole.id]),
         partial = names(neighborhood[!whole.id]))
  })

  ## ----- observed ----- ##

  # list of whole traversed segments
  obs.whole <- lapply(segment.audit, function(x) x$`whole`)

  # list of partially traversed segments
  obs.partial <- lapply(segment.audit, function(x) x$`partial`)
  partial.segs <- unname(unlist(obs.partial))
  obs.partial.whole <- wholeSegments(partial.segs, dat, edges, p.name,
    p.node, x)

  # list of of split segments (lead to different pumps)
  # the cutpoint is found using appox. 1 meter increments via cutpointValues()
  obs.partial.segments <- setdiff(partial.segs, unlist(obs.partial.whole))

  if (length(obs.partial.segments) > 0) {
    if (x$dev.mode) {
      cl <- parallel::makeCluster(x$cores)

      parallel::clusterExport(cl = cl, envir = environment(),
        varlist = c("edges", "p.name", "p.node", "x", "checkSegment",
        "splitSegments"))

      obs.partial.split.data <- parallel::parLapply(cl, obs.partial.segments,
        function(seg) splitSegments(seg, edges, p.name, p.node, x))

      parallel::stopCluster(cl)
    } else {
      obs.partial.split.data <- parallel::mclapply(obs.partial.segments,
        splitSegments, edges, p.name, p.node, x, mc.cores = x$cores)
    }

    cutpoints <- cutpointValues(obs.partial.split.data)
    obs.partial.split.pump <- lapply(obs.partial.split.data, function(x)
      unique(x$pump))
    obs.partial.split <- splitData(obs.partial.segments, cutpoints, edges)
  }

  ## ----- unobserved ----- ##

  # list of edges that are wholly or partially traversed
  obs.segments <- lapply(neighborhood.path.edges, function(x) {
    unique(edges[unique(unlist(x)), "id"])
  })

  # list of edges that are untouched by any path
  unobs.segments <- setdiff(cholera::road.segments$id, unlist(obs.segments))

  falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
  unobs.segments <- unobs.segments[unobs.segments %in%
    falconberg.ct.mews == FALSE]

  # Exclude segment if A&E pump is not among selected.
  if (is.null(x$pump.select) == FALSE) {
    sel <- "Adam and Eve Court"
    AE.pump <- cholera::pumps[cholera::pumps$street == sel, "id"]
    AE <- cholera::road.segments[cholera::road.segments$name == sel, "id"]

    if (all(x$pump.select > 0)) {
      if (AE.pump %in% x$pump.select == FALSE) {
        unobs.segments <- unobs.segments[unobs.segments %in% AE == FALSE]
      }
    } else if (all(x$pump < 0)) {
      if (AE.pump %in% abs(x$pump.select)) {
        unobs.segments <- unobs.segments[unobs.segments %in% AE == FALSE]
      }
    }
  }

  unobs.whole <- wholeSegments(unobs.segments, dat, edges, p.name, p.node, x)
  unobs.split.segments <- setdiff(unobs.segments, unlist(unobs.whole))

  if (length(unobs.split.segments) > 0) {
    if (x$dev.mode) {
      cl <- parallel::makeCluster(x$cores)

      parallel::clusterExport(cl = cl, envir = environment(),
        varlist = c("edges", "p.name", "p.node", "x", "splitSegments"))

      unobs.split.data <- parallel::parLapply(cl, unobs.split.segments,
        function(seg) splitSegments(seg, edges, p.name, p.node, x))

      parallel::stopCluster(cl)
    } else {
      unobs.split.data <- parallel::mclapply(unobs.split.segments,
        splitSegments, edges, p.name, p.node, x, mc.cores = x$cores)
    }

    cutpoints <- cutpointValues(unobs.split.data)
    unobs.split.pump <- lapply(unobs.split.data, function(x) unique(x$pump))
    unobs.split <- splitData(unobs.split.segments, cutpoints, edges)
  }

  if (x$vestry) {
    pumpID <- seq_len(nrow(cholera::pumps.vestry))
  } else {
    pumpID <- seq_len(nrow(cholera::pumps))
  }

  ## ----- data assembly ----- ##

  observed.wholes <- lapply(pumpID, function(nm) {
    c(obs.whole[[paste(nm)]], obs.partial.whole[[paste(nm)]])
  })

  names(observed.wholes) <- pumpID

  obs.split.test <- length(obs.partial.segments)

  if (obs.split.test > 0) {
    obs.splits <- obs.partial.split
    obs.splits.pump <- obs.partial.split.pump
    obs.splits.segs <- obs.partial.segments
  }

  expected.wholes <- lapply(pumpID, function(nm) {
    c(observed.wholes[[nm]], unobs.whole[[paste(nm)]])
  })

  names(expected.wholes) <- pumpID

  obs.split.test <- length(obs.partial.segments)
  unobs.split.test <- length(unobs.split.segments)

  if (obs.split.test > 0 & unobs.split.test == 0) {
    exp.splits <- obs.partial.split
    exp.splits.pump <- obs.partial.split.pump
    exp.splits.segs <- obs.partial.segments
  } else if (obs.split.test == 0 & unobs.split.test > 0) {
    exp.splits <- unobs.split
    exp.splits.pump <- unobs.split.pump
    exp.splits.segs <- unobs.split.segments
  } else if (obs.split.test > 0 & unobs.split.test > 0) {
    exp.splits <- c(obs.partial.split, unobs.split)
    exp.splits.pump <- c(obs.partial.split.pump, unobs.split.pump)
    exp.splits.segs <- c(obs.partial.segments, unobs.split.segments)
  } else {
    exp.splits <- NULL
    exp.splits.pump <- NULL
    exp.splits.segs <- NULL
  }

  list(observed.wholes = observed.wholes,
       expected.wholes = expected.wholes,
       obs.split.test = obs.split.test,
       unobs.split.test = unobs.split.test,
       obs.splits = exp.splits,
       obs.splits.pump = exp.splits.pump,
       obs.splits.segs = exp.splits.segs,
       exp.splits = exp.splits,
       exp.splits.pump = exp.splits.pump,
       exp.splits.segs = exp.splits.segs)
}

neighborhoodPathData <- function(x) {
  if (class(x) != "walking") {
    stop('"x"\'s class needs to be "walking".')
  }

  dat <- neighborhoodData(vestry = x$vestry, case.set = "observed")
  edges <- dat$edges
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

  if (x$dev.mode) {
    neighborhood.path.edges <- lapply(x$paths, function(neigh) {
      lapply(neigh, auditEdge, edges)
    })
  } else {
    neighborhood.path.edges <- parallel::mclapply(x$paths, function(neigh) {
      lapply(neigh, auditEdge, edges)
    }, mc.cores = x$cores)
  }

  list(dat = dat,
       edges = edges,
       p.node = p.node,
       p.name = p.name,
       neighborhood.path.edges = neighborhood.path.edges)
}

expectedCount <- function(x) {
  OE <- observedExpected(x, neighborhoodPathData(x))
  wholes <- OE$expected.wholes
  splits <- OE$exp.splits
  splits.pump <- OE$exp.splits.pump
  splits.segs <- OE$exp.splits.segs

  sim.proj <- cholera::sim.ortho.proj
  sim.proj.segs <- unique(sim.proj$road.segment)

  snow.colors <- snowColors(x$vestry)

  if (OE$obs.split.test > 0 | OE$unobs.split.test > 0) {
    split.outcome <- splitOutcomes(x, splits.segs, sim.proj, splits,
      splits.pump)
    split.outcome <- do.call(rbind, split.outcome)
    split.outcome <- split.outcome[!is.na(split.outcome$pump), ]
    split.cases <- lapply(sort(unique(split.outcome$pump)), function(p) {
      split.outcome[split.outcome$pump == p, "case"]
    })
    names(split.cases) <- sort(unique(split.outcome$pump))
  } else split.cases <- NULL

  ap <- areaPointsData(sim.proj.segs, wholes, snow.colors, sim.proj,
    split.cases)

  split.count <- table(ap$sim.proj.splits$pump)
  whole.count <- table(ap$sim.proj.wholes$pump)

  split.count <- data.frame(pump = as.numeric(names(split.count)),
                            count = unclass(split.count),
                            stringsAsFactors = FALSE)
  whole.count <- data.frame(pump = as.numeric(names(whole.count)),
                            count = unclass(whole.count),
                            stringsAsFactors = FALSE)

  count.data <- merge(whole.count, split.count, by = "pump", all.x = TRUE)
  count.data[is.na(count.data)] <- 0
  stats::setNames(count.data$count.x + count.data$count.y,
    paste0("p", count.data$pump))
}

splitOutcomes <- function(x, splits.segs, sim.proj, splits, splits.pump) {
  split_outcomes <- function(i, splits.segs, sim.proj, splits, splits.pump) {
    id <- sim.proj$road.segment == splits.segs[i] &
          is.na(sim.proj$road.segment) == FALSE
    sim.data <- sim.proj[id, ]
    split.data <- splits[[i]]
    sel <- vapply(seq_len(nrow(sim.data)), function(j) {
      obs <- sim.data[j, c("x.proj", "y.proj")]
      distance <- vapply(seq_len(nrow(split.data)), function(k) {
        stats::dist(matrix(c(obs, split.data[k, ]), 2, 2, byrow = TRUE))
      }, numeric(1L))
      test1 <- signif(sum(distance[1:2])) ==
        signif(c(stats::dist(split.data[c(1, 2), ])))
      test2 <- signif(sum(distance[3:4])) ==
        signif(c(stats::dist(split.data[c(3, 4), ])))
      ifelse(any(c(test1, test2)), which(c(test1, test2)), NA)
    }, integer(1L))

    data.frame(case = sim.data$case, pump = splits.pump[[i]][sel])
  }

  if (x$dev.mode) {
    cl <- parallel::makeCluster(x$cores)
    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = c("splits.segs", "sim.proj", "splits", "splits.pump",
      "split_outcomes"))
    output <- parallel::parLapply(cl, seq_along(splits.segs), function(i) {
      split_outcomes(i, splits.segs, sim.proj, splits, splits.pump)
    })
    parallel::stopCluster(cl)
  } else {
    output <- parallel::mclapply(seq_along(splits.segs), function(i) {
      split_outcomes(i, splits.segs, sim.proj, splits, splits.pump)
    }, mc.cores = x$cores)
  }

  output
}
