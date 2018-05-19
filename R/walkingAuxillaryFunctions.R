#' Auxillary functions for walking() and walkingPath().
#'
#' @noRd

walkingAuxillaryFunctions <- function() NULL

## Functions ##

auditEdge <- function(p, edges) {
  vapply(seq_along(p[-1]), function(i) {
    ab <- edges$node1 %in% p[i] &
          edges$node2 %in% p[i + 1]
    ba <- edges$node2 %in% p[i] &
          edges$node1 %in% p[i + 1]
    which(ab | ba)
  }, numeric(1L))
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


pearlStringRadius <- function() {
  c(stats::dist(cholera::regular.cases[c(1, 3), ]))
}

# remove observations with neighbors at each of the 4 cardinal directions
peripheryCases <- function(n.points, radius = pearlStringRadius()) {
  n.area <- cholera::regular.cases[n.points, ]
  periphery.test <- vapply(seq_len(nrow(n.area)), function(i) {
    case.point <- n.area[i, ]

    N <- signif(case.point$x) == signif(n.area$x) &
         signif(case.point$y + radius) == signif(n.area$y)

    E <- signif(case.point$x + radius) == signif(n.area$x) &
         signif(case.point$y) == signif(n.area$y)

    S <- signif(case.point$x) == signif(n.area$x) &
         signif(case.point$y - radius) == signif(n.area$y)

    W <- signif(case.point$x - radius) == signif(n.area$x) &
         signif(case.point$y) == signif(n.area$y)

    sum(c(N, E, S, W)) == 4
  }, logical(1L))

  row.names(n.area[which(periphery.test == FALSE), ])
}

# sort points on periphery to form a concave hull
pearlString <- function(vertices, radius = pearlStringRadius(),
  orientation = "clockwise") {

  dat <- cholera::regular.cases[vertices, ]
  dat <- dat[order(dat$y), ] # set southern most point as first observation.
  pearl.string <- vector(mode = "character", length = length(vertices))
  pearl.string[1] <- row.names(dat[1, ])

  for (j in 2:length(pearl.string)) {
    added.pearls <- pearl.string[pearl.string != ""]
    ego.case <- added.pearls[length(added.pearls)]
    alter.sel <- row.names(dat) %in% added.pearls == FALSE
    alters <- dat[alter.sel, ]

    N  <- signif(alters$x) == signif(dat[ego.case, "x"]) &
          signif(alters$y) == signif(dat[ego.case, "y"] + radius)

    NE <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
          signif(alters$y) == signif(dat[ego.case, "y"] + radius)

    E  <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
          signif(alters$y) == signif(dat[ego.case, "y"])

    SE <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
          signif(alters$y) == signif(dat[ego.case, "y"] - radius)

    S  <- signif(alters$x) == signif(dat[ego.case, "x"]) &
          signif(alters$y) == signif(dat[ego.case, "y"] - radius)

    SW <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
          signif(alters$y) == signif(dat[ego.case, "y"] - radius)

    W  <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
          signif(alters$y) == signif(dat[ego.case, "y"])

    NW <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
          signif(alters$y) == signif(dat[ego.case, "y"] + radius)

    master.list <- list(N = N, NE = NE, E = E, SE = SE, S = S, SW = SW, W = W,
      NW = NW)

    if (j > 2) {
      clockwise.compass <- lapply(-seq_len(length(master.list)), function(i) {
        vec <- names(master.list)[i]
        if (abs(i) == 1 | abs(i) == length(master.list)) vec
        else vec[c(abs(i):length(vec), 1:(abs(i) - 1))]
      })

      counterclockwise.compass <- lapply(clockwise.compass, rev)
      names(clockwise.compass) <- names(master.list)
      names(counterclockwise.compass) <- names(master.list)

      if (orientation == "clockwise") {
        compass <- clockwise.compass
      } else if (orientation == "counterclockwise") {
        compass <- counterclockwise.compass
      }

      delta <- dat[ego.case, ] - dat[added.pearls[(length(added.pearls) - 1)], ]

      if (delta$x == 0 & delta$y < 0) {
        lst <- compass["N"]  # Prev: North
      } else if (delta$x < 0 & delta$y < 0) {
        lst <- compass["NE"] # Prev: North-East
      } else if (delta$x < 0 & delta$y == 0) {
        lst <- compass["E"]  # Prev: East
      } else if (delta$x < 0 & delta$y > 0) {
        lst <- compass["SE"] # Prev: South-East
      } else if (delta$x == 0 & delta$y > 0) {
        lst <- compass["S"]  # Prev: South
      } else if (delta$x > 0 & delta$y > 0) {
        lst <- compass["SW"] # Prev: South-West
      } else if (delta$x > 0 & delta$y == 0) {
        lst <- compass["W"]  # Prev: West
      } else if (delta$x > 0 & delta$y < 0) {
        lst <- compass["NW"] # Prev: North-West
      }

      candidates <- vapply(master.list, any, logical(1L))[unlist(lst)]

      # Exception to consider second-order candidates for pearl string.
      if (all(candidates == FALSE)) {
        n   <- signif(alters$x) == signif(dat[ego.case, "x"]) &
               signif(alters$y) == signif(dat[ego.case, "y"] + 2 * radius)

        nne <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + 2 * radius)

        ne  <- signif(alters$x) == signif(dat[ego.case, "x"] + 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + 2 * radius)

        ene <- signif(alters$x) == signif(dat[ego.case, "x"] + 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + radius)

        e   <- signif(alters$x) == signif(dat[ego.case, "x"] + 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"])

        ese <- signif(alters$x) == signif(dat[ego.case, "x"] + 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - radius)

        se  <- signif(alters$x) == signif(dat[ego.case, "x"] + 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - 2 * radius)

        sse <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - 2 * radius)

        s   <- signif(alters$x) == signif(dat[ego.case, "x"]) &
               signif(alters$y) == signif(dat[ego.case, "y"] - 2 * radius)

        ssw <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - 2 * radius)

        sw  <- signif(alters$x) == signif(dat[ego.case, "x"] - 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - 2 * radius)

        wsw <- signif(alters$x) == signif(dat[ego.case, "x"] - 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - radius)

        w   <- signif(alters$x) == signif(dat[ego.case, "x"] - 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"])

        wnw <- signif(alters$x) == signif(dat[ego.case, "x"] - 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + radius)

        nw  <- signif(alters$x) == signif(dat[ego.case, "x"] - 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + 2 * radius)

        nnw <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + 2 * radius)

        # closest second order neighbors
        master.listB <- list(n = n, e = e, s = s, w = w,
                             nne = nne, ene = ene,
                             ese = ese, sse = sse,
                             ssw = ssw, wsw = wsw,
                             wnw = wnw, nnw = nnw,
                             ne = ne, se = se, sw = sw, nw = nw)

        idx <- -seq_len(length(master.listB))

        clockwise.compassB <- lapply(idx, function(i) {
          vec <- names(master.listB)[i]
          if (abs(i) == 1 | abs(i) == length(master.listB)) vec
          else vec[c(abs(i):length(vec), 1:(abs(i) - 1))]
        })

        counterclockwise.compassB <- lapply(clockwise.compassB, rev)
        names(clockwise.compassB) <- names(master.listB)
        names(counterclockwise.compassB) <- names(master.listB)

        if (orientation == "clockwise") {
          compassB <- clockwise.compassB
        } else if (orientation == "counterclockwise") {
          compassB <- counterclockwise.compassB
        }

        # increment by one compass point
        if (delta$x == 0 & delta$y < 0) {
          lstB <- compassB["n"]  # Prev: North
        } else if (delta$x < 0 & delta$y < 0) {
          lstB <- compassB["ne"] # Prev: North-East
        } else if (delta$x < 0 & delta$y == 0) {
          lstB <- compassB["e"]  # Prev: East
        } else if (delta$x < 0 & delta$y > 0) {
          lstB <- compassB["se"] # Prev: South-East
        } else if (delta$x == 0 & delta$y > 0) {
          lstB <- compassB["s"]  # Prev: South
        } else if (delta$x > 0 & delta$y > 0) {
          lstB <- compassB["sw"] # Prev: South-West
        } else if (delta$x > 0 & delta$y == 0) {
          lstB <- compassB["w"]  # Prev: West
        } else if (delta$x > 0 & delta$y < 0) {
          lstB <- compassB["nw"] # Prev: North-West
        }

        candidatesB <- vapply(master.listB, any, logical(1L))[unlist(lstB)]
        sel <- which(get(names(which(candidatesB)[1])))

      } else {
        sel <- which(get(names(which(candidates)[1])))
      }

    } else {
      candidates <- vapply(master.list, any, logical(1L))

      if (orientation == "clockwise") {
        second.pearl <- vapply(c("W", "NW", "N", "NE"), function(x) {
          x %in% names(candidates[candidates])
        }, logical(1L))
      } else if (orientation == "counterclockwise") {
        second.pearl <- vapply(c("E", "NE", "N", "NW"), function(x) {
          x %in% names(candidates[candidates])
        }, logical(1L))
      }

      sel <- which(get(names(second.pearl[second.pearl])))
    }
    pearl.string[j] <- row.names(alters[sel, ])
  }
  pearl.string
}

pumpTokens <- function(pump.select, vestry, case.set, snow.colors, type) {
  if (vestry) {
    dat <- cholera::pumps.vestry
  } else {
    dat <- cholera::pumps
  }

  if (case.set == "observed") {
    if (is.null(pump.select)) {
      points(dat[, c("x", "y")], pch = 24, lwd = 1.25, col = snow.colors)
      text(dat[, c("x", "y")], pos = 1, cex = 0.9, labels = paste0("p", dat$id))
    } else {
      if (all(pump.select > 0)) {
        sel <- dat$id %in% pump.select
      } else if (all(pump.select < 0)) {
        sel <- dat$id %in% abs(pump.select) == FALSE
      }
      points(dat[sel, c("x", "y")], pch = 24, lwd = 1.25,
        col = snow.colors[sel])
      text(dat[sel, c("x", "y")], pos = 1, cex = 0.9,
        labels = paste0("p", dat$id[sel]))
    }

  } else if (case.set == "expected") {
    if (type == "road") {
      if (is.null(pump.select)) {
        points(dat[, c("x", "y")], pch = 24, lwd = 1.25, bg = snow.colors)
        text(dat[, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id))
      } else {
        if (all(pump.select > 0)) {
          sel <- dat$id %in% pump.select
        } else if (all(pump.select < 0)) {
          sel <- dat$id %in% abs(pump.select) == FALSE
        }
        points(dat[sel, c("x", "y")], pch = 24, lwd = 1.25,
          bg = snow.colors[sel])
        text(dat[sel, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id[sel]))
      }

    } else if (type %in% c("area.points", "area.polygons")) {
      if (is.null(pump.select)) {
        points(dat[, c("x", "y")], pch = 24, lwd = 1.25,
          col = "white", bg = snow.colors)
        text(dat[, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id))
      } else {
        if (all(pump.select > 0)) {
          sel <- dat$id %in% pump.select
        } else if (all(pump.select < 0)) {
          sel <- dat$id %in% abs(pump.select) == FALSE
        }
        points(dat[sel, c("x", "y")], pch = 24, lwd = 1.25,
          col = "white", bg = snow.colors[sel])
        text(dat[sel, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id[sel]))
      }
    }
  }
}

expectedCount <- function(x) {
  arguments <- list(pump.select = x$pump.select,
                    vestry = x$vestry,
                    weighted = x$weighted,
                    case.set = "expected",
                    multi.core = x$cores)

  nearest.pump <- do.call("nearestPump", c(arguments))
  nearest.pump <- nearest.pump[is.infinite(nearest.pump$distance) == FALSE, ]
  out <- table(nearest.pump$pump)
  stats::setNames(as.vector(out), names(out))
}
