#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Exclusion (negative selection) is possible (e.g., -6). Note that you can't just select the pump on Adam and Eve Court (#2): it's a technical isolate.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path weighted by road length. FALSE computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected" or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @return An R list with 7 objects:
#' \itemize{
#'   \item{\code{paths}: list of paths to nearest or selected pump(s).}
#'   \item{\code{cases}: list of cases by pump.}
#'   \item{\code{vestry}: "vestry" from neighborhoodWalking().}
#'   \item{\code{observed}: "observed" from neighborhoodWalking().}
#'   \item{\code{pump.select}: "pump.select" from neighborhoodWalking().}
#'   \item{\code{cores}: number of cores to use for parallel implementation.}
#'   \item{\code{metric}: incremental metric used to find cut point on split road segments.}
#' }
#' @section Note: This function is computationally intensive. On a single core of a 2.3 GHz Intel i7, plotting observed paths to PDF takes about 6 seconds while doing so for expected paths takes about 30 seconds. Using the parallel implementation on 4 physical (8 logical) cores, these times fall to about 4 and 12 seconds. Note that parallelization is currently only available on Linux and Mac, and that although some precautions are taken in R.app on macOS, the developers of the 'parallel' package, which neighborhoodWalking() uses, strongly discourage against using parallelization within a GUI or embedded environment. See vignette("parallel") for details.
#' @export
#' @examples
#' # neighborhoodWalking()
#' # neighborhoodWalking(pump.select = -6)

neighborhoodWalking <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, case.set = "observed", multi.core = FALSE) {

  if (is.null(pump.select) == FALSE) {
    if (length(pump.select) == 1) {
      if (pump.select == 2) {
        msg1 <- "You can't just select the pump on Adam and Eve Court (#2).\n"
        msg2 <- " It's an isolate, unreachable for observed fatalities."
        stop(paste(msg1, msg2))
      }
    }
    if (vestry) {
      if (any(abs(pump.select) %in% 1:14 == FALSE)) {
        stop('With "vestry = TRUE", 1 >= |"pump.select"| <= 14')
      }
    } else {
      if (any(abs(pump.select) %in% 1:13 == FALSE)) {
        stop('With "vestry = FALSE", 1 >= |"pump.select"| <= 13')
      }
    }
  }

  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('"case.set" must be "observed", "expected" or "snow".')
  }

  cores <- multiCore(multi.core)

  if (case.set == "expected") {
    arguments <- list(pump.select = pump.select,
                      vestry = vestry,
                      weighted = weighted,
                      case.set = "observed",
                      multi.core = cores)

  } else {
    arguments <- list(pump.select = pump.select,
                      vestry = vestry,
                      weighted = weighted,
                      case.set = case.set,
                      multi.core = cores)
  }

  nearest.path <- do.call("nearestPump", c(arguments, output = "path"))

  if (vestry) {
    nearest.pump <- vapply(nearest.path, function(paths) {
      sel <- cholera::ortho.proj.pump.vestry$node %in% paths[length(paths)]
      cholera::ortho.proj.pump.vestry[sel, "pump.id"]
    }, numeric(1L))
  } else {
    nearest.pump <- vapply(nearest.path, function(paths) {
      sel <- cholera::ortho.proj.pump$node %in% paths[length(paths)]
      cholera::ortho.proj.pump[sel, "pump.id"]
    }, numeric(1L))
  }

  if (case.set == "snow") {
    snow.anchors <- cholera::snow.neighborhood[cholera::snow.neighborhood %in%
      cholera::fatalities.address$anchor.case]
    nearest.pump <- data.frame(case = snow.anchors,
                               pump = nearest.pump)
  } else {
    nearest.pump <- data.frame(case = cholera::fatalities.address$anchor.case,
                               pump = nearest.pump)
  }

  pumpID <- sort(unique(nearest.pump$pump))

  neighborhood.cases <- lapply(pumpID, function(p) {
    nearest.pump[nearest.pump$pump == p, "case"]
  })

  names(neighborhood.cases) <- pumpID

  neighborhood.paths <- lapply(pumpID, function(p) {
    n.case <- neighborhood.cases[[paste(p)]]
    nearest.path[which(nearest.pump$case %in% n.case)]
  })

  names(neighborhood.paths) <- pumpID

  out <- list(paths = neighborhood.paths,
              cases = neighborhood.cases,
              vestry = vestry,
              weighted = weighted,
              case.set = case.set,
              pump.select = pump.select,
              cores = cores,
              metric = 1 / cholera::unitMeter(1, "meter"))

  class(out) <- "walking"
  out
}

#' Print method for neighborhoodWalking().
#'
#' Return count of paths (anchor cases) by pump neighborhood.
#' @param x An object of class "walking" created by neighborhoodWalking().
#' @param ... Additional parameters.
#' @return An R vector.
#' @section Note: Printing an object with case.set = "expected" is computationally intensive (you are computing the paths of almost 5000 cases): on a 2.3 GHz Intel i7 this takes approximately 98 seconds on single core and 49 seconds on 4 physical (8 logical) cores.
#' @export
#' @examples
#' # neighborhoodWalking()
#' # print(neighborhoodWalking())

print.walking <- function(x, ...) {
  if (class(x) != "walking") {
    stop('"x"\'s class needs to be "walking".')
  }

  if (x$case.set == "observed" | x$case.set == "snow") {
    out <- vapply(x$paths, length, numeric(1L))
  } else if (x$case.set == "expected") {
    out <- expectedCount(x)
  }
  print(out)
}

#' Plot method for neighborhoodWalking().
#'
#' @param x An object of class "walking" created by neighborhoodWalking().
#' @param type Character. "road", "area.points" or "area.polygons". "area" flavors only valid when case.set = "expected".
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @section Note: "area.polygons" is under development and, for certain configurations, may return an error. For now, "area.points" is the more robust choice, especially for data exploration.
#' @export
#' @examples
#' # plot(neighborhoodWalking())
#' # plot(neighborhoodWalking(case.set = "expected"))
#' # plot(neighborhoodWalking(case.set = "expected"), type = "area.points")
#' # plot(neighborhoodWalking(case.set = "expected"), type = "area.polygons")

plot.walking <- function(x, type = "road", ...) {
  if (class(x) != "walking") {
    stop('"x"\'s class needs to be "walking".')
  }

  if (type %in% c("road", "area.points", "area.polygons") == FALSE) {
    stop('"type" must be "road", "area.points", "area.polygons".')
  }

  if (type %in% c("area.points", "area.polygons")) {
    if (x$case.set != "expected") {
      stop('area plots valid only when case.set = "expected".')
    }
  }

  ## Functions ##

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

  ## ------------ Plot ------------ ##

  snow.colors <- cholera::snowColors(x$vestry)

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  road.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  plot(cholera::fatalities[, c("x", "y")], xlim = x.range, ylim = y.range,
    pch = NA, asp = 1)
  invisible(lapply(border.list, lines))

  if (x$case.set == "observed") {
    invisible(lapply(road.list, lines, col = "gray"))

    edge.data <- lapply(n.path.edges, function(x) unique(unlist(x)))

    invisible(lapply(names(edge.data), function(nm) {
      n.edges <- edges[edge.data[[nm]], ]
      segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 2,
        col = snow.colors[paste0("p", nm)])
    }))

    invisible(lapply(names(x$cases), function(nm) {
      sel <- cholera::fatalities.address$anchor.case %in% x$cases[[nm]]
      points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
        cex = 0.75, col = snow.colors[paste0("p", nm)])
    }))

  } else if (x$case.set == "snow") {
    invisible(lapply(road.list, lines, col = "gray"))

    obs.whole.edges <- lapply(n.path.edges, function(x) {
      edges[unique(unlist(x)), "id2"]
    })

    invisible(lapply(names(obs.whole.edges), function(nm) {
      n.edges <- edges[edges$id2 %in% obs.whole.edges[[nm]], ]
      segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 4,
        col = snow.colors[paste0("p", nm)])
    }))

  } else if (x$case.set == "expected") {
    obs.segment.count <- lapply(n.path.edges, function(x) {
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

    ## ------------ Observed ------------ ##

    # list of whole traversed segments
    obs.whole <- lapply(segment.audit, function(x) x$`whole`)

    # list of partially traversed segments
    obs.partial <- lapply(segment.audit, function(x) x$`partial`)
    partial.segs <- unname(unlist(obs.partial))
    obs.partial.whole <- wholeSegments(partial.segs)

    # list of of split segments (lead to different pumps)
    # the cutpoint is found using appox. 1 meter increments via cutpointValues()
    obs.partial.segments <- setdiff(partial.segs, unlist(obs.partial.whole))

    if (length(obs.partial.segments) > 0) {
      obs.partial.split.data <- parallel::mclapply(obs.partial.segments,
        splitSegments, mc.cores = x$cores)
      cutpoints <- cutpointValues(obs.partial.split.data)
      obs.partial.split.pump <- lapply(obs.partial.split.data, function(x)
        unique(x$pump))
      obs.partial.split <- splitData(obs.partial.segments, cutpoints)
    }

    ## ------------ Unobserved ------------ ##

    # list of edges that are wholly or partially traversed
    obs.segments <- lapply(n.path.edges, function(x) {
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

    unobs.whole <- wholeSegments(unobs.segments)
    unobs.split.segments <- setdiff(unobs.segments, unlist(unobs.whole))

    if (length(unobs.split.segments) > 0) {
      unobs.split.data <- parallel::mclapply(unobs.split.segments,
        splitSegments, mc.cores = x$cores)
      cutpoints <- cutpointValues(unobs.split.data)
      unobs.split.pump <- lapply(unobs.split.data, function(x) unique(x$pump))
      unobs.split <- splitData(unobs.split.segments, cutpoints)
    }

    ## ------------ Data Assembly ------------ ##

    if (x$vestry) {
      wholes <- lapply(1:14, function(nm) {
        c(obs.whole[[paste(nm)]],
          unobs.whole[[paste(nm)]],
          obs.partial.whole[[paste(nm)]])
      })
      names(wholes) <- 1:14
    } else {
      wholes <- lapply(1:13, function(nm) {
        c(obs.whole[[paste(nm)]],
          unobs.whole[[paste(nm)]],
          obs.partial.whole[[paste(nm)]])
      })
      names(wholes) <- 1:13
    }

    # split segments #
    split.test1 <- length(obs.partial.segments)
    split.test2 <- length(unobs.split.segments)

    if (split.test1 > 0 & split.test2 == 0) {
      splits <- obs.partial.split
      splits.pump <- obs.partial.split.pump
      split.segs <- obs.partial.segments
    } else if (split.test1 == 0 & split.test2 > 0) {
      splits <- unobs.split
      splits.pump <- unobs.split.pump
      split.segs <- unobs.split.segments
    } else if (split.test1 > 0 & split.test2 > 0) {
      splits <- c(obs.partial.split, unobs.split)
      splits.pump <- c(obs.partial.split.pump, unobs.split.pump)
      split.segs <- c(obs.partial.segments, unobs.split.segments)
    }

    sim.proj <- simProj()  # in neighborhoodData.R
    sim.proj.segs <- unique(sim.proj$road.segment)
    sim.proj.segs <- sim.proj.segs[!is.na(sim.proj.segs)]

    if (split.test1 > 0 | split.test2 > 0) {
      split.outcome <- parallel::mclapply(seq_along(split.segs), function(i) {
        id <- sim.proj$road.segment == split.segs[i] &
          is.na(sim.proj$road.segment) == FALSE

        sim.data <- sim.proj[id, ]
        split.data <- splits[[i]]

        sel <- vapply(seq_len(nrow(sim.data)), function(j) {
          obs <- sim.data[j, c("x.proj", "y.proj")]
          ds <- vapply(seq_len(nrow(split.data)), function(k) {
            stats::dist(matrix(c(obs, split.data[k, ]), 2, 2, byrow = TRUE))
          }, numeric(1L))

          test1 <- signif(sum(ds[1:2])) ==
            signif(c(stats::dist(split.data[c(1, 2), ])))
          test2 <- signif(sum(ds[3:4])) ==
            signif(c(stats::dist(split.data[c(3, 4), ])))

          ifelse(any(c(test1, test2)), which(c(test1, test2)), NA)
        }, integer(1L))

        data.frame(case = sim.data$case, pump = splits.pump[[i]][sel])
      }, mc.cores = x$cores)

      split.outcome <- do.call(rbind, split.outcome)
      split.outcome <- split.outcome[!is.na(split.outcome$pump), ]
      split.cases <- lapply(sort(unique(split.outcome$pump)), function(p) {
        split.outcome[split.outcome$pump == p, "case"]
      })

      names(split.cases) <- sort(unique(split.outcome$pump))
    }

    if (type == "area.points") {
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

      points(cholera::regular.cases[sim.proj.wholes$case, ],
        col = sim.proj.wholes$color, pch = 15, cex = 1.25)
      points(cholera::regular.cases[sim.proj.splits$case, ],
        col = sim.proj.splits$color, pch = 15, cex = 1.25)
      invisible(lapply(road.list, lines))

    } else if (type == "area.polygons") {
      invisible(lapply(road.list, lines))

      # wholes #
      whole.cases <- lapply(names(wholes), function(nm) {
        sel <- sim.proj$road.segment %in% wholes[[nm]]
        cases <- sim.proj[sel, "case"]
        as.numeric(row.names(cholera::regular.cases[cases, ]))
      })

      names(whole.cases) <- names(wholes)

      pearl.neighborhood <- vapply(whole.cases, length, integer(1L))
      pearl.neighborhood <- names(pearl.neighborhood[pearl.neighborhood != 0])

      if (split.test1 | split.test2) {
        neighborhood.cases <- lapply(pearl.neighborhood, function(nm) {
          c(whole.cases[[nm]], split.cases[[nm]])
        })
      } else {
        neighborhood.cases <- lapply(pearl.neighborhood, function(nm) {
          whole.cases[[nm]]
        })
      }

      names(neighborhood.cases) <- pearl.neighborhood

      periphery.cases <- parallel::mclapply(neighborhood.cases, peripheryCases,
        mc.cores = x$cores)

      pearl.string <- parallel::mclapply(periphery.cases, pearlString,
        mc.cores = x$cores)

      invisible(lapply(names(pearl.string), function(nm) {
        sel <- paste0("p", nm)
        polygon(cholera::regular.cases[pearl.string[[nm]], ],
          col = grDevices::adjustcolor(snow.colors[sel], alpha.f = 2/3))
      }))

    } else {
      invisible(lapply(road.list, lines, col = "gray"))

      invisible(lapply(names(wholes), function(nm) {
        n.edges <- edges[edges$id %in% wholes[[nm]], ]
        segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 3,
          col = snow.colors[paste0("p", nm)])
      }))

      if (split.test1 | split.test2) {
        invisible(lapply(seq_along(splits), function(i) {
          dat <- splits[[i]]
          ps <- splits.pump[[i]]
          ps.col <- snow.colors[paste0("p", ps)]
          segments(dat[1, "x"], dat[1, "y"], dat[2, "x"], dat[2, "y"], lwd = 3,
            col = ps.col[1])
          segments(dat[3, "x"], dat[3, "y"], dat[4, "x"], dat[4, "y"], lwd = 3,
            col = ps.col[2])
        }))
      }
    }
  }

  pumpTokens(x$pump.select, x$vestry, x$case.set, snow.colors, type)
  title(main = "Pump Neighborhoods: Walking")
}

multiCore <- function(x) {
  if (is.logical(x)) {
    if (x) {
      cores <- parallel::detectCores()
    } else {
      if (is.numeric(x)) {
        if (is.integer(x)) {
          cores <- x
        } else {
          cores <- as.integer(x)
        }
      } else {
        cores <- 1L
      }
    }
  } else if (is.numeric(x)) {
    if (is.integer(x)) {
      cores <- x
    } else {
      cores <- as.integer(x)
    }
  }
  cores
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
  if (case.set == "observed") {
    if (is.null(pump.select)) {
      if (vestry) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 24, lwd = 1.25,
          col = cholera::snowColors(vestry = TRUE))
        text(cholera::pumps.vestry[, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", cholera::pumps.vestry$id))
      } else {
        points(cholera::pumps[, c("x", "y")], pch = 24, lwd = 1.25,
          col = cholera::snowColors())
        text(cholera::pumps[, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", cholera::pumps$id))
      }
    } else {
      if (all(pump.select > 0)) {
        if (vestry) {
          sel <- cholera::pumps.vestry$id %in% pump.select
          points(cholera::pumps.vestry[sel, c("x", "y")], pch = 24,
            lwd = 1.25, col = snow.colors[sel])
          text(cholera::pumps.vestry[sel, c("x", "y")], pos = 1, cex = 0.9,
            labels = paste0("p", cholera::pumps.vestry$id[sel]))
        } else {
          sel <- cholera::pumps$id %in% abs(pump.select)
          points(cholera::pumps[sel, c("x", "y")], pch = 24, lwd = 1.25,
            col = snow.colors[sel])
          text(cholera::pumps[sel, c("x", "y")], pos = 1, cex = 0.9,
            labels = paste0("p", cholera::pumps$id[sel]))
        }
      } else if (all(pump.select < 0)) {
        if (vestry) {
          sel <- cholera::pumps.vestry$id %in% abs(pump.select) == FALSE
          points(cholera::pumps.vestry[sel, c("x", "y")], pch = 24, lwd = 1.25,
            col = snow.colors[sel])
          text(cholera::pumps.vestry[sel, c("x", "y")], pos = 1, cex = 0.9,
            labels = paste0("p", cholera::pumps.vestry$id[sel]))

        } else {
          sel <- cholera::pumps$id %in% abs(pump.select) == FALSE
          points(cholera::pumps[sel, c("x", "y")], pch = 24, lwd = 1.25,
            col = snow.colors[sel])
          text(cholera::pumps[sel, c("x", "y")], pos = 1, cex = 0.9,
            labels = paste0("p", cholera::pumps$id[sel]))
        }
      }
    }
  } else if (case.set == "expected") {
    if (type == "road") {
      if (is.null(pump.select)) {
        if (vestry) {
          points(cholera::pumps.vestry[, c("x", "y")], pch = 24, lwd = 1.25,
            bg = cholera::snowColors(vestry = TRUE))
          text(cholera::pumps.vestry[, c("x", "y")], pos = 1, cex = 0.9,
            labels = paste0("p", cholera::pumps.vestry$id))
        } else {
          points(cholera::pumps[, c("x", "y")], pch = 24, lwd = 1.25,
            bg = cholera::snowColors())
          text(cholera::pumps[, c("x", "y")], pos = 1, cex = 0.9,
            labels = paste0("p", cholera::pumps$id))
        }
      } else {
        if (all(pump.select > 0)) {
          if (vestry) {
            sel <- cholera::pumps.vestry$id %in% pump.select
            points(cholera::pumps.vestry[sel, c("x", "y")], pch = 24,
              lwd = 1.25, bg = snow.colors[sel])
            text(cholera::pumps.vestry[sel, c("x", "y")], pos = 1, cex = 0.9,
              labels = paste0("p", cholera::pumps.vestry$id[sel]))
          } else {
            sel <- cholera::pumps$id %in% abs(pump.select)
            points(cholera::pumps[sel, c("x", "y")], pch = 24, lwd = 1.25,
              bg = snow.colors[sel])
            text(cholera::pumps[sel, c("x", "y")], pos = 1, cex = 0.9,
              labels = paste0("p", cholera::pumps$id[sel]))
          }
        } else if (all(pump.select < 0)) {
          if (vestry) {
            sel <- cholera::pumps.vestry$id %in% abs(pump.select) == FALSE
            points(cholera::pumps.vestry[sel, c("x", "y")], pch = 24,
              lwd = 1.25, bg = snow.colors[sel])
            text(cholera::pumps.vestry[sel, c("x", "y")], pos = 1, cex = 0.9,
              labels = paste0("p", cholera::pumps.vestry$id[sel]))

          } else {
            sel <- cholera::pumps$id %in% abs(pump.select) == FALSE
            points(cholera::pumps[sel, c("x", "y")], pch = 24, lwd = 1.25,
              bg = snow.colors[sel])
            text(cholera::pumps[sel, c("x", "y")], pos = 1, cex = 0.9,
              labels = paste0("p", cholera::pumps$id[sel]))
          }
        }
      }
    } else if (type %in% c("area.points", "area.polygons")) {
      if (is.null(pump.select)) {
        if (vestry) {
          points(cholera::pumps.vestry[, c("x", "y")], pch = 24, lwd = 1.25,
            col = "white", bg = cholera::snowColors(vestry = TRUE))
          text(cholera::pumps.vestry[, c("x", "y")], pos = 1, cex = 0.9,
            labels = paste0("p", cholera::pumps.vestry$id))
        } else {
          points(cholera::pumps[, c("x", "y")], pch = 24, lwd = 1.25,
            col = "white", bg = cholera::snowColors())
          text(cholera::pumps[, c("x", "y")], pos = 1, cex = 0.9,
            labels = paste0("p", cholera::pumps$id))
        }
      } else {
        if (all(pump.select > 0)) {
          if (vestry) {
            sel <- cholera::pumps.vestry$id %in% pump.select
            points(cholera::pumps.vestry[sel, c("x", "y")], pch = 24,
              lwd = 1.25, col = "white", bg = snow.colors[sel])
            text(cholera::pumps.vestry[sel, c("x", "y")], pos = 1, cex = 0.9,
              labels = paste0("p", cholera::pumps.vestry$id[sel]))
          } else {
            sel <- cholera::pumps$id %in% abs(pump.select)
            points(cholera::pumps[sel, c("x", "y")], pch = 24, lwd = 1.25,
              col = "white", bg = snow.colors[sel])
            text(cholera::pumps[sel, c("x", "y")], pos = 1, cex = 0.9,
              labels = paste0("p", cholera::pumps$id[sel]))
          }
        } else if (all(pump.select < 0)) {
          if (vestry) {
            sel <- cholera::pumps.vestry$id %in% abs(pump.select) == FALSE
            points(cholera::pumps.vestry[sel, c("x", "y")], pch = 24,
              lwd = 1.25, col = "white", bg = snow.colors[sel])
            text(cholera::pumps.vestry[sel, c("x", "y")], pos = 1, cex = 0.9,
              labels = paste0("p", cholera::pumps.vestry$id[sel]))
          } else {
            sel <- cholera::pumps$id %in% abs(pump.select) == FALSE
            points(cholera::pumps[sel, c("x", "y")], pch = 24, lwd = 1.25,
              col = "white", bg = snow.colors[sel])
            text(cholera::pumps[sel, c("x", "y")], pos = 1, cex = 0.9,
              labels = paste0("p", cholera::pumps$id[sel]))
          }
        }
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
