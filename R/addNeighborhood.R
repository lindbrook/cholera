#' Add expected neighborhood polygons
#'
#' In-development prototype.
#' @param pump.subset Numeric. Vector of pumps to select (subset) from neighborhoods defined by "pump.select". Negative selection possible. NULL selects all pumps in "pump.select".
#' @param pump.select Numeric. Numeric vector of pumps to define pump neighborhoods (i.e. the "population"). Negative selection possible. NULL selects all pumps.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path weighted by road length. FALSE computes shortest path in terms of the number of nodes.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @param type Character. Type of annotation plot: "area", "border" or "street".
#' @param color Character. Neighborhood color.
#' @param alpha.level Numeric. Alpha level transparency: a value in [0, 1].
#' @param ... Additional plotting parameters.
#' @seealso \code{\link{snowMap}},
#' \code{\link{addIndexCase}},
#' \code{\link{addKernelDensity}},
#' \code{\link{addLandmarks}},
#' \code{\link{addPlaguePit}},
#' \code{\link{addVoronoi}},
#' \code{\link{addWhitehead}}
#' @import graphics
#' @section Note: This function is computationally intensive. On a single core of a 2.3 GHz Intel i7, plotting observed paths to PDF takes about 30 seconds. Using the parallel implementation on 4 physical (8 logical) cores, this time falls to about 12 seconds. Note that parallelization is currently only available on Linux and Mac, and that although some precautions are taken in R.app on macOS, the developers of the 'parallel' package, which neighborhoodWalking() uses, strongly discourage against using parallelization within a GUI or embedded environment. See vignette("parallel") for details.
#' @export
#' @examples
#' # streetNameLocator("marshall street", zoom = TRUE, highlight = FALSE,
#' #   unit = "meter", cases = NULL)
#' # addNeighborhood(6:7)

addNeighborhood <- function(pump.subset = NULL, pump.select = NULL,
  vestry = FALSE, weighted = TRUE, multi.core = FALSE, type = "area",
  color = NULL, alpha.level = 0.125, ...) {

  cores <- multiCore(multi.core)

  arguments <- list(pump.select = pump.select,
                    vestry = vestry,
                    weighted = weighted,
                    case.set = "observed",
                    multi.core = cores)

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

  nearest.pump <- data.frame(case = cholera::fatalities.address$anchor.case,
                             pump = nearest.pump)

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

  x <- list(paths = neighborhood.paths,
            cases = neighborhood.cases,
            vestry = vestry,
            weighted = weighted,
            case.set = arguments$case.set,
            pump.select = pump.select,
            cores = cores,
            metric = 1 / cholera::unitMeter(1, "meter"))

  snow.colors <- cholera::snowColors(x$vestry)

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

  ##

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

  if (is.null(pump.subset)) {
    invisible(lapply(names(pearl.string), function(nm) {
      sel <- paste0("p", nm)
      polygon(cholera::regular.cases[pearl.string[[nm]], ],
        col = grDevices::adjustcolor(snow.colors[sel], alpha.f = alpha.level))
    }))

  } else {
    n.subset <- pearl.string[pump.subset]

    invisible(lapply(names(n.subset), function(nm) {
      sel <- paste0("p", nm)
      polygon(cholera::regular.cases[pearl.string[[nm]], ],
        col = grDevices::adjustcolor(snow.colors[sel], alpha.f = alpha.level))
    }))
  }
}
