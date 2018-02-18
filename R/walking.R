#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Exclusion (negative selection) is possible (e.g., -6). Note that you can't just select the pump on Adam and Eve Court (#2): it's a technical isolate.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path weighted by road length. FALSE computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected", or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
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
#' @section Note: This function is computationally intensive. On a single core of a 2.3 GHz Intel i7, plotting observed paths takes about 8 seconds while plotting expected paths takes about 35 seconds. With the parallel implementation (currently only available on Linux and Mac and which the developer strongly discourages against using in a GUI or embedded environment), these times fall to 6 and 15 seconds using 4 physical (8 logical) cores.
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

  if (case.set == "snow" & is.null(pump.select) == FALSE) {
    warning('With case.set = "snow", "pump.select" is ignored.')
    pump.select <- NULL
  }

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
               case.set = case.set,
               multi.core = cores)

  nearest.path <- do.call("nearestPump", c(args, output = "path"))
  nearest.pump <- do.call("nearestPump", c(args, output = "distance"))

  # Falconberg Court and Falconberg Mews isolate
  if (case.set == "expected") {
    falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
    sel <- cholera::sim.ortho.proj$road.segment %in% falconberg.ct.mews
    falconberg.cases <- cholera::sim.ortho.proj[sel, "case"]

    sel <- nearest.pump$case %in% falconberg.cases
    nearest.path <- nearest.path[sel == FALSE]
    nearest.pump <- nearest.pump[sel == FALSE, ]

    names(nearest.path) <- nearest.pump$case
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
#' @export
#' @examples
#' # neighborhoodWalking()
#' # print(neighborhoodWalking())

print.walking <- function(x, ...) {
  if (class(x) != "walking") {
    stop('"x"\'s class needs to be "walking".')
  }
   out <- vapply(x$paths, length, numeric(1L))
   print(out)
}

#' Plot method for neighborhoodWalking().
#'
#' @param x An object of class "walking" created by neighborhoodWalking().
#' @param area Logical. TRUE returns area plot. FALSE returns walking paths plot. Works only with case.set = "expected" or case.set = "snow".
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @examples
#' # plot(neighborhoodWalking())
#' # plot(neighborhoodWalking(case.set = "expected"))
#' # plot(neighborhoodWalking(case.set = "expected"), area = TRUE)

plot.walking <- function(x, area = FALSE, ...) {
  if (class(x) != "walking") {
    stop('"x"\'s class needs to be "walking".')
  }

  if (area) {
    if (all(x$case.set %in% c("expected", "snow") == FALSE)) {
      stop('"area = TRUE" is valid only when case.set is "expected" or "snow".')
    }
  }

  dat <- neighborhoodData(vestry = x$vestry, case.set = x$case.set)
  edges <- dat$edges
  nodes <- dat$nodes

  ## Check if path spans entire segment (traverse both endpoints).
  auditEdge <- function(p) {
    vapply(seq_along(p[-1]), function(i) {
      ab <- edges$node1 %in% p[i] &
            edges$node2 %in% p[i + 1]
      ba <- edges$node2 %in% p[i] &
            edges$node1 %in% p[i + 1]
      which(ab | ba)
    }, numeric(1L))
  }

  n.path.edges <- parallel::mclapply(x$paths, function(neighborhood) {
    lapply(neighborhood, auditEdge)
  }, mc.cores = x$cores)

  whole.edges <- lapply(n.path.edges, function(x) {
    edges[unique(unlist(x)), "id2"]
  })

  ## expected computation

  if (x$case.set == "expected") {
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

    leftover.edges <- edges[setdiff(edgesID, unname(unlist(whole.edges))), ]

    falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")

    leftover.edges <- leftover.edges[leftover.edges$id %in%
      falconberg.ct.mews == FALSE, "id2"]

    checkEndpoints <- function(e) {
      e.data <- edges[edges$id2 == e, ]
      case.node <- c(e.data[1, "node1"], e.data[nrow(e.data), "node2"])
      lapply(case.node, function(node) {
        igraph::distances(dat$g, node, p.node, weights = edges$d)
      })
    }

    endpoint.distances <- parallel::mclapply(leftover.edges,
      checkEndpoints, mc.cores = x$cores)

    leftover.audit <- lapply(endpoint.distances, function(d) {
      unique(vapply(d, which.min, integer(1L)))
    })

    #

    missing.id <- vapply(leftover.audit, function(x) length(x) == 1,
      logical(1L))

    missing.edges <- leftover.edges[missing.id]
    missing.edges.pump <- unlist(leftover.audit[missing.id])
    pumpID <- sort(unique(missing.edges.pump))

    missing.edges <- lapply(pumpID, function(p) {
      missing.edges[missing.edges.pump %in% p]
    })

    names(missing.edges) <- pumpID

    #

    split.edges <- leftover.edges[missing.id == FALSE]
    split.edges.pump <- leftover.audit[missing.id == FALSE]

    split.data <- parallel::mclapply(split.edges, function(e) {
      seg.data <- edges[edges$id2 == e, c("x1", "y1", "x2", "y2")]

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

        # Set uniform "East-West" ordering of segment endpoints
        EW <- which.min(seg.data[, c("x1", "x2")])

        if (EW == 1) {
          test.x <- seg.data$x1 + delta.x
          test.y <- seg.data$y1 + delta.y
        } else {
          test.x <- seg.data$x2 + delta.x
          test.y <- seg.data$y2 + delta.y
        }

        case.node <- paste0(test.x, "-", test.y)
        seg.edge <- edges[edges$id2 == e, ]
        seg.edge <- seg.edge[c(1, nrow(seg.edge)), ]
        seg.edge[1, c("x2", "y2")] <- c(test.x, test.y)
        seg.edge[2, c("x1", "y1")] <- c(test.x, test.y)
        seg.edge[1, "node2"] <- case.node
        seg.edge[2, "node1"] <- case.node
        seg.edge[2, "id2"] <- paste0(e, "b")
        seg.edge$d <- sqrt((seg.edge$x1 - seg.edge$x2)^2 +
                           (seg.edge$y1 - seg.edge$y2)^2)

        edges2 <- rbind(seg.edge, edges[edges$id2 != e, ])
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

    rle.audit <- lapply(split.data, function(x) rle(x$pump))

    cutpoints <- lapply(seq_along(split.data), function(i) {
      rle.obs <- rle.audit[[i]]
      cutpoint.obs <- split.data[[i]]

      sel <- rle.obs$lengths[1]

      if (length(rle.obs$lengths) != 1) {
        c(cutpoint.obs$cutpoint[sel], cutpoint.obs$cutpoint[sel + 1])
      } else {
        c(cutpoint.obs$cutpoint[sel], cutpoint.obs$cutpoint[sel])
      }
    })

    split.edges.data <- lapply(seq_along(split.edges), function(i) {
      seg.data <- edges[edges$id2 == split.edges[i], c("x1", "y1", "x2", "y2")]

      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      ols <- stats::lm(y ~ x, data = seg.df)
      segment.slope <- stats::coef(ols)[2]
      theta <- atan(segment.slope)
      h <- cutpoints[[i]]
      delta.x <- h * cos(theta)
      delta.y <- h * sin(theta)

      EW <- which.min(seg.data[, c("x1", "x2")])

      if (EW == 1) {
        x.cut <- seg.data$x1 + delta.x
        y.cut <- seg.data$y1 + delta.y
        data.frame(x = c(seg.data$x1, x.cut, seg.data$x2),
                   y = c(seg.data$y1, y.cut, seg.data$y2))
      } else {
        x.cut <- seg.data$x2 + delta.x
        y.cut <- seg.data$y2 + delta.y
        data.frame(x = c(seg.data$x2, x.cut, seg.data$x1),
                   y = c(seg.data$y2, y.cut, seg.data$y1))
      }
    })
  }

  n.sel <- as.numeric(names(x$paths))
  snow.colors <- cholera::snowColors()
  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  road.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  plot(cholera::fatalities[, c("x", "y")], xlim = x.range, ylim = y.range,
    pch = NA, asp = 1)

  if (area) {
    if (x$case.set == "expected") {
      invisible(lapply(names(x$cases), function(nm) {
        sel <- x$cases[[nm]]
        points(cholera::regular.cases[sel, ], col = snow.colors[paste0("p", nm)],
          pch = 16, cex = 1.25)
      }))

      if (is.null(x$pump.select)) {
        if (x$vestry) {
          points(cholera::pumps.vestry[, c("x", "y")], pch = 24, bg = "white",
            col = cholera::snowColors(vestry = TRUE))
          text(cholera::pumps.vestry[, c("x", "y")], pos = 1, cex = 0.9,
            col = "white", labels = paste0("p", cholera::pumps.vestry$id))
        } else {
          points(cholera::pumps[, c("x", "y")], pch = 24, bg = "white",
            col = cholera::snowColors())
          text(cholera::pumps[, c("x", "y")], pos = 1, cex = 0.9,
            col = "white", labels = paste0("p", cholera::pumps$id))
        }
      } else {
        if (x$vestry) {
          points(cholera::pumps.vestry[n.sel, c("x", "y")], pch = 24,
            bg = "white", col = snow.colors)
          text(cholera::pumps.vestry[n.sel, c("x", "y")], pos = 1, cex = 0.9,
            col = "white",
            labels = paste0("p", cholera::pumps.vestry$id[n.sel]))
        } else {
          points(cholera::pumps[n.sel, c("x", "y")], pch = 24, bg = "white",
            col = snow.colors)
          text(cholera::pumps[n.sel, c("x", "y")], pos = 1, cex = 0.9,
            col = "white", labels = paste0("p", cholera::pumps$id[n.sel]))
        }
      }

      invisible(lapply(road.list, lines, col = "gray"))
      invisible(lapply(border.list, lines))
    }

  } else {
    invisible(lapply(road.list, lines, col = "gray"))
    invisible(lapply(border.list, lines))

    invisible(lapply(names(whole.edges), function(nm) {
      n.edges <- edges[edges$id2 %in% whole.edges[[nm]], ]
      segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 2,
        col = snow.colors[paste0("p", nm)])
    }))

    if (x$case.set == "observed") {
      invisible(lapply(names(x$cases), function(nm) {
        sel <- cholera::fatalities.address$anchor.case %in% x$cases[[nm]]
        points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
          cex = 0.75, col = snow.colors[paste0("p", nm)])
      }))

    } else if (x$case.set == "expected") {
      invisible(lapply(names(missing.edges), function(nm) {
        n.edges <- edges[edges$id2 %in% missing.edges[[nm]], ]
        segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 2,
                 col = snow.colors[paste0("p", nm)])
      }))

      invisible(lapply(seq_along(split.edges.data), function(i) {
        dat <- split.edges.data[[i]]
        ps <- split.edges.pump[[i]]
        ps.col <- snow.colors[paste0("p", ps)]
        segments(dat[1, "x"], dat[1, "y"], dat[2, "x"], dat[2, "y"], lwd = 2,
          col = ps.col[1])
        segments(dat[3, "x"], dat[3, "y"], dat[4, "x"], dat[4, "y"], lwd = 2,
          col = ps.col[2])
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
  }

  title(main = "Pump Neighborhoods: Walking")
}
