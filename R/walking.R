#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected" or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
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
#' @note This function is computationally intensive. On a single core of a 2.3 GHz Intel i7, plotting observed paths to PDF takes about 5 seconds while doing so for expected paths takes about 28 seconds. Using the parallel implementation on 4 physical (8 logical) cores, these times fall to about 4 and 11 seconds. Note that parallelization is currently only available on Linux and Mac, and that although some precautions are taken in R.app on macOS, the developers of the 'parallel' package, which \code{neighborhoodWalking()} uses, strongly discourage against using parallelization within a GUI or embedded environment. See \code{vignette("parallel")} for details.
#' @export
#' @examples
#' \dontrun{
#'
#' neighborhoodWalking()
#' neighborhoodWalking(pump.select = -6)
#' }

neighborhoodWalking <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, case.set = "observed", multi.core = FALSE) {

  if (is.null(pump.select) == FALSE) {
    if (is.numeric(pump.select) == FALSE) stop("pump.select must be numeric.")
    if (length(pump.select) == 1) {
      if (pump.select == 2) {
        msg1 <- "You can't just select the pump on Adam and Eve Court (#2).\n"
        msg2 <- " It's an isolate, unreachable for observed fatalities."
        stop(msg1, msg2)
      }
    }

    if (vestry) {
      p.count <- nrow(cholera::pumps.vestry)
    } else {
      p.count <- nrow(cholera::pumps)
    }

    p.ID <- seq_len(p.count)

    if (any(abs(pump.select) %in% p.ID == FALSE)) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ', p.count)
    }
  }

  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('case.set must be "observed", "expected" or "snow".')
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
#' @param x An object of class "walking" created by \code{neighborhoodWalking()}.
#' @param ... Additional parameters.
#' @return An R vector.
#' @export
#' @examples
#' \dontrun{
#'
#' neighborhoodWalking()
#' print(neighborhoodWalking())
#' }

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

expectedCount <- function(x) {
  dat <- cholera::neighborhoodData(vestry = x$vestry, case.set = "observed")
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

  n.path.edges <- parallel::mclapply(x$paths, function(neighborhood) {
    lapply(neighborhood, auditEdge, edges)
  }, mc.cores = x$cores)

  ## ------------ Plot ------------ ##

  snow.colors <- cholera::snowColors(x$vestry)

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
  obs.partial.whole <- wholeSegments(partial.segs, dat, edges, p.name,
    p.node, x)

  # list of of split segments (lead to different pumps)
  # the cutpoint is found using appox. 1 meter increments via cutpointValues()
  obs.partial.segments <- setdiff(partial.segs, unlist(obs.partial.whole))

  if (length(obs.partial.segments) > 0) {
    obs.partial.split.data <- parallel::mclapply(obs.partial.segments,
      splitSegments, edges, p.name, p.node, x, mc.cores = x$cores)
    cutpoints <- cutpointValues(obs.partial.split.data)
    obs.partial.split.pump <- lapply(obs.partial.split.data, function(x)
      unique(x$pump))
    obs.partial.split <- splitData(obs.partial.segments, cutpoints, edges)
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

  unobs.whole <- wholeSegments(unobs.segments, dat, edges, p.name, p.node, x)
  unobs.split.segments <- setdiff(unobs.segments, unlist(unobs.whole))

  if (length(unobs.split.segments) > 0) {
    unobs.split.data <- parallel::mclapply(unobs.split.segments,
      splitSegments, edges, p.name, p.node, x, mc.cores = x$cores)
    cutpoints <- cutpointValues(unobs.split.data)
    unobs.split.pump <- lapply(unobs.split.data, function(x) unique(x$pump))
    unobs.split <- splitData(unobs.split.segments, cutpoints, edges)
  }

  ## ------------ Data Assembly ------------ ##

  if (x$vestry) {
    pumpID <- seq_len(nrow(cholera::pumps.vestry))
  } else {
    pumpID <- seq_len(nrow(cholera::pumps))
  }

  wholes <- lapply(pumpID, function(nm) {
    c(obs.whole[[paste(nm)]],
      unobs.whole[[paste(nm)]],
      obs.partial.whole[[paste(nm)]])
  })

  names(wholes) <- pumpID

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

  sim.proj <- cholera::sim.ortho.proj
  sim.proj.segs <- unique(sim.proj$road.segment)

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
  stats::setNames(count.data$count.x + count.data$count.y, count.data$pump)
}

#' Plot method for neighborhoodWalking().
#'
#' @param x An object of class "walking" created by \code{neighborhoodWalking()}.
#' @param type Character. "road", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @note When plotting area graphs with simulated data (i.e., \code{case.set = "expected"}), there may be discrepancies between observed cases and expected neighborhoods, particularly between neighborhoods. The "area.points" plot takes about 28 seconds (11 using the parallel implementation). The "area.polygons" plot takes 49 seconds (17 using the parallel implementation).
#' @export
#' @examples
#' \dontrun{
#'
#' plot(neighborhoodWalking())
#' plot(neighborhoodWalking(case.set = "expected"))
#' plot(neighborhoodWalking(case.set = "expected"), type = "area.points")
#' plot(neighborhoodWalking(case.set = "expected"), type = "area.polygons")
#' }

plot.walking <- function(x, type = "road", ...) {
  if (class(x) != "walking") {
    stop('"x"\'s class needs to be "walking".')
  }

  if (type %in% c("road", "area.points", "area.polygons") == FALSE) {
    stop('type must be "road", "area.points", "area.polygons".')
  }

  if (type %in% c("area.points", "area.polygons")) {
    if (x$case.set != "expected") {
      stop('area plots valid only when case.set = "expected".')
    }
  }

  dat <- cholera::neighborhoodData(vestry = x$vestry, case.set = "observed")
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

  n.path.edges <- parallel::mclapply(x$paths, function(neighborhood) {
    lapply(neighborhood, auditEdge, edges)
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
    obs.partial.whole <- wholeSegments(partial.segs, dat, edges, p.name,
      p.node, x)

    # list of of split segments (lead to different pumps)
    # the cutpoint is found using appox. 1 meter increments via cutpointValues()
    obs.partial.segments <- setdiff(partial.segs, unlist(obs.partial.whole))

    if (length(obs.partial.segments) > 0) {
      obs.partial.split.data <- parallel::mclapply(obs.partial.segments,
        splitSegments, edges, p.name, p.node, x, mc.cores = x$cores)
      cutpoints <- cutpointValues(obs.partial.split.data)
      obs.partial.split.pump <- lapply(obs.partial.split.data, function(x)
        unique(x$pump))
      obs.partial.split <- splitData(obs.partial.segments, cutpoints, edges)
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

    unobs.whole <- wholeSegments(unobs.segments, dat, edges, p.name, p.node, x)
    unobs.split.segments <- setdiff(unobs.segments, unlist(unobs.whole))

    if (length(unobs.split.segments) > 0) {
      unobs.split.data <- parallel::mclapply(unobs.split.segments,
        splitSegments, edges, p.name, p.node, x, mc.cores = x$cores)
      cutpoints <- cutpointValues(unobs.split.data)
      unobs.split.pump <- lapply(unobs.split.data, function(x) unique(x$pump))
      unobs.split <- splitData(unobs.split.segments, cutpoints, edges)
    }

    ## ------------ Data Assembly ------------ ##

    if (x$vestry) {
      pumpID <- seq_len(nrow(cholera::pumps.vestry))
    } else {
      pumpID <- seq_len(nrow(cholera::pumps))
    }

    wholes <- lapply(pumpID, function(nm) {
      c(obs.whole[[paste(nm)]],
        unobs.whole[[paste(nm)]],
        obs.partial.whole[[paste(nm)]])
    })

    names(wholes) <- pumpID

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

    sim.proj <- cholera::sim.ortho.proj
    sim.proj.segs <- unique(sim.proj$road.segment)

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
      ap <- areaPointsData(sim.proj.segs, wholes, snow.colors, sim.proj,
        split.cases)
      points(cholera::regular.cases[ap$sim.proj.wholes$case, ],
        col = ap$sim.proj.wholes$color, pch = 15, cex = 1.25)
      points(cholera::regular.cases[ap$sim.proj.splits$case, ],
        col = ap$sim.proj.splits$color, pch = 15, cex = 1.25)
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
