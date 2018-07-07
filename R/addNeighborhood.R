#' Add expected walking neighborhoods.
#'
#' @param pump.subset Numeric. Vector of numeric pump IDs to select (subset) from the neighborhoods defined by "pump.select". Negative selection possible. NULL selects all pumps in "pump.select".
#' @param pump.select Numeric. Numeric vector of pumps to define pump neighborhoods (i.e. the "population"). Negative selection possible. NULL selects all pumps.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path weighted by road length. FALSE computes shortest path in terms of the number of nodes.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @param area Logical. Area polygons.
#' @param path Character. "expected" or "observed".
#' @param path.color Character. Use a single color for all paths. NULL uses neighborhood colors defined by snowColors().
#' @param path.width Numeric. Set width of paths.
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param ... Additional plotting parameters.
#' @seealso \code{\link{snowMap}},
#' \code{\link{addIndexCase}},
#' \code{\link{addKernelDensity}},
#' \code{\link{addLandmarks}},
#' \code{\link{addPlaguePit}},
#' \code{\link{addVoronoi}},
#' \code{\link{addWhitehead}}
#' @import graphics
#' @note This function is computationally intensive. On a single core of a 2.3 GHz Intel i7, plotting observed paths to PDF takes about 5 seconds while doing so for expected paths takes about 28 seconds. Using the parallel implementation on 4 physical (8 logical) cores, these times fall to about 4 and 11 seconds. Note that parallelization is currently only available on Linux and Mac, and that although some precautions are taken in R.app on macOS, the developers of the 'parallel' package, which neighborhoodWalking() uses, strongly discourage against using parallelization within a GUI or embedded environment. See vignette("parallel") for details.
#' @export
#' @examples
#' \dontrun{
#' streetNameLocator("marshall street", zoom = TRUE, highlight = FALSE,
#'   unit = "meter", cases = NULL)
#' addNeighborhood(6:7)
#' }

addNeighborhood <- function(pump.subset = NULL, pump.select = NULL,
  vestry = FALSE, weighted = TRUE, multi.core = FALSE, area = TRUE,
  path = NULL, path.color = NULL, path.width = 3, alpha.level = 0.25, ...) {

  if (is.null(path) == FALSE) {
    if (path %in% c("expected", "observed") == FALSE) {
      stop('If specified, "path" must be "expected" or "observed".')
    }
  }

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

  if (!is.null(path.color)) {
    snow.colors <- stats::setNames(rep(path.color, length(snow.colors)),
      names(snow.colors))
  }

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
    lapply(neighborhood, auditEdge, edges)
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
  obs.partial.whole <- wholeSegments(partial.segs, dat, edges, p.name, p.node,
    x)

  # list of of split segments (lead to different pumps)
  # the cutpoint is found using appox. 1 meter increments via cutpointValues()
  obs.partial.segments <- setdiff(partial.segs, unlist(obs.partial.whole))

  if (length(obs.partial.segments) > 0) {
    obs.partial.split.data <- parallel::mclapply(obs.partial.segments,
      splitSegments, edges, p.name, p.node, x, mc.cores = x$cores)
    cutpoints <- cutpointValues(obs.partial.split.data, x)
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
    cutpoints <- cutpointValues(unobs.split.data, x)
    unobs.split.pump <- lapply(unobs.split.data, function(x) unique(x$pump))
    unobs.split <- splitData(unobs.split.segments, cutpoints, edges)
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

  sim.proj <- cholera::sim.ortho.proj
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

  if (area) {
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

  if (is.null(path) == FALSE) {
    if (path == "expected") {
      if (is.null(pump.subset)) {
        invisible(lapply(names(wholes), function(nm) {
          n.edges <- edges[edges$id %in% wholes[[nm]], ]
          segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2,
            lwd = path.width, col = snow.colors[paste0("p", nm)])
        }))

        if (split.test1 | split.test2) {
          invisible(lapply(seq_along(splits), function(i) {
            dat <- splits[[i]]
            ps <- splits.pump[[i]]
            ps.col <- snow.colors[paste0("p", ps)]
            segments(dat[1, "x"], dat[1, "y"], dat[2, "x"], dat[2, "y"],
              lwd = path.width, col = ps.col[1])
            segments(dat[3, "x"], dat[3, "y"], dat[4, "x"], dat[4, "y"],
              lwd = path.width, col = ps.col[2])
          }))
        }
      } else {
        if (all(pump.subset > 0)) {
          invisible(lapply(paste(pump.subset), function(nm) {
            n.edges <- edges[edges$id %in% wholes[[nm]], ]
            segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2,
              lwd = path.width, col = snow.colors[paste0("p", nm)])
          }))

          if (split.test1 | split.test2) {
            p.subset <- vapply(splits.pump, function(x) {
              any(pump.subset %in% x)
            }, logical(1L))

            splits.pump.subset <- splits.pump[p.subset]
            splits.subset <- splits[p.subset]

            split.select <- vapply(splits.pump.subset, function(x) {
              which(x %in% pump.subset)
            }, integer(1L))

            invisible(lapply(seq_along(splits.subset), function(i) {
              dat <- splits.subset[[i]]
              ps <- splits.pump.subset[[i]]
              ps.col <- snow.colors[paste0("p", ps)]

              if (split.select[i] == 1) {
                segments(dat[1, "x"], dat[1, "y"], dat[2, "x"], dat[2, "y"],
                  lwd = path.width, col = ps.col[1])
              } else if (split.select[i] == 2) {
                segments(dat[3, "x"], dat[3, "y"], dat[4, "x"], dat[4, "y"],
                  lwd = path.width, col = ps.col[2])
              }
            }))
          }

        } else if (all(pump.subset < 0)) {
          if (x$vestry) selected.pumps <- 1:14 else selected.pumps <- 1:13
          select <- selected.pumps[selected.pumps %in%
                    abs(pump.subset) == FALSE]

          invisible(lapply(paste(select), function(nm) {
            n.edges <- edges[edges$id %in% wholes[[nm]], ]
            segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2,
              lwd = path.width, col = snow.colors[paste0("p", nm)])
          }))

          if (split.test1 | split.test2) {
            p.subset <- vapply(splits.pump, function(x) {
              any(select %in% x)
            }, logical(1L))

            splits.pump.subset <- splits.pump[p.subset]
            splits.subset <- splits[p.subset]

            split.select <- lapply(splits.pump.subset, function(x) {
              which(x %in% select)
            })

            singles <- vapply(split.select, function(x) {
              length(x) == 1
            }, logical(1L))

            invisible(lapply(seq_along(splits.subset[singles]), function(i) {
              dat <- splits.subset[singles][[i]]
              ps <- splits.pump.subset[singles][[i]]
              ps.col <- snow.colors[paste0("p", ps)]

              if (split.select[singles][i] == 1) {
                segments(dat[1, "x"], dat[1, "y"], dat[2, "x"], dat[2, "y"],
                  lwd = path.width, col = ps.col[1])
              } else if (split.select[singles][i] == 2) {
                segments(dat[3, "x"], dat[3, "y"], dat[4, "x"], dat[4, "y"],
                  lwd = path.width, col = ps.col[2])
              }
            }))

            invisible(lapply(seq_along(splits.subset[!singles]), function(i) {
              dat <- splits.subset[!singles][[i]]
              ps <- splits.pump.subset[!singles][[i]]
              ps.col <- snow.colors[paste0("p", ps)]
              segments(dat[1, "x"], dat[1, "y"], dat[2, "x"], dat[2, "y"],
                lwd = path.width, col = ps.col[1])
              segments(dat[3, "x"], dat[3, "y"], dat[4, "x"], dat[4, "y"],
                lwd = path.width, col = ps.col[2])
            }))
          }
        } else {
          stop('Use all positive or all negative "pump.subset"!')
        }
      }

    } else if (path == "observed") {
      if (is.null(pump.subset)) {
        edge.data <- lapply(n.path.edges, function(x) unique(unlist(x)))
        invisible(lapply(names(edge.data), function(nm) {
          n.edges <- edges[edge.data[[nm]], ]
          segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2,
            lwd = path.width, col = snow.colors[paste0("p", nm)])
        }))
      } else {
        if (all(pump.subset > 0)) {
          sel <- names(n.path.edges) %in% pump.subset
        } else if (all(pump.subset < 0)) {
          sel <- names(n.path.edges) %in% abs(pump.subset) == FALSE
        } else {
          stop('Use all positive or all negative "pump.subset"!')
        }

        edge.data <- lapply(n.path.edges[sel], function(x) unique(unlist(x)))
        invisible(lapply(names(edge.data), function(nm) {
          n.edges <- edges[edge.data[[nm]], ]
          segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2,
            lwd = path.width, col = snow.colors[paste0("p", nm)])
        }))
      }
    }
  }
}
