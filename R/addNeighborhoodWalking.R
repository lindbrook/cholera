#' Add expected walking neighborhoods.
#'
#' @param pump.select Numeric. Numeric vector of pump IDs that define which pump neighborhoods to consider (i.e., specify the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param pump.subset Numeric. Vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} uses all pumps in \code{pump.select}.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param path Character. "expected" or "observed".
#' @param path.color Character. Use a single color for all paths. \code{NULL} uses neighborhood colors defined by \code{snowColors()}.
#' @param path.width Numeric. Set width of paths.
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param polygon.type Character. "perimeter" or "solid".
#' @param polygon.col Character.
#' @param polygon.lwd Numeric.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @param latlong Logical. Use estimated longitude and latitude.
#' @import graphics
#' @export
#' @examples
#' \dontrun{
#' streetNameLocator("marshall street", zoom = 0.5)
#' addNeighborhoodWalking()
#' }

addNeighborhoodWalking <- function(pump.select = NULL, pump.subset = NULL,
  vestry = FALSE, weighted = TRUE, path = NULL, path.color = NULL,
  path.width = 3, alpha.level = 0.25, polygon.type = "solid",
  polygon.col = NULL, polygon.lwd = 2, multi.core = TRUE, dev.mode = FALSE,
  latlong = FALSE) {

  cores <- multiCore(multi.core)

  if (latlong) {
    w <- latlongNeighborhoodWalking(pump.select = pump.select, vestry = vestry,
      multi.core = cores)

    dat <- w$neigh.data
    edges <- dat$edges
    paths <- w$paths
    vars <- c("lon", "lat")

    obs.edges <- lapply(paths, function(p) {
      oe <- lapply(p, function(x) {
        nodes.tmp <- names(unlist(unname(x)))
        auditEdge(nodes.tmp, edges)
      })
      unique(unlist(oe))
    })

    if (is.null(path.color)) {
      invisible(lapply(names(obs.edges), function(nm) {
        n.edges <- edges[obs.edges[[nm]], ]
        segments(n.edges$lon1, n.edges$lat1, n.edges$lon2, n.edges$lat2, 
          lwd = path.width, col = w$snow.colors[paste0("p", nm)])
      }))

      invisible(lapply(names(w$cases), function(nm) {
        sel <- cholera::fatalities.address$anchor %in% w$cases[[nm]]
        points(cholera::fatalities.address[sel, vars], pch = 20, cex = 0.75,
          col = w$snow.colors[nm])
      }))

    } else {
      invisible(lapply(names(obs.edges), function(nm) {
      n.edges <- edges[obs.edges[[nm]], ]
        segments(n.edges$lon1, n.edges$lat1, n.edges$lon2, n.edges$lat2, 
          lwd = path.width, col = path.color)
      }))

      invisible(lapply(names(w$cases), function(nm) {
        sel <- cholera::fatalities.address$anchor %in% w$cases[[nm]]
        points(cholera::fatalities.address[sel, vars], pch = 20, cex = 0.75,
          col = path.color)
      }))
    }
    
    if (vestry) {
      p.data <- cholera::pumps.vestry
    } else {
      p.data <- cholera::pumps
    }

    if (is.null(pump.select)) {
      points(p.data[, vars], col = w$snow.colors, lwd = 2, pch = 24)
      text(p.data[, vars], labels = paste0("p", p.data$id), cex = 0.9, pos = 1)
    } else {
      pump.id <- selectPump(p.data, pump.select = w$pump.select, 
        vestry = w$vestry)
      sel <- p.data$id %in% pump.id
      unsel <- setdiff(p.data$id, pump.id)
      points(p.data[sel, vars], col = w$snow.colors[sel], lwd = 2, pch = 24)
      text(p.data[sel, vars], labels = paste0("p", p.data$id[sel]), cex = 0.9,
        pos = 1)
      points(p.data[unsel, vars], col = "gray", lwd = 2, pch = 24)
      text(p.data[unsel, vars], labels = paste0("p", p.data$id[unsel]), 
        cex = 0.9, pos = 1, col = "gray")
    }

    if (is.null(w$pump.select)) {
      title(main = "Pump Neighborhoods: Walking")
    } else {
      title(main = paste0("Pump Neighborhoods: Walking", "\n", "Pumps ",
        paste(sort(w$pump.select), collapse = ", ")))
    }

  } else {
    if (is.null(path) == FALSE) {
      if (path %in% c("expected", "observed") == FALSE) {
        stop('If specified, path must be "expected" or "observed".')
      }
    }

    if (vestry) {
      p.count <- nrow(cholera::pumps.vestry)
    } else {
      p.count <- nrow(cholera::pumps)
    }

    p.ID <- seq_len(p.count)

    if (is.null(pump.select) == FALSE) {
      if (any(abs(pump.select) %in% p.ID == FALSE)) {
        stop('If specified, 1 >= |pump.select| <= ', p.count, " when vestry = ",
          vestry, ".")
      }
    }

    if (is.null(pump.select) & is.null(pump.subset) == FALSE) {
      if (any(abs(pump.subset) %in% p.ID == FALSE)) {
        stop('If specified, 1 >= |pump.subset| <= ', p.count, " when vestry = ",
          vestry, ".")
      }
    }

    if (is.null(pump.subset) == FALSE & is.null(pump.select) == FALSE) {
      if (all(pump.select > 0)) {
        if (any(pump.subset %in% pump.select == FALSE)) {
          stop('pump.subset should be a subset of pump.select.')
        }
      } else if (all(pump.select < 0)) {
        if (any(pump.subset %in% p.ID[pump.select])) {
          stop('pump.subset should be a subset of pump.select.')
        }
      }
    }

    nearest.data <- nearestPump(pump.select = pump.select,
                                vestry = vestry,
                                weighted = weighted,
                                case.set = "observed",
                                multi.core = cores,
                                dev.mode = dev.mode)

    nearest.dist <- nearest.data$distance
    nearest.path <- nearest.data$path

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

    nearest.pump <- data.frame(case = cholera::fatalities.address$anchor,
                               pump = nearest.dist$pump)

    pumpID <- sort(unique(nearest.dist$pump))

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
              case.set = "observed",
              pump.select = pump.select,
              cores = cores,
              metric = 1 / unitMeter(1),
              dev.mode = dev.mode)

    snow.colors <- snowColors(x$vestry)

    if (!is.null(path.color)) {
      snow.colors <- stats::setNames(rep(path.color, length(snow.colors)),
        names(snow.colors))
    }

    n.walk <- neighborhoodWalking(pump.select = x$pump.select, 
      vestry = x$vestry, case.set = x$case.set, multi.core = x$cores)
    n.data <- neighborhoodPathData(n.walk)
    dat <- n.data$dat
    edges <- n.data$edges
    n.path.edges <- n.data$neighborhood.path.edges
    p.node <- n.data$p.node
    p.name <- n.data$p.name

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
      if ((.Platform$OS.type == "windows" & x$cores > 1) | x$dev.mode) {
        cl <- parallel::makeCluster(x$cores)
        parallel::clusterExport(cl = cl, envir = environment(),
          varlist = c("edges", "p.name", "p.node", "x"))
        obs.partial.split.data <- parallel::parLapply(cl, obs.partial.segments,
          splitSegments, edges, p.name, p.node, x)
        parallel::stopCluster(cl)
      } else {
        obs.partial.split.data <- parallel::mclapply(obs.partial.segments,
          splitSegments, edges, p.name, p.node, x, mc.cores = x$cores)
      }

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

    wholes <- lapply(paste(p.ID), function(nm) {
      c(obs.whole[[nm]],
        unobs.whole[[nm]],
        obs.partial.whole[[nm]])
    })

    names(wholes) <- p.ID

    # split segments #
    split.test1 <- length(obs.partial.segments)
    split.test2 <- length(unobs.split.segments)

    if (split.test1 > 0 & split.test2 == 0) {
      splits <- obs.partial.split
      splits.pump <- obs.partial.split.pump
      splits.segs <- obs.partial.segments
    } else if (split.test1 == 0 & split.test2 > 0) {
      splits <- unobs.split
      splits.pump <- unobs.split.pump
      splits.segs <- unobs.split.segments
    } else if (split.test1 > 0 & split.test2 > 0) {
      splits <- c(obs.partial.split, unobs.split)
      splits.pump <- c(obs.partial.split.pump, unobs.split.pump)
      splits.segs <- c(obs.partial.segments, unobs.split.segments)
    }

    sim.proj <- cholera::sim.ortho.proj
    sim.proj.segs <- unique(sim.proj$road.segment)
    sim.proj.segs <- sim.proj.segs[!is.na(sim.proj.segs)]

    if (split.test1 > 0 | split.test2 > 0) {
      split.outcome <- splitOutcomes(x, splits.segs, sim.proj, splits,
        splits.pump)
      split.outcome <- do.call(rbind, split.outcome)
      split.outcome <- split.outcome[!is.na(split.outcome$pump), ]
      split.cases <- lapply(sort(unique(split.outcome$pump)), function(p) {
        split.outcome[split.outcome$pump == p, "case"]
      })

      names(split.cases) <- sort(unique(split.outcome$pump))
    }

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

    periphery.cases <- lapply(neighborhood.cases, peripheryCases)
    pearl.string <- lapply(periphery.cases, travelingSalesman)

    if (is.null(pump.subset)) {
      invisible(lapply(names(pearl.string), function(nm) {
        sel <- paste0("p", nm)

        if (is.null(polygon.col)) {
          polygon.col <- grDevices::adjustcolor(snow.colors[sel],
            alpha.f = alpha.level)
        } else {
          polygon.col <- grDevices::adjustcolor(polygon.col,
            alpha.f = alpha.level)
        }

        if (polygon.type == "perimeter") {
          polygon(cholera::regular.cases[pearl.string[[nm]], ],
            border = polygon.col, lwd = polygon.lwd)
        } else if (polygon.type == "solid") {
          polygon(cholera::regular.cases[pearl.string[[nm]], ],
            col = polygon.col)
        } else stop('polygon.type must be "perimeter" or "solid".')
      }))
    } else {
      n.subset <- pearl.string[pump.subset]
      invisible(lapply(names(n.subset), function(nm) {
        sel <- paste0("p", nm)

        if (is.null(polygon.col)) {
          polygon.col <- grDevices::adjustcolor(snow.colors[sel],
            alpha.f = alpha.level)
        } else {
          polygon.col <- grDevices::adjustcolor(polygon.col,
            alpha.f = alpha.level)
        }

        if (polygon.type == "perimeter") {
          polygon(cholera::regular.cases[pearl.string[[nm]], ],
            border = polygon.col, lwd = polygon.lwd)
        } else if (polygon.type == "solid") {
          polygon(cholera::regular.cases[pearl.string[[nm]], ],
            col = polygon.col)
        } else stop('polygon.type must be "perimeter" or "solid".')
      }))
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
            select <- p.ID[p.ID %in% abs(pump.subset) == FALSE]

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
            stop("Use all positive or all negative numbers for pump.subset.")
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
            stop("Use all positive or all negative numbers for pump.subset.")
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
}

