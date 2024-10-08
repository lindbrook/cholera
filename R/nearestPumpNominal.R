#' Compute shortest distances or paths to selected pumps.
#'
#' @param pump.select Numeric. Pump candidates to consider. Default is \code{NULL}: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param metric Character. "euclidean" or "walking".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected", or "snow".
#' @param location Character. For cases and pumps. "nominal", "anchor" or "orthogonal".
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. Meaningful only when "weighted" is \code{TRUE}. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @note Time is computed using \code{distanceTime()}.
#' @noRd
#' @return An R data frame or list of 'igraph' path nodes.

nearestPumpNominal <- function(pump.select = NULL, metric = "walking",
  vestry = FALSE, weighted = TRUE, case.set = "observed", location = "nominal",
  distance.unit = "meter", time.unit = "second", walking.speed = 5,
  multi.core = TRUE, dev.mode = FALSE) {

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  p.sel <- selectPump(pump.data, pump.select = pump.select, metric = metric,
    vestry = vestry)

  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('case.set must be "observed", "expected" or "snow".')
  }

  if (location %in% c("nominal", "anchor", "orthogonal") == FALSE) {
    stop('location must be "nominal", "anchor", or "orthogonal".',
      call. = FALSE)
  }

  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
    stop('distance.unit must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('unit must be "hour", "minute" or "second".')
  }

  cores <- multiCore(multi.core)

  w1 <- .Platform$OS.type == "windows" & metric == "euclidean" & cores > 1
  w2 <- .Platform$OS.type == "windows" & case.set == "expected" & cores > 1
  win.exception <- w1 | w2

  if (metric %in% c("euclidean", "walking") == FALSE) {
    stop('metric must either be "euclidean" or "walking".', call. = FALSE)

  } else if (metric == "euclidean") {
    if (case.set == "observed") {
      if (location == "nominal") {
        case <- cholera::fatalities$case
      } else if (location %in% c("anchor", "orthogonal")) {
        case <- cholera::fatalities.address$anchor
      }
    } else if (case.set == "expected") {
      case <- seq_len(nrow(cholera::regular.cases))
    } else if (case.set == "snow") {
      case <- cholera::snow.neighborhood[cholera::snow.neighborhood %in%
        cholera::fatalities.address$anchor]
    }

    if (cores == 1 | !win.exception) {
      distance.data <- lapply(case, function(x) {
        euclideanPath(x, destination = p.sel, case.set = case.set,
          location = location, vestry = vestry)$data
      })
    } else if (dev.mode | win.exception) {
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl = cl, envir = environment(),
        varlist = c("p.sel", "case.set", "vestry"))
      distance.data <- parallel::parLapply(cl, case, function(x) {
        euclideanPath(x, destination = p.sel, case.set = case.set,
          location = location, vestry = vestry)$data
      })
      parallel::stopCluster(cl)
    } else if (.Platform$OS.type != "windows") {
      distance.data <- parallel::mclapply(case, function(x) {
        euclideanPath(x, destination = p.sel, case.set = case.set,
          location = location, vestry = vestry)$data
      }, mc.cores = cores)
    } else stop("nearestPump error")

    out.distance <- do.call(rbind, distance.data)
    out.distance$anchor <- NULL

  } else if (metric == "walking") {
    dat <- neighborhoodData(vestry, case.set)

    if (case.set == "observed" | case.set == "snow") {
      path.data <- pathData(dat, weighted, case.set, cores, dev.mode,
        win.exception)

      distance.data <- lapply(path.data$distances, function(x) {
        if (is.null(pump.select)) {
          candidates <- x
        } else {
          if (all(pump.select > 0)) {
            candidates <- x[names(x) %in% pump.select]
          } else if (all(pump.select < 0)) {
            candidates <- x[names(x) %in% abs(pump.select) == FALSE]
          }
        }
        dat <- candidates[which.min(candidates)]
        data.frame(pump = as.numeric(names(dat)), distance = dat)
      })

      out.distance <- data.frame(case = path.data$case,
                        do.call(rbind, distance.data),
                        pump.name = NA,
                        row.names = NULL)

      for (x in cholera::pumps$id) {
        out.distance[out.distance$pump == x, "pump.name"] <-
          cholera::pumps[cholera::pumps$id == x, "street"]
      }

      out.distance <- out.distance[, c("case", "pump", "pump.name", "distance")]
      out.distance$distance <- unitMeter(out.distance$distance, distance.unit)

      out.distance$time <- distanceTime(out.distance$distance,
        time.unit = time.unit, walking.speed = walking.speed)

      paths <- path.data$paths
      out.path <- lapply(seq_along(paths), function(i) {
       names(paths[[i]][[paste(distance.data[[i]]$pump)]])
      })

    } else if (case.set == "expected") {
      g <- dat$g
      nodes <- dat$nodes
      edges <- dat$edges
      nodes.pump <- dat$nodes.pump

      ## Adam and Eve Court: isolate with pump (#2) ##
      rd <- "Adam and Eve Court"
      sel <- cholera::road.segments$name == rd
      adam.eve.ct <- cholera::road.segments[sel, "id"]
      sel <- cholera::sim.ortho.proj$road.segment == adam.eve.ct &
             !is.na(cholera::sim.ortho.proj$road.segment)
      AE.cases <- cholera::sim.ortho.proj[sel, "case"]

      ## Falconberg Court and Mews: isolate without pump ##
      falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
      sel <- cholera::sim.ortho.proj$road.segment %in% falconberg.ct.mews &
             !is.na(cholera::sim.ortho.proj$road.segment)
      FCM.cases <- cholera::sim.ortho.proj[sel, "case"]

      case <- nodes[nodes$anchor != 0 & nodes$anchor < 20000, "anchor"]
      exp.case <- case[case %in% FCM.cases == FALSE]


      if (cores == 1 | !win.exception) {
        ## Adam and Eve Court (#2): 106 expected cases ##
        if (is.null(pump.select) == FALSE & 2 %in% p.sel == FALSE) {
          exp.case.AE <- exp.case[exp.case %in% AE.cases]
          exp.case.not_AE <- exp.case[exp.case %in% AE.cases == FALSE]

          nearest.pump <- lapply(exp.case.not_AE, function(x) {
            case.node <- nodes[nodes$anchor == x, "node"]
            if (is.null(pump.select)) {
              d <- c(igraph::distances(g, case.node, nodes.pump$node,
                weights = edges$d))
            } else {
              d <- c(igraph::distances(g, case.node, nodes.pump[p.sel, "node"],
                weights = edges$d))
            }
            names(d) <- p.sel
            p <- as.numeric(names(which.min(d[is.infinite(d) == FALSE])))
            data.frame(case = x,
                       pump = p,
                       distance = min(d[is.infinite(d) == FALSE]))
          })

          out.distance <- do.call(rbind, nearest.pump)
          out.distance.AE <- data.frame(case = exp.case.AE, pump = NA,
            distance = NA)
          out.distance <- rbind(out.distance, out.distance.AE)

        } else if (is.null(pump.select) |
                  (is.null(pump.select) == FALSE & 2 %in% p.sel == TRUE)) {

          nearest.pump <- lapply(exp.case, function(x) {
            case.node <- nodes[nodes$anchor == x, "node"]
            if (is.null(pump.select)) {
              d <- c(igraph::distances(g, case.node, nodes.pump$node,
                weights = edges$d))
            } else {
              d <- c(igraph::distances(g, case.node, nodes.pump[p.sel, "node"],
                weights = edges$d))
            }
            names(d) <- p.sel
            p <- as.numeric(names(which.min(d[is.infinite(d) == FALSE])))
            data.frame(case = x,
                       pump = p,
                       distance = min(d[is.infinite(d) == FALSE]))
          })

          out.distance <- do.call(rbind, nearest.pump)
        }


      } else if (dev.mode | win.exception) {
        ## Adam and Eve Court (#2): 106 expected cases ##
        if (is.null(pump.select) == FALSE & 2 %in% p.sel == FALSE) {
          exp.case.AE <- exp.case[exp.case %in% AE.cases]
          exp.case.not_AE <- exp.case[exp.case %in% AE.cases == FALSE]

          cl <- parallel::makeCluster(cores)

          parallel::clusterExport(cl = cl, envir = environment(),
            varlist = c("exp.case.AE", "g", "nodes", "edges", "nodes.pump",
              "p.sel", "pump.select"))

          nearest.pump <- parallel::parLapply(cl, exp.case.not_AE, function(x) {
            case.node <- nodes[nodes$anchor == x, "node"]
            if (is.null(pump.select)) {
              d <- c(igraph::distances(g, case.node, nodes.pump$node,
                weights = edges$d))
            } else {
              d <- c(igraph::distances(g, case.node, nodes.pump[p.sel, "node"],
                weights = edges$d))
            }
            names(d) <- p.sel
            p <- as.numeric(names(which.min(d[is.infinite(d) == FALSE])))
            data.frame(case = x,
                       pump = p,
                       distance = min(d[is.infinite(d) == FALSE]))
          })

          parallel::stopCluster(cl)

          out.distance <- do.call(rbind, nearest.pump)
          out.distance.AE <- data.frame(case = exp.case.AE, pump = NA,
            distance = NA)
          out.distance <- rbind(out.distance, out.distance.AE)

        } else if (is.null(pump.select) |
                  (is.null(pump.select) == FALSE & 2 %in% p.sel == TRUE)) {

          cl <- parallel::makeCluster(cores)

          parallel::clusterExport(cl = cl, envir = environment(),
            varlist = c("exp.case", "g", "nodes", "edges", "nodes.pump",
            "p.sel", "pump.select"))

          nearest.pump <- parallel::parLapply(cl, exp.case, function(x) {
            case.node <- nodes[nodes$anchor == x, "node"]
            if (is.null(pump.select)) {
              d <- c(igraph::distances(g, case.node, nodes.pump$node,
                weights = edges$d))
            } else {
              d <- c(igraph::distances(g, case.node, nodes.pump[p.sel, "node"],
                weights = edges$d))
            }
            names(d) <- p.sel
            p <- as.numeric(names(which.min(d[is.infinite(d) == FALSE])))
            data.frame(case = x,
                       pump = p,
                       distance = min(d[is.infinite(d) == FALSE]))
          })

          parallel::stopCluster(cl)

          out.distance <- do.call(rbind, nearest.pump)
        }

      } else if (.Platform$OS.type != "windows") {

        ## Adam and Eve Court (#2): 106 expected cases ##
        if (is.null(pump.select) == FALSE & 2 %in% p.sel == FALSE) {
          exp.case.AE <- exp.case[exp.case %in% AE.cases]
          exp.case.not_AE <- exp.case[exp.case %in% AE.cases == FALSE]

          nearest.pump <- parallel::mclapply(exp.case.not_AE, function(x) {
            case.node <- nodes[nodes$anchor == x, "node"]
            if (is.null(pump.select)) {
              d <- c(igraph::distances(g, case.node, nodes.pump$node,
                weights = edges$d))
            } else {
              d <- c(igraph::distances(g, case.node, nodes.pump[p.sel, "node"],
                weights = edges$d))
            }
            names(d) <- p.sel
            p <- as.numeric(names(which.min(d[is.infinite(d) == FALSE])))
            data.frame(case = x,
                       pump = p,
                       distance = min(d[is.infinite(d) == FALSE]))
          }, mc.cores = cores)

          out.distance <- do.call(rbind, nearest.pump)
          out.distance.AE <- data.frame(case = exp.case.AE, pump = NA,
            distance = NA)
          out.distance <- rbind(out.distance, out.distance.AE)

        } else if (is.null(pump.select) |
                  (is.null(pump.select) == FALSE & 2 %in% p.sel == TRUE)) {

          nearest.pump <- parallel::mclapply(exp.case, function(x) {
            case.node <- nodes[nodes$anchor == x, "node"]
            if (is.null(pump.select)) {
              d <- c(igraph::distances(g, case.node, nodes.pump$node,
                weights = edges$d))
            } else {
              d <- c(igraph::distances(g, case.node, nodes.pump[p.sel, "node"],
                weights = edges$d))
            }
            names(d) <- p.sel
            p <- as.numeric(names(which.min(d[is.infinite(d) == FALSE])))
            data.frame(case = x,
                       pump = p,
                       distance = min(d[is.infinite(d) == FALSE]))
          }, mc.cores = cores)

          out.distance <- do.call(rbind, nearest.pump)
        }
      }

      if (any(is.na(out.distance$pump))) {
        temp <- out.distance[!is.na(out.distance$pump), ]
        temp$pump.name <- NA

        for (x in cholera::pumps$id) {
          temp[temp$pump == x , "pump.name"] <-
            cholera::pumps[cholera::pumps$id == x, "street"]
        }

        temp2 <- out.distance[is.na(out.distance$pump), ]
        temp2$pump.name <- NA
        out.distance <- rbind(temp, temp2)
      } else {
        for (x in cholera::pumps$id) {
          out.distance[out.distance$pump == x , "pump.name"] <-
            cholera::pumps[cholera::pumps$id == x, "street"]
        }
      }

      out.distance <- out.distance[, c("case", "pump", "pump.name", "distance")]
      out.distance$distance <- unitMeter(out.distance$distance, distance.unit)
      out.distance$time <- distanceTime(out.distance$distance,
        time.unit = time.unit, walking.speed = walking.speed)
      out.distance <- out.distance[order(out.distance$case), ]
    }
  }

  if ((case.set == "observed" | case.set == "snow") & metric == "walking") {
    list(path = out.path, distance = out.distance)
  } else {
    out.distance
  }
}

pathData <- function(dat, weighted, case.set, cores, dev.mode, win.exception) {
  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges
  nodes.pump <- dat$nodes.pump

  ## Adam and Eve Court: isolate with pump ##
  rd <- "Adam and Eve Court"
  adam.eve.ct <- cholera::road.segments[cholera::road.segments$name == rd, "id"]
  sel <- cholera::sim.ortho.proj$road.segment == adam.eve.ct &
         !is.na(cholera::sim.ortho.proj$road.segment)
  AE.cases <- cholera::sim.ortho.proj[sel, "case"]

  paths <- function(x) {
    if (cores == 1 | !win.exception) {
      pths <- lapply(x, function(a) {
        case.node <- nodes[nodes$anchor == a, "node"]
        if (weighted) {
          if (case.set == "observed") {
            p <- igraph::shortest_paths(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"], weights = edges$d)$vpath
            stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            if (a %in% AE.cases) {
              p.nodes <- nodes.pump[nodes.pump$pump == 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes,
                                          weights = edges$d)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump == 2, "pump"])
            } else {
              p.nodes <- nodes.pump[nodes.pump$pump != 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes,
                                          weights = edges$d)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
            }
          }
        } else {
          if (case.set == "observed") {
            p <- igraph::shortest_paths(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"])$vpath
            stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            if (a %in% AE.cases) {
              p.nodes <- nodes.pump[nodes.pump$pump == 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump == 2, "pump"])
            } else {
              p.nodes <- nodes.pump[nodes.pump$pump != 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
            }
          }
        }
      })

    } else if (dev.mode | win.exception) {
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl = cl, envir = environment(),
        varlist = c("weighted", "case.set", "g", "nodes", "edges",
          "nodes.pump"))
      pths <- parallel::parLapply(cl, x, function(a) {
        case.node <- nodes[nodes$anchor == a, "node"]
        if (weighted) {
          if (case.set == "observed") {
            p <- igraph::shortest_paths(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"], weights = edges$d)$vpath
            stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            if (a %in% AE.cases) {
              p.nodes <- nodes.pump[nodes.pump$pump == 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes,
                weights = edges$d)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump == 2, "pump"])
            } else {
              p.nodes <- nodes.pump[nodes.pump$pump != 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes,
                weights = edges$d)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
            }
          }
        } else {
          if (case.set == "observed") {
            p <- igraph::shortest_paths(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"])$vpath
            stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            if (a %in% AE.cases) {
              p.nodes <- nodes.pump[nodes.pump$pump == 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump == 2, "pump"])
            } else {
              p.nodes <- nodes.pump[nodes.pump$pump != 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
            }
          }
        }
      })

      parallel::stopCluster(cl)

    } else if (.Platform$OS.type != "windows") {
      pths <- parallel::mclapply(x, function(a) {
        case.node <- nodes[nodes$anchor == a, "node"]
        if (weighted) {
          if (case.set == "observed") {
            p <- igraph::shortest_paths(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"], weights = edges$d)$vpath
            stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            if (a %in% AE.cases) {
              p.nodes <- nodes.pump[nodes.pump$pump == 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes,
                weights = edges$d)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump == 2, "pump"])
            } else {
              p.nodes <- nodes.pump[nodes.pump$pump != 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes,
                weights = edges$d)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
            }
          }
        } else {
          if (case.set == "observed") {
            p <- igraph::shortest_paths(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"])$vpath
            stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            if (a %in% AE.cases) {
              p.nodes <- nodes.pump[nodes.pump$pump == 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump == 2, "pump"])
            } else {
              p.nodes <- nodes.pump[nodes.pump$pump != 2, "node"]
              p <- igraph::shortest_paths(g, case.node, p.nodes)$vpath
              stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
            }
          }
        }
      }, mc.cores = cores)
    }
    pths
  }

  distances <- function(x) {
    if (cores == 1 | !win.exception) {
      dists <- lapply(x, function(a) {
        case.node <- nodes[nodes$anchor == a, "node"]
        if (weighted) {
          if (case.set == "observed") {
            d <- c(igraph::distances(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"], weights = edges$d))
            stats::setNames(d, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            d <- c(igraph::distances(g, case.node, nodes.pump$node,
                                     weights = edges$d))
            stats::setNames(d, nodes.pump$pump)
          }
        } else {
          if (case.set == "observed") {
            d <- c(igraph::distances(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"]))
            stats::setNames(d, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            d <- c(igraph::distances(g, case.node, nodes.pump$node))
            stats::setNames(d, nodes.pump$pump)
          }
        }
      })

    } else if (dev.mode | win.exception) {
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl = cl, envir = environment(),
        varlist = c("weighted", "case.set", "g", "nodes", "edges",
          "nodes.pump"))
      dists <- parallel::parLapply(cl, x, function(a) {
        case.node <- nodes[nodes$anchor == a, "node"]
        if (weighted) {
          if (case.set == "observed") {
            d <- c(igraph::distances(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"], weights = edges$d))
            stats::setNames(d, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            d <- c(igraph::distances(g, case.node, nodes.pump$node,
              weights = edges$d))
            stats::setNames(d, nodes.pump$pump)
          }
        } else {
          if (case.set == "observed") {
            d <- c(igraph::distances(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"]))
            stats::setNames(d, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            d <- c(igraph::distances(g, case.node, nodes.pump$node))
            stats::setNames(d, nodes.pump$pump)
          }
        }
      })

      parallel::stopCluster(cl)

    } else if (.Platform$OS.type != "windows") {
      dists <- parallel::mclapply(x, function(a) {
        case.node <- nodes[nodes$anchor == a, "node"]
        if (weighted) {
          if (case.set == "observed") {
            d <- c(igraph::distances(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"], weights = edges$d))
            stats::setNames(d, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            d <- c(igraph::distances(g, case.node, nodes.pump$node,
              weights = edges$d))
            stats::setNames(d, nodes.pump$pump)
          }
        } else {
          if (case.set == "observed") {
            d <- c(igraph::distances(g, case.node,
              nodes.pump[nodes.pump$pump != 2, "node"]))
            stats::setNames(d, nodes.pump[nodes.pump$pump != 2, "pump"])
          } else if (case.set == "expected") {
            d <- c(igraph::distances(g, case.node, nodes.pump$node))
            stats::setNames(d, nodes.pump$pump)
          }
        }
      }, mc.cores = cores)
    }
    dists
  }

  if (case.set == "observed") {
    anchor <- cholera::fatalities.address$anchor
    list(case = anchor, distances = distances(anchor), paths = paths(anchor))

  } else if (case.set == "snow") {
    snow <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
      cholera::snow.neighborhood, "anchor"])

    paths.snow <- parallel::mclapply(snow, function(x) {
      case.node <- nodes[nodes$anchor == x, "node"]
      if (weighted) {
        stats::setNames(igraph::shortest_paths(g, case.node,
          nodes.pump[nodes.pump$pump == 7, "node"], weights = edges$d)$vpath,
          7)
      } else {
        stats::setNames(igraph::shortest_paths(g, case.node,
          nodes.pump[nodes.pump$pump == 7, "node"])$vpath, 7)
      }
    }, mc.cores = cores)

    distances.snow <- parallel::mclapply(snow, function(x) {
      case.node <- nodes[nodes$anchor == x, "node"]
      if (weighted) {
        stats::setNames(c(igraph::distances(g, case.node,
          nodes.pump[nodes.pump$pump == 7, "node"], weights = edges$d)),
          7)
      } else {
        stats::setNames(c(igraph::distances(g, case.node,
          nodes.pump[nodes.pump$pump == 7, "node"])), 7)
      }
    }, mc.cores = cores)

    list(case = snow, distances = distances.snow, paths = paths.snow)

  } else if (case.set == "expected") {
    exp.case <- sort(nodes$anchor[nodes$anchor != 0 & nodes$anchor <= 20000])

    ## Falconberg Court and Mews: isolate without pumps ##
    falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
    sel <- cholera::sim.ortho.proj$road.segment %in% falconberg.ct.mews &
           !is.na(cholera::sim.ortho.proj$road.segment)
    FCM.cases <- cholera::sim.ortho.proj[sel, "case"]
    exp.case <- exp.case[exp.case %in% FCM.cases == FALSE]

    list(case = exp.case,
         distances = distances(exp.case),
         paths = paths(exp.case))
  }
}
