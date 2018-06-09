#' Compute shortest walking distances or paths.
#'
#' @param pump.select Numeric. Pump candidates to consider. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param output Character. "distance" or "path".
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected", or "snow".
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. Meaningful only when "weighted" is TRUE and "output" is "distance". See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Default walking speed is 5 km/hr.
#' @note Time is computed using distanceTime().
#' @export
#' @return An R data frame or list of 'igraph' paths.

nearestPump <- function(pump.select = NULL, output = "distance", vestry = FALSE,
  weighted = TRUE, case.set = "observed", unit = "meter", multi.core = FALSE,
  time.unit = "second", walking.speed = 5) {

  if (output %in% c("distance", "path") == FALSE) {
    stop('"output" must be "distance" or "path".')
  }

  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('"case.set" must be "observed", "expected" or "snow".')
  }

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('"unit" must be "meter", "yard" or "native".')
  }

  cores <- multiCore(multi.core)
  dat <- cholera::neighborhoodData(vestry, case.set)
  path.data <- pathData(dat, weighted, case.set, cores)
  distances <- path.data$distances
  paths <- path.data$paths

  if (is.null(pump.select)) {
    distance.data <- lapply(distances, function(x) {
      data.frame(pump = as.numeric(names(which.min(x))),
        distance = x[which.min(x)])
    })

  } else {
    if (all(pump.select > 0)) {
      distance.data <- lapply(distances, function(x) {
        candidates <- x[names(x) %in% pump.select]
        dat <- candidates[which.min(candidates)]
        data.frame(pump = as.numeric(names(dat)), distance = dat)
      })

    } else if (all(pump.select < 0)) {
      distance.data <- lapply(distances, function(x) {
        candidates <- x[names(x) %in% abs(pump.select) == FALSE]
        dat <- candidates[which.min(candidates)]
        data.frame(pump = as.numeric(names(dat)), distance = dat)
      })
    }
  }

  if (output == "path") {
    parallel::mclapply(seq_along(paths), function(i) {
      out <- names(paths[[i]][[paste(distance.data[[i]]$pump)]])
    }, mc.cores = cores)
  } else if (output == "distance") {
    out <- data.frame(case = path.data$case,
                      do.call(rbind, distance.data),
                      pump.name = NA,
                      row.names = NULL)

    for (x in cholera::pumps$id) {
      out[out$pump == x, "pump.name"] <-
        cholera::pumps[cholera::pumps$id == x, "street"]
    }

    out <- out[, c("case", "pump", "pump.name", "distance")]

    out$time <- cholera::distanceTime(out$distance, unit = time.unit,
      speed = walking.speed)

    if (unit == "meter") {
      out$distance <- cholera::unitMeter(out$distance, "meter")
    } else if (unit == "yard") {
      out$distance <- cholera::unitMeter(out$distance, "yard")
    } else if (unit == "native") {
      out$distance <- cholera::unitMeter(out$distance, "native")
    }

    out
  }
}

pathData <- function(dat, weighted, case.set, cores) {
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
    parallel::mclapply(x, function(a) {
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

  distances <- function(x) {
    parallel::mclapply(x, function(a) {
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

  if (case.set == "observed") {
    anchor <- cholera::fatalities.address$anchor.case
    list(case = anchor, distances = distances(anchor), paths = paths(anchor))

  } else if (case.set == "snow") {
    snow <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
      cholera::snow.neighborhood, "anchor.case"])

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
    exp.case <- sort(nodes$anchor[nodes$anchor != 0])

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
