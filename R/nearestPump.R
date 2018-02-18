#' Compute shortest walking distances or paths.
#'
#' Compute the distance or path from observed or expected cases to nearest pump (or from among selected pumps).
#' @param pump.select Numeric. Pump candidates to consider. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param output Character. "distance" or "path".
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected", or "snow".
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. Meaningful only when "weighted" is TRUE and "output" is "distance". See \code{vignette("roads")} for information on unit distances.
#' @export
#' @return An R data frome or list of 'igraph' paths.

nearestPump <- function(pump.select = NULL, output = "distance", vestry = FALSE,
  weighted = TRUE, case.set = "observed", unit = NULL, multi.core = FALSE) {

  if (output %in% c("distance", "path") == FALSE) {
    stop('"output" must be "distance" or "path".')
  }

  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('"case.set" must be "observed", "expected" or "snow".')
  }

  # if (case.set == "observed") {
  #   if (origin %in% 1:578 == FALSE) {
  #     txt1 <- 'With type = "case-pump" and "observed" = TRUE,'
  #     txt2 <- '"origin" must be between 1 and 578.'
  #     stop(paste(txt1, txt2))
  #   }
  # } else if (case.set == "expected") {
  #   if (origin %in% 1:4993 == FALSE) {
  #     txt1 <- 'With type = "case-pump" and "observed" = FALSE,'
  #     txt2 <- '"origin" must be between 1 and 4993.'
  #     stop(paste(txt1, txt2))
  #   }
  # }

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
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

    if (!is.null(unit)) {
      if (unit == "meter") {
        out$distance <- cholera::unitMeter(out$distance, "meter")
      } else if (unit == "yard") {
        out$distance <- cholera::unitMeter(out$distance, "yard")
      }
    }

    out
  }
}

pathData <- function(dat, weighted, case.set, cores) {
  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges
  nodes.pump <- dat$nodes.pump

  paths <- function(x) {
    parallel::mclapply(x, function(a) {
      case.node <- nodes[nodes$anchor == a, "node"]

      if (weighted) {
        if (case.set == "observed") {
          p <- igraph::shortest_paths(g, case.node,
            nodes.pump[nodes.pump$pump != 2, "node"], weights = edges$d)$vpath
          stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
        } else if (case.set == "expected") {
          p <- igraph::shortest_paths(g, case.node, nodes.pump$node,
            weights = edges$d)$vpath
          stats::setNames(p, nodes.pump$pump)
        }
      } else {
        if (case.set == "observed") {
          p <- igraph::shortest_paths(g, case.node,
            nodes.pump[nodes.pump$pump != 2, "node"])$vpath
          stats::setNames(p, nodes.pump[nodes.pump$pump != 2, "pump"])
        } else if (case.set == "expected") {
          p <- igraph::shortest_paths(g, case.node, nodes.pump$node)$vpath
          stats::setNames(p, nodes.pump$pump)
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
    list(case = exp.case,
         distances = distances(exp.case),
         paths = paths(exp.case))
  }
}
