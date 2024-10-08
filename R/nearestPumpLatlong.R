#' Compute shortest georeferenced distances (and walking paths) to selected pumps (prototype).
#'
#' @param pump.select Numeric. Pump candidates to consider. Default is \code{NULL}: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param metric Character. "euclidean" or "walking".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected" # or "snow".
#' @param location Character. For cases and pumps. "nominal", "anchor" or "orthogonal".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @importFrom geosphere distGeo
#' @noRd
#' @return An R data frame or list of 'igraph' path nodes.

nearestPumpLatlong <- function(pump.select = NULL, metric = "walking",
  vestry = FALSE, weighted = TRUE, case.set = "observed", location = "nominal",
  time.unit = "second", walking.speed = 5, multi.core = TRUE) {

  cores <- multiCore(multi.core)
  vars <- c("lon", "lat")

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  if (!case.set %in% c("observed", "expected", "snow")) {
    stop('case.set must be "observed", "expected" or "snow".', call. = FALSE)
  }

  if (location %in% c("nominal", "anchor", "orthogonal") == FALSE) {
    stop('location must be "nominal", "anchor", or "orthogonal".',
      call. = FALSE)
  }

  if (metric %in% c("euclidean", "walking") == FALSE) {
    stop('metric must either be "euclidean" or "walking".', call. = FALSE)
  } else if (metric == "euclidean") {
    if (case.set == "observed") {
      if (location == "nominal") {
        case <- cholera::fatalities$case
        case.data <- cholera::fatalities
      } else if (location == "anchor") {
        case <- cholera::fatalities.address$anchor
        case.data <- cholera::fatalities.address
      } else if (location == "orthogonal") {
        case <- cholera::ortho.proj$case
        case.data <- cholera::ortho.proj
      }
    } else if (case.set == "expected") {
      case <- seq_len(nrow(cholera::regular.cases))
      case.data <- cholera::regular.cases
    } else if (case.set == "snow") {
      sel <- cholera::snow.neighborhood %in% cholera::fatalities.address$anchor
      case <- cholera::snow.neighborhood[sel, ]
      case.data <- cholera::snow.neighborhood
    }

    p.sel <- selectPump(pump.data, pump.select = pump.select, metric = metric,
      vestry = vestry)

    if (cores == 1 | .Platform$OS.type == "windows") {
      out <- lapply(case, function(x) {
        if (location %in% c("nominal", "orthogonal")) {
          sel <- case.data$case == x
        } else if (location == "anchor") {
          sel <- case.data$anchor == x
        }

        ego <- case.data[sel == x, vars]

        if (is.null(pump.select)) {
          alters <- pump.data[, c("id", vars)]
        } else {
          alters <- pump.data[pump.data$id %in% p.sel, c("id", vars)]
        }

        d <- vapply(alters$id, function(id) {
          geosphere::distGeo(ego, alters[alters$id == id, vars])
        }, numeric(1L))

        sel <- which.min(d)
        data.frame(case = x, pump = alters$id[sel], distance = d[sel])
      })
    } else if (cores > 1 & .Platform$OS.type != "windows") {
      out <- parallel::mclapply(case, function(x) {
        if (location %in% c("nominal", "orthogonal")) {
          sel <- case.data$case == x
        } else if (location == "anchor") {
          sel <- case.data$anchor == x
        }

        ego <- case.data[sel, vars]

        if (is.null(pump.select)) {
          alters <- pump.data[, c("id", vars)]
        } else {
          alters <- pump.data[pump.data$id %in% p.sel, c("id", vars)]
        }

        d <- vapply(alters$id, function(id) {
          geosphere::distGeo(ego, alters[alters$id == id, vars])
        }, numeric(1L))

        sel <- which.min(d)
        data.frame(case = x, pump = alters$id[sel], distance = d[sel])
      }, mc.cores = cores)
    }

    out <- do.call(rbind, out)

    out$time <- walkingTime(out$distance, time.unit = time.unit,
      walking.speed = walking.speed)

  } else if (metric == "walking") {
    dat <- neighborhoodDataB(case.set = case.set, vestry = vestry,
      latlong = TRUE)
    p.sel <- selectPump(pump.data, pump.select = pump.select, vestry = vestry)
    path.data <- latlong_pathData(dat, p.sel, case.set, vestry, weighted, cores)
    walking.time <- walkingTime(path.data$distance, time.unit = time.unit,
      walking.speed = walking.speed)
    distance <- data.frame(case = path.data$case, pump = path.data$pump,
      distance = path.data$distance, time = walking.time)
    out <- list(neigh.data = dat, distance = distance, path = path.data$path)
  }
  out
}

latlong_pathData <- function(dat, p.sel, case.set, vestry, weighted, cores) {
  g <- dat$g
  edges <- dat$edges

  if (case.set == "observed") {
    ortho.addr <- cholera::latlong.ortho.addr
  } else if (case.set == "expected") {
    ortho.addr <- cholera::latlong.sim.ortho.proj
  } else if (case.set == "snow") {
    sel <- cholera::latlong.ortho.addr$case %in% cholera::snow.neighborhood
    ortho.addr <- cholera::latlong.ortho.addr[sel, ]
  }

  if (vestry) {
    ortho.pump <- cholera::latlong.ortho.pump.vestry[p.sel, ]
    pmp <- cholera::pumps.vestry[p.sel, ]
  } else {
    ortho.pump <- cholera::latlong.ortho.pump[p.sel, ]
    pmp <- cholera::pumps[p.sel, ]
  }

  ortho.addr$node <- paste0(ortho.addr$lon, "_&_", ortho.addr$lat)
  ortho.pump$node <- paste0(ortho.pump$lon, "_&_", ortho.pump$lat)

  if (case.set %in% c("observed", "expected", "snow")) {
    if (cores == 1 | .Platform$OS.type == "windows") {
      distances <- lapply(ortho.addr$node, function(case.node) {
        igraph::distances(g, case.node, ortho.pump$node, weights = edges$d)
      })
    } else if (cores > 1 & .Platform$OS.type != "windows") {
      distances <- parallel::mclapply(ortho.addr$node, function(case.node) {
        igraph::distances(g, case.node, ortho.pump$node, weights = edges$d)
      }, mc.cores = cores)
    }

    names(distances) <- ortho.addr$case

    if (case.set == "expected") {
      ## Adam and Eve Court: isolate with pump (#2) - infinite = unreachable
      inf.dist <- vapply(distances, function(x) {
        any(is.infinite(x))
      }, logical(1L))

      if (any(inf.dist)) {
        # e.g., cases on Falconberg Mews - no reachable pumps - street isolate
        all.infinite <- vapply(distances, function(x) {
          all(is.infinite(x))
        }, logical(1L))
        no_pump.isolate <- distances[all.infinite]
        distances <- distances[!all.infinite]

        # e.g. only reachable pump Adam and Eve Court Pump (#2) - pump isolate
        one.finite <- vapply(distances, function(x) {
          sum(is.finite(x)) == 1
        }, logical(1L))
        pump.isolate <- distances[one.finite]
        distances <- distances[!one.finite]
      }

      # cases
      case.no_pump.isolate <- as.numeric(names(no_pump.isolate))
      case.pump.isolate <- as.numeric(names(pump.isolate))
      isolates <- c(case.no_pump.isolate, case.pump.isolate)

      # ortho.addr data frames
      sel <- ortho.addr$case %in% case.pump.isolate
      ortho.addr.pump.isolate <- ortho.addr[sel, ]

      pump.isolate.id <- vapply(pump.isolate, function(x) {
        which(vapply(x, is.finite, logical(1L)))
      }, integer(1L))

      # cases inside of pump isolate neighborhood
      idx <- seq_along(case.pump.isolate)

      if (cores == 1 | .Platform$OS.type == "windows") {
        paths.pump.isolate <- lapply(idx, function(i) {
          sel <- ortho.addr.pump.isolate$case == case.pump.isolate[i]
          case.node <- ortho.addr.pump.isolate[sel, "node"]
          sel <- ortho.pump$id == pump.isolate.id[i]
          pump.node <- ortho.pump[sel, "node"]
          igraph::shortest_paths(g, case.node, pump.node,
            weights = edges$d)$vpath
        })
      } else if (cores > 1 & .Platform$OS.type != "windows") {
        paths.pump.isolate <- parallel::mclapply(idx, function(i) {
          sel <- ortho.addr.pump.isolate$case == case.pump.isolate[i]
          case.node <- ortho.addr.pump.isolate[sel, "node"]
          sel <- ortho.pump$id == pump.isolate.id[i]
          pump.node <- ortho.pump[sel, "node"]
          igraph::shortest_paths(g, case.node, pump.node,
            weights = edges$d)$vpath
        }, mc.cores = cores)
      }

      names(paths.pump.isolate) <- case.pump.isolate

      # Pump 2 isolate cases
      min.dist.iso <- vapply(pump.isolate, function(x) {
        x[which.min(x)]
      }, numeric(1L))
      p.id.iso <- vapply(pump.isolate, which.min, integer(1L))
      idx <- seq_along(case.pump.isolate)

      short.path.pump.isolate <- lapply(idx, function(i) {
        case.node <- ortho.addr.pump.isolate[i, "node"]
        pump.node <- ortho.pump[ortho.pump$id == p.id.iso[i], "node"]
        igraph::shortest_paths(g, case.node, pump.node, weights = edges$d)$vpath
      })

      names(short.path.pump.isolate) <- case.pump.isolate

      # filter out "isolates"
      ortho.addr <- ortho.addr[ortho.addr$case %in% isolates == FALSE, ]
    }

    # other pump cases
    min.dist <- vapply(distances, min, numeric(1L))
    p.id <- p.sel[vapply(distances, which.min, integer(1L))]
    idx <- seq_along(ortho.addr$case)

    if (cores == 1 | .Platform$OS.type == "windows") {
      short.path <- lapply(idx, function(i) {
        case.node <- ortho.addr[i, "node"]
        pump.node <- ortho.pump[ortho.pump$id == p.id[i], "node"]
        igraph::shortest_paths(g, case.node, pump.node, weights = edges$d)$vpath
      })
    } else if (cores > 1 & .Platform$OS.type != "windows") {
      short.path <- parallel::mclapply(idx, function(i) {
        case.node <- ortho.addr[i, "node"]
        pump.node <- ortho.pump[ortho.pump$id == p.id[i], "node"]
        igraph::shortest_paths(g, case.node, pump.node, weights = edges$d)$vpath
      }, mc.cores = cores)
    }

    names(short.path) <- ortho.addr$case
  }

  if (case.set == "expected") {
    case <- c(case.pump.isolate, ortho.addr$case)
    pump <- c(p.id.iso, p.id)
    distance <- c(min.dist.iso, min.dist)
    path <- c(short.path.pump.isolate, short.path)
  } else {
    case <- ortho.addr$case
    pump <- p.id
    distance <- min.dist
    path <- short.path
  }

  list(case = case, pump = pump, distance = distance, path = path)
}

