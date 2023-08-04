#' Compute shortest georeferenced distances (and walking paths) to selected pumps (prototype).
#'
#' @param pump.select Numeric. Pump candidates to consider. Default is \code{NULL}: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param metric Character. "euclidean" or "walking".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param case.set Character. "observed" or "expected".
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export
#' @return An R data frame or list of 'igraph' path nodes.

latlongNearestPump <- function(pump.select = NULL, metric = "walking",
  vestry = FALSE, case.set = "observed", weighted = TRUE, time.unit = "second",
  walking.speed = 5, multi.core = TRUE, dev.mode = FALSE) {

  cores <- multiCore(multi.core)
  vars <- c("lon", "lat")

  if (metric %in% c("euclidean", "walking") == FALSE) {
    stop('metric must either be "euclidean" or "walking".', call. = FALSE)

  } else if (metric == "euclidean") {
    if (vestry) {
      pump.data <- cholera::pumps.vestry
    } else {
      pump.data <- cholera::pumps
    }

    p.sel <- selectPump(pump.data, pump.select = pump.select,
      metric = "euclidean", vestry = vestry)

    out <- parallel::mclapply(cholera::fatalities.address$anchor, function(x) {
      sel <- cholera::fatalities.address$anchor == x
      ego <- cholera::fatalities.address[sel, vars]

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

    out <- do.call(rbind, out)

    out$time <- walkingTime(out$distance, time.unit = time.unit,
      walking.speed = walking.speed)

  } else if (metric == "walking") {
    dat <- latlongNeighborhoodData(case.set = case.set, vestry = vestry,
      multi.core = cores)

    path.data <- latlong_pathData(dat, pump.select, case.set, vestry, weighted,
      cores)

    walking.time <- walkingTime(path.data$distance, time.unit = time.unit,
      walking.speed = walking.speed)

    distance <- data.frame(case = path.data$case, pump = path.data$pump,
      distance = path.data$distance, time = walking.time)
    out <- list(neigh.data = dat, distance = distance, path = path.data$path)

  }
  out
}

latlong_pathData <- function(dat, pump.select, case.set, vestry, weighted,
  cores) {

  g <- dat$g
  edge.list <- dat$edge.list
  edges <- dat$edges

  if (case.set == "observed") {
    ortho.addr <- cholera::latlong.ortho.addr
  } else if (case.set == "expected") {
    ortho.addr <- cholera::latlong.sim.ortho.proj
  }

  if (vestry) {
    ortho.pump <- cholera::latlong.ortho.pump.vestry
    pmp <- cholera::pumps.vestry
  } else {
    ortho.pump <- cholera::latlong.ortho.pump
    pmp <- cholera::pumps
  }

  if (!is.null(pump.select)) {
    if (all(pump.select > 0)) {
      ortho.pump <- ortho.pump[ortho.pump$id %in% pump.select, ]
    } else if (all(pump.select < 0)) {
      ortho.pump <- ortho.pump[!ortho.pump$id %in% -pump.select, ]
    } else {
      stop('If not NULL, "pump.select" must be strictly positive or negative.')
    }
  }

  ortho.addr$node <- paste0(ortho.addr$lon, "_&_", ortho.addr$lat)
  ortho.pump$node <- paste0(ortho.pump$lon, "_&_", ortho.pump$lat)

  ## Adam and Eve Court: isolate with pump (#2) ##

  if (case.set == "expected" & 2L %in% pump.select) {
    rd.nm <- "Adam and Eve Court"
    sel <- cholera::road.segments[cholera::road.segments$name == rd.nm, ]$id
    adam.eve <- ortho.addr$road.segment %in% sel

    if (any(adam.eve)) {
      ortho.addr.adam.eve <- ortho.addr[adam.eve, ]
      ortho.addr <- ortho.addr[!adam.eve, ]
    }

    adam.eve.pump <- pmp[pmp$street == rd.nm, ]$id
    ortho.pump.adam.eve <- ortho.pump[ortho.pump$id == adam.eve.pump, ]
    ortho.pump <- ortho.pump[ortho.pump$id != adam.eve.pump, ]

    short.path.AE <- parallel::mclapply(ortho.addr.adam.eve$node, function(n) {
      p <- igraph::shortest_paths(g, n, ortho.pump.adam.eve$node,
        weights = edges$d)$vpath
      stats::setNames(p, ortho.pump.adam.eve$id)
    }, mc.cores = cores)

    distances.AE <- parallel::mclapply(ortho.addr.adam.eve$node, function(n) {
      igraph::distances(g, n, ortho.pump.adam.eve$node, weights = edges$d)
    }, mc.cores = cores)
  }

  ## Falconberg Court and Mews: isolate without pump ##

  rd.nm <- c("Falconberg Court", "Falconberg Mews")
  sel <- cholera::road.segments$name %in% rd.nm
  falconberg <- ortho.addr$road.segment %in% cholera::road.segments[sel, ]$id

  if (any(falconberg)) {
    ortho.addr.falconberg <- ortho.addr[falconberg, ]
    ortho.addr <- ortho.addr[!falconberg, ]
  }

  ## graph computation ##

  paths <- parallel::mclapply(ortho.addr$node, function(case.node) {
    p <- igraph::shortest_paths(g, case.node, ortho.pump$node,
      weights = edges$d)$vpath
    stats::setNames(p, ortho.pump$id)
  }, mc.cores = cores)

  distances <- parallel::mclapply(ortho.addr$node, function(case.node) {
    igraph::distances(g, case.node, ortho.pump$node, weights = edges$d)
  }, mc.cores = cores)

  min.dist <- vapply(distances, function(x) x[which.min(x)], numeric(1L))
  path.sel <- vapply(distances, which.min, integer(1L))

  nearest.pump <- vapply(path.sel, function(i) ortho.pump[i, "id"],
    numeric(1L))

  short.path <- lapply(seq_along(paths), function(i) {
    paths[[i]][path.sel[i]]
  })

  if (case.set == "expected" & 2L %in% pump.select) {
    list(case = c(ortho.addr$case, ortho.addr.adam.eve$case),
         pump = c(nearest.pump, rep(2L, nrow(ortho.addr.adam.eve))),
         distance = c(min.dist, unlist(distances.AE)),
         path = c(short.path, short.path.AE))
  } else {
    list(case = ortho.addr$case,
         pump = nearest.pump,
         distance = min.dist,
         path = short.path)
  }
}

# latlong.nearest.pump <- latlongNearestPump()
# latlong.nearest.pump.vestry <- latlongNearestPump(vestry = TRUE)
# usethis::use_data(latlong.nearest.pump, overwrite = TRUE)
# usethis::use_data(latlong.nearest.pump.vestry, overwrite = TRUE)

walkingTime <- function(dist.data, time.unit = "second", walking.speed = 5) {
  if (time.unit == "hour") {
    out <- dist.data / (1000L * walking.speed)
  } else if (time.unit == "minute") {
    out <- (60L * dist.data) / (1000L * walking.speed)
  } else if (time.unit == "second") {
    out <- (3600L * dist.data) / (1000L * walking.speed)
  }
  out
}
