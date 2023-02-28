#' Compute shortest georeferenced distances (and walking paths) to selected pumps (prototype).
#'
#' @param pump.select Numeric. Pump candidates to consider. Default is \code{NULL}: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param metric Character. "euclidean" or "walking".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export
#' @return An R data frame or list of 'igraph' path nodes.

latlongNearestPump <- function(pump.select = NULL, metric = "walking",
  vestry = FALSE, weighted = TRUE, time.unit = "second", walking.speed = 5,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)

  if (metric == "euclidean") {
    if (vestry) {
      pump.data <- cholera::pumps.vestry
    } else {
      pump.data <- cholera::pumps
    }

    p.sel <- selectPump(pump.data, pump.select = pump.select,
      metric = "euclidean", vestry = vestry)

    vars <- c("lon", "lat")

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

    if (time.unit == "hour") {
      out$time <- out$distance / (1000L * walking.speed)
    } else if (time.unit == "minute") {
      out$time <- (60L * out$distance) / (1000L * walking.speed)
    } else if (time.unit == "second") {
      out$time <- (3600L * out$distance) / (1000L * walking.speed)
    }

  } else if (metric == "walking") {
    dat <- latlongNeighborhoodData(vestry, multi.core = cores)
    path.data <- latlong_pathData(dat, pump.select, weighted, vestry,
      cores)

    if (time.unit == "hour") {
      walking.time <- path.data$distance / (1000L * walking.speed)
    } else if (time.unit == "minute") {
      walking.time <- (60L * path.data$distance) / (1000L * walking.speed)
    } else if (time.unit == "second") {
      walking.time <- (3600L * path.data$distance) / (1000L * walking.speed)
    }

    distance <- data.frame(case = path.data$case, pump = path.data$pump,
      distance = path.data$distance, time = walking.time)
    out <- list(neigh.data = dat, distance = distance, path = path.data$path)

  } else stop('metric must be  "euclidean" or "walking".', call. = FALSE)

  out
}

latlong_pathData <- function(dat, pump.select, weighted, vestry, cores) {
  g <- dat$g
  edge.list <- dat$edge.list
  edges <- dat$edges
  ortho.addr <- cholera::latlong.ortho.addr

  if (vestry) ortho.pump <- cholera::latlong.ortho.pump.vestry
  else ortho.pump <- cholera::latlong.ortho.pump
  names(ortho.pump)[names(ortho.pump) == "pump.id"] <- "pump"

  if (!is.null(pump.select)) {
    if (all(pump.select > 0)) {
      ortho.pump <- ortho.pump[ortho.pump$pump %in% pump.select, ]
    } else if (all(pump.select < 0)) {
      ortho.pump <- ortho.pump[!ortho.pump$pump %in% -pump.select, ]
    } else {
      stop('If not NULL, "pump.select" must be strictly positive or negative.',
        call. = FALSE)
    }
  }

  ortho.addr$node <- paste0(ortho.addr$lon, "_&_", ortho.addr$lat)
  ortho.pump$node <- paste0(ortho.pump$lon, "_&_", ortho.pump$lat)

  ## Adam and Eve Court: isolate with pump (#2) ##
  sel <- cholera::road.segments$name == "Adam and Eve Court"
  adam.eve <- cholera::road.segments[sel, "id"]
  adam.eve.pump <- ortho.pump[ortho.pump$road.segment == adam.eve, "node"]
  ortho.pump <- ortho.pump[!ortho.pump$node %in% adam.eve.pump, ]

  ## Falconberg Court and Mews: isolate without pump ##
  # falconberg.court.mews <- c("40-1", "41-1", "41-2", "63-1")
  # ortho.addr[ortho.addr$seg %in% falconberg.court.mews, ]

  paths <- lapply(ortho.addr$node, function(case.node) {
     p <- igraph::shortest_paths(g, case.node, ortho.pump$node,
       weights = edges$d)$vpath
     stats::setNames(p, ortho.pump$pump)
  })

  distances <- lapply(ortho.addr$node, function(case.node) {
    igraph::distances(g, case.node, ortho.pump$node, weights = edges$d)
  })

  min.dist <- vapply(distances, function(x) x[which.min(x)], numeric(1L))
  path.sel <- vapply(distances, which.min, integer(1L))

  nearest.pump <- vapply(path.sel, function(i) ortho.pump[i, "pump"],
    numeric(1L))

  short.path <- lapply(seq_along(paths), function(i) {
    paths[[i]][path.sel[i]]
  })

  list(case = ortho.addr$case, pump = nearest.pump, distance = min.dist,
    path = short.path)
}

# latlong.nearest.pump <- latlongNearestPump()
# latlong.nearest.pump.vestry <- latlongNearestPump(vestry = TRUE)
# usethis::use_data(latlong.nearest.pump, overwrite = TRUE)
# usethis::use_data(latlong.nearest.pump.vestry, overwrite = TRUE)
