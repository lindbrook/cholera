#' Compute shortest georeferenced distances and paths to selected pumps (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param pump.select Numeric. Pump candidates to consider. Default is \code{NULL}: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param metric Character. "eucldidean" or "walking".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export
#' @return An R data frame or list of 'igraph' path nodes.

latlongNearestPump <- function(path, pump.select = NULL, metric = "walking",
  vestry = FALSE, weighted = TRUE, time.unit = "second", walking.speed = 5,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)

  if (metric == "euclidean") {
    vars <- c("lon", "lat")

    out <- parallel::mclapply(cholera::fatalities.address$anchor, function(x) {
      sel <- cholera::fatalities.address$anchor == x
      ego <- cholera::fatalities.address[sel, vars]
      if (is.null(pump.select)) {
        alters <- cholera::pumps[, c("id", vars)]
      } else {
        sel <- cholera::pumps$id %in% pump.select
        alters <- cholera::pumps[sel, c("id", vars)]
      }

      d <- vapply(alters$id, function(id) {
        geosphere::distGeo(ego, alters[alters$id == id, vars])
      }, numeric(1L))

      sel <- which.min(d)
      data.frame(case = x, pump = alters$id[sel], dist = d[sel])
    }, mc.cores = cores)

    out <- do.call(rbind, out)

  } else if (metric == "walking") {
    dat <- latlongNeighborhoodData(path, vestry)
    path.data <- latlong_pathData(path, dat, pump.select, weighted, vestry, cores)

    if (time.unit == "hour") {
      walking.time <- path.data$distance / (1000L * walking.speed)
    } else if (time.unit == "minute") {
      walking.time <- (60L * path.data$distance) / (1000L * walking.speed)
    } else if (time.unit == "second") {
      walking.time <- (3600L * path.data$distance) / (1000L * walking.speed)
    }

    distance <-  data.frame(case = path.data$case, pump = path.data$pump,
      d = path.data$distance, time = walking.time)
    out <- list(distance = distance, path = path.data$path)
  }

  out
}

latlong_pathData <- function(path, dat, pump.select, weighted, vestry, cores) {
  g <- dat$g
  edge.list <- dat$edge.list
  edges <- dat$edges
  ortho.addr <- latlongOrthoAddress(path)
  ortho.pump <- latlongOrthoPump(path, vestry = vestry)
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

  ortho.addr$node <- paste0(ortho.addr$lon, "-", ortho.addr$lat)
  ortho.pump$node <- paste0(ortho.pump$lon, "-", ortho.pump$lat)

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
