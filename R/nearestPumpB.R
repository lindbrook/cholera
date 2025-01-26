#' Compute shortest distances from case anchors to selected pumps.
#'
#' @param pump.select Numeric. Pump candidates to consider. Default is \code{NULL}: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param metric Character. "euclidean" or "walking".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed" or "expected".
#' @param location Character. For cases and pumps. "nominal", "anchor" or "orthogonal".
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. Meaningful only when "weighted" is \code{TRUE}. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param latlong Logical. \code{TRUE} Use longitude and latitude coordinates.
#' @note Time is computed using \code{distanceTime()}.
#' @noRd

nearestPumpB <- function(pump.select = NULL, metric = "walking",
  vestry = FALSE, weighted = TRUE, case.set = "observed", location = "nominal",
  distance.unit = "meter", time.unit = "second", walking.speed = 5,
  latlong = FALSE) {

 if (!distance.unit %in% c("meter", "yard", "native")) {
    stop('distance.unit must be "meter", "yard" or "native".', call. = FALSE)
  }

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  p.sel <- selectPump(pump.data, pump.select = pump.select, metric = metric,
    vestry = vestry)

  pump.data <- pump.data[pump.data$id %in% p.sel, ]

  if (metric == "walking" | (latlong = TRUE & metric == "euclidean")) {
    dat <- neighborhoodDataB(vestry = vestry, case.set = case.set,
      latlong = latlong)

    g <- dat$g
    edges <- dat$edges
    nodes <- dat$nodes
    nodes.pump <- dat$nodes.pump[dat$nodes.pump$pump %in% p.sel, ]
  }

  idx <- seq_len(nrow(cholera::fatalities.address))

  if (latlong) {
    vars <- c("lon", "lat")

    if (metric == "euclidean") {
      pump.dist <- lapply(idx, function(i) {
        egos <- nodes[nodes$case != 0, ]
        egos <- egos[order(egos$case), ]
        alters <- nodes[nodes$pump != 0, ]
        alters <- alters[order(alters$pump), ]

        ds <- geosphere::distGeo(egos[i, vars], alters[, vars])

        sel <- which.min(ds)
        d <- ds[sel] / unitMeter(1)
        p <- p.sel[sel]
        data.frame(pump = p, d = d)
      })

      pump.dist <- do.call(rbind, pump.dist)

      t <- distanceTime(pump.dist$d, distance.unit = distance.unit,
        time.unit = time.unit, walking.speed = walking.speed)

      nr.pump <- data.frame(case = cholera::fatalities.address$anchor,
        pump = pump.dist$pump, distance = unitMeter(pump.dist$d, distance.unit),
        time = t)

    } else if (metric == "walking") {
      egos <- nodes[nodes$case != 0, ]
      egos <- egos[order(egos$case), ]
      alters <- nodes[nodes$pump != 0, ]
      alters <- alters[order(alters$pump), ]

      ds <- igraph::distances(graph = g, v = egos$node, to = alters$node,
        weights = edges$d)

      sel <- apply(ds, 1, which.min)
      p <- p.sel[sel]
      d <- vapply(seq_along(sel), function(i) ds[i, sel[i]], numeric(1L))
      d <- d / unitMeter(1)

      t <- distanceTime(d, distance.unit = distance.unit, time.unit = time.unit,
        walking.speed = walking.speed)

      nr.pump <- data.frame(case = egos$case, pump = p, distance = unitMeter(d),
        time = t)
    }

  } else {
    vars <- c("x", "y")

    if (metric == "euclidean") {
      pump.dist <- lapply(idx, function(i) {
        dat <- rbind(cholera::fatalities.address[i, vars], pump.data[, vars])
        ds <- stats::dist(dat)[1:nrow(pump.data)]
        sel <- which.min(ds)
        data.frame(pump = p.sel[sel], d = ds[sel])
      })

      pump.dist <- do.call(rbind, pump.dist)

      t <- distanceTime(pump.dist$d, distance.unit = distance.unit,
        time.unit = time.unit, walking.speed = walking.speed)

      nr.pump <- data.frame(case = cholera::fatalities.address$anchor,
        pump = pump.dist$pump, distance = unitMeter(pump.dist$d, distance.unit),
        time = t)

    } else if (metric == "walking") {
      sel <- nodes$case %in% cholera::fatalities.address$anchor
      anchors <- nodes[sel, ]
      anchors <- anchors[order(anchors$case), ]

      ds <- igraph::distances(graph = g, v = anchors$node, to = nodes.pump$node,
        weights = edges$d)

      sel <- apply(ds, 1, which.min)
      d <- vapply(seq_along(sel), function(i) ds[i, sel[i]], numeric(1L))

      t <- distanceTime(d, distance.unit = distance.unit, time.unit = time.unit,
        walking.speed = walking.speed)

      nr.pump <- data.frame(case = anchors$case, pump = p.sel[sel],
        distance = unitMeter(d, distance.unit = distance.unit), time = t,
        row.names = NULL)
    }
  }
  nr.pump
}
