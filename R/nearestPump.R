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
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @note Time is computed using \code{distanceTime()}.
#' @noRd

nearestPump <- function(pump.select = NULL, metric = "walking",
  vestry = FALSE, weighted = TRUE, case.set = "observed", location = "nominal",
  distance.unit = "meter", time.unit = "second", walking.speed = 5,
  latlong = FALSE, multi.core = FALSE) {

  if (!metric %in% c("euclidean", "walking")) {
    stop('metric must be "euclidean" or "walking".', call. = FALSE)
  }

  if (!case.set %in% c("observed", "expected")) {
    stop('case.set must be "observed" or "expected".', call. = FALSE)
  }

  if (!distance.unit %in% c("meter", "yard", "native")) {
    stop('distance.unit must be "meter", "yard" or "native".', call. = FALSE)
  }

  if (!time.unit %in% c("hour", "minute", "second")) {
    stop('distance.unit must be "hour", "minute" or "second".', call. = FALSE)
  }

  if (.Platform$OS.type == "windows") {
    cores <- 1L
  } else {
    cores <- multiCore(multi.core)
  }

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  p.sel <- selectPump(pump.data, pump.select = pump.select, vestry = vestry)
  pump.data <- pump.data[pump.data$id %in% p.sel, ]

  if (metric == "walking" | (latlong == TRUE & metric == "euclidean")) {
    dat <- neighborhoodData(vestry = vestry, case.set = case.set,
      latlong = latlong)

    g <- dat$g
    edges <- dat$edges
    nodes <- dat$nodes
    nodes.pump <- dat$nodes.pump[dat$nodes.pump$pump %in% p.sel, ]
  }

  if (case.set == "observed") {
    case.data <- cholera::fatalities.address
  } else if (case.set == "expected") {
    case.data <- cholera::regular.cases
  }

  if (latlong) {
    vars <- c("lon", "lat")
    egos <- nodes[nodes$case != 0, ]
    egos <- egos[order(egos$case), ]
    alters <- nodes[nodes$pump != 0, ]
    alters <- alters[order(alters$pump), ]

    idx <- seq_len(nrow(egos))

    if (metric == "euclidean") {
      pump.dist <- parallel::mclapply(idx, function(i) {
        ds <- geosphere::distGeo(egos[i, vars], alters[, vars])
        sel <- which.min(ds)
        d <- ds[sel]
        p <- p.sel[sel]
        data.frame(pump = p, d = d)
      }, mc.cores = cores)

      pump.dist <- do.call(rbind, pump.dist)

      if (distance.unit %in% c("native", "yard")) {
        d <- pump.dist$d / unitMeter(1)
        if (distance.unit == "yard") {
          d <- unitMeter(d, distance.unit = "yard")
        }
      } else if (distance.unit == "meter") {
        d <- pump.dist$d
      }

      t <- distanceTime(pump.dist$d / unitMeter(1L),
        distance.unit = distance.unit, time.unit = time.unit,
        walking.speed = walking.speed)

      nr.pump <- data.frame(case = egos$case, pump = pump.dist$pump,
        distance = d, time = t)

    } else if (metric == "walking") {
      ds <- igraph::distances(graph = g, v = egos$node, to = alters$node,
        weights = edges$d)

      sel <- apply(ds, 1, which.min)
      p <- p.sel[sel]

      d <- vapply(seq_along(sel), function(i) ds[i, sel[i]], numeric(1L))

      t <- distanceTime(d / unitMeter(1), distance.unit = distance.unit,
        time.unit = time.unit, walking.speed = walking.speed)

      if (distance.unit %in% c("native", "yards")) {
        d <- unitMeter(d, distance.unit = distance.unit)
      }

      nr.pump <- data.frame(case = egos$case, pump = p, distance = d, time = t)
    }

  } else {
    vars <- c("x", "y")
    idx <- seq_len(nrow(case.data))

    if (metric == "euclidean") {
      if (case.set == "observed") {
        dat <- rbind(case.data[, vars], pump.data[, vars])
        ds <- stats::dist(dat)
        n <- nrow(case.data) + nrow(pump.data)

        values1 <- seq_len(n - 1)
        times1 <- rev(values1)
        v1 <- unlist(lapply(seq_along(values1), function(i) {
          rep(values1[i], times1[i])
        }))

        values2 <- seq_len(n)[-1]
        v2 <- lapply(seq_along(values2), function(i) {
          values2[i]:max(values2)
        })

        distances <- data.frame(v1 = unlist(v1), v2 = unlist(v2))
        distances$d <- c(ds)

        sel <- distances$v1 %in% 1:nrow(case.data) &
               distances$v2 %in% (nrow(case.data) + 1):n

        nr.pump <- distances[sel, ]
        nr.pump$case <- 0
        nr.pump$pump <- 0

        for (i in unique(nr.pump$v1)) {
          nr.pump$case[nr.pump$v1 == i] <- case.data$anchor[i]
        }

        p.id <- unique(nr.pump$v2)

        for (i in seq_along(p.id)) {
          nr.pump$pump[nr.pump$v2 == p.id[i]] <- pump.data$id[i]
        }

        nr.pump <- lapply(split(nr.pump, nr.pump$v1), function(x) {
          x[which.min(x$d), ]
        })

        nr.pump <- do.call(rbind, nr.pump)

        nr.pump$distance <- unitMeter(nr.pump$d, distance.unit = distance.unit)
        nr.pump$time <- distanceTime(nr.pump$d, distance.unit = distance.unit,
          time.unit = time.unit, walking.speed = walking.speed)

        nr.pump <- nr.pump[, !names(nr.pump) %in% c("v1", "v2", "d")]

      } else if (case.set == "expected") {
        pump.dist <- parallel::mclapply(idx, function(i) {
          tmp <- rbind(case.data[i, vars], pump.data[, vars])
          dat <- data.frame(tmp, row.names = NULL)
          ds <- stats::dist(dat)[1:nrow(pump.data)]
          pmp.id <- which.min(ds)
          data.frame(case = i + 2000L, pump = p.sel[pmp.id], d = ds[pmp.id])
        }, mc.cores = cores)

        nr.pump <- do.call(rbind, pump.dist)
        nr.pump$distance <- unitMeter(nr.pump$d, distance.unit)
        nr.pump$time <- distanceTime(nr.pump$d, distance.unit = distance.unit,
          time.unit = time.unit, walking.speed = walking.speed)

        nr.pump <- nr.pump[, names(nr.pump) != "d"]
      }

    } else if (metric == "walking") {
      if (case.set == "observed") {
        sel <- nodes$case %in% case.data$anchor
        anchors <- nodes[sel, ]
      } else if (case.set == "expected") {
        anchors <- nodes[nodes$case != 0, ]
      }

      anchors <- anchors[order(anchors$case), ]

      ds <- igraph::distances(graph = g, v = anchors$node, to = nodes.pump$node,
        weights = edges$d)

      pmp.id <- apply(ds, 1, which.min)
      d <- vapply(seq_along(pmp.id), function(i) ds[i, pmp.id[i]], numeric(1L))

      t <- distanceTime(d, distance.unit = distance.unit, time.unit = time.unit,
        walking.speed = walking.speed)

      d <- unitMeter(d, distance.unit = distance.unit)

      nr.pump <- data.frame(case = anchors$case, pump = p.sel[pmp.id],
        distance = d, time = t, row.names = NULL)
    }
  }
  nr.pump
}
