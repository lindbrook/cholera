#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected" or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @param latlong Logical.
#' @export

walkingB <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE,
  case.set = "observed", multi.core = TRUE, dev.mode = FALSE, latlong = FALSE) {

  cores <- multiCore(multi.core)
  win.exception <- .Platform$OS.type == "windows" & cores > 1
  if (win.exception) cores <- 1L

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  p.sel <- selectPump(pump.data, pump.select = pump.select, vestry = vestry)

  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('case.set must be "observed", "expected" or "snow".', call. = FALSE)
  }

  snow.colors <- snowColors(vestry = vestry)

  dat <- neighborhoodDataB(case.set = case.set, vestry = vestry,
    latlong = latlong)

  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges
  nodes.pump <- dat$nodes.pump

  case.data <- nodes[nodes$case != 0, ]
  case.data <- case.data[order(case.data$case), ]
  case <- case.data$case

  ds <- lapply(case, function(x) {
    igraph::distances(graph = g,
                      v = nodes[nodes$case == x, "node"],
                      to = nodes.pump[nodes.pump$pump %in% p.sel, "node"],
                      weights = edges$d)
  })

  d <- vapply(ds, min, numeric(1L))

  pump <- p.sel[vapply(ds, which.min, numeric(1L))]
  pump.nm <- paste0("p", sort(unique(pump)))

  paths <- lapply(seq_along(case), function(i) {
    igraph::shortest_paths(graph = g,
                           from = nodes[nodes$case == case[i], "node"],
                           to = nodes.pump[nodes.pump$pump == pump[i], "node"],
                           weights = edges$d)$vpath
  })

  names(paths) <- case

  pump.case <- lapply(sort(unique(pump)), function(x) case[pump %in% x])
  names(pump.case) <- pump.nm

  path.pump <- lapply(sort(unique(pump)), function(x) paths[pump %in% x])
  names(path.pump) <- pump.nm

  neigh.edges <- lapply(names(path.pump), function(nm) {
    p.neigh <- path.pump[[nm]]
    p <- lapply(p.neigh, function(x) names(unlist(x)))
    edge.id <- lapply(p, identifyEdge, edges)
    edges[unique(unlist(edge.id)), "id2"]
  })

  names(neigh.edges) <- pump.nm

  out <- list(pump.case = pump.case,
              pump.data = pump.data,
              edges = edges,
              neigh.edges = neigh.edges,
              case.set = case.set,
              p.sel = p.sel,
              snow.colors = snow.colors,
              pump.select = pump.select,
              latlong = latlong)

  class(out) <- "walkingB"
  out
}

#' Plot method for walkingB().
#'
#' @param x An object of class "walking" created by \code{walkingNominal()}.
#' @param type Character. "roads", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param tsp.method Character. Traveling salesperson problem algorithm.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @note When plotting area graphs with simulated data (i.e., \code{case.set = "expected"}), there may be discrepancies between observed cases and expected neighborhoods, particularly between neighborhoods. type = "roads" inspired by Shiode et. al. (2015).
#' @export

plot.walkingB <- function(x, type = "roads", tsp.method = "repetitive_nn",
  ...) {

  if (type %in% c("roads", "area.points", "area.polygons") == FALSE) {
    stop('type must be "roads", "area.points", "area.polygons".', call. = FALSE)
  }

  if (type %in% c("area.points", "area.polygons")) {
    if (x$case.set != "expected") {
      stop('area plots valid only when case.set = "expected".', call. = FALSE)
    }
  }

  edges <- x$edges
  neigh.edges <- x$neigh.edges

  if (x$latlong) {
    vars <- c("lon", "lat")
    seg.vars <- paste0(vars, c(rep(1, 2), rep(2, 2)))
  } else {
    vars <- c("x", "y")
    seg.vars <- paste0(vars, c(rep(1, 2), rep(2, 2)))
  }

  snowMap(add.cases = FALSE, add.pumps = FALSE, latlong = x$latlong)

  invisible(lapply(names(neigh.edges), function(nm) {
    n.edges <- edges[edges$id2 %in% neigh.edges[[nm]], ]
    segments(n.edges[, seg.vars[1]], n.edges[, seg.vars[2]],
             n.edges[, seg.vars[3]], n.edges[, seg.vars[4]],
             lwd = 2, col = x$snow.colors[nm])
  }))

  invisible(lapply(names(x$pump.case), function(nm) {
    sel <- cholera::fatalities.address$anchor %in% x$pump.case[[nm]]
    points(cholera::fatalities.address[sel, vars], pch = 20,
      cex = 0.75, col = x$snow.colors[nm])
  }))

  if (is.null(x$pump.select)) {
    points(x$pump.data[, vars], pch = 24, lwd = 2, col = x$snow.colors)
    text(x$pump.data[, vars], pos = 1, cex = 0.9,
      labels = paste0("p", x$pump.data$id))
  } else {
    obs <- x$pump.data$id %in% x$p.sel
    pos.data <- x$pump.data[obs, vars]
    neg.data <- x$pump.data[!obs, vars]
    pos.labels <- paste0("p", x$pump.data$id[obs])
    neg.labels <- paste0("p", x$pump.data$id[!obs])
    points(pos.data, pch = 24, lwd = 2, col = x$snow.colors[obs])
    points(neg.data, pch = 24, lwd = 1, col = "gray")
    text(pos.data, pos = 1, cex = 0.9, labels = pos.labels)
    text(neg.data, pos = 1, cex = 0.9, col = "gray", labels = neg.labels)
  }

  if (is.null(x$pump.select)) {
    title(main = "Pump Neighborhoods: Walking")
  } else {
    if (length(x$pump.select) > 1) {
      title(main = paste0("Pump Neighborhoods: Walking", "\n", "Pumps ",
        paste(sort(x$pump.select), collapse = ", ")))
    } else if (length(x$pump.select) == 1) {
      title(main = paste0("Pump Neighborhoods: Walking", "\n", "Pump ",
        paste(sort(x$pump.select), collapse = ", ")))
    }
  }
}
