#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

latlongNeighborhoodWalking <- function(pump.select = NULL, vestry = FALSE,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)
  snow.colors <- snowColors(vestry = vestry)

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  pump.id <- selectPump(pump.data, pump.select = pump.select, vestry = vestry)

  nearest.data <- latlongNearestPump(pump.select = pump.id, vestry = vestry,
    multi.core = cores)

  nearest.dist <- nearest.data$distance
  nearest.path <- nearest.data$path
  neigh.data <- nearest.data$neigh.data

  nearest.pump <- data.frame(case = cholera::fatalities.address$anchor,
                             pump = nearest.dist$pump)

  pumpID <- sort(unique(nearest.dist$pump))

  neighborhood.cases <- lapply(pumpID, function(p) {
    nearest.pump[nearest.pump$pump == p, "case"]
  })

  names(neighborhood.cases) <- pumpID

  neighborhood.paths <- lapply(pumpID, function(p) {
    n.case <- neighborhood.cases[[paste(p)]]
    nearest.path[which(nearest.pump$case %in% n.case)]
  })

  names(neighborhood.paths) <- pumpID

  out <- list(neigh.data = neigh.data,
              paths = neighborhood.paths,
              cases = stats::setNames(neighborhood.cases, paste0("p", pumpID)),
              vestry = vestry,
              pump.select = pump.select,
              snow.colors = snow.colors,
              pumpID = pumpID,
              cores = cores)

  class(out) <- "latlong_walking"
  out
}

#' Plot method for latlongNeighborhoodWalking().
#'
#' @param x An object of class "latlong_walking" created by \code{latlongNeighborhoodWalking()}.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.latlong_walking <- function(x, ...) {
  dat <- x$neigh.data
  edges <- dat$edges
  paths <- x$paths
  vars <- c("lon", "lat")

  obs.edges <- lapply(paths, function(p) {
    oe <- lapply(p, function(x) {
      nodes.tmp <- names(unlist(unname(x)))
      identifyEdgesB(nodes.tmp, edges)
    })
    unique(unlist(oe))
  })

  snowMap(latlong = TRUE, add.cases = FALSE, add.pumps = FALSE)

  invisible(lapply(names(obs.edges), function(nm) {
    n.edges <- edges[obs.edges[[nm]], ]
    segments(n.edges$lon1, n.edges$lat1, n.edges$lon2, n.edges$lat2, lwd = 2,
      col = x$snow.colors[paste0("p", nm)])
  }))

  invisible(lapply(names(x$cases), function(nm) {
    sel <- cholera::fatalities.address$anchor %in% x$cases[[nm]]
    points(cholera::fatalities.address[sel, vars], pch = 20, cex = 0.75,
      col = x$snow.colors[nm])
  }))

  if (x$vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }

  if (is.null(x$pump.select)) {
    points(p.data[, vars], col = x$snow.colors, lwd = 2, pch = 24)
    text(p.data[, vars], labels = paste0("p", p.data$id), cex = 0.9, pos = 1)
  } else {
    pump.id <- selectPump(p.data, pump.select = x$pump.select, 
      vestry = x$vestry)
    sel <- p.data$id %in% pump.id
    unsel <- setdiff(p.data$id, pump.id)
    points(p.data[sel, vars], col = x$snow.colors[sel], lwd = 2, pch = 24)
    text(p.data[sel, vars], labels = paste0("p", p.data$id[sel]), cex = 0.9,
      pos = 1)
    points(p.data[unsel, vars], col = "gray", lwd = 2, pch = 24)
    text(p.data[unsel, vars], labels = paste0("p", p.data$id[unsel]), cex = 0.9,
      pos = 1, col = "gray")
  }

  if (is.null(x$pump.select)) {
    title(main = "Pump Neighborhoods: Walking")
  } else {
    title(main = paste0("Pump Neighborhoods: Walking", "\n", "Pumps ",
      paste(sort(x$pump.select), collapse = ", ")))
  }
}

identifyEdgesB <- function(p, edges) {
  vapply(seq_along(p[-1]), function(i) {
    ab <- edges$node1 %in% p[i] &
      edges$node2 %in% p[i + 1]
    ba <- edges$node2 %in% p[i] &
      edges$node1 %in% p[i + 1]
    which(ab | ba)
  }, numeric(1L))
}
