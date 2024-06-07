#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
# #' @param case.location Character. "address" or "orthogonal". "address" uses the longitude and latitude of \code{fatalities.address}. "orthogonal" uses the longitude and latitude of \code{latlong.ortho.address}.
#' @param case.set Character. "observed" or "expected".
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

latlongNeighborhoodWalking <- function(pump.select = NULL, vestry = FALSE,
  case.set = "observed", weighted = TRUE, multi.core = TRUE) {

  if (!case.set %in% c("expected", "observed", "snow")) {
    stop('case.location must be "observed", "expected" or "snow".',
      call. = FALSE)
  }

  cores <- multiCore(multi.core)
  snow.colors <- snowColors(vestry = vestry)

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  pump.id <- selectPump(pump.data, pump.select = pump.select, vestry = vestry)

  nearest.data <- latlongNearestPump(pump.select = pump.id,
                                     case.set = case.set,
                                     vestry = vestry,
                                     weighted = weighted,
                                     multi.core = cores)

  nearest.dist <- nearest.data$distance
  nearest.path <- nearest.data$path
  neigh.data <- nearest.data$neigh.data
  nearest.pump <- data.frame(case = nearest.dist$case, pump = nearest.dist$pump)

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
              pump.data = pump.data,
              case.set = case.set)

  class(out) <- "latlong_walking"
  out
}

#' Plot method for latlongNeighborhoodWalking().
#'
#' @param x An object of class "latlong_walking" created by \code{latlongNeighborhoodWalking()}.
#' @param type Character. For case.set = "expected", area.points" or "area.polygons".
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.latlong_walking <- function(x, type = "area.points", ...) {
  vars <- c("lon", "lat")

  if (x$case.set %in% c("observed", "snow")) {
    snowMap(latlong = TRUE, add.cases = FALSE, add.pumps = FALSE)
    edges <- x$neigh.data$edges
    paths <- x$paths

    obs.edges <- lapply(paths, function(neighborhood) {
      edge.names <- lapply(neighborhood, function(x) names(unlist(x)))
      audited.edges <- lapply(edge.names, auditEdge, edges)
      unique(unlist(audited.edges))
    })

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

    p.data <- x$pump.data

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
      text(p.data[unsel, vars], labels = paste0("p", p.data$id[unsel]),
        cex = 0.9, pos = 1, col = "gray")
    }

    if (x$case.set == "snow") {
      if (is.null(x$pump.select)) {
        title(main = "Snow Pump Neighborhood: Walking")
      } else {
        title(main = paste0("Snow Pump Neighborhood: Walking", "\n", "Pumps ",
          paste(sort(x$pump.select), collapse = ", ")))
      }
    } else {
      if (is.null(x$pump.select)) {
        title(main = "Pump Neighborhoods: Walking")
      } else {
        title(main = paste0("Pump Neighborhoods: Walking", "\n", "Pumps ",
          paste(sort(x$pump.select), collapse = ", ")))
      }
    }
  } else if (x$case.set == "expected") {
    if (type == "area.points") {
      snowMap(latlong = TRUE, add.cases = FALSE, add.pumps = FALSE,
        add.roads = FALSE)

      invisible(lapply(names(x$cases), function(nm) {
        points(cholera::latlong.regular.cases[x$cases[[nm]], vars],
          col = x$snow.colors[nm], pch = 15)
      }))

      addRoads(latlong = TRUE, col = "white")
      addPump(latlong = TRUE, col = "white")

    } else if (type == "area.polygons") {
      snowMap(latlong = TRUE, add.cases = FALSE)

      periphery.cases <- parallel::mclapply(x$cases, peripheryCases,
        latlong = TRUE, mc.cores = x$cores)
      pearl.strings <- parallel::mclapply(periphery.cases, travelingSalesman,
        latlong = TRUE, mc.cores = x$cores)

      invisible(lapply(names(pearl.strings), function(nm) {
        polygon(cholera::latlong.regular.cases[pearl.strings[[nm]], vars],
          col = grDevices::adjustcolor(x$snow.colors[nm], alpha.f = 0.5),
          border = "black")
      }))
    }

    title(main = "Expected Pump Neighborhoods: Walking")
  }
}
