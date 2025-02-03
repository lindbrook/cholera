#' Compute Euclidean pump neighborhoods (lat-long prototype).
#'
#' Plots star graph from pump to its cases.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param case.set Character. "observed" or "expected".
#' @param location "nominal", "anchor" or "orthogonal".
#' @importFrom sp point.in.polygon
#' @noRd

euclideanLatlong <- function(pump.select = NULL, vestry = FALSE,
  case.set = "observed", location = "nominal") {

  if (case.set %in% c("observed", "expected") == FALSE) {
    stop('case.set must be "observed" or "expected".', call. = FALSE)
  }

  if (location %in% c("nominal", "anchor", "orthogonal") == FALSE) {
    stop('location must be "nominal", "anchor" or "orthogonal".', call. = FALSE)
  } else if (location == "orthogonal") {
    if (vestry) pump.data <- cholera::latlong.ortho.pump.vestry
    else pump.data <- cholera::latlong.ortho.pump
  } else if (location %in% c("anchor", "nominal")) {
    if (vestry) pump.data <- cholera::pumps.vestry
    else pump.data <- cholera::pumps
  }

  cells <- latlongVoronoiVertices(pump.select = pump.select,
    vestry = vestry)$cells
  pump.id <- selectPump(pump.data, pump.select, vestry)

  if (case.set == "observed") {
    if (location == "nominal") {
      case.data <- cholera::fatalities
    } else if (location == "anchor") {
      case.data <- cholera::fatalities.address
    } else if (location == "orthogonal") {
      case.data <- cholera::latlong.ortho.addr
    }
  } else if (case.set == "expected") {
    if (location %in% c("nominal", "anchor")) {
      case.data <- cholera::latlong.regular.cases
    } else if (location == "orthogonal") {
      case.data <- cholera::latlong.sim.ortho.proj
    }
  }

  statistic.data <- lapply(cells, function(cell) {
    sp::point.in.polygon(case.data$lon, case.data$lat, cell$lon, cell$lat)
  })

  out <- list(pump.select = pump.select,
              pump.id = pump.id,
              vestry = vestry,
              cells = cells,
              pump.data = pump.data,
              case.set = case.set,
              location = location,
              snow.colors = snowColors(vestry = vestry),
              statistic.data = statistic.data)

  class(out) <- "euclideanLatlong"
  out
}

#' Plot method for euclideanLatlong()
#' @param x Object.
#' @param type Character. "star", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param ... Additional plotting parameters.
#' @export

plot.euclideanLatlong <- function(x, type = "star", ...) {
  if (!type %in% c("area.points", "area.polygons", "star")) {
    stop('type must be "area.points", "area.polygons" or "star".',
      call. = FALSE)
  }

  if (type %in% c("area.points", "area.polygons")) {
    if (x$case.set != "expected") {
      stop('area plots valid only when case.set = "expected".', call. = FALSE)
    }
  }

  if (x$location == "orthogonal") {
    if (x$case.set != "observed") {
      msgA <- 'location = "orthogonal" valid '
      msgB <- 'only with case.set = "observed".'
      stop(msgA, msgB, call. = FALSE)
    }
  }

  pump.id <- x$pump.id
  vars <- c("lon", "lat")

  if (x$case.set == "observed") {
    snowMap(vestry = x$vestry, latlong = TRUE, add.cases = FALSE,
      add.pumps = FALSE)
    pumpTokens(x, type, latlong = TRUE)

    if (!is.null(type)) {
      if (type == "star") {
        latlongEuclideanStar(x, vars)
      }
    }

    latlongEuclideanCases(x, vars)

  } else if (x$case.set == "expected") {
    snowMap(vestry = x$vestry, add.cases = FALSE, add.pumps = FALSE,
      add.roads = FALSE, latlong = TRUE)

    if (type == "star") latlongEuclideanStar(x, vars)
    else if (type == "area.points") latlongEuclideanCases(x, vars)
    else if (type == "area.polygons") latlongEuclideanAreaPolygons(x)
    else stop('type must be "star", "area.points" or "area.polygons".')

    addRoads(latlong = TRUE, col = "black")
    pumpTokens(x, type, latlong = TRUE)
  }

  if (!is.null(pump.id)) {
    if (x$location == "nominal") {
      title(main = paste0("Pump Neighborhoods: Euclidean (nominal)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    } else if (x$location == "orthogonal") {
      title(main = paste0("Pump Neighborhoods: Euclidean (orthogonal)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    }
  } else {
    if (x$location == "nominal") {
      title(main = "Pump Neighborhoods: Euclidean (nominal)")
    } else if (x$location == "orthogonal") {
      title(main = "Pump Neighborhoods: Euclidean (orthogonal)")
    }
  }
}

latlongEuclideanCases <- function(x, vars) {
  if (x$case.set == "observed") {
    if (x$location == "nominal") {
      dat <- cholera::fatalities
    } else if (x$location == "anchor") {
      dat <- cholera::fatalities.address
      names(dat)[names(dat) == "anchor"] <- "case"
    } else if (x$location == "orthogonal") {
      dat <- cholera::latlong.ortho.addr
    }
  } else if (x$case.set == "expected") {
    if (x$location %in% c("nominal", "anchor")) {
      dat <- cholera::latlong.regular.cases
      dat$case <- seq_len(nrow(dat))
    } else if (x$location == "orthogonal") {
      dat <- cholera::latlong.sim.ortho.proj
    }
  }

  case.partition <- lapply(x$statistic.data, function(neighbor) {
    dat$case[neighbor == 1]
  })

  invisible(lapply(names(case.partition), function(nm) {
    sel <- dat$case %in% case.partition[[nm]]
    points(dat[sel, vars], col = x$snow.colors[nm], pch = 20, cex = 0.5)
  }))
}

latlongEuclideanStar <- function(x, vars) {
  if (x$case.set == "observed") {
    if (x$location == "nominal") {
      cases <- cholera::fatalities
    } else if (x$location == "anchor") {
      cases <- cholera::fatalities.address
      names(cases)[names(cases) == "anchor"] <- "case"
    } else if (x$location == "orthogonal") {
      cases <- cholera::latlong.ortho.addr
    }

  } else if (x$case.set == "expected") {
    if (x$location %in% c("nominal", "anchor")) {
      cases <- cholera::latlong.regular.cases
      cases$case <- seq_len(nrow(cases))
    } else if (x$location == "orthogonal") {
      cases <- cholera::latlong.sim.ortho.proj
    }
  }

  pump.id <- x$pump.id

  nearest.pump <- do.call(rbind, lapply(cases$case, function(cs) {
    p1 <- cases[cases$case == cs, vars]
    d <- vapply(pump.id, function(p) {
      p2 <- x$pump.data[x$pump.data$id == p, vars]
      geosphere::distGeo(p1, p2)
    }, numeric(1L))
    near.id <- which.min(d)
    if (is.null(pump.id)) p.nr <- x$pump.data$id[near.id]
    else p.nr <- pump.id[near.id]
    data.frame(case = cs, pump = p.nr, meters = d[near.id])
  }))

  invisible(lapply(nearest.pump$case, function(cs) {
    ego <- cases[cases$case == cs, vars]
    p <- nearest.pump[nearest.pump$case == cs, "pump"]
    alter <- x$pump.data[x$pump.data$id == p, vars]
    segments(ego$lon, ego$lat, alter$lon, alter$lat,
             col = x$snow.colors[paste0("p", p)], lwd = 0.5)
  }))
}

latlongEuclideanAreaPolygons <- function(x) {
  if (x$case.set == "expected") {
    if (x$location %in% c("nominal", "anchor")) {
      dat <- cholera::latlong.regular.cases
      dat$case <- seq_len(nrow(dat))
    } else if (x$location == "orthogonal") {
      dat <- cholera::latlong.sim.ortho.proj
    }
  }

  neighborhood.cases <- lapply(x$statistic.data, function(neighbor) {
    dat$case[neighbor == 1]
  })

  periphery.cases <- lapply(neighborhood.cases, function(x) {
    peripheryCases(x, latlong = TRUE)
  })

  pearl.string <- lapply(periphery.cases, function(x) {
    travelingSalesman(x, latlong = TRUE)
  })

  invisible(lapply(names(pearl.string), function(nm) {
    polygon(dat[pearl.string[[nm]], c("lon", "lat")],
      col = grDevices::adjustcolor(x$snow.colors[nm], alpha.f = 2/3))
  }))
}
