#' Compute Euclidean pump neighborhoods (lat-long prototype).
#'
#' Plots star graph from pump to its cases.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param case.location Character. "address" or "orthogonal". "address" uses the longitude and latitude of \code{fatalities.address}. "orthogonal" uses the longitude and latitude of \code{latlong.ortho.address}.
#' @param case.set Character. "observed" or "expected".
#' @param pump.location Character. "address" or "orthogonal". "address" uses the longitude and latitude coordinates of \code{pumps} or \code{pumps.vestry}. "orthogonal" uses the longitude and latitude coordinates of \code{latlong.ortho.pump} or \code{latlong.ortho.pump.vestry}.
#' @export

latlongNeighborhoodEuclidean <- function(pump.select = NULL, vestry = FALSE,
  case.location = "address", case.set = "observed", pump.location = "address") {

  if (case.location %in% c("address", "orthogonal") == FALSE) {
    stop('case.location must be "address" or "orthogonal".', call. = FALSE)
  } else {
    if (case.location == "orthogonal") statistic <- "orthogonal"
    else if (case.location == "address") statistic <- "address"
  }

  if (case.set %in% c("observed", "expected") == FALSE) {
    stop('case.set must be "observed" or "expected".', call. = FALSE)
  }

  if (pump.location %in% c("address", "orthogonal") == FALSE) {
    stop('pump.location must be "address" or "orthogonal".', call. = FALSE)
  } else if (pump.location == "orthogonal") {
    if (vestry) pump.data <- cholera::latlong.ortho.pump.vestry
    else pump.data <- cholera::latlong.ortho.pump
  } else if (pump.location == "address") {
    if (vestry) pump.data <- cholera::pumps.vestry
    else pump.data <- cholera::pumps
  }

  cells <- latlongVoronoi(pump.select = pump.select, vestry = vestry)

  if (is.null(pump.select)) {
    pump.id <- pump.select
  } else {
    pump.id <- selectPump(pump.data, pump.select, "euclidean", vestry)
  }

  if (case.set == "observed") {
    if (statistic == "address") {
      case.data <- cholera::fatalities.address
    } else if (statistic == "orthogonal") {
      case.data <- cholera::latlong.ortho.addr
    }
  } else if (case.set == "expected") {
    if (statistic == "address") {
      case.data <- cholera::latlong.regular.cases
    } else if (statistic == "orthogonal") {
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
              case.location = case.location,
              case.set = case.set,
              snow.colors = snowColors(vestry = vestry),
              statistic.data = statistic.data)

  class(out) <- "latlongEuclidean"
  out
}

#' Plot method for latlongNeighborhoodEuclidean()
#' @param x Object.
#' @param type Character. "star", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param ... Additional plotting parameters.
#' @export

plot.latlongEuclidean <- function(x, type = "star", ...) {
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

    if (type == "star") {
      latlongEuclideanStar(x, vars)
      addRoads(latlong = TRUE, col = "black")
      pumpTokens(x, type, latlong = TRUE)

    } else if (type == "area.points") {
      latlongEuclideanCases(x, vars)
      addRoads(latlong = TRUE, col = "black")
      pumpTokens(x, type, latlong = TRUE)

    } else if (type == "area.polygons") {
      latlongEuclideanAreaPolygons(x)
      addRoads(latlong = TRUE, col = "black")
      pumpTokens(x, type, latlong = TRUE)
    } else {
      stop('type must be "star", "area.points" or "area.polygons".',
        call. = FALSE)
    }
  }

  if (!is.null(pump.id)) {
    if (x$case.location == "address") {
      title(main = paste0("Pump Neighborhoods: Euclidean (address)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    } else if (x$case.location == "orthogonal") {
      title(main = paste0("Pump Neighborhoods: Euclidean (orthogonal)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    }
  } else {
    if (x$case.location == "address") {
      title(main = "Pump Neighborhoods: Euclidean (address)")
    } else if (x$case.location == "orthogonal") {
      title(main = "Pump Neighborhoods: Euclidean (orthogonal)")
    }
  }
}

latlongEuclideanCases <- function(x, vars) {
  if (x$case.set == "observed") {
    if (x$case.location == "address") {
      dat <- cholera::fatalities.address
      names(dat)[names(dat) == "anchor"] <- "case"
    } else if (x$case.location == "orthogonal") {
      dat <- cholera::latlong.ortho.addr
    }
  } else if (x$case.set == "expected") {
    if (x$case.location == "address") {
      dat <- cholera::latlong.regular.cases
      dat$case <- seq_len(nrow(dat))
    } else if (x$case.location == "orthogonal") {
      dat <- cholera::latlong.sim.ortho.proj
    }
  } else stop()

  case.partition <- lapply(x$statistic.data, function(neighbor) {
    dat$case[neighbor == 1]
  })

  invisible(lapply(names(case.partition), function(nm) {
    sel <- dat$case %in% case.partition[[nm]]
    points(dat[sel, vars], col = x$snow.colors[nm], pch = 20, cex = 0.75)
  }))
}

latlongEuclideanStar <- function(x, vars) {
  if (x$case.set == "observed") {
    cases <- cholera::fatalities.address
  } else if (x$case.set == "expected") {
    cases <- cholera::latlong.regular.cases
    cases$anchor <- seq_len(nrow(cases))
  }

  if (is.null(x$pump.id)) pump.id <- x$pump.data$id
  else pump.id <- x$pump.id

  nearest.pump <- do.call(rbind, lapply(cases$anchor, function(a) {
    p1 <- cases[cases$anchor == a, vars]
    d <- vapply(pump.id, function(p) {
      p2 <- x$pump.data[x$pump.data$id == p, vars]
      geosphere::distGeo(p1, p2)
    }, numeric(1L))
    near.id <- which.min(d)
    if (is.null(pump.id)) p.nr <- x$pump.data$id[near.id]
    else p.nr <- pump.id[near.id]
    data.frame(case = a, pump = p.nr, meters = d[near.id])
  }))

  invisible(lapply(nearest.pump$case, function(c) {
    ego <- cases[cases$anchor == c, vars]
    p <- nearest.pump[nearest.pump$case == c, "pump"]
    alter <- x$pump.data[x$pump.data$id == p, vars]
    segments(ego$lon, ego$lat, alter$lon, alter$lat,
             col = x$snow.colors[paste0("p", p)], lwd = 0.5)
  }))
}

latlongEuclideanAreaPolygons <- function(x) {
  if (x$case.set == "expected") {
    if (x$case.location == "address") {
      dat <- cholera::latlong.regular.cases
      dat$case <- seq_len(nrow(dat))
    } else if (x$case.location == "orthogonal") {
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
