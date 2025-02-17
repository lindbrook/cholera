#' Compute Voronoi pump neighborhoods (lat-long prototype).
#'
#' Group cases into neighborhoods using Voronoi tessellation.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param location Character. "nominal" or "orthogonal". "nominal" uses the longitude and latitude of \code{fatalities.address}. "orthogonal" uses the longitude and latitude of \code{latlong.ortho.address}.
#' @param polygon.vertices Logical. \code{TRUE} returns a list of lon-lat coordinates of the vertices of Voronoi cells.
#' @importFrom sp point.in.polygon
#' @noRd

voronoiLatlong <- function(pump.select = NULL, vestry = FALSE,
  location = "nominal", polygon.vertices = FALSE) {

  snow.colors <- snowColors(vestry = vestry)

  if (location %in% c("nominal", "orthogonal") == FALSE) {
    stop('location must be "nominal" or "orthogonal".', call. = FALSE)
  }

  if (location %in% c("nominal", "orthogonal") == FALSE) {
    stop('location must be "nominal" or "orthogonal".', call. = FALSE)
  } else if (location == "orthogonal") {
    if (vestry) pump.data <- cholera::latlong.ortho.pump.vestry
    else pump.data <- cholera::latlong.ortho.pump
  } else if (location == "nominal") {
    if (vestry) pump.data <- cholera::pumps.vestry
    else pump.data <- cholera::pumps
  }

  cells.triangles <- latlongVoronoiVertices(pump.select = pump.select, vestry = vestry)

  if (location == "orthogonal") {
    statistic.data <- lapply(cells.triangles$cells, function(c) {
      sp::point.in.polygon(cholera::latlong.ortho.addr$lon,
        cholera::latlong.ortho.addr$lat, c$lon, c$lat)
    })
  } else if (location == "nominal") {
    statistic.data <- lapply(cells.triangles$cells, function(c) {
      sp::point.in.polygon(cholera::fatalities.address$lon,
        cholera::fatalities.address$lat, c$lon, c$lat)
    })
  }

  pump.id <- selectPump(pump.data, pump.select = pump.select, vestry = vestry)

  out <- list(pump.select = pump.select, pump.id = pump.id, vestry = vestry,
    cells.triangles = cells.triangles, pump.data = pump.data,
    statistic.data = statistic.data, location = location,
    snow.colors = snow.colors)

  class(out) <- c("voronoi", "voronoi_latlong")

  if (polygon.vertices) {
    out <- lapply(out$cells.triangles$cells, function(dat) {
      tmp <- dat[, c("lon", "lat")]
      row.names(tmp) <- NULL
      tmp
    })

    if (!is.null(pump.select)) {
      names(out) <- paste0("p", pump.id)
    } else {
      if (vestry) names(out) <- paste0("p", cholera::pumps.vestry$id)
      else names(out) <- paste0("p", cholera::pumps$id)
    }
    out
  } else {
    out
  }
}

#' Plot method for voronoiLatlong()
#' @param x Object.
#' @param add.pumps Logical.
#' @param delaunay.voronoi Character "delaunay", "voronoi", or "both".
#' @param euclidean.paths Logical.
#' @param ... Additional plotting parameters.
#' @export

plot.voronoi_latlong <- function(x, add.pumps = TRUE,
  delaunay.voronoi = "voronoi", euclidean.paths = FALSE, ...) {

  vars <- c("lon", "lat")

  snowMap(vestry = x$vestry, latlong = TRUE, add.cases = FALSE,
    add.pumps = FALSE)

  if (add.pumps) pumpTokens(x, type = NULL)

  if (!is.null(x$pump.id)) {
    if (x$location == "nominal") {
      title(main = paste0("Pump Neighborhoods: Voronoi (nominal)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    } else if (x$location == "orthogonal") {
      title(main = paste0("Pump Neighborhoods: Voronoi (orthogonal)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    }
  } else {
    if (x$location == "nominal") {
      title(main = "Pump Neighborhoods: Voronoi (nominal)")
    } else if (x$location == "orthogonal") {
      title(main = "Pump Neighborhoods: Voronoi (orthogonal)")
    }
  }

  case.partition <- lapply(x$statistic.data, function(dat) {
    cholera::fatalities.address$anchor[dat == 1]
  })

  if (delaunay.voronoi == "voronoi") {
    invisible(lapply(x$cells.triangles$cells, function(dat) {
      polygon(dat[, vars])
    }))
  } else if (delaunay.voronoi == "delaunay") {
    invisible(lapply(x$cells.triangles$triangles, function(dat) {
      polygon(dat[, vars])
    }))
  } else if (delaunay.voronoi == "both") {
    invisible(lapply(x$cells.triangles$cells, function(dat) {
      polygon(dat[, vars])
    }))
    invisible(lapply(x$cells.triangles$triangles, function(dat) {
      polygon(dat[, vars])
    }))
  } else {
    stop('cells.triangles must be "cells", "triangles", or "both".',
      call. = FALSE)
  }

  if (euclidean.paths) {
    plotLatlongEuclideanPaths(x, x$pump.id, vars)
  } else {
    plotLatlongVoronoiCases(x, vars)
  }
}

#' @importFrom geosphere distGeo

plotLatlongEuclideanPaths <- function(x, pump.id, vars) {
  cases <- cholera::fatalities.address

  if (is.null(pump.id)) p.id <- x$pump.data$id
  else p.id <- pump.id

  nearest.pump <- do.call(rbind, lapply(cases$anchor, function(a) {
    p1 <- cases[cases$anchor == a, vars]
    d <- vapply(p.id, function(p) {
      p2 <- x$pump.data[x$pump.data$id == p, vars]
      geosphere::distGeo(p1, p2)
    }, numeric(1L))
    near.id <- which.min(d)
    if (is.null(pump.id)) p.nr <- x$pump.data$id[near.id]
    else p.nr <- p.id[near.id]
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

plotLatlongVoronoiCases <- function(x, vars) {
  if (x$location == "nominal") {
    case.partition <- lapply(x$statistic.data, function(dat) {
      cholera::fatalities.address$anchor[dat == 1]
    })
    invisible(lapply(names(case.partition), function(nm) {
      sel <- cholera::fatalities.address$anchor %in% case.partition[[nm]]
      points(cholera::fatalities.address[sel, vars], col = x$snow.colors[nm],
        pch = 20, cex = 0.75)
    }))
  } else if (x$location == "orthogonal") {
    case.partition <- lapply(x$statistic.data, function(dat) {
      cholera::latlong.ortho.addr$case[dat == 1]
    })
    invisible(lapply(names(case.partition), function(nm) {
      sel <- cholera::latlong.ortho.addr$case %in% case.partition[[nm]]
      points(cholera::latlong.ortho.addr[sel, vars], col = x$snow.colors[nm],
        pch = 20, cex = 0.75)
    }))
  }
}

#' Print method for voronoiLatlong().
#'
#' Parameter values for voronoiLatlong().
#' @param x An object of class "voronoiLatlong" created by \code{voronoiLatlong()}.
#' @param ... Additional arguments.
#' @return A list of argument values.
#' @export

print.voronoi_latlong <- function(x, ...) {
  print(x[c("pump.select", "vestry")])
}
