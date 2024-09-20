#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
# #' @param case.location Character. "address" or "orthogonal". "address" uses the longitude and latitude of \code{fatalities.address}. "orthogonal" uses the longitude and latitude of \code{latlong.ortho.address}.
#' @param case.set Character. "observed" or "expected".
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @noRd

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
              nearest.pump = nearest.pump,
              paths = neighborhood.paths,
              cases = stats::setNames(neighborhood.cases, paste0("p", pumpID)),
              vestry = vestry,
              pump.select = pump.select,
              snow.colors = snow.colors,
              pump.data = pump.data,
              case.set = case.set,
              cores = cores)

  class(out) <- "latlong_walking"
  out
}

#' Plot method for latlongNeighborhoodWalking().
#'
#' @param x An object of class "latlong_walking" created by \code{latlongNeighborhoodWalking()}.
#' @param type Character. "area.points", "area.polygons" or "streets". For latlongNeighborhoodWalking(case.set = "expected").
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @noRd

plot.latlong_walking <- function(x, type = "area.points", ...) {
  vars <- c("lon", "lat")
  p.data <- x$pump.data

  if (!is.null(x$pump.select) | x$case.set == "snow") {
    pump.id <- selectPump(p.data, pump.select = x$pump.select,
      vestry = x$vestry)
  }

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

      addRoads(latlong = TRUE, col = "black")

    } else if (type == "area.polygons") {
      snowMap(latlong = TRUE, add.cases = FALSE, add.pumps = FALSE,
        add.roads = FALSE)

      periphery.cases <- parallel::mclapply(x$cases, peripheryCases,
        latlong = TRUE, mc.cores = x$cores)
      pearl.strings <- parallel::mclapply(periphery.cases, travelingSalesman,
        latlong = TRUE, mc.cores = x$cores)

      addRoads(latlong = TRUE, col = "black")

      invisible(lapply(names(pearl.strings), function(nm) {
        polygon(cholera::latlong.regular.cases[pearl.strings[[nm]], vars],
          col = grDevices::adjustcolor(x$snow.colors[nm], alpha.f = 2/3),
          border = "black")
      }))

    } else if (type == "streets") {
      snowMap(latlong = TRUE, add.cases = FALSE, add.pumps = FALSE,
        add.roads = FALSE)

      road.segments <- roadSegments(latlong = TRUE)
      g <- x$neigh.data$g
      edges <- x$neigh.data$edges

      if (is.null(x$pump.select)) {
        p.nodes <- x$neigh.data$nodes.pump
      } else {
        p.nodes <- x$neigh.data$nodes.pump[x$pump.select, ]
      }

      endpt.pump <- parallel::mclapply(road.segments$id, function(e) {
        e.data <- edges[edges$id == e, ]
        parsed <- strsplit(e.data$id2, "-")
        nominal.id <- vapply(parsed, function(x) as.integer(x[3]), integer(1L))
        e.data <- e.data[order(nominal.id), ]
        ep1 <- e.data[1, "node1"]
        ep2 <- e.data[nrow(e.data), "node2"]

        ds <- lapply(list(ep1, ep2), function(ep) {
          igraph::distances(g, ep, p.nodes$node, weights = edges$d)
        })

        if (is.null(x$pump.select)) {
          ep.pmp <- vapply(ds, which.min, integer(1L))
        } else {
          ep.pmp <- pump.id[vapply(ds, which.min, integer(1L))]
        }

        ep.pmp.d <- vapply(ds, min, numeric(1L))
        data.frame(id = e, ep = 1:2, pump = ep.pmp, dist = ep.pmp.d)
      }, mc.cores = x$cores)

      endpt.test <- vapply(endpt.pump, function(ep) {
        length(unique(ep$pump)) == 1
      }, logical(1L))

      ## whole segments ##

      whole.seg <- road.segments$id[endpt.test]

      pmp <- vapply(endpt.pump[endpt.test], function(x) {
        unique(x$pump)
      }, integer(1L))

      invisible(lapply(seq_along(whole.seg), function(i) {
        e.data <- edges[edges$id %in% whole.seg[i], ]
        segments(e.data$lon1, e.data$lat1, e.data$lon2, e.data$lat2,
          col = x$snow.colors[paste0("p", pmp[i])], lwd = 3)
      }))

      ## split/partial segments ##

      origin <- data.frame(lon = min(cholera::roads$lon),
                           lat = min(cholera::roads$lat))

      split.seg <- road.segments$id[!endpt.test]

      split.data <- endpt.pump[!endpt.test]
      names(split.data) <- split.seg

      split.seg.dist <- vapply(split.seg, function(s) {
        sum(edges[edges$id == s, "d"])
      }, numeric(1L))

      mid.point <- parallel::mclapply(seq_along(split.data), function(i) {
        rd <- road.segments[road.segments$id == names(split.data)[i], ]
        ep.data <- split.data[[i]]
        seg.dist <- split.seg.dist[i]
        mid.pt <- sum(ep.data$d, seg.dist) / 2

        # 1 = end point 1 is left/west; 2 = end point 2 is left/west
        left.endpt <- which.min(c(rd$lon1, rd$lon2))

        rd.data <- data.frame(lon = c(rd$lon1, rd$lon2),
                              lat = c(rd$lat1, rd$lat2))

        rd.data <- do.call(rbind, lapply(seq_along(rd.data$lon), function(i) {
          tmp <- rd.data[i, c("lon", "lat")]
          x.proj <- c(tmp$lon, origin$lat)
          y.proj <- c(origin$lon, tmp$lat)
          m.lon <- geosphere::distGeo(y.proj, tmp)
          m.lat <- geosphere::distGeo(x.proj, tmp)
          data.frame(x = m.lon, y = m.lat)
        }))

        ols <- stats::lm(y ~ x, rd.data)
        slope <- stats::coef(ols)[2]
        theta <- atan(slope)

        mid.pt.delta <- coordinateDelta(mid.pt, theta)

        if (left.endpt == 1) {
          delta.left <- coordinateDelta(ep.data[1, "dist"], theta)
          delta.right <- coordinateDelta(ep.data[2, "dist"], theta)
          pump.left <- rd.data[1, ] - delta.left
          pump.right <- rd.data[2, ] + delta.right
        } else if (left.endpt == 2) {
          delta.left <- coordinateDelta(ep.data[2, "dist"], theta)
          delta.right <- coordinateDelta(ep.data[1, "dist"], theta)
          pump.left <- data.frame(x = rd.data[2, "x"] - delta.left$x,
                                  y = rd.data[2, "y"] - delta.left$y)
          pump.right <- data.frame(x = rd.data[1, "x"] + delta.left$x,
                                   y = rd.data[1, "y"] + delta.left$y)
        }

        soln <- meterLatLong(pump.left + mid.pt.delta)

        data.frame(seg = names(split.data)[i], soln,
                   pump.left = ep.data$pump[left.endpt],
                   pump.right = ep.data$pump[-left.endpt],
                   left.endpt = left.endpt)
      }, mc.cores = x$cores)

      invisible(lapply(mid.point, function(dat) {
        rd.seg <- road.segments[road.segments$id == dat$seg, ]
        if (dat$left.endpt == 1) {
          segments(rd.seg$lon1, rd.seg$lat1, dat$lon, dat$lat,
            col = x$snow.colors[paste0("p", dat$pump.left)], lwd = 3)
          segments(rd.seg$lon2, rd.seg$lat2, dat$lon, dat$lat,
            col = x$snow.colors[paste0("p", dat$pump.left)], lwd = 3)
        } else if (dat$left.endpt == 2) {
          segments(rd.seg$lon2, rd.seg$lat2, dat$lon, dat$lat,
            col = x$snow.colors[paste0("p", dat$pump.left)], lwd = 3)
          segments(rd.seg$lon1, rd.seg$lat1, dat$lon, dat$lat,
            col = x$snow.colors[paste0("p", dat$pump.left)], lwd = 3)
        }
      }))
    }
    title(main = "Expected Pump Neighborhoods: Walking")
  }

  if (is.null(x$pump.select) & x$case.set != "snow") {
    if (x$case.set == "expected" & type == "area.points") {
      points(p.data[, vars], col = "white", pch = 24)
      text(p.data[, vars], labels = paste0("p", p.data$id), cex = 0.9, pos = 1,
        col = "white")
    } else if (x$case.set == "expected" & type == "area.polygons") {
      points(p.data[, vars], pch = 24)
      text(p.data[, vars], labels = paste0("p", p.data$id), cex = 0.9, pos = 1)
    } else {
      points(p.data[, vars], col = x$snow.colors, pch = 24)
      text(p.data[, vars], labels = paste0("p", p.data$id), cex = 0.9, pos = 1)
    }
  } else {
    if (x$case.set == "snow") {
      sel <- p.data$id %in% unique(x$nearest.pump$pump)
      unsel <- setdiff(p.data$id,  unique(x$nearest.pump$pump))
    } else {
      sel <- p.data$id %in% pump.id
      unsel <- setdiff(p.data$id, pump.id)
    }

    if (x$case.set == "expected" & type == "area.points") {
      points(p.data[sel, vars], col = "white", pch = 24)
      text(p.data[sel, vars], labels = paste0("p", p.data$id[sel]), cex = 0.9,
        pos = 1, col = "white")
    } else if (x$case.set == "expected" & type == "area.polygons") {
      points(p.data[sel, vars], pch = 24, lwd = 1.5)
      text(p.data[sel, vars], labels = paste0("p", p.data$id[sel]), cex = 0.9,
        pos = 1)
    } else {
      points(p.data[sel, vars], col = x$snow.colors[sel], pch = 24)
      text(p.data[sel, vars], labels = paste0("p", p.data$id[sel]), cex = 0.9,
        pos = 1)
    }

    points(p.data[unsel, vars], col = "gray", pch = 24)
    text(p.data[unsel, vars], labels = paste0("p", p.data$id[unsel]),
      cex = 0.9, pos = 1, col = "gray")
  }
}

coordinateDelta <- function(h, theta) {
  data.frame(x = h * cos(theta), y = h * sin(theta), row.names = NULL)
}
