#' Add walking neighbhorood.
#'
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
# #' @param case.location Character. "address" or "orthogonal". "address" uses the longitude and latitude of \code{fatalities.address}. "orthogonal" uses the longitude and latitude of \code{latlong.ortho.address}.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed" or "expected".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param type Character. "area.points", "area.polygons" or "roads". For walkingLatlong(case.set = "expected").
#' @param alpha.f Numeric. alpha transparency.
#' @return A base R plot graphic.
#' @noRd

addNeighborhoodWalkingLatlong <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, case.set = "observed", multi.core = TRUE,
  type = "area.polygons", alpha.f = 1/5, ...) {

  args <- list(pump.select = pump.select, vestry = vestry,
    weighted = weighted, case.set = case.set, multi.core = multi.core,
    latlong = TRUE)

  x <- do.call("neighborhoodWalking", args)

  vars <- c("lon", "lat")
  p.data <- x$pump.data

  if (!is.null(pump.select) | case.set == "snow") {
    pump.id <- selectPump(p.data, pump.select = pump.select, vestry = vestry)
  }

  if (case.set %in% c("observed", "snow")) {
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

  } else if (case.set == "expected") {
    if (type == "area.points") {
      invisible(lapply(names(x$cases), function(nm) {
        points(cholera::latlong.regular.cases[x$cases[[nm]], vars],
          col = grDevices::adjustcolor(x$snow.colors[nm], alpha.f = alpha.f))
      }))
    } else if (type == "area.polygons") {
      periphery.cases <- lapply(x$cases, peripheryCases, latlong = TRUE)
      pearl.strings <- lapply(periphery.cases, travelingSalesman,
        latlong = TRUE)

      invisible(lapply(names(pearl.strings), function(nm) {
        polygon(cholera::latlong.regular.cases[pearl.strings[[nm]], vars],
          col = grDevices::adjustcolor(x$snow.colors[nm], alpha.f = alpha.f),
          border = "black")
      }))
    } else if (type == "roads") {
      road.segments <- roadSegments(latlong = TRUE)
      g <- x$neigh.data$g
      edges <- x$neigh.data$edges

      if (is.null(pump.select)) {
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

        if (is.null(pump.select)) {
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
  }
}
