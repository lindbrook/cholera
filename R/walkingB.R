#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected" or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param latlong Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

walkingB <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE,
  case.set = "observed", latlong = FALSE, multi.core = TRUE) {

  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('case.set must be "observed", "expected" or "snow".', call. = FALSE)
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

  # pumps to consider or target
  p.sel <- selectPump(pump.data, pump.select = pump.select, vestry = vestry)

  dat <- neighborhoodDataB(case.set = case.set, vestry = vestry,
    latlong = latlong)

  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges

  p.select <- nodes[nodes$pump != 0 & nodes$pump %in% p.sel, ]
  p.select <- p.select[order(p.select$pump), ]

  if (case.set == "observed") {
    case.data <- nodes[nodes$case != 0, ]
    case.data <- case.data[order(case.data$case), ]
    case <- case.data$case

    ds <- igraph::distances(graph = g, v = case.data$node, to = p.select$node,
      weights = edges$d)

    id <- vapply(seq_len(nrow(ds)), function(i) which.min(ds[i, ]), numeric(1L))
    d <- vapply(seq_along(id), function(i) ds[i, ][id[i]], numeric(1L))
    pump <- vapply(seq_along(id), function(i) p.sel[id[i]], integer(1L))
    nr.pump <- data.frame(case = case, pump = pump, distance = d)

    obs.pump <- sort(unique(pump))
    case.pump <- lapply(obs.pump, function(x) case[pump %in% x])
    names(case.pump) <- obs.pump

    ## compute observed path edges ##

    neigh.edges <- parallel::mclapply(names(case.pump), function(p.nm) {
      pump.sel <- p.select$pump == p.nm

      id2 <- lapply(case.pump[[p.nm]], function(cs) {
        case.sel <- nodes$case == cs
        p <- igraph::shortest_paths(graph = g, from = nodes[case.sel, "node"],
          to = p.select[pump.sel, "node"], weights = edges$d)$vpath
        p <- names(unlist(p))

        edge.select <- vapply(seq_along(p[-1]), function(i) {
          ab <- edges$node1 %in% p[i] & edges$node2 %in% p[i + 1]
          ba <- edges$node2 %in% p[i] & edges$node1 %in% p[i + 1]
          which(ab | ba)
        }, numeric(1L))

        edges[edge.select, "id2"]
      })
      unique(unlist(id2))
    }, mc.cores = cores)

    obs.pump.nm <- paste0("p", obs.pump)
    names(neigh.edges) <- obs.pump.nm
    names(case.pump) <- obs.pump.nm

    out <- list(case.pump = case.pump,
                pump.data = pump.data,
                edges = edges,
                neigh.edges = neigh.edges,
                case.set = case.set,
                pump.select = pump.select,
                p.sel = p.sel,
                snow.colors = snowColors(vestry = vestry),
                latlong = latlong,
                cores = cores)

  } else if (case.set == "expected") {
    ## Falconberg Court and Mews isolate
    falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
    sel <- !cholera::road.segments$id %in% falconberg.ct.mews
    rd.segs <- cholera::road.segments[sel, ]
    rd.segs$n1 <- paste0(rd.segs$x1, "_&_", rd.segs$y1)
    rd.segs$n2 <- paste0(rd.segs$x2, "_&_", rd.segs$y2)

    eps <- unique(c(rd.segs$n1, rd.segs$n2))

    ds <- igraph::distances(graph = g, v = eps, to = p.select$node,
      weights = edges$d)

    nr.pmp <- vapply(seq_len(nrow(ds)), function(r) {
      p.select$pump[which.min(ds[r, ])]
    }, numeric(1L))

    d <- vapply(seq_len(nrow(ds)), function(r) min(ds[r, ]), numeric(1L))

    endpt.pump <- data.frame(node = eps, pump.id = nr.pmp, d = d)
    endpt.data <- merge(rd.segs, endpt.pump, by.x = "n1", by.y = "node")
    endpt.data <- merge(endpt.data, endpt.pump, by.x = "n2", by.y = "node")

    sel <- names(endpt.data) %in% c("pump.id.x", "d.x", "pump.id.y", "d.y")
    names(endpt.data)[sel] <- c("pump1", "d1", "pump2", "d2")

    vars <- c("street", "id", "name", "x1", "y1", "x2", "y2", "n1", "n2",
      "pump1", "pump2", "d1", "d2")
    endpt.data <- endpt.data[, vars]

    same_pump.endpts <- endpt.data[endpt.data$pump1 == endpt.data$pump2, ]
    same_pump.road_segs <- split(same_pump.endpts$id, same_pump.endpts$pump1)

    same_pump.cases <- lapply(same_pump.road_segs, function(segs) {
      sel <- cholera::sim.ortho.proj$road.segment %in% segs
      cholera::sim.ortho.proj[sel, "case"]
    })

    diff_pump.endpts <- endpt.data[endpt.data$pump1 != endpt.data$pump2, ]

    midpoint <- lapply(diff_pump.endpts$id, function(seg) {
      sel <- diff_pump.endpts$id == seg
      seg.df <- data.frame(x = unlist(diff_pump.endpts[sel, c("x1", "x2")]),
                           y = unlist(diff_pump.endpts[sel, c("y1", "y2")]),
                           row.names = NULL)

      ols <- stats::lm(y ~ x, data = seg.df)
      segment.slope <- stats::coef(ols)[2]
      theta <- atan(segment.slope)

      delta.x1 <- diff_pump.endpts[sel, "d1"] * cos(theta)
      delta.y1 <- diff_pump.endpts[sel, "d1"] * sin(theta)
      delta.x2 <- diff_pump.endpts[sel, "d2"] * cos(theta)
      delta.y2 <- diff_pump.endpts[sel, "d2"] * sin(theta)

      # pseudo-origin (west or left endpoint)
      ego <- which.min(seg.df$x)

      if (ego == 1) {
        alter <- 2
        xs <- c(seg.df[1, "x"] - delta.x1, seg.df[2, "x"] + delta.x2)
        ys <- c(seg.df[1, "y"] - delta.y1, seg.df[2, "y"] + delta.y2)
      } else if (ego == 2) {
        alter <- 1
        xs <- c(seg.df[2, "x"] + delta.x2, seg.df[1, "x"] - delta.x1)
        ys <- c(seg.df[2, "y"] + delta.y2, seg.df[1, "y"] - delta.y1)
      }

      extended.seg <- data.frame(x = xs, y = ys)

      h <- stats::dist(extended.seg) / 2
      delta.x <- unname(h * cos(theta))
      delta.y <- unname(h * sin(theta))

      data.frame(x = c(extended.seg[ego, "x"] + delta.x),
                 y = c(extended.seg[ego, "y"] + delta.y))
    })

    midpoint <- data.frame(id = diff_pump.endpts$id, do.call(rbind, midpoint),
      row.names = NULL)

    diff_pump.cases <- parallel::mclapply(midpoint$id, function(seg) {
      sel <- cholera::sim.ortho.proj$road.segment == seg
      seg.data <- cholera::sim.ortho.proj[sel, ]

      mid.pt <- midpoint[midpoint$id == seg, c("x", "y")]
      ep.data <- endpt.data[endpt.data$id == seg, ]

      case.data <- stats::setNames(seg.data[, c("x.proj", "y.proj")],
        c("x", "y"))

      tmp <- lapply(seq_along(case.data$x), function(i) case.data[i, ] - mid.pt)
      mid.pt.delta <- sign(do.call(rbind, tmp))

      one <- sign(ep.data[, c("x1", "y1")] - mid.pt)
      two <- sign(ep.data[, c("x2", "y2")] - mid.pt)

      pmp <- vapply(seq_along(mid.pt.delta$x), function(i) {
        if (all(mid.pt.delta[i, ] == one)) {
          ep.data[, paste0("pump", 1)]
        } else if (all(mid.pt.delta[i, ] == two)) {
          ep.data[, paste0("pump", 2)]
        }
      }, numeric(1L))

      data.frame(case = seg.data$case, pump = pmp)
    }, mc.cores = cores)

    diff_pump.cases <- do.call(rbind, diff_pump.cases)
    diff_pump.cases <- split(diff_pump.cases$case, diff_pump.cases$pump)

    exp.pump.case <- lapply(names(same_pump.cases), function(nm) {
      c(same_pump.cases[[nm]], diff_pump.cases[[nm]])
    })

    names(exp.pump.case) <- names(same_pump.cases)

    diff_pump.road_segs <- lapply(midpoint$id, function(seg) {
      vars <- c("x", "y")
      mid.pt <- midpoint[midpoint$id == seg, vars]

      ep.data <- endpt.data[endpt.data$id == seg, ]
      tmp <- rbind(ep.data, ep.data)
      tmp[1, c("x2", "y2")] <- mid.pt
      tmp[2, c("x1", "y1")] <- mid.pt
      tmp[, c("n1", "n2")] <- NULL
      tmp$pump <- c(tmp[1, "pump1"], tmp[2, "pump2"])
      tmp[, c("pump1", "pump2")] <- NULL

      mid.d <- vapply(seq_along(tmp$street), function(i) {
        stats::dist(rbind(stats::setNames(tmp[i, paste0(vars, 1)], vars),
                          stats::setNames(tmp[i, paste0(vars, 2)], vars)))
      }, numeric(1L))

      tmp$d <- unlist(ep.data[, c("d1", "d2")]) + mid.d
      tmp[, c("d1", "d2")] <- NULL
      tmp$id <- paste0(tmp$street, "-", c("A", "Z"))
      tmp
    })

    diff_pump.road_segs <- do.call(rbind, diff_pump.road_segs)
    diff_pump.road_segs <- split(diff_pump.road_segs, diff_pump.road_segs$pump)

    out <- list(exp.pump.case = exp.pump.case,
                same_pump.road_segs = same_pump.road_segs,
                diff_pump.road_segs = diff_pump.road_segs,
                pump.data = pump.data,
                case.set = case.set,
                pump.select = pump.select,
                p.sel = p.sel,
                snow.colors = snowColors(vestry = vestry),
                latlong = latlong,
                cores = cores)
  }

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

plot.walkingB <- function(x, type = "area.points", tsp.method = "repetitive_nn",
  ...) {

  if (x$latlong) {
    vars <- c("lon", "lat")
    seg.vars <- paste0(vars, c(rep(1, 2), rep(2, 2)))
  } else {
    vars <- c("x", "y")
    seg.vars <- paste0(vars, c(rep(1, 2), rep(2, 2)))
  }

  if (x$case.set == "observed") {
    edges <- x$edges
    neigh.edges <- x$neigh.edges
    snowMap(add.cases = FALSE, add.pumps = FALSE, latlong = x$latlong)
    invisible(lapply(names(neigh.edges), function(nm) {
      n.edges <- edges[edges$id2 %in% neigh.edges[[nm]], ]
      segments(n.edges[, seg.vars[1]], n.edges[, seg.vars[2]],
               n.edges[, seg.vars[3]], n.edges[, seg.vars[4]],
               lwd = 2, col = x$snow.colors[nm])
    }))

    invisible(lapply(names(x$case.pump), function(nm) {
      sel <- cholera::fatalities.address$anchor %in% x$case.pump[[nm]]
      points(cholera::fatalities.address[sel, vars], pch = 20, cex = 0.75,
        col = x$snow.colors[nm])
    }))

    if (is.null(x$pump.select)) {
      points(x$pump.data[, vars], pch = 24, lwd = 2, col = x$snow.colors)
      text(x$pump.data[, vars], pos = 1, cex = 0.9,
        labels = paste0("p", x$p.sel))
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

  } else if (x$case.set == "expected") {
    snowMap(add.cases = FALSE, add.pumps = FALSE, add.roads = FALSE,
      latlong = x$latlong)

    if (x$latlong) {
      reg.cases <- cholera::latlong.regular.cases
    } else {
      reg.cases <- cholera::regular.cases
    }

    if (type == "roads") {
      invisible(lapply(names(x$same_pump.road_segs), function(nm) {
        sel <- cholera::road.segments$id %in% x$same_pump.road_seg[[nm]]
        tmp <- cholera::road.segments[sel, ]
        pmp <- paste0("p", nm)
        segments(tmp$x1, tmp$y1, tmp$x2, tmp$y2, col = x$snow.colors[pmp],
          lwd = 2)
      }))

      invisible(lapply(names(x$diff_pump.road_segs), function(nm) {
        tmp <- x$diff_pump.road_segs[[nm]]
        pmp <- paste0("p", nm)
        segments(tmp$x1, tmp$y1, tmp$x2, tmp$y2, col = x$snow.colors[pmp],
          lwd = 2)
      }))

      falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
      sel <- cholera::road.segments$id %in% falconberg.ct.mews
      FCM <- cholera::road.segments[sel, ]
      segments(FCM$x1, FCM$y1, FCM$x2, FCM$y2, lty = "dotted")

    } else if (type == "area.points") {
      invisible(lapply(names(x$exp.pump.case), function(nm) {
        points(reg.cases[x$exp.pump.case[[nm]] - 2000L, ], pch = 15, cex = 1.25,
          col = x$snow.colors[paste0("p", nm)])
      }))

      addRoads(col = "black", latlong = x$latlong)

    } else if (type == "area.polygons") {
      neighborhood.cases <- lapply(x$exp.pump.case, function(x) x - 2000L)
      periphery.cases <- parallel::mclapply(neighborhood.cases,
        peripheryCases, mc.cores = x$cores)
      pearl.string <- parallel::mclapply(periphery.cases, travelingSalesman,
        tsp.method = tsp.method, mc.cores = x$cores)

      addRoads(col = "black", latlong = x$latlong)

      invisible(lapply(names(pearl.string), function(nm) {
        polygon(cholera::regular.cases[pearl.string[[nm]], ],
          col = grDevices::adjustcolor(x$snow.colors[paste0("p", nm)],
          alpha.f = 2/3))
      }))
    }

    pumpTokensB(x, type)
  }

  if (is.null(x$pump.select)) {
    title(main = "Pump Neighborhoods: Walking")
  } else {
    if (length(x$pump.select) > 1) {
      pmp <- "Pumps "
    } else if (length(x$pump.select) == 1) {
      pmp <- "Pump "
    }

    title(main = paste0("Pump Neighborhoods: Walking", "\n", pmp,
      paste(sort(x$pump.select), collapse = ", ")))
  }
}
