#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed" or "expected".
#' @param latlong Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

neighborhoodWalking <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, case.set = "observed", latlong = FALSE, multi.core = FALSE) {

  if (!is.null(pump.select)) {
    if (length(pump.select) == 1) {
      if (pump.select == 2) {
        msg1 <- "You can't just select the pump on Adam and Eve Court (#2).\n"
        msg2 <- " It's an isolate, unreachable for observed and most expected fatalities."
        stop(paste(msg1, msg2))
      }
    }
  }

  if (!case.set %in% c("observed", "expected")) {
    stop('case.set must be "observed" or "expected.', call. = FALSE)
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

  dat <- neighborhoodData(case.set = case.set, vestry = vestry,
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
                vestry = vestry,
                snow.colors = snowColors(vestry = vestry),
                latlong = latlong,
                cores = cores)

  } else if (case.set == "expected") {
    ## Isolates: Adam and Eve Court; Falconberg Court and Mews
    adam.eve.ct <- "44-1"
    falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
    sel <- !cholera::road.segments$id %in% c(falconberg.ct.mews, adam.eve.ct)

    ## Isolate: Adam and Eve Court pump (#2)
    p.select.adam.eve <- p.select[p.select$pump == 2, ]
    p.select <- p.select[p.select$pump != 2, ]

    if (latlong) {
      rd.segs <- roadSegments(latlong = TRUE)

      adam.eve.seg <- rd.segs[rd.segs$id == adam.eve.ct, ]
      adam.eve.seg$n1 <- paste0(adam.eve.seg$lon1, "_&_", adam.eve.seg$lat1)
      adam.eve.seg$n2 <- paste0(adam.eve.seg$lon2, "_&_", adam.eve.seg$lat2)

      rd.segs <- rd.segs[sel, ]
      rd.segs$n1 <- paste0(rd.segs$lon1, "_&_", rd.segs$lat1)
      rd.segs$n2 <- paste0(rd.segs$lon2, "_&_", rd.segs$lat2)
    } else {
      rd.segs <- cholera::road.segments

      adam.eve.seg <- rd.segs[rd.segs$id == adam.eve.ct, ]
      adam.eve.seg$n1 <- paste0(adam.eve.seg$x1, "_&_", adam.eve.seg$y1)
      adam.eve.seg$n2 <- paste0(adam.eve.seg$x2, "_&_", adam.eve.seg$y2)

      rd.segs <- rd.segs[sel, ]
      rd.segs$n1 <- paste0(rd.segs$x1, "_&_", rd.segs$y1)
      rd.segs$n2 <- paste0(rd.segs$x2, "_&_", rd.segs$y2)
    }

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

    if (latlong) {
      vars <- c("street", "id", "name", "lon1", "lat1", "lon2", "lat2", "n1",
        "n2", "pump1", "pump2", "d1", "d2")
    } else {
      vars <- c("street", "id", "name", "x1", "y1", "x2", "y2", "n1", "n2",
        "pump1", "pump2", "d1", "d2")
    }

    endpt.data <- endpt.data[, vars]

    if (2L %in% p.sel) {
      ds.AE <- igraph::distances(
        graph = g,
        v = unlist(adam.eve.seg[, c("n1", "n2")]),
        to = p.select.adam.eve$node,
        weights = edges$d)

      ds.AE <- as.data.frame(ds.AE)
      names(ds.AE) <- "d"
      ds.AE$node <- row.names(ds.AE)

      adam.eve.seg <- merge(adam.eve.seg, ds.AE, by.x = "n1", by.y = "node")
      adam.eve.seg <- merge(adam.eve.seg, ds.AE, by.x = "n2", by.y = "node")
      names(adam.eve.seg)[grep("d.", names(adam.eve.seg))] <- paste0("d", 1:2)
      adam.eve.seg[, c("pump1", "pump2")] <- 2L
      adam.eve.seg <- adam.eve.seg[, vars]

      endpt.data <- rbind(endpt.data, adam.eve.seg)
    }

    same_pump.endpts <- endpt.data[endpt.data$pump1 == endpt.data$pump2, ]

    # road segments with endpoints with shortest path to _same_ pump
    same_pump.road_segs <- split(same_pump.endpts$id, same_pump.endpts$pump1)

    if (latlong) {
      same_pump.cases <- lapply(same_pump.road_segs, function(segs) {
        sel <- cholera::latlong.sim.ortho.proj$road.segment %in% segs
        cholera::latlong.sim.ortho.proj[sel, "case"]
      })
    } else {
      same_pump.cases <- lapply(same_pump.road_segs, function(segs) {
        sel <- cholera::sim.ortho.proj$road.segment %in% segs
        cholera::sim.ortho.proj[sel, "case"]
      })
    }

    if (any(endpt.data$pump1 != endpt.data$pump2)) {
      # road segments with endpoints with shortest path to _different_ pump
      diff_pump.endpts <- endpt.data[endpt.data$pump1 != endpt.data$pump2, ]

      # find midpoint/cutpoint along segment that leads to different pumps
      if (latlong) {
        mid.point <- midpointLatlong(diff_pump.endpts, endpt.data,
          same_pump.cases, cores)
      } else {
        mid.point <- midpointNative(diff_pump.endpts, endpt.data,
          same_pump.cases, cores)
      }

      out <- list(exp.pump.case = mid.point$exp.pump.case,
                  same_pump.road_segs = same_pump.road_segs,
                  diff_pump.road_segs = mid.point$diff_pump.road_segs,
                  pump.data = pump.data,
                  case.set = case.set,
                  pump.select = pump.select,
                  p.sel = p.sel,
                  vestry = vestry,
                  snow.colors = snowColors(vestry = vestry),
                  latlong = latlong,
                  cores = cores)
    } else {
      out <- list(exp.pump.case = same_pump.cases,
                  same_pump.road_segs = same_pump.road_segs,
                  pump.data = pump.data,
                  case.set = case.set,
                  pump.select = pump.select,
                  p.sel = p.sel,
                  vestry = vestry,
                  snow.colors = snowColors(vestry = vestry),
                  latlong = latlong,
                  cores = cores)
    }
  }

  class(out) <- "walking"
  out
}

#' Plot method for neighborhoodWalking().
#'
#' @param x An object of class "walking" created by \code{neighborhoodWalking()}.
#' @param type Character. Type of expected plot: "roads", "area.points" or "area.polygons". Valid only when \code{case.set = "expected"}.
#' @param add Logical. Add graphic to plot.
#' @param tsp.method Character. Traveling salesperson problem algorithm.
#' @param path.width Numeric. Set width of paths.
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param polygon.type Character. "border" or "solid".
#' @param polygon.col Character.
#' @param polygon.lwd Numeric.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @note When plotting area graphs with simulated data (i.e., \code{case.set = "expected"}), there may be discrepancies between observed cases and expected neighborhoods, particularly between neighborhoods. type = "roads" inspired by Shiode et. al. (2015).
#' @export

plot.walking <- function(x, type = "area.points", add = FALSE,
  tsp.method = "repetitive_nn", path.width = 2, alpha.level = 0.75,
  polygon.type = "solid", polygon.col = NULL, polygon.lwd = 2, ...) {

  if (x$case.set == "expected") {
    if (!type %in% c("area.points", "area.polygons", "roads")) {
      stop('type must be "area.points", "area.polygons" or "roads".',
        call. = FALSE)
    }
  }

  if (!polygon.type %in% c("border", "solid")) {
    stop('polygon.type must be "border" or "solid".', call. = FALSE)
  }

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
    if (!add) snowMap(add.cases = FALSE, add.pumps = FALSE, latlong = x$latlong)
    invisible(lapply(names(neigh.edges), function(nm) {
      n.edges <- edges[edges$id2 %in% neigh.edges[[nm]], ]
      segments(n.edges[, seg.vars[1]], n.edges[, seg.vars[2]],
               n.edges[, seg.vars[3]], n.edges[, seg.vars[4]],
               lwd = path.width, col = x$snow.colors[nm])
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
    if (!add) snowMap(add.cases = FALSE, add.pumps = FALSE, add.roads = FALSE,
      latlong = x$latlong)

    if (x$latlong) {
      rd.segs <- roadSegments(latlong = TRUE)
      reg.cases <- cholera::latlong.regular.cases[, c("lon", "lat")]
    } else {
      rd.segs <- cholera::road.segments
      reg.cases <- cholera::regular.cases
    }

    case.fix <- ifelse(x$latlong, 0, 2000L)

    if (type == "roads") {
      invisible(lapply(names(x$same_pump.road_segs), function(nm) {
        tmp <- rd.segs[rd.segs$id %in% x$same_pump.road_seg[[nm]], ]
        pmp <- paste0("p", nm)
        if (x$latlong) {
          segments(tmp$lon1, tmp$lat1, tmp$lon2, tmp$lat2,
            col = x$snow.colors[pmp], lwd = path.width)
        } else {
          segments(tmp$x1, tmp$y1, tmp$x2, tmp$y2, col = x$snow.colors[pmp],
            lwd = path.width)
        }
      }))

      invisible(lapply(names(x$diff_pump.road_segs), function(nm) {
        tmp <- x$diff_pump.road_segs[[nm]]
        pmp <- paste0("p", nm)
        if (x$latlong) {
          segments(tmp$lon1, tmp$lat1, tmp$lon2, tmp$lat2,
            col = x$snow.colors[pmp], lwd = path.width)
        } else {
          segments(tmp$x1, tmp$y1, tmp$x2, tmp$y2, col = x$snow.colors[pmp],
            lwd = path.width)
        }
      }))

      falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
      FCM <- rd.segs[rd.segs$id %in% falconberg.ct.mews, ]
      if (x$latlong) {
        segments(FCM$lon1, FCM$lat1, FCM$lon2, FCM$lat2, lty = "dotted")
      } else {
        segments(FCM$x1, FCM$y1, FCM$x2, FCM$y2, lty = "dotted")
      }

    } else if (type == "area.points") {
      invisible(lapply(names(x$exp.pump.case), function(nm) {
        points(reg.cases[x$exp.pump.case[[nm]] - case.fix, ], pch = 15,
          cex = 1.25, col = x$snow.colors[paste0("p", nm)])
      }))

      if (!add) addRoads(col = "black", latlong = x$latlong)

    } else if (type == "area.polygons") {
      neighborhood.cases <- lapply(x$exp.pump.case, function(x) x - case.fix)
      periphery.cases <- parallel::mclapply(neighborhood.cases,
        peripheryCases, latlong = x$latlong, mc.cores = x$cores)

      # p8 fix with pump.select = 6:9
      if (identical(x$p.sel, 6:9)) {
        # "Picadilly", "Bruton Street" (x2), "New Burlington Street" (x2)
        sel <- periphery.cases$`8` %in% c("206", "4022", "3875", "7131", "7277")
        periphery.cases$`8` <- periphery.cases$`8`[!sel]
      }

      pearl.string <- parallel::mclapply(periphery.cases, travelingSalesman,
        latlong = x$latlong, tsp.method = tsp.method, mc.cores = x$cores)

      if (!add) addRoads(col = "black", latlong = x$latlong)

      if (polygon.type == "border") {
        invisible(lapply(names(pearl.string), function(nm) {
          polygon(reg.cases[pearl.string[[nm]], ],
            border = x$snow.colors[paste0("p", nm)], lwd = polygon.lwd)
        }))
      } else if (polygon.type == "solid") {
        invisible(lapply(names(pearl.string), function(nm) {
          polygon(reg.cases[pearl.string[[nm]], ],
            col = grDevices::adjustcolor(x$snow.colors[paste0("p", nm)],
            alpha.f = alpha.level))
        }))
      }
    }

    if (!add) pumpTokens(x, type, alpha.level, polygon.type)
  }

  if (!add) {
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
}

#' Print method for neighborhoodWalking().
#'
#' Parameter values for neighborhoodWalking().
#' @param x An object of class "walking" created by \code{neighborhoodWalking()}.
#' @param ... Additional parameters.
#' @return A list of argument values.
#' @export
#' @examples
#' \dontrun{
#' neighborhoodWalking()
#' print(neighborhoodWalking())
#' }

print.walking <- function(x, ...) {
  print(x[c("p.sel", "case.set", "vestry", "latlong")])
}
