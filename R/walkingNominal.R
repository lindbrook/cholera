#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected" or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @noRd

walkingNominal <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE,
  case.set = "observed", multi.core = TRUE, dev.mode = FALSE) {

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  pump.id <- selectPump(pump.data, pump.select = pump.select, vestry = vestry)

  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('case.set must be "observed", "expected" or "snow".', call. = FALSE)
  }

  snow.colors <- snowColors(vestry = vestry)
  cores <- multiCore(multi.core)

  nearest.data <- nearestPump(pump.select = pump.select,
                              vestry = vestry,
                              weighted = weighted,
                              case.set = case.set,
                              multi.core = cores,
                              dev.mode = dev.mode)

  nearest.dist <- nearest.data$distance
  nearest.path <- nearest.data$path

  if (case.set == "snow") {
    sel <- cholera::snow.neighborhood %in% cholera::fatalities.address$anchor
    snow.anchors <- cholera::snow.neighborhood[sel]
    nearest.pump <- data.frame(case = snow.anchors, pump = nearest.dist$pump)
  } else if (case.set == "observed") {
    nearest.pump <- data.frame(case = cholera::fatalities.address$anchor,
                               pump = nearest.dist$pump)
  } else if (case.set == "expected") {
    nearest.pump <- data.frame(case = nearest.dist$case,
                               pump = nearest.dist$pump)
  }

  pump.id <- sort(unique(nearest.dist$pump))

  neighborhood.cases <- lapply(pump.id, function(p) {
    nearest.pump[nearest.pump$pump == p, "case"]
  })

  names(neighborhood.cases) <- pump.id

  neighborhood.paths <- lapply(pump.id, function(p) {
    n.case <- neighborhood.cases[[paste(p)]]
    nearest.path[which(nearest.pump$case %in% n.case)]
  })

  names(neighborhood.paths) <- pump.id

  out <- list(paths = neighborhood.paths,
              cases = stats::setNames(neighborhood.cases, paste0("p", pump.id)),
              vestry = vestry,
              weighted = weighted,
              case.set = case.set,
              pump.select = pump.select,
              snow.colors = snow.colors,
              pump.id = pump.id,
              cores = cores,
              metric = 1 / unitMeter(1),
              dev.mode = dev.mode)

  class(out) <- "walking"
  out
}

#' Plot method for walkingNominal().
#'
#' @param x An object of class "walking" created by \code{walkingNominal()}.
#' @param type Character. "roads", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param tsp.method Character. Traveling salesperson problem algorithm.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @note When plotting area graphs with simulated data (i.e., \code{case.set = "expected"}), there may be discrepancies between observed cases and expected neighborhoods, particularly between neighborhoods. type = "roads" inspired by Shiode et. al. (2015).
#' @export

plot.walking <- function(x, type = "roads", tsp.method = "repetitive_nn",
  ...) {

  if (type %in% c("roads", "area.points", "area.polygons") == FALSE) {
    stop('type must be "roads", "area.points", "area.polygons".')
  }

  if (type %in% c("area.points", "area.polygons")) {
    if (x$case.set != "expected") {
      stop('area plots valid only when case.set = "expected".')
    }
  }

  n.data <- neighborhoodPathData(x)
  dat <- n.data$dat
  edges <- n.data$edges
  neighborhood.path.edges <- n.data$neighborhood.path.edges
  p.node <- n.data$p.node
  p.name <- n.data$p.name

  if (x$case.set == "observed") {
    snowMap(add.cases = FALSE, add.pumps = FALSE)
    edge.data <- lapply(neighborhood.path.edges, function(x) unique(unlist(x)))

    invisible(lapply(names(edge.data), function(nm) {
      n.edges <- edges[edge.data[[nm]], ]
      segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 2,
        col = x$snow.colors[paste0("p", nm)])
    }))

    invisible(lapply(names(x$cases), function(nm) {
      sel <- cholera::fatalities.address$anchor %in% x$cases[[nm]]
      points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
        cex = 0.75, col = x$snow.colors[nm])
    }))

  } else if (x$case.set == "snow") {
    obs.whole.edges <- lapply(neighborhood.path.edges, function(x) {
      edges[unique(unlist(x)), "id2"]
    })

    snowMap(add.cases = FALSE, add.pumps = FALSE)

    invisible(lapply(names(obs.whole.edges), function(nm) {
      n.edges <- edges[edges$id2 %in% obs.whole.edges[[nm]], ]
      segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 4,
        col = x$snow.colors[paste0("p", nm)])
    }))

  } else if (x$case.set == "expected") {
    snowMap(add.cases = FALSE, add.pumps = FALSE, add.roads = FALSE)
    OE <- observedExpected(x, n.data)
    wholes <- OE$expected.wholes
    splits <- OE$exp.splits
    splits.pump <- OE$exp.splits.pump
    splits.segs <- OE$exp.splits.segs

    sim.proj <- cholera::sim.ortho.proj
    sim.proj.segs <- unique(sim.proj$road.segment)

    if (OE$obs.split.test > 0 | OE$unobs.split.test > 0) {
      split.outcome <- splitOutcomes(x, splits.segs, sim.proj, splits,
        splits.pump)
      split.outcome <- do.call(rbind, split.outcome)
      split.outcome <- split.outcome[!is.na(split.outcome$pump), ]
      split.cases <- lapply(sort(unique(split.outcome$pump)), function(p) {
        split.outcome[split.outcome$pump == p, "case"]
      })

      names(split.cases) <- sort(unique(split.outcome$pump))
    }

    if (type == "area.points") {
      ap <- areaPointsData(sim.proj.segs, wholes, x$snow.colors, sim.proj,
        split.cases)
      points(cholera::regular.cases[ap$sim.proj.wholes$case, ],
        col = ap$sim.proj.wholes$color, pch = 15, cex = 1.25)
      points(cholera::regular.cases[ap$sim.proj.splits$case, ],
        col = ap$sim.proj.splits$color, pch = 15, cex = 1.25)
      addRoads(col = "black")
    } else if (type == "area.polygons") {
      whole.cases <- lapply(names(wholes), function(nm) {
        sel <- sim.proj$road.segment %in% wholes[[nm]]
        cases <- sim.proj[sel, "case"]
        as.numeric(row.names(cholera::regular.cases[cases, ]))
      })

      names(whole.cases) <- names(wholes)

      pearl.neighborhood <- vapply(whole.cases, length, integer(1L))
      pearl.neighborhood <- names(pearl.neighborhood[pearl.neighborhood != 0])

      if (OE$obs.split.test > 0 | OE$unobs.split.test > 0) {
        neighborhood.cases <- lapply(pearl.neighborhood, function(nm) {
          c(whole.cases[[nm]], split.cases[[nm]])
        })
      } else {
        neighborhood.cases <- lapply(pearl.neighborhood, function(nm) {
          whole.cases[[nm]]
        })
      }

      names(neighborhood.cases) <- pearl.neighborhood

      # Exception fix
      # plot(neighborhoodWalking(-(7:8), case.set = "expected"),
      #   type = "area.polygons")
      neg78 <- identical(as.integer(x$pump.id), c(1:6, 9:13)) |
               identical(as.integer(x$pump.id), c(1:6, 9:14))

      if (neg78) {
        # Air Street: 2344, 2346
        # Chapel Place: 7302
        # Queen Street (III): 3390
        sel <- !neighborhood.cases$`9` %in% c(2344, 2346, 3390, 7302)
        neighborhood.cases$`9` <- neighborhood.cases$`9`[sel]
      }

      # Exception fix
      # plot(neighborhoodWalking( case.set = "expected"), "area.polygons")
      all.pumps <- identical(as.integer(x$pump.id), c(1:13)) |
                   identical(as.integer(x$pump.id), c(1:14))

     if (all.pumps) {
        # Air Street: 2344, 2346
        # Queen Street (III): 3390
        sel <- !neighborhood.cases$`8` %in% c(2344, 2346)
        neighborhood.cases$`8` <- neighborhood.cases$`8`[sel]
        sel <- !neighborhood.cases$`9` %in% 3390
        neighborhood.cases$`9` <- neighborhood.cases$`9`[sel]
      }

      if ((.Platform$OS.type == "windows" & x$cores > 1) | x$dev.mode) {
        cl <- parallel::makeCluster(x$cores)
        parallel::clusterExport(cl = cl, envir = environment(),
          varlist = c("peripheryCases", "pearlStringRadius",
          "travelingSalesman"))
        periphery.cases <- parallel::parLapply(cl, neighborhood.cases,
          peripheryCases)
        pearl.string <- parallel::parLapply(cl, periphery.cases,
          travelingSalesman, tsp.method = tsp.method)
        parallel::stopCluster(cl)
      } else {
        periphery.cases <- parallel::mclapply(neighborhood.cases,
          peripheryCases, mc.cores = x$core)
        pearl.string <- parallel::mclapply(periphery.cases, travelingSalesman,
          tsp.method = tsp.method, mc.cores = x$cores)
      }

      addRoads(col = "black")

      invisible(lapply(names(pearl.string), function(nm) {
        sel <- paste0("p", nm)
        polygon(cholera::regular.cases[pearl.string[[nm]], ],
          col = grDevices::adjustcolor(x$snow.colors[sel], alpha.f = 2/3))
      }))

    } else if (type == "roads") {
      addRoads()
      invisible(lapply(names(wholes), function(nm) {
        n.edges <- edges[edges$id %in% wholes[[nm]], ]
        segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 3,
          col = x$snow.colors[paste0("p", nm)])
      }))

      if (OE$obs.split.test > 0 | OE$unobs.split.test > 0) {
        invisible(lapply(seq_along(splits), function(i) {
          dat <- splits[[i]]
          ps <- splits.pump[[i]]
          ps.col <- x$snow.colors[paste0("p", ps)]
          segments(dat[1, "x"], dat[1, "y"], dat[2, "x"], dat[2, "y"], lwd = 3,
            col = ps.col[1])
          segments(dat[3, "x"], dat[3, "y"], dat[4, "x"], dat[4, "y"], lwd = 3,
            col = ps.col[2])
        }))
      }
    }
  }

  pumpTokens(x, type)

  if (is.null(x$pump.select)) {
    title(main = "Pump Neighborhoods: Walking")
  } else {
    title(main = paste0("Pump Neighborhoods: Walking", "\n", "Pumps ",
      paste(sort(x$pump.select), collapse = ", ")))
  }
}

#' Print method for walkingNominal().
#'
#' Parameter values for neighborhoodWalking().
#' @param x An object of class "walking" created by \code{neighborhoodWalking()}.
#' @param ... Additional parameters.
#' @return A list of argument values.
#' @export

print.walking <- function(x, ...) {
  print(x[c("pump.id", "case.set", "vestry")])
}

#' Summary method for walkingNominal().
#'
#' Return computed counts for walking neighborhoods.
#' @param object Object. An object of class "walking" created by \code{walkingNominal()}.
#' @param ... Additional parameters.
#' @return An R vector.
#' @export

summary.walking <- function(object, ...) {
  if (object$case.set == "observed" | object$case.set == "snow") {
    out <- vapply(object$paths, length, numeric(1L))
    out <- stats::setNames(out, paste0("p", object$pump.id))
  } else if (object$case.set == "expected") {
    out <- expectedCount(object)
  }
  out
}
