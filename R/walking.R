#' Compute walking path pump neighborhoods.
#'
#' Group cases into neighborhoods based on walking distance.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected" or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @return An R list with 7 objects:
#' \itemize{
#'   \item{\code{paths}: list of paths to nearest or selected pump(s).}
#'   \item{\code{cases}: list of cases by pump.}
#'   \item{\code{vestry}: "vestry" from neighborhoodWalking().}
#'   \item{\code{observed}: "observed" from neighborhoodWalking().}
#'   \item{\code{pump.select}: "pump.select" from neighborhoodWalking().}
#'   \item{\code{cores}: number of cores to use for parallel implementation.}
#'   \item{\code{metric}: incremental metric used to find cut point on split road segments.}
#' }
#' @export
#' @examples
#' \dontrun{
#' neighborhoodWalking()
#' neighborhoodWalking(pump.select = -6)
#' }

neighborhoodWalking <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, case.set = "observed", multi.core = TRUE,
  dev.mode = FALSE) {

  if (is.null(pump.select) == FALSE) {
    if (is.numeric(pump.select) == FALSE) {
      stop("pump.select must be numeric.", call. = FALSE)
    }
    if (length(pump.select) == 1) {
      if (pump.select == 2) {
        msg1 <- "You can't just select the pump on Adam and Eve Court (#2).\n"
        msg2 <- " It's an isolate, unreachable for observed fatalities."
        stop(msg1, msg2, call. = FALSE)
      }
    }

    if (vestry) {
      p.count <- nrow(cholera::pumps.vestry)
    } else {
      p.count <- nrow(cholera::pumps)
    }

    p.ID <- seq_len(p.count)

    if (any(abs(pump.select) %in% p.ID == FALSE)) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ', p.count,
        call. = FALSE)
    }
  }

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
    snow.anchors <- cholera::snow.neighborhood[cholera::snow.neighborhood %in%
      cholera::fatalities.address$anchor]
    nearest.pump <- data.frame(case = snow.anchors,
                               pump = nearest.dist$pump)
  } else if (case.set == "observed") {
    nearest.pump <- data.frame(case = cholera::fatalities.address$anchor,
                               pump = nearest.dist$pump)
  } else if (case.set == "expected") {
    nearest.pump <- data.frame(case = nearest.dist$case,
                               pump = nearest.dist$pump)
  }

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

  out <- list(paths = neighborhood.paths,
              cases = stats::setNames(neighborhood.cases, paste0("p", pumpID)),
              vestry = vestry,
              weighted = weighted,
              case.set = case.set,
              pump.select = pump.select,
              snow.colors = snow.colors,
              pumpID = pumpID,
              cores = cores,
              metric = 1 / unitMeter(1),
              dev.mode = dev.mode)

  class(out) <- "walking"
  out
}

#' Plot method for neighborhoodWalking().
#'
#' @param x An object of class "walking" created by \code{neighborhoodWalking()}.
#' @param type Character. "roads", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param msg Logical. Toggle in-progress messages.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @note When plotting area graphs with simulated data (i.e., \code{case.set = "expected"}), there may be discrepancies between observed cases and expected neighborhoods, particularly between neighborhoods.
#' @export
#' @examples
#' \dontrun{
#' plot(neighborhoodWalking())
#' plot(neighborhoodWalking(case.set = "expected"))
#' plot(neighborhoodWalking(case.set = "expected"), type = "area.points")
#' plot(neighborhoodWalking(case.set = "expected"), type = "area.polygons")
#' }

plot.walking <- function(x, type = "roads", msg = FALSE, ...) {
  if (type %in% c("roads", "area.points", "area.polygons") == FALSE) {
    stop('type must be "roads", "area.points", "area.polygons".')
  }

  if (type %in% c("area.points", "area.polygons")) {
    if (x$case.set != "expected") {
      stop('area plots valid only when case.set = "expected".')
    }
  }

  if (msg) {
    if (x$case.set == "expected") message("Working...")
  }

  n.data <- neighborhoodPathData(x)
  dat <- n.data$dat
  edges <- n.data$edges
  neighborhood.path.edges <- n.data$neighborhood.path.edges
  p.node <- n.data$p.node
  p.name <- n.data$p.name

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  road.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  plot(cholera::fatalities[, c("x", "y")], xlim = x.range, ylim = y.range,
    pch = NA, asp = 1)
  invisible(lapply(border.list, lines))

  if (x$case.set == "observed") {
    invisible(lapply(road.list, lines, col = "gray"))
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
    invisible(lapply(road.list, lines, col = "gray"))

    obs.whole.edges <- lapply(neighborhood.path.edges, function(x) {
      edges[unique(unlist(x)), "id2"]
    })

    invisible(lapply(names(obs.whole.edges), function(nm) {
      n.edges <- edges[edges$id2 %in% obs.whole.edges[[nm]], ]
      segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 4,
        col = x$snow.colors[paste0("p", nm)])
    }))

  } else if (x$case.set == "expected") {
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
      invisible(lapply(road.list, lines))

    } else if (type == "area.polygons") {
      invisible(lapply(road.list, lines))

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

      if ((.Platform$OS.type == "windows" & x$cores > 1) | x$dev.mode) {
        cl <- parallel::makeCluster(x$cores)
        parallel::clusterExport(cl = cl, envir = environment(),
          varlist = c("peripheryCases", "pearlStringRadius",
          "travelingSalesman"))
        periphery.cases <- parallel::parLapply(cl, neighborhood.cases,
          peripheryCases)
        pearl.string <- parallel::parLapply(cl, periphery.cases,
          travelingSalesman)
        parallel::stopCluster(cl)
      } else {
        periphery.cases <- parallel::mclapply(neighborhood.cases,
          peripheryCases, mc.cores = x$core)
        pearl.string <- parallel::mclapply(periphery.cases, travelingSalesman,
          mc.cores = x$cores)
      }

      invisible(lapply(names(pearl.string), function(nm) {
        sel <- paste0("p", nm)
        polygon(cholera::regular.cases[pearl.string[[nm]], ],
          col = grDevices::adjustcolor(x$snow.colors[sel], alpha.f = 2/3))
      }))

    } else {
      invisible(lapply(road.list, lines, col = "gray"))

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

  if (msg) {
    if (x$case.set == "expected") message("Done!")
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
  print(x[c("pumpID", "case.set", "vestry")])
}

#' Summary method for neighborhoodWalking().
#'
#' Return computed counts for walking neighborhoods.
#' @param object Object. An object of class "walking" created by \code{neighborhoodWalking()}.
#' @param ... Additional parameters.
#' @return An R vector.
#' @export
#' @examples
#' \dontrun{
#' summary(neighborhoodWalking())
#' }

summary.walking <- function(object, ...) {
  if (object$case.set == "observed" | object$case.set == "snow") {
    out <- vapply(object$paths, length, numeric(1L))
    out <- stats::setNames(out, paste0("p", object$pumpID))
  } else if (object$case.set == "expected") {
    out <- expectedCount(object)
  }
  out
}
