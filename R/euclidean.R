#' Compute Euclidean path pump neighborhoods.
#'
#' Plots star graph from pump to its cases.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param case.location Character. "address" or "nominal". For \code{observed = TRUE}: "address" uses \code{ortho.proj} and "nominal" uses \code{fatalities}. For \code{observed = TRUE}: "address" uses \code{sim.ortho.proj} and "nominal" uses \code{regular.cases}.
#' @param case.set Character. "observed" or "expected".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @return An R vector.
#' @note This function is computationally intensive when \code{case.set = "expected"}.
#' @export
#' @examples
#' \dontrun{
#'
#' neighborhoodEuclidean()
#' neighborhoodEuclidean(-6)
#' neighborhoodEuclidean(pump.select = 6:7)
#' }

neighborhoodEuclidean <- function(pump.select = NULL, vestry = FALSE,
   case.location = "nominal", case.set = "observed", multi.core = FALSE) {

  if (case.set %in% c("observed", "expected") == FALSE) {
    stop('case.set must be "observed" or "expected".')
  }

  if (case.location %in% c("address", "nominal") == FALSE) {
    stop('case.location must be "address" or "nominal".')
  }

  cores <- multiCore(multi.core)

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  p.count <- nrow(pump.data)
  p.ID <- seq_len(p.count)
  snow.colors <- snowColors(vestry = vestry)

  if (is.null(pump.select)) {
    pump.id <- pump.data$id
  } else {
    if (is.numeric(pump.select) == FALSE) stop("pump.select must be numeric.")
    if (any(abs(pump.select) %in% p.ID) == FALSE) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ', p.count, ".")
    }

    if (all(pump.select > 0)) {
      pump.id <- pump.data$id[pump.select]
    } else if (all(pump.select < 0)) {
      sel <- pump.data$id %in% abs(pump.select) == FALSE
      pump.id <- pump.data$id[pump.select]
    } else {
      stop("Use all positive or all negative numbers for pump.select.")
    }
  }

  if (case.set == "observed") {
    anchors <- cholera::fatalities.address$anchor
    observed <- TRUE
  } else if (case.set == "expected") {
    anchors <- seq_len(nrow(cholera::regular.cases))
    observed <- FALSE
  }

  nearest.pump <- parallel::mclapply(anchors, function(x) {
    euclideanPath(x, destination = pump.id, vestry = vestry,
      observed = observed, case.location = case.location)$data$pump
  }, mc.cores = cores)

  out <- list(pump.data = pump.data,
              pump.select = pump.select,
              vestry = vestry,
              case.set = case.set,
              case.location = case.location,
              pump.id = pump.id,
              snow.colors = snow.colors,
              anchors = anchors,
              observed = observed,
              nearest.pump = unlist(nearest.pump),
              cores = cores)

  class(out) <- "euclidean"
  out
}

#' Plot method for neighborhoodEuclidean().
#'
#' @param x An object of class "euclidean" created by \code{neighborhoodEuclidean()}.
#' @param type Character. "star", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param polygon.method Character. Method of computing polygon vertices: "pearl.string" or "traveling.salesman".
#' @param add.observed.points Logical. Add observed fatality "addresses".
#' @param msg Logical. Toggle in-progress messages.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @note This uses an approximate computation of polygons, using the 'TSP' package, that may produce non-simple and/or overlapping polygons.
#' @export
#' @examples
#' \dontrun{
#'
#' plot(neighborhoodEuclidean())
#' plot(neighborhoodEuclidean(-6))
#' plot(neighborhoodEuclidean(pump.select = 6:7))
#' plot(neighborhoodEuclidean(case.set = "expected"), type = "area.points")
#' plot(neighborhoodEuclidean(case.set = "expected"), type = "area.polygons")
#' }

plot.euclidean <- function(x, type = "star",
  polygon.method = "traveling.salesman", add.observed.points = TRUE,
  msg = FALSE, ...) {

  if (class(x) != "euclidean") {
    stop('"x"\'s class needs to be "euclidean".')
  }

  if (type %in% c("area.points", "area.polygons")) {
    if (x$case.set != "expected") {
      stop('area plots valid only when case.set = "expected".')
    }

    if (polygon.method == "pearl.string") {
      verticesFn <- pearlString
    } else if (polygon.method == "traveling.salesman") {
      verticesFn <- travelingSalesman
    } else {
      stop('polygon.method must be "pearl.string" or "traveling.salesman".')
    }
  }

  if (msg) {
    if (x$case.set == "expected") message("Working...")
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  road.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.rng <- range(cholera::roads$x)
  y.rng <- range(cholera::roads$y)

  pump.data <- x$pump.data
  pump.id <- x$pump.id
  anchors <- x$anchors
  pump.select <- x$pump.select
  nearest.pump <- x$nearest.pump

  plot(cholera::fatalities.address[, c("x", "y")], xlim = x.rng, ylim = y.rng,
    pch = NA, asp = 1)
  invisible(lapply(border.list, lines))

  if (type == "star") {
    invisible(lapply(road.list, lines, col = "gray"))

    invisible(lapply(seq_along(anchors), function(i) {
      p.data <- pump.data[pump.data$id == nearest.pump[[i]], ]
      n.color <- x$snow.colors[paste0("p", nearest.pump[[i]])]
      if (x$observed) {
        sel <- cholera::fatalities.address$anchor %in% anchors[i]
        n.data <- cholera::fatalities.address[sel, ]
        lapply(n.data$anchor, function(case) {
          c.data <- n.data[n.data$anchor == case, ]
          segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color,
            lwd = 0.5)
        })
      } else {
        n.data <- cholera::regular.cases[anchors[i], ]
        lapply(seq_len(nrow(n.data)), function(case) {
          c.data <- n.data[case, ]
          segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color,
            lwd = 0.5)
        })
      }
    }))

    if (add.observed.points) {
      if (x$case.set == "observed") {
        addNeighborhoodCases(pump.select = x$pump.select, vestry = x$vestry,
          metric = "euclidean", case.location = x$case.location,
          multi.core = x$cores)
      }
    }

  } else if (type == "area.points") {
    invisible(lapply(seq_along(anchors), function(i) {
      n.color <- x$snow.colors[paste0("p", nearest.pump[[i]])]
      n.data <- cholera::regular.cases[anchors[i], ]
      lapply(seq_len(nrow(n.data)), function(case) {
        c.data <- n.data[case, ]
        points(c.data$x, c.data$y, col = n.color, pch = 15, cex = 1.25)
      })
    }))

    invisible(lapply(road.list, lines))

  } else if (type == "area.polygons") {
    invisible(lapply(road.list, lines))
    p.num <- sort(unique(nearest.pump))

    neighborhood.cases <- lapply(p.num, function(n) {
      which(nearest.pump == n)
    })

    periphery.cases <- parallel::mclapply(neighborhood.cases, peripheryCases,
      mc.cores = x$cores)
    pearl.string <- parallel::mclapply(periphery.cases, verticesFn,
      mc.cores = x$cores)
    names(pearl.string) <- p.num

    invisible(lapply(names(pearl.string), function(nm) {
      sel <- paste0("p", nm)
      polygon(cholera::regular.cases[pearl.string[[nm]], ],
        col = grDevices::adjustcolor(x$snow.colors[sel], alpha.f = 2/3))
    }))
  }

  pumpTokens(x$pump.select, x$vestry, x$case.set, x$snow.colors, type)
  title(main = "Pump Neighborhoods: Euclidean")

  if (msg) {
    if (x$case.set == "expected") message("Done!")
  }
}

#' Print method for neighborhoodEuclidean().
#'
#' @param x An object of class "euclidean" created by \code{neighborhoodEuclidean()}.
#' @param ... Additional parameters.
#' @return An R class 'table' vector.
#' @export

print.euclidean <- function(x, ...) {
  if (class(x) != "euclidean") {
    stop('"x"\'s class needs to be "euclidean".')
  }
  print(table(x$nearest.pump))
}
