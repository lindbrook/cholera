#' Compute Euclidean path pump neighborhoods.
#'
#' Plots star graph from pump to its cases.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param case.set Character. "observed" or "expected".
#' @param case.location Character. "address" or "orthogonal". For \code{case.set = "observed"}: "address" uses \code{fatalities} and "orthogonal" uses \code{ortho.proj}. For \code{case.set = "expected"}: "address" uses \code{regular.cases} and "orthogonal" uses \code{sim.ortho.proj}.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @return An R vector.
#' @noRd
#' @examples
#' \dontrun{
#' euclideanNominal()
#' euclideanNominal(-6)
#' euclideanNominal(pump.select = 6:7)
#' }

euclideanNominal <- function(pump.select = NULL, vestry = FALSE,
  case.set = "observed", case.location = "address", multi.core = TRUE,
  dev.mode = FALSE) {

  if (case.set %in% c("observed", "expected") == FALSE) {
    stop('case.set must be "observed" or "expected".', call. = FALSE)
  }

  if (case.location %in% c("address", "orthogonal") == FALSE) {
    stop('case.location must be "address" or "orthogonal".', call. = FALSE)
  }

  cores <- multiCore(multi.core)
  snow.colors <- snowColors(vestry = vestry)

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  pump.id <- selectPump(pump.data, pump.select = pump.select,
    metric = "euclidean", vestry = vestry)

  if (case.set == "observed") {
    observed <- TRUE
    if (case.location == "address") {
      anchors <- cholera::fatalities.address$anchor
    } else if (case.location == "orthogonal") {
      anchors <- cholera::ortho.proj$case
    }
  } else if (case.set == "expected") {
    anchors <- seq_len(nrow(cholera::regular.cases))
    observed <- FALSE
  }

  if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = c("pump.id", "vestry", "case.set", "case.location"))
    nearest.pump <- parallel::parLapply(cl, anchors, function(x) {
      cholera::euclideanPath(x, destination = pump.id, vestry = vestry,
        case.set = case.set, case.location = case.location)$data$pump
    })
    parallel::stopCluster(cl)
  } else {
    nearest.pump <- parallel::mclapply(anchors, function(x) {
      euclideanPath(x, destination = pump.id, vestry = vestry,
        case.set = case.set, case.location = case.location)$data$pump
    }, mc.cores = cores)
  }

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
              cores = cores,
              dev.mode = dev.mode)

  class(out) <- "euclidean"
  out
}

#' Plot method for neighborhoodEuclidean().
#'
#' @param x An object of class "euclidean" created by \code{neighborhoodEuclidean()}.
#' @param type Character. "star", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param add.observed.points Logical. Add observed fatality "addresses".
#' @param add.title Logical. Add title.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @note This uses an approximate computation of polygons, using the 'TSP' package, that may produce non-simple and/or overlapping polygons.
#' @export
#' @examples
#' \dontrun{
#' plot(neighborhoodEuclidean())
#' plot(neighborhoodEuclidean(-6))
#' plot(neighborhoodEuclidean(pump.select = 6:7))
#' plot(neighborhoodEuclidean(case.set = "expected"), type = "area.points")
#' plot(neighborhoodEuclidean(case.set = "expected"), type = "area.polygons")
#' }

plot.euclidean <- function(x, type = "star", add.observed.points = TRUE,
  add.title = TRUE, ...) {

  if (type %in% c("area.points", "area.polygons")) {
    if (x$case.set != "expected") {
      stop('area plots valid only when case.set = "expected".')
    }
  }

  snowMap(add.cases = FALSE, add.roads = FALSE, add.pumps = FALSE)
  pump.data <- x$pump.data
  pump.id <- x$pump.id
  anchors <- x$anchors
  pump.select <- x$pump.select
  nearest.pump <- x$nearest.pump

  if (type == "star") {
    if (x$case.set == "observed") {
      addRoads()
    } else if (x$case.set == "expected") {
      addRoads(col = "black")
    }
    euclideanStar(x, anchors, nearest.pump, pump.data,
      add.observed.points = add.observed.points)
  } else if (type == "area.points") {
    euclideanAreaPoints(x, anchors, nearest.pump)
    addRoads(col = "black")
  } else if (type == "area.polygons") {
    euclideanAreaPolygons(x, nearest.pump)
    addRoads(col = "black")
  }

  pumpTokens(x, type)

  if (add.title) {
    if (is.null(x$pump.select)) {
      title(main = "Pump Neighborhoods: Euclidean")
    } else {
      title(main = paste0("Pump Neighborhoods: Euclidean", "\n", "Pumps ",
        paste(sort(x$pump.select), collapse = ", ")))
    }
  }
}

euclideanStar <- function(x, anchors, nearest.pump, pump.data,
  add.observed.points = add.observed.points) {

  invisible(lapply(seq_along(anchors), function(i) {
    p.data <- pump.data[pump.data$id == nearest.pump[i], ]
    n.color <- x$snow.colors[paste0("p", nearest.pump[i])]

    if (x$observed) {
      if (x$case.location == "address") {
        sel <- cholera::fatalities.address$anchor %in% anchors[i]
        n.data <- cholera::fatalities.address[sel, ]
      } else if (x$case.location == "orthogonal") {
        sel <- cholera::ortho.proj$case %in% anchors[i]
        n.data <- cholera::ortho.proj[sel, ]
        vars <- c("case", "x.proj", "y.proj")
        names(n.data)[names(n.data) %in% vars] <- c("x", "y", "anchor")
      }
      c.data <- n.data[n.data$anchor == anchors[i], ]
      segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color, lwd = 0.5)
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
}

euclideanAreaPoints <- function(x, anchors, nearest.pump) {
  invisible(lapply(seq_along(anchors), function(i) {
    n.color <- x$snow.colors[paste0("p", nearest.pump[[i]])]
    n.data <- cholera::regular.cases[anchors[i], ]
    lapply(seq_len(nrow(n.data)), function(case) {
      c.data <- n.data[case, ]
      points(c.data$x, c.data$y, col = n.color, pch = 16, cex = 1.25)
    })
  }))
}

euclideanAreaPolygons <- function(x, nearest.pump) {
  p.num <- sort(unique(nearest.pump))
  neighborhood.cases <- lapply(p.num, function(n) {
    which(nearest.pump == n)
  })
  periphery.cases <- lapply(neighborhood.cases, peripheryCases)
  pearl.string <- lapply(periphery.cases, travelingSalesman)
  names(pearl.string) <- p.num
  invisible(lapply(names(pearl.string), function(nm) {
    sel <- paste0("p", nm)
    polygon(cholera::regular.cases[pearl.string[[nm]], ],
      col = grDevices::adjustcolor(x$snow.colors[sel], alpha.f = 2/3))
  }))
}

#' Print method for neighborhoodEuclidean().
#'
#' Parameter values for neighborhoodEuclidean().
#' @param x An object of class "euclidean" created by \code{neighborhoodEuclidean()}.
#' @param ... Additional parameters.
#' @return A list of argument values.
#' @export
#' @examples
#' \dontrun{
#' neighborhoodEuclidean()
#' print(neighborhoodEuclidean())
#' }

print.euclidean <- function(x, ...) {
  print(x[c("pump.id", "case.set", "case.location", "vestry")])
}

#' Summary method for neighborhoodEuclidean().
#'
#' Return computed counts for Euclidean neighborhoods.
#' @param object Object. An object of class "euclidean" created by \code{neighborhoodEuclidean()}.
#' @param ... Additional parameters.
#' @return A vector of counts by neighborhood.
#' @export
#' @examples
#' \dontrun{
#' summary(neighborhoodEuclidean())
#' }

summary.euclidean <- function(object, ...) {
  xtab <- table(object$nearest.pump)
  stats::setNames(xtab, paste0("p", names(xtab)))
}
