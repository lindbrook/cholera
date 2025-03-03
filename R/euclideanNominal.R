#' Compute Euclidean path pump neighborhoods.
#'
#' Plots star graph from pump to its cases.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param case.set Character. "observed" or "expected".
#' @param location Character. "nominal", "anchor" or "orthogonal".
#' @param brute.force Logical. TRUE computes nearest pump for each case. FALSE uses Voronoi cells as shortcut.
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
  case.set = "observed", location = "nominal", brute.force = FALSE,
  multi.core = FALSE, dev.mode = FALSE) {

  if (case.set %in% c("observed", "expected") == FALSE) {
    stop('case.set must be "observed" or "expected".', call. = FALSE)
  }

  if (location %in% c("nominal", "anchor", "orthogonal") == FALSE) {
    stop('location must be "nominal", "anchor", or "orthogonal".',
      call. = FALSE)
  }

  cores <- multiCore(multi.core)
  snow.colors <- snowColors(vestry = vestry)

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  p.sel <- selectPump(pump.data, pump.select = pump.select, vestry = vestry)

  if (case.set == "observed") {
    if (location %in% c("anchor", "orthogonal")) {
      case.num <- cholera::fatalities.address$anchor
    } else if (location == "nominal") {
      case.num <- cholera::fatalities$case
    }
  } else if (case.set == "expected") {
    case.num <- seq_len(nrow(cholera::regular.cases))
  }

  if (brute.force) {
    if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl = cl, envir = environment(),
        varlist = c("p.sel", "vestry", "case.set", "location"))
      nearest.pump <- parallel::parLapply(cl, case.num, function(x) {
        cholera::euclideanPath(x, destination = p.sel, vestry = vestry,
          case.set = case.set, location = location)$data$pump
      })
      parallel::stopCluster(cl)
    } else {
      nearest.pump <- parallel::mclapply(case.num, function(x) {
        euclideanPath(x, destination = p.sel, vestry = vestry,
          case.set = case.set, location = location)$data$pump
      }, mc.cores = cores)
    }

    nearest.pump <- unlist(nearest.pump)

  } else {
    if (case.set == "observed") {
      case.data <- cholera::fatalities[cholera::fatalities$case %in% case.num, ]
    } else if (case.set == "expected"){
      case.data <- cholera::regular.cases
    }

    if (length(p.sel) > 1) {
      sel <- pump.data$id %in% p.sel
      vars <- c("x", "y")
      cells <- voronoiPolygons(pump.data[sel, vars], rw.data = mapRange())
      names(cells) <- p.sel

      cell.census <- lapply(names(cells), function(nm) {
        cell <- cells[[nm]]
        census <- sp::point.in.polygon(case.data$x, case.data$y, cell$x, cell$y)
        census[census == 1] <- as.numeric(nm)
        census
      })

      nearest.pump <- rowSums(do.call(cbind, cell.census))
    } else if (length(p.sel) == 1) {
      nearest.pump <- rep(p.sel, nrow(case.data))
    }
  }

  out <- list(pump.data = pump.data,
              pump.select = pump.select,
              vestry = vestry,
              case.set = case.set,
              location = location,
              p.sel = p.sel,
              snow.colors = snow.colors,
              case.num = case.num,
              nearest.pump = nearest.pump,
              cores = cores,
              dev.mode = dev.mode,
              latlong = FALSE)

  class(out) <- "euclidean"
  out
}

#' Plot method for neighborhoodEuclidean().
#'
#' @param x An object of class "euclidean" created by \code{neighborhoodEuclidean()}.
#' @param type Character. "star", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param add Logical. Add graphic to an existing plot.
#' @param add.observed.points Logical. Add observed fatality "addresses".
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param polygon.type Character. "perimeter" or "solid".
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

plot.euclidean <- function(x, type = "star", add = FALSE,
  add.observed.points = TRUE, alpha.level = 0.75, polygon.type = "solid", ...) {

  if (!type %in% c("area.points", "area.polygons", "star")) {
    stop('type must be "area.points", "area.polygons" or "star".',
      call. = FALSE)
  }

  if (type %in% c("area.points", "area.polygons")) {
    if (x$case.set != "expected") {
      stop('area plots valid only when case.set = "expected".', call. = FALSE)
    }
  }

  if (!polygon.type %in% c("border", "solid")) {
    stop('polygon.type must be "border" or "solid".', call. = FALSE)
  }

  if (!add) snowMap(add.cases = FALSE, add.roads = FALSE, add.pumps = FALSE)
  pump.data <- x$pump.data
  p.sel <- x$p.sel
  case.num <- x$case.num
  pump.select <- x$pump.select
  nearest.pump <- x$nearest.pump

  if (type == "star") {
    if (!add) {
      if (x$case.set == "observed") {
        addRoads()
      } else if (x$case.set == "expected") {
        addRoads(col = "black")
      }
    }
    euclideanStar(x, case.num, nearest.pump, pump.data,
      add.observed.points = add.observed.points)
  } else if (type == "area.points") {
    euclideanAreaPoints(x, case.num, nearest.pump)
    if (!add) addRoads(col = "black")
  } else if (type == "area.polygons") {
    euclideanAreaPolygons(x, nearest.pump, alpha.level, polygon.type)
    if (!add) addRoads(col = "black")
  }

  if (!add) pumpTokens(x, type, alpha.level, polygon.type)

  if (!add){
    if (is.null(x$pump.select)) {
      title(main = "Pump Neighborhoods: Euclidean")
    } else {
      title(main = paste0("Pump Neighborhoods: Euclidean", "\n", "Pumps ",
        paste(sort(x$pump.select), collapse = ", ")))
    }
  }
}

euclideanStar <- function(x, case.num, nearest.pump, pump.data,
  add.observed.points = add.observed.points) {

  invisible(lapply(seq_along(case.num), function(i) {
    p.data <- pump.data[pump.data$id == nearest.pump[i], ]
    n.color <- x$snow.colors[paste0("p", nearest.pump[i])]

    if (x$case.set == "observed") {
      if (x$location %in% c("nominal", "anchor")) {
        sel <- cholera::fatalities$case %in% case.num[i]
        n.data <- cholera::fatalities[sel, ]
      } else if (x$location == "orthogonal") {
        sel <- cholera::ortho.proj$case %in% case.num[i]
        n.data <- cholera::ortho.proj[sel, ]
        vars <- c("case", "x.proj", "y.proj")
        names(n.data)[names(n.data) %in% vars] <- c("x", "y", "anchor")
      }
      segments(n.data$x, n.data$y, p.data$x, p.data$y, col = n.color, lwd = 0.5)
    } else {
      n.data <- cholera::regular.cases[case.num[i], ]
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
        metric = "euclidean", case.set = x$case.set, location = x$location,
        multi.core = x$cores)
    }
  }
}

euclideanAreaPoints <- function(x, case.num, nearest.pump) {
  invisible(lapply(seq_along(case.num), function(i) {
    n.color <- x$snow.colors[paste0("p", nearest.pump[[i]])]
    n.data <- cholera::regular.cases[case.num[i], ]
    lapply(seq_len(nrow(n.data)), function(case) {
      c.data <- n.data[case, ]
      points(c.data$x, c.data$y, col = n.color, pch = 16, cex = 1.25)
    })
  }))
}

euclideanAreaPolygons <- function(x, nearest.pump, alpha.level, polygon.type) {
  p.num <- sort(unique(nearest.pump))
  neighborhood.cases <- lapply(p.num, function(n) {
    which(nearest.pump == n)
  })
  periphery.cases <- lapply(neighborhood.cases, peripheryCases)
  pearl.string <- lapply(periphery.cases, travelingSalesman)
  names(pearl.string) <- p.num

  if (polygon.type == "border") {
    invisible(lapply(names(pearl.string), function(nm) {
      sel <- paste0("p", nm)
      polygon(cholera::regular.cases[pearl.string[[nm]], ],
        border = x$snow.colors[sel])
    }))
  } else if (polygon.type == "solid") {
    invisible(lapply(names(pearl.string), function(nm) {
      sel <- paste0("p", nm)
      polygon(cholera::regular.cases[pearl.string[[nm]], ],
        col = grDevices::adjustcolor(x$snow.colors[sel], alpha.f = alpha.level))
    }))
  }
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
  print(x[c("p.sel", "case.set", "location", "vestry", "latlong")])
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
