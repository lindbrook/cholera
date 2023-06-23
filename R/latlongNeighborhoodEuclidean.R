#' Compute Euclidean pump neighborhoods (lat-long prototype).
#'
#' Plots star graph from pump to its cases.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param case.location Character. "address" or "orthogonal". "address" uses the longitude and latitude of \code{fatalities.address}. "orthogonal" uses the longitude and latitude of \code{latlong.ortho.address}.
#' @param case.set Character. "observed" or "expected".
#' @param pump.location Character. "address" or "orthogonal". "address" uses the longitude and latitude coordinates of \code{pumps} or \code{pumps.vestry}. "orthogonal" uses the longitude and latitude coordinates of \code{latlong.ortho.pump} or \code{latlong.ortho.pump.vestry}.
#' @export

latlongNeighborhoodEuclidean <- function(pump.select = NULL, vestry = FALSE,
  case.location = "address", case.set = "observed", pump.location = "address") {

  if (case.location %in% c("address", "orthogonal") == FALSE) {
    stop('case.location must be "address" or "orthogonal".', call. = FALSE)
  } else {
    if (case.location == "orthogonal") statistic <- "orthogonal"
    else if (case.location == "address") statistic <- "address"
  }

  if (case.set %in% c("observed", "expected") == FALSE) {
    stop('case.set must be "observed" or "expected".', call. = FALSE)
  }

  if (pump.location %in% c("address", "orthogonal") == FALSE) {
    stop('pump.location must be "address" or "orthogonal".', call. = FALSE)
  } else if (pump.location == "orthogonal") {
    if (vestry) pump.data <- cholera::latlong.ortho.pump.vestry
    else pump.data <- cholera::latlong.ortho.pump
  } else if (pump.location == "address") {
    if (vestry) pump.data <- cholera::pumps.vestry
    else pump.data <- cholera::pumps
  }

  cells <- latlongVoronoi(pump.select = pump.select, vestry = vestry)

  if (!is.null(pump.select)) {
    pump.id <- selectPump(pump.data, pump.select = pump.select,
      metric = "euclidean", vestry = vestry)
  } else {
    pump.id <- pump.select
  }

  if (case.set == "observed") {
    if (statistic == "address") {
      case.data <- cholera::fatalities.address
    } else if (statistic == "orthogonal") {
      case.data <- cholera::latlong.ortho.addr
    }
  } else if (case.set == "expected") {
    if (statistic == "address") {
      case.data <- cholera::latlong.regular.cases
    } else if (statistic == "orthogonal") {
      case.data <- cholera::latlong.sim.ortho.proj
    }
  }

  statistic.data <- lapply(cells, function(cell) {
    sp::point.in.polygon(case.data$lon, case.data$lat, cell$lon, cell$lat)
  })

  out <- list(pump.select = pump.id, vestry = vestry, cells = cells,
    pump.data = pump.data, case.location = case.location,
    statistic.data = statistic.data)
  class(out) <- "latlongNeighborhoodEuclidean"
  out
}

#' Plot method for latlongNeighborhoodEuclidean()
#' @param x Object.
#' @param euclidean.paths Logical.
#' @param ... Additional plotting parameters.
#' @export

plot.latlongNeighborhoodEuclidean <- function(x, euclidean.paths = TRUE, ...) {
  pump.select <- x$pump.select
  vars <- c("lon", "lat")

  snowMap(vestry = x$vestry, latlong = TRUE, add.cases = FALSE,
    add.pumps = FALSE)
  addPump(pump.select, vestry = x$vestry, latlong = TRUE)

  if (!is.null(pump.select)) {
    unselected <- x$pump.data[!x$pump.data$id %in% pump.select, ]
    names(x$statistic.data) <- pump.select
    snow.colors <- snowColors(vestry = x$vestry)[paste0("p", pump.select)]
    points(unselected[, vars], pch = 2, col = "gray")
    text(unselected[, vars], labels = paste0("p", unselected$id), pos = 1,
      col = "gray")
    if (x$case.location == "address") {
      title(main = paste0("Pump Neighborhoods: Euclidean (address)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    } else if (x$case.location == "orthogonal") {
      title(main = paste0("Pump Neighborhoods: Euclidean (orthogonal)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    }
  } else {
    snow.colors <- snowColors(vestry = x$vestry)
    if (x$case.location == "address") {
      title(main = "Pump Neighborhoods: Euclidean (address)")
    } else if (x$case.location == "orthogonal") {
      title(main = "Pump Neighborhoods: Euclidean (orthogonal)")
    }
  }

  if (euclidean.paths) {
    plotLatlongEuclideanPaths(x, pump.select, snow.colors, vars)
  }

  plotLatlongVoronoiCases(x, snow.colors, vars)
}
