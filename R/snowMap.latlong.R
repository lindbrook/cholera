#' Plot John Snow's cholera map.
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param stacked Logical. Use stacked fatalities.
#' @param add.axes_box Logical. Add plot axes and plot box.
#' @param add.cases Logical. Add observed cases.
#' @param add.landmarks Logical. Add landmarks.
#' @param add.pumps Logical. Add pumps.
#' @param add.roads Logical. Add roads.
#' @param add.frame Logical. Add map frame.
#' @param main Character. Title of graph.
#' @param case.col Character. Color of fatalities.
#' @param case.pch Character. Color of fatalities.
#' @param road.col Character. Color of roads/streets
#' @param ... Additional plotting parameters.
#' @note Uses amended version of Dodson and Tobler's data included in this package.
#' @return A base R graphics plot.
#' @noRd

snowMap.latlong <- function(path, vestry = FALSE, stacked = TRUE, 
  add.axes_box = TRUE, add.cases = TRUE, add.landmarks = FALSE, 
  add.pumps = TRUE, add.roads = TRUE, add.frame = TRUE, main = NA, 
  case.col = "gray", case.pch = 15, road.col = "gray", ...) {

  vars <- c("lon", "lat")
  asp <- 1.6

  if (stacked) {
    dat <- paste0(path, "fatality_modified.gpkg")
    coords <- sf::st_coordinates(sf::st_read(dat, quiet = TRUE))
    coords <- stats::setNames(data.frame(coords), vars)
    cases <- data.frame(id = cholera::fatalities$case, coords)
  } else {
    dat <- paste0(path, "anchor_modified.gpkg")
    coords <- sf::st_coordinates(sf::st_read(dat, quiet = TRUE))
    coords <- stats::setNames(data.frame(coords), vars)
    cases <- data.frame(id = cholera::fatalities.anchor$anchor, coords)
  }

  rd.gpkg <- sf::st_read(paste0(path, "roadSegment_modified.gpkg"), quiet = TRUE)
  rds <- data.frame(sf::st_coordinates(rd.gpkg))
  rng <- data.frame(x = range(rds$X), y = range(rds$Y))

  if (add.axes_box) {
    plot(cases[, vars], xlim = rng$x, ylim = rng$y, pch = NA, asp = asp,
      main = main, ...)
  } else {
    plot(cases[, vars], xlim = rng$x, ylim = rng$y, pch = NA, asp = asp,
      main = main, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA, bty = "n",
      ...)
  }

  if (add.roads) {
    rd.gpkg <- sf::st_read(paste0(path, "roadSegment_modified.gpkg"), quiet = TRUE)
    coords <- data.frame(sf::st_coordinates(rd.gpkg))
    invisible(lapply(unique(coords$L1), function(id) {
     lines(coords[coords$L1 == id, c("X", "Y")], col = road.col)
    }))
  }

  if (add.cases) {
    points(cases[, vars], pch = case.pch, col = case.col, cex = 0.5)
  }

  if (add.pumps) {
    if (vestry) {
        dat <- paste0(path, "pumpvestry_modified.gpkg")
        dat.attr <- cholera::pumps.vestry[, c("id", "street")]
      } else {
        dat <- paste0(path, "pump_modified.gpkg")
        dat.attr <- cholera::pumps[, c("id", "street")]
      }
      coords <- sf::st_coordinates(sf::st_read(dat, quiet = TRUE))
      coords <- stats::setNames(data.frame(coords), vars)
      p.data <- data.frame(dat.attr, coords)
      points(p.data[, vars], col = "blue", pch = 2)
      text(p.data[, vars], col = "blue", pos = 1, 
        labels = paste0("p", p.data$id))
  }

  if (add.frame)
    dat <- paste0(path, "frameSegment_modified.gpkg")
    frame.gpkg <- sf::st_read(dat, quiet = TRUE)
    coords <- data.frame(sf::st_coordinates(frame.gpkg))
    invisible(lapply(unique(coords$L1), function(id) {
      lines(coords[coords$L1 == id, c("X", "Y")])
    }))
  }
