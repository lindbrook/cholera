#' Plot John Snow's cholera map.
#'
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
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param ... Additional plotting parameters.
#' @note Uses amended version of Dodson and Tobler's data included in this package.
#' @return A base R graphics plot.
#' @export
#' @examples
#' snowMap()
#' snowMap(vestry = TRUE, stacked = FALSE)

snowMap <- function(vestry = FALSE, stacked = TRUE, add.axes_box = TRUE,
  add.cases = TRUE, add.landmarks = FALSE, add.pumps = TRUE, add.roads = TRUE,
  add.frame = TRUE, main = NA, case.col = "gray", case.pch = 15, 
  road.col = "gray", latlong = FALSE, ...) {

  if (latlong) {
    vars <- c("lon", "lat")
    asp <- 1.6
  } else {
    vars <- c("x", "y")
    asp <- 1
  }

  if (stacked) {
    cases <- cholera::fatalities
  } else {
    cases <- cholera::fatalities.address
  }

  rng <- mapRange(latlong = latlong)

  if (add.axes_box) {
    plot(cases[, vars], xlim = rng$x, ylim = rng$y, pch = NA, asp = asp,
      main = main, ...)
  } else {
    plot(cases[, vars], xlim = rng$x, ylim = rng$y, pch = NA, asp = asp,
      main = main, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA, bty = "n",
      ...)
  }

  if (add.roads) addRoads(latlong = latlong, col = road.col)
  if (add.cases) {
    points(cases[, vars], pch = case.pch, col = case.col, cex = 0.5)
  }
  if (add.pumps) {
    addPump(vestry = vestry, col = "blue", pch = 2, latlong = latlong)
  }
  if (add.landmarks) addLandmarks(latlong = latlong)
  if (add.frame) addFrame(latlong = latlong)
}

#' Add all streets and roads to plot.
#'
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param col Character. Color
#' @export

addRoads <- function(latlong = FALSE, col = "gray") {
  postfix <- c(1, 1, 2, 2)
  if (latlong) {
    vars <- paste0(c("lon", "lat"), postfix)
  } else {
    vars <- paste0(c("x", "y"), postfix)
  }
  segments(cholera::road.segments[, vars[1]],
           cholera::road.segments[, vars[2]],
           cholera::road.segments[, vars[3]],
           cholera::road.segments[, vars[4]], col = col)
}

#' Add map border to plot.
#'
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param col Character. Color
#' @param ... Additional plotting parameters.
#' @noRd

addFrame <- function(latlong = FALSE, col = "black", ...) {
  postfix <- c(1, 1, 2, 2)
  if (latlong) {
    vars <- paste0(c("lon", "lat"), postfix)
  } else {
    vars <- paste0(c("x", "y"), postfix)
  }
  segments(cholera::frame.segments[, vars[1]],
           cholera::frame.segments[, vars[2]],
           cholera::frame.segments[, vars[3]],
           cholera::frame.segments[, vars[4]], col = col)
}

#' Compute xlim and ylim of Snow's map.
#'
#' @param latlong Logical. Use estimated longitude and latitude.
#' @export

mapRange <- function(latlong = FALSE) {
  if (latlong) {
    x.range <- range(cholera::roads$lon)
    y.range <- range(cholera::roads$lat)
  } else {
    x.range <- range(cholera::roads$x)
    y.range <- range(cholera::roads$y)
  }
  data.frame(x = x.range, y = y.range)
}
