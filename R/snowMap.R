#' Plot John Snow's cholera map.
#'
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param stacked Logical. Use stacked fatalities.
#' @param add.cases Logical. Add observed cases.
#' @param add.landmarks Logical. Add landmarks.
#' @param add.pumps Logical. Add pumps.
#' @param add.roads Logical. Add roads.
#' @param add.frame Logical. Add map frame.
#' @param main Character. Title of graph.
#' @param case.col Character. Color of fatalities.
#' @param case.pch Character. Color of fatalities.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param ... Additional plotting parameters.
#' @note Uses amended version of Dodson and Tobler's data included in this package.
#' @return A base R graphics plot.
#' @export
#' @examples
#' snowMap()
#' snowMap(vestry = TRUE, stacked = FALSE)

snowMap <- function(vestry = FALSE, stacked = TRUE, add.cases = TRUE,
  add.landmarks = FALSE, add.pumps = TRUE, add.roads = TRUE, add.frame = TRUE,
  main = NA, case.col = "gray", case.pch = 15, latlong = FALSE, ...) {

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

  rng <- mapRange(latlong)

  plot(cases[, vars], xlim = rng$x, ylim = rng$y, pch = NA, asp = asp,
    main = main, ...)
  if (add.roads) addRoads(latlong)
  if (add.cases) points(cases[, vars], pch = case.pch, col = case.col,
    cex = 0.5)
  if (add.pumps) {
    addPump(vestry = vestry, col = "blue", pch = 2, latlong = latlong)
  }
  # if (add.landmarks) addLandmarks()
  if (add.frame) addFrame(latlong)
}

#' Add all streets and roads to plot.
#'
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param col Character. Color
#' @export

addRoads <- function(latlong = FALSE, col = "gray") {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  roads.list <- split(rd[, vars], rd$street)
  invisible(lapply(roads.list, lines, col = col))
}

#' Add map border to plot.
#'
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param col Character. Color
#' @param ... Additional plotting parameters.
#' @export

addFrame <- function(latlong = FALSE, col = "black", ...) {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")
  borders <- cholera::roads[cholera::roads$name == "Map Frame", ]
  border.list <- split(borders[, vars], borders$street)
  invisible(lapply(border.list, lines, col = col, ...))
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
