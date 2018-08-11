#' Plot John Snow's cholera map.
#'
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param stacked Logical. Use stacked fatalities.
#' @param add.cases Logical. Add observed cases.
#' @param add.landmarks Logical. Add landmarks.
#' @param add.pumps Logical. Add pumps.
#' @param add.roads Logical. Add roads.
#' @param add.title Logical. Add title.
#' @param ... Additional plotting parameters.
#' @note Uses amended version of Dodson and Tobler's data included in this package.
#' @return A base R graphics plot.
#' @seealso \code{\link{addLandmarks}},
#' \code{\link{addKernelDensity}},
#' \code{\link{addLandmarks}},
#' \code{\link{addPlaguePit}},
#' \code{\link{addVoronoi}}.
#' \code{\link{addWhitehead}}
#' @export
#' @examples
#' snowMap()
#' snowMap(vestry = TRUE, stacked = FALSE)

snowMap <- function(vestry = FALSE, stacked = TRUE, add.cases = TRUE,
  add.landmarks = FALSE, add.pumps = TRUE, add.roads = TRUE, add.title = TRUE,
  ...) {

  rng <- mapRange()

  if (stacked) {
    cases <- cholera::fatalities
  } else {
    cases <- cholera::fatalities.address
  }

  plot(cases[, c("x", "y")], xlim = rng$x, ylim = rng$y, pch = NA, asp = 1)
  if (add.roads) addRoads()
  if (add.cases) points(cases[, c("x", "y")], pch = 15, col = "gray", cex = 0.5)
  if (add.pumps) addPump(vestry = vestry, col = "blue", pch = 2)
  if (add.title) title(main = "Snow's Cholera Map")
  if (add.landmarks) addLandmarks()
  addBorder()
}

#' Add roads to plot.
#'
#' @param col Character. Color
#' @param ... Additional plotting parameters.
#' @export

addRoads <- function(col = "gray", ...) {
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  invisible(lapply(roads.list, lines, col = "gray", ...))
}

#' Add map border to plot.
#'
#' @param ... Additional plotting parameters.
#' @export

addBorder <- function(...) {
  borders <- cholera::roads[cholera::roads$name == "Map Frame", ]
  border.list <- split(borders[, c("x", "y")], borders$street)
  invisible(lapply(border.list, lines, ...))
}

#' Compute xlim and ylim of Snow's map.
#'
#' @export

mapRange <- function() {
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)
  data.frame(x = x.range, y = y.range)
}
