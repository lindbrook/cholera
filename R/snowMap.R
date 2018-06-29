#' Plot John Snow's cholera map.
#'
#' Uses amended version of Dodson and Tobler's data.
#' @param vestry Logical. TRUE uses the 14 pumps from the map in the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param stacked Logical. Use stacked fatalities.
#' @param add.cases Logical. Add observed cases.
#' @param add.landmarks Logical. Add landmarks.
#' @param add.pumps Logical. Add pumps.
#' @param add.roads Logical. Add roads.
#' @param add.title Logical. Add title.
#' @param ... Additional plotting parameters.
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

  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)
  borders <- cholera::roads[cholera::roads$name == "Map Frame", ]
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(borders[, c("x", "y")], borders$street)

  if (stacked) {
    cases <- cholera::fatalities
  } else {
    cases <- cholera::fatalities.address
  }

  plot(cases[, c("x", "y")], xlim = x.range, ylim = y.range, pch = NA, asp = 1)
  invisible(lapply(border.list, lines))

  if (add.roads) {
    invisible(lapply(roads.list, lines, col = "gray"))
  }

  if (add.cases) {
    points(cases[, c("x", "y")], pch = 15, col = "gray", cex = 0.5)
  }

  if (add.pumps) {
    if (vestry) {
      well <- cholera::pumps.vestry
      points(well[, c("x", "y")], pch = 2, cex = 1, col = "blue")
      text(well[, c("x", "y")], label = paste0("p", well$id), pos = 1,
        col = "blue")
    } else {
      well <- cholera::pumps
      points(well[, c("x", "y")], pch = 2, cex = 1, col = "blue")
      text(well[, c("x", "y")], label = paste0("p", well$id), pos = 1,
        col = "blue")
    }
  }

  if (add.title) {
    title(main = "Snow's Cholera Map")
  }

  if (add.landmarks) addLandmarks()
}
