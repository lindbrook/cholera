#' Plots the 13 expected walking neighborhoods.
#'
#' Neighborhoods are based on the shortest paths between a fatality's address and its nearest pump.
#'
#' @param streets Logical. TRUE plots neighborhoods by street. FALSE plots orthogonal neighborhoods (area).
#' @param add.landmarks Logical. Include landmarks.
#' @param color Character. Uses snowColor().
#' @return A base R graphics plot.
#' @export
#' @examples
#' trimPlot()
#' trimPlot(streets = FALSE)

trimPlot <- function(streets = TRUE, add.landmarks = TRUE,
  color = snowColors()) {

  roadsB <- cholera::roads[cholera::roads$street %in%
                           cholera::border == FALSE, ]

  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(roadsB[, c("x", "y")], roadsB$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  plot(cholera::fatalities[, c("x", "y")], xlim = x.range, ylim =  y.range,
    pch = NA, asp = 1)

  if (streets) {
    invisible(lapply(roads.list, lines, col = "gray"))
    invisible(lapply(border.list, lines))

    for (i in cholera::pumps$id) {
      plotSegment(cholera::neighborhood.segments[[i]], color[i])
      points(cholera::pumps.vestry[i, c("x", "y")], pch = 17,
        col = color[i])
    }

    text(cholera::pumps.vestry[, c("x", "y")], cex = 1, pos = 1,
      label = cholera::pumps.vestry$id)
    title(main = "Expected Paths")

  } else {
    for (i in cholera::pumps$id) {
      points(cholera::regular.cases[cholera::pump.cases[[i]], ], col = color[i],
        pch = 15)
      points(cholera::pumps.vestry[i, c("x", "y")], pch = 24, bg = color[i],
        col = "white")
    }

    invisible(lapply(roads.list, lines))
    invisible(lapply(border.list, lines))
    text(cholera::pumps.vestry[, c("x", "y")], cex = 1, pos = 1, col = "white",
      label = cholera::pumps.vestry$id)
    title(main = "Expected Path Neighborhoods")
  }

  if (add.landmarks) cholera::addLandmarks(text.size = 0.5)
}

#' Uniform set of colors for plots.
#'
#' @param vestry Logical. TRUE uses the 14 pumps in the Vestry Report. FALSE uses the original 13.
#' @return A character vector of colors.

snowColors <- function(vestry = FALSE) {
  colors.pair <- RColorBrewer::brewer.pal(10, "Paired")
  colors.dark <- RColorBrewer::brewer.pal(8, "Dark2")
  if (!vestry) {
    c("dodgerblue", "gray", colors.dark[1:4], colors.pair[2], colors.dark[5:8],
      "red", colors.pair[1])
  } else {
    c("dodgerblue", "gray", colors.dark[1:4], colors.pair[2], colors.dark[5:8],
      "red", colors.pair[1], "darkorange")
  }
}

plotSegment <- function(neighborhood, snow.color) {
  invisible(lapply(neighborhood$id, function(x) {
    lines(cholera::road.segments[cholera::road.segments$id == x, c("x1", "x2")],
          cholera::road.segments[cholera::road.segments$id == x, c("y1", "y2")],
          col = snow.color, lwd = 2)
  }))
}
