#' Plots observed and expected walking neighborhoods.
#'
#' Neighborhoods are based on the shortest paths between a fatality's address and its nearest pump.
#'
#' @param streets Logical. TRUE plots neighborhoods by street. FALSE plots orthogonal neighborhoods (area).
#' @param obs Logical. TRUE uses observed cases. FALSE uses "regular" simulated cases.
#' @param add.landmarks Logical. Include landmarks.
#' @param color Character. Uses snowColor().
#' @return A base R graphics plot.
#' @export
#' @examples
#' trimPlot()
#' trimPlot(streets = FALSE)

trimPlot <- function(streets = TRUE, obs = TRUE, add.landmarks = TRUE,
  color = cholera::snowColors()) {

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

    if (obs) {
      obs.pump <- which(vapply(cholera::pump.cases, function(x) {
        length(x) != 0
      }, logical(1L)))

      invisible(lapply(seq_along(cholera::neighborhood.segments), function(i) {
        plotSegment(cholera::neighborhood.segments[[i]], color[obs.pump[i]])
      }))

      invisible(lapply(cholera::pumps$id, function(i) {
        points(cholera::pumps[i, c("x", "y")], pch = 17, col = color[i])
      }))

      invisible(lapply(obs.pump, function(i) {
        points(
          cholera::fatalities.address[cholera::fatalities.address$anchor.case
          %in% cholera::pump.cases[[i]], c("x", "y")], pch = 20, cex = 0.75,
          col = color[i])
      }))

      title(main = "Observed Paths")

    } else {
      for (i in cholera::pumps$id) {
        plotSegment(cholera::neighborhood.segments.sp[[i]], color[i])
        points(cholera::pumps[i, c("x", "y")], pch = 17, col = color[i])
      }

      title(main = "Expected Paths")
    }

    text(cholera::pumps[, c("x", "y")], cex = 1, pos = 1,
      label = cholera::pumps$id)

  } else {
    if (obs) {
      obs.pump <- which(vapply(cholera::pump.cases, function(x) {
        length(x) != 0
      }, logical(1L)))

      invisible(lapply(roads.list, lines, col = "lightgray"))
      invisible(lapply(border.list, lines))

      for (i in cholera::pumps$id) {
        points(cholera::regular.cases[cholera::pump.cases.sp[[i]], ],
          col = scales::alpha(color[i], 0.33), pch = 15)
      }

      invisible(lapply(seq_along(cholera::neighborhood.segments), function(i) {
        plotSegment(cholera::neighborhood.segments[[i]], color[obs.pump[i]])
      }))

      invisible(lapply(obs.pump, function(i) {
        points(
          cholera::fatalities.address[cholera::fatalities.address$anchor.case
          %in% cholera::pump.cases[[i]], c("x", "y")], pch = 20, cex = 0.75,
          col = color[i])
      }))

      for (i in cholera::pumps$id) {
        points(cholera::pumps[i, c("x", "y")], pch = 24, bg = color[i])
      }

      text(cholera::pumps[, c("x", "y")], cex = 1, pos = 1,
        label = cholera::pumps$id)

      title(main = "Observed Neighborhoods and Paths")

    } else {
      for (i in cholera::pumps$id) {
        points(cholera::regular.cases[cholera::pump.cases.sp[[i]], ],
          col = color[i], pch = 15)
        points(cholera::pumps[i, c("x", "y")], pch = 24, bg = color[i],
          col = "white")
      }

      invisible(lapply(roads.list, lines))
      invisible(lapply(border.list, lines))
      text(cholera::pumps[, c("x", "y")], cex = 1, pos = 1, col = "white",
           label = cholera::pumps$id)
      title(main = "Expected Neighborhoods")
    }
  }

  if (add.landmarks) cholera::addLandmarks(text.size = 0.5)
}

plotSegment <- function(neighborhood, snow.color) {
  invisible(lapply(neighborhood$id, function(x) {
    lines(neighborhood[neighborhood$id == x, c("x1", "x2")],
          neighborhood[neighborhood$id == x, c("y1", "y2")],
          col = snow.color, lwd = 2)
  }))
}
