#' Plot John Snow's cholera map.
#'
#' Plots Dodson and Tober's digitization of Snow's map and landmarks
#' @param add.landmarks Logical. Add landmarks.
#' @param vestry Logical. Use the 14 pumps from the Vestry Report.
#' @param stacked Logical. Use stacked fatalities.
#' @return A base R graphics plot.
#' @seealso \code{\link[cholera]{addLandmarks}}
#' @export
#' @examples
#' snowMap()
#' snowMap(vestry = TRUE, stacked = FALSE)

snowMap <- function(add.landmarks = TRUE, vestry = FALSE, stacked = TRUE) {
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)
  borders <- cholera::roads[cholera::roads$name == "Map Frame", ]
  roadsB <- cholera::roads[cholera::roads$name != "Map Frame", ]
  roads.list <- split(roadsB[, c("x", "y")], roadsB$street)
  border.list <- split(borders[, c("x", "y")], borders$street)

  if (stacked) {
    death <- cholera::fatalities
  } else {
    death <- cholera::fatalities.address
  }

  plot(death[, c("x", "y")], xlim = x.range, ylim = y.range, pch = 15,
    col = "gray", cex = 0.5, asp = 1)
  invisible(lapply(roads.list, lines, col = "gray"))
  invisible(lapply(border.list, lines))

  if (!vestry) {
    well <- cholera::pumps
    points(well[, c("x", "y")], pch = 2, cex = 1, col = "blue")
    text(well[, c("x", "y")], label = well$id, pos = 1, col = "blue")
  } else {
    well <- cholera::pumps.vestry
    points(well[, c("x", "y")], pch = 2, cex = 1, col = "blue")
    text(well[, c("x", "y")], label = well$id, pos = 1, col = "blue")
  }

  if (add.landmarks) addLandmarks()
}
