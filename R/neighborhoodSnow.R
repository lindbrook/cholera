#' Plot Snow's Broad Street pump walking neighborhood.
#'
#' Reproduces Snow's graphic annotation in the Vestry Report.
#' @param zoom Logical.
#' @param add.address Logical. Plots the address of cases in the neighborhood, the base case in the stack of fatalities.
#' @param add.landmarks Logical. Include landmarks.
#' @import graphics
#' @export
#' @examples
#' neighborhoodSnow()

neighborhoodSnow <- function(zoom = TRUE, add.address = TRUE,
  add.landmarks = TRUE) {

  roadsB <- cholera::roads[cholera::roads$street %in%
                           cholera::border == FALSE, ]

  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(roadsB[, c("x", "y")], roadsB$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  sel <- cholera::fatalities.unstacked$case %in% cholera::snow.neighborhood

  if (zoom) {
    x.rng <- range(cholera::fatalities.unstacked[sel, "x"])
    y.rng <- range(cholera::fatalities.unstacked[sel, "y"])
  } else {
    x.rng <- range(cholera::roads$x)
    y.rng <- range(cholera::roads$y)
  }

  plot(cholera::fatalities.unstacked[sel, c("x", "y")],
       xlim = x.rng,
       ylim = y.rng,
       pch = NA, asp = 1, cex = 1)

  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(border.list, lines))

  if (add.address) {
    id <- cholera::fatalities.address$anchor.case %in%
      cholera::snow.neighborhood
    if (zoom) {
      points(cholera::fatalities.address[id, c("x", "y")], pch = 16,
        col = "gray")
    } else  {
      points(cholera::fatalities.address[id, c("x", "y")], cex = 0.5)
    }
  }

  for (i in seq_along(cholera::snow.trimmed.segments$road.segment)) {
    segments(cholera::snow.trimmed.segments[i, "x1"],
             cholera::snow.trimmed.segments[i, "y1"],
             cholera::snow.trimmed.segments[i, "x2"],
             cholera::snow.trimmed.segments[i, "y2"],
             lwd = 4, col = "dodgerblue")
  }

  points(cholera::pumps[7, c("x", "y")], pch = 2, cex = 1)
  title(main = "Snow's Broad Street Pump Walking Neighborhood")
  if (add.landmarks) cholera::addLandmarks()
}
