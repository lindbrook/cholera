#' Plot John Snow's cholera map.
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param asp Numeric. Aspect ratio.
#' @export

latlongSnowMap <- function(path, asp = 1.65) {
  rd <- latitudeLongitudeRoads(path)
  frame <- latitudeLongitudeFrame(path)
  fatality <- latitudeLongitudeFatality(path)
  pump <- latlongPumps(path)
  plot(rd[, c("long", "lat")], pch = NA, asp = asp)
  roads.list <- split(rd[, c("long", "lat")], rd$street)
  frame.list <- split(frame[, c("long", "lat")], frame$street)
  invisible(lapply(roads.list, lines, col = "gray"))
  invisible(lapply(frame.list, lines))
  points(fatality[, c("long", "lat")], col = "red", pch = 16, cex = 0.5)
  points(pump[, c("long", "lat")], col = "blue", pch = 24)
  text(pump[, c("long", "lat")], col = "blue", pos = 1, labels = pump$id)
}
