#' Plot John Snow's cholera map.
#'
#' @param path Character. e.g., "~/Documents/Data/".
#' @param stacked Logical. Use stacked fatalities.
#' @param asp Numeric. Aspect ratio.
#' @export

latlongSnowMap <- function(path, stacked = TRUE, asp = 1.65) {
  rd <- latlongRoads(path)
  frame <- latlongFrame(path)
  addr <- latlongAddress(path)
  pump <- latlongPumps(path)
  plot(rd[, c("lon", "lat")], pch = NA, asp = asp)
  roads.list <- split(rd[, c("lon", "lat")], rd$street)
  frame.list <- split(frame[, c("lon", "lat")], frame$street)
  invisible(lapply(roads.list, lines, col = "gray"))
  invisible(lapply(frame.list, lines))
  points(addr[, c("lon", "lat")], col = "red", pch = 16, cex = 0.5)
  if (stacked) {
    fatality <- latlongFatalities(path)
    points(fatality[, c("lon", "lat")], col = "red", pch = 16, cex = 0.5)
  }
  points(pump[, c("lon", "lat")], col = "blue", pch = 24)
  text(pump[, c("lon", "lat")], col = "blue", pos = 1, labels = pump$id)
}
