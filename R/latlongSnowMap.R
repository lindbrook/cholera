#' Plot John Snow's cholera map.
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param asp Numeric. Aspect ratio.
#' @export

latlongSnowMap <- function(path, asp = 1.65) {
  rd <- latitudeLongitudeRoads(path)
  plot(rd[, c("long", "lat")], pch = NA, asp = asp)
  roads.list <- split(rd[, c("long", "lat")], rd$street)
  invisible(lapply(roads.list, lines))
}
