#' Adds Snow's graphical annotation of the Broad Street pump walking neighborhood.
#'
#' @param latlong Logical.
#' @param type Character. Type of annotation plot: ""area.points", "area.polygons", or "roads".
#' @param non.snow.cases Logical. Plot anchor cases outside Snow neighborhood.
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param polygon.type Character. "border" or "solid".
#' @param polygon.col Character.
#' @param polygon.lwd Numeric.
#' @import graphics
#' @export
#' @examples
#' \dontrun{
#' plot(neighborhoodVoronoi())
#' addNeighborhoodSnow()
#' }

addNeighborhoodSnow <- function(latlong = FALSE, type = "area.polygons",
  non.snow.cases = FALSE, alpha.level = 1/3, polygon.type = "solid",
  polygon.col = NULL, polygon.lwd = NULL) {

  if (!type %in% c("area.points", "area.polygons", "roads")) {
    stop('type must be area.points", "area.polygons", or "roads".',
      call. = FALSE)
  }

  snow <- neighborhoodSnow(latlong = latlong)

  plot(snow, type = type, non.snow.cases = non.snow.cases,
    alpha.level = alpha.level, polygon.type = polygon.type,
    polygon.col = polygon.col, polygon.lwd = polygon.lwd, add = TRUE)
}
