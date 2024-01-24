#' Plot isochrone and isodistance regions (prototype)
#'
#' @param post Numeric. Distance or time increment.
#' @param post.type Character. "distance" or "time".
#' @param palette Character.
#' @param alpha.level Numeric. Alpha level transparency
#' @importFrom viridisLite plasma
#' @noRd

isoLines <- function(post = 50, post.type = "distance", palette = "plasma",
  alpha.level = 1/2) {

  if (post.type == "distance") isobands <- seq(0, 600, post)
  if (post.type == "time") isobands <- seq(0, 500, post)

  pump.dist <- cholera::sim.walking.distance

  if (palette == "plasma") {
    mypalette <- viridisLite::plasma(length(isobands), alpha = alpha.level,
      begin = 0, end = 1, direction = -1)
  }

  snowMap(add.cases = FALSE, add.roads = FALSE, add.pumps = FALSE)

  invisible(lapply(seq_along(isobands), function(i) {
    sel <- pump.dist$distance > isobands[i] &
           pump.dist$distance <= isobands[i] + post
    neighborhood.cases <- pump.dist[sel, "case"]
    points(cholera::regular.cases[neighborhood.cases, ], pch = 16,
      col = mypalette[i], cex = 1)
  }))

  addRoads()
  addPump()
}
