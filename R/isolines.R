#' Plot isochrone and isodistance regions (prototype)
#'
#' @param pump.select Numeric.
#' @param post Numeric.
#' @param post.type Character. "distance or "time".
#' @param palette Character. RColorBrewer palette.
#' @export

isoLines <- function(pump.select = 7, post = 50, post.type = "distance",
  palette = "Spectral") {

  pump.dist <- cholera::sim.walking.distance[[paste(pump.select)]]
  cutpoint <- seq(0, 350, post)
  mypalette <- RColorBrewer::brewer.pal(length(cutpoint), palette)
  snowMap(add.cases = FALSE, add.roads = FALSE, add.pumps = FALSE)

  invisible(lapply(seq_along(cutpoint), function(i) {
    if (post.type == "distance") {
      sel <- pump.dist$distance > cutpoint[i] &
             pump.dist$distance <= cutpoint[i] + post
    } else if (post.type == "time") {
      sel <- pump.dist$time * 60 > cutpoint[i] &
             pump.dist$time * 60 <= cutpoint[i] + post
    }

    neighborhood.cases <- pump.dist[sel, "case"]
    points(cholera::regular.cases[neighborhood.cases, ], pch = 16,
      col = mypalette[i], cex = 1)
  }))

  addRoads()
  addPump()
}
