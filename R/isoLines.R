#' Plot isochrone and isodistance regions (prototype)
#'
#' @param post Numeric. Distance or time increment.
#' @param post.type Character. "distance or "time".
#' @param palette Character. RColorBrewer palette.
#' @export
#' @note The number of possible bands or bins is determined by the max number of colors in 'RColorBrewer' palettes.

isoLines <- function(post = 50, post.type = "distance", palette = "Spectral") {
  if (palette %in% row.names(RColorBrewer::brewer.pal.info) == FALSE) {
    stop("Invalid palette name. Check RColorBrewer::brewer.pal.info")
  }

  sel <- row.names(RColorBrewer::brewer.pal.info) == palette
  bins <- RColorBrewer::brewer.pal.info[sel, "maxcolors"] - 1

  pump.dist <- cholera::sim.walking.distance
  cutpoint <- seq(0, post * bins, post)
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
