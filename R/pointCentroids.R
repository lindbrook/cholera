#' @param tif Character. TIFF.
#' @noRd

pointCentroids <- function(tif) {
  dat <- pointsFromGeoTIFF(tif)
  f.data <- dat[dat$road.segments4_modified != 0 &
                dat$road.segments4_modified != 255, ]
  f.data <- f.data[order(f.data$x, f.data$y), c("x", "y")]

  distances <- stats::dist(f.data)
  tree <- stats::hclust(distances)
  cutpoint <- 0.000006

  clusters <- stats::cutree(tree, h = cutpoint)
  cluster.id <- unique(clusters)
  pts <- lapply(cluster.id, function(grp) names(clusters[clusters == grp]))

  centroids <- lapply(pts, function(p) {
    data.frame(t(colMeans(f.data[p, ])))
  })

  do.call(rbind, centroids)
}
