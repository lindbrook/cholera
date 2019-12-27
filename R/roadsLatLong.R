#' Compute latitude and longitude from geo TIFF (prototype).
#'
#' Georeferenced TIFF file from QGIS3.10.
#' @param geotiff Object. GeoTIFF file.
#' @param cutpoint Numeric. Cutpoint for agglomerative hierarchical cluster analysis.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @note This function documents the code that generates "roads2" latitude and longitude estimates.
#' @export

roadsLatLong <- function(geotiff, cutpoint = 0.000006, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  vars1 <- c("x", "y")
  vars2 <- c("longitude", "latitude")
  dat <- cholera::pointsFromGeoTIFF(geotiff)

  # clean TIFF #
  f.data <- dat[dat$road.segments4_modified != 0 &
                dat$road.segments4_modified != 255, ]
  f.data <- f.data[order(f.data$x, f.data$y), vars1]

  # cluster analysis #
  distances <- stats::dist(f.data)
  tree <- stats::hclust(distances)
  clusters <- stats::cutree(tree, h = cutpoint)
  cluster.id <- unique(clusters)

  # estimate center of a point's points (pixels?)#
  pts <- lapply(cluster.id, function(grp) names(clusters[clusters == grp]))
  centroids <- lapply(pts, function(p) data.frame(t(colMeans(f.data[p, ]))))
  centroids <- do.call(rbind, centroids)

  # unique road segment endpoints #
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rd$point.id <- paste0(rd$x, "-", rd$y)
  rd.duplicate <- rd[duplicated(rd$point.id), ]
  rd.unique <- rd[!duplicated(rd$point.id), ]

  # rotate native coordinates to "match" georeferenced coordinates #
  coordinates <- parallel::mclapply(rd.unique$id, cholera::rotatePoint,
    mc.cores = cores)
  coordinates <- do.call(rbind, coordinates)
  coordinates <- data.frame(id = rd.unique$id, coordinates)

  # normalize and rescale native and georeferenced coordinates #
  coordinates.scaled <- data.frame(id = coordinates$id,
    scale(coordinates[, vars1]))

  centroids.scaled <- data.frame(scale(centroids[, vars1]))

  # compute distances between native and and georeferenced coordinates #
  alters <- centroids.scaled[, vars1]

  translation <- parallel::mclapply(coordinates.scaled$id, function(id) {
    ego <- coordinates.scaled[coordinates.scaled$id == id, vars1]
    d <- vapply(seq_len(nrow(alters)), function(i) {
      stats::dist(rbind(ego, alters[i, ]))
    }, numeric(1L))
    data.frame(id = id, centroid = which.min(d), stringsAsFactors = FALSE)
  }, mc.cores = cores)

  translation <- do.call(rbind, translation)

  # data "fixes": parallel shifts rather than proximity #
  translation[translation$id == 1101, "centroid"] <- 9
  translation[translation$id == 102, "centroid"] <- 101
  translation[translation$id == 113, "centroid"] <- 225
  translation[translation$id == 116, "centroid"] <- 304

  # assembly #
  rd.unique[, vars2] <- NA

  for (x in rd.unique$id) {
    c.id <- translation[translation$id == x, "centroid"]
    rd.unique[rd.unique$id == x, vars2] <- centroids[c.id, ]
  }

  rd.duplicate[, vars2] <- NA

  for (x in unique(rd.duplicate$point.id)) {
    c2 <- rd.unique[rd.unique$point.id == x, vars2]
    rd.duplicate[rd.duplicate$point.id %in% x, vars2] <- c2
  }

  roads2 <- rbind(rd.unique, rd.duplicate)
  roads2[order(roads2$id), ]
}

## plot checks

# plot(roads2[, vars1], pch = NA, asp = 1)
# roads.list <- split(roads2[, vars1], roads2$street)
# invisible(lapply(roads.list, lines))

# plot(roads2[, vars2], pch = NA)
# roads.list <- split(roads2[, vars2], roads2$street)
# invisible(lapply(roads.list, lines))

## add data to package

# roads2 <- roadsLatLong()
# usethis::use_data(roads2)
# usethis::use_data(roads2, overwrite = TRUE)
