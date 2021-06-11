#' Compute Georeferenced Latitude and Longitude (prototype).
#'
#' @param tif Character. Georeferenced QGIS TIFF file.
#' @param cutpoint Numeric. Cutpoint for hierarchical cluster analysis.
#' @param vestry Logical.
#' @export

latlongPumps <- function(tif, cutpoint = 0.001, vestry = FALSE) {
  u.data <- pointsFromGeoTIFF(tif)
  names(u.data)[3] <- "modified"
  sel <- u.data$modified != 0 & u.data$modified != 255
  obs.x.min <- min(u.data[sel, "x"])
  obs.x.max <- max(u.data[sel, "x"])
  obs.y.min <- min(u.data[sel, "y"])
  obs.y.max <- max(u.data[sel, "y"])
  obs.x <- u.data$x >= obs.x.min & u.data$x <= obs.x.max
  obs.y <- u.data$y >= obs.y.min & u.data$y <= obs.y.max
  filter <- obs.x & obs.y & u.data$modified != 255
  f.data <- u.data[filter, c("x", "y")]

  distances <- stats::dist(f.data)
  tree <- stats::hclust(distances)
  clusters <- stats::cutree(tree, h = cutpoint)
  cluster.id <- unique(clusters)
  pts <- lapply(cluster.id, function(grp) names(clusters[clusters == grp]))

  coords <- lapply(seq_along(pts), function(i) {
    rect.data <- f.data[pts[[i]], ]
    y.val <- sort(unique(rect.data$y), decreasing = FALSE) # quadrant II adj
    x.val <- sort(unique(rect.data$x))
    row.element.ct <- as.data.frame(table(rect.data$y),
      stringsAsFactors = FALSE)
    col.element.ct <- as.data.frame(table(rect.data$x),
      stringsAsFactors = FALSE)
    row.id <- kmeansRectanlge(row.element.ct$Freq)
    col.id <- kmeansRectanlge(col.element.ct$Freq)
    rect.x <- x.val[range(col.id)]
    rect.y <- y.val[range(row.id)]
    longitude <- mean(rect.x)
    latitude <- mean(rect.y)
    data.frame(id = i, long = longitude, lat = latitude)
  })

  out <- do.call(rbind, coords)
  if (vestry) out$pump <- c(2, 1, 4:3, 5:6, 14, 7, 11:8, 13, 12)
  else out$pump <- c(2, 1, 4:3, 5:7, 11:8, 13, 12)
  out <- out[order(out$pump), ]
  row.names(out) <- NULL
  out
}

#' Extract points from GeoTiff (prototype).
#'
#' @param x Object. GeoTIFF.
#' @export

pointsFromGeoTIFF <- function(x) {
  ras <- raster::raster(x)
  pts <- raster::rasterToPoints(ras)
  data.frame(pts)
}

#' Compute rectangle vertices (prototype).
#'
#' @param x Object. Points/pixel count.
#' @export

kmeansRectanlge <- function(x) {
  if (length(unique(x)) > 1) {
    km <- stats::kmeans(x, 2)
    km.df <- data.frame(ct = x, cluster = km$cluster)
    sel <- km.df[km.df$cluster == which.max(km$centers), ]
    as.numeric(row.names(sel))
  } else seq_along(x)
}
