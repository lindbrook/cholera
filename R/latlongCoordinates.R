#' Compute Georeferenced Latitude and Longitude (prototype).
#'
#' @param tif Character. Georeferenced QGIS TIFF file.
# #' @param cutpoint Numeric. Cutpoint for hierarchical cluster analysis.
#' @param k Numeric. Number of clusters, k, to identify.
#' @param path Character. e.g., "~/Documents/Data/".
#' @importFrom sp point.in.polygon
#' @noRd

latlongCoordinates <- function(tif, k, path) {
  u.data <- pointsFromGeoTIFF(tif)
  names(u.data)[3] <- "modified"

  map.data <- u.data[u.data$modified != 255, c("x", "y")]

  # use rectangular polygon filter
  rectangle.filter <- cholera::rectangle.filter

  filter.sel <- sp::point.in.polygon(map.data$x, map.data$y,
    rectangle.filter$x, rectangle.filter$y)

  f.data <- map.data[filter.sel != 0, ]

  distances <- stats::dist(f.data)
  tree <- stats::hclust(distances)
  clusters <- stats::cutree(tree, k = k)
  cluster.id <- unique(clusters)
  pts <- lapply(cluster.id, function(grp) names(clusters[clusters == grp]))

  coords <- lapply(seq_along(pts), function(i) {
    rect.data <- f.data[pts[[i]], ]
    tbl <- t(table(rect.data))

    if (all(dim(tbl) > 1)) {
      row.order <- order(as.numeric(rownames(tbl)), decreasing = TRUE)
      col.order <- order(as.numeric(colnames(tbl)))

      row.chk <- identical(row.order, seq_len(nrow(tbl)))
      col.chk <- identical(col.order, seq_len(ncol(tbl)))

      if (row.chk & !col.chk) {
        tbl <- tbl[, col.order]
      } else if (!row.chk & col.chk) {
        tbl <- tbl[row.order, ]
      } else if (!row.chk & !col.chk) {
        tbl <- tbl[row.chk, col.order]
      }

      # top to bottom point orientation
      row.element.ct <- as.data.frame(table(rect.data$y),
        stringsAsFactors = FALSE)
      row.element.ct$Var1 <- as.numeric(row.element.ct$Var1)
      sel <- order(row.element.ct$Var1, decreasing = TRUE)
      row.element.ct <- row.element.ct[sel, ]
      row.names(row.element.ct) <- NULL

      # left to right point orientation
      col.element.ct <- as.data.frame(table(rect.data$x),
        stringsAsFactors = FALSE)
      col.element.ct$Var1 <- as.numeric(col.element.ct$Var1)
      sel <- order(col.element.ct$Var1)
      col.element.ct <- col.element.ct[sel, ]
      row.names(col.element.ct) <- NULL

      max.row <- max(row.element.ct$Freq)
      max.col <- max(col.element.ct$Freq)

      row.idx <- lapply(max.col:nrow(row.element.ct), function(endpt) {
        delta <- max.col - 1
        seq(endpt - delta, endpt)
      })

      col.idx <- lapply(max.row:nrow(col.element.ct), function(endpt) {
        delta <- max.row - 1
        seq(endpt - delta, endpt)
      })

      idxB <- expand.grid(seq_along(row.idx), seq_along(col.idx))

      point.ct <- vapply(seq_len(nrow(idxB)), function(i) {
        r.sel <- row.idx[[idxB[i, "Var1"]]]
        c.sel <- col.idx[[idxB[i, "Var2"]]]
        sum(tbl[r.sel, c.sel])
      }, integer(1L))

      sel <- which.max(point.ct)
      row.id <- row.idx[[idxB[sel, "Var1"]]]
      col.id <- col.idx[[idxB[sel, "Var2"]]]

      lon <- mean(col.element.ct[col.id, "Var1"])
      lat <- mean(row.element.ct[row.id, "Var1"])
      data.frame(id = i, lon = lon, lat = lat)

    } else {
      lonlat <- colMeans(rect.data)
      data.frame(id = i, lon = lonlat["x"], lat = lonlat["y"])
    }
  })

  out <- do.call(rbind, coords)
  row.names(out) <- NULL
  out
}
