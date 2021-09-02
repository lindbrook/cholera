#' Compute Georeferenced Latitude and Longitude (prototype).
#'
#' @param tif Character. Georeferenced QGIS TIFF file.
# #' @param cutpoint Numeric. Cutpoint for hierarchical cluster analysis.
#' @param k Numeric. Number of clusters, k, to identify.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

latlongCoordinatesB <- function(tif, k, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  u.data <- pointsFromGeoTIFF(tif)
  names(u.data)[3] <- "modified"

  image.data <- u.data[u.data$modified == 255, c("x", "y")]
  image.chull <- grDevices::chull(image.data)
  image.chull <- image.data[image.chull, ]

  map.data <- u.data[u.data$modified != 255, c("x", "y")]

  data.in.chull <- parallel::mclapply(seq_len(nrow(map.data)), function(i) {
    sp::point.in.polygon(map.data[i, ]$x, map.data[i, ]$y, image.chull$x,
      image.chull$y)
  }, mc.cores = cores)

  data.in.chull <- unlist(data.in.chull)

  xdata <- map.data[data.in.chull != 0, ]$x
  ydata <- map.data[data.in.chull != 0, ]$y
  xtable <- table(xdata)
  ytable <- table(ydata)

  xvals <- data.frame(coord = round(as.numeric(names(xtable)), 6),
                      count = c(xtable),
                      row.names = NULL)
  yvals <- data.frame(coord = round(as.numeric(names(ytable)), 6),
                      count = c(ytable),
                      row.names = NULL)

  xcbd <- coordsByDelta(xvals, deltaRank(xvals))
  ycbd <- coordsByDelta(yvals, deltaRank(yvals))
  xcbd <- xcbd[vapply(xcbd, nrow, integer(1L)) == 1]
  ycbd <- ycbd[vapply(ycbd, nrow, integer(1L)) == 1]

  x.rng <- range(xvals$coord)
  y.rng <- range(yvals$coord)

  x.minmax <- vapply(xcbd, function(z) {
    which.min(c(stats::dist(c(z$coordA, x.rng[1])),
                stats::dist(c(z$coordA, x.rng[2]))))
  }, integer(1L))

  y.minmax <- vapply(ycbd, function(z) {
    which.min(c(stats::dist(c(z$coordA, y.rng[1])),
                stats::dist(c(z$coordA, y.rng[2]))))
  }, integer(1L))

  xlist <- c(xcbd[x.minmax == 1][1], xcbd[x.minmax == 2][1])
  ylist <- c(ycbd[y.minmax == 1][1], ycbd[x.minmax == 2][1])

  x.cutpoints <- vapply(xlist, function(z) {
    mean(c(z$coordA, xvals[as.numeric(row.names(z)) + 1, "coord"]))
  }, numeric(1L))

  y.cutpoints <- vapply(ylist, function(z) {
    mean(c(z$coordA, yvals[as.numeric(row.names(z)) + 1, "coord"]))
  }, numeric(1L))

  x.lo <- x.cutpoints[1]
  x.hi <- x.cutpoints[2]
  y.lo <- y.cutpoints[1]
  y.hi <- y.cutpoints[2]

  filter <- map.data$x > x.lo & map.data$x < x.hi &
            map.data$y > y.lo & map.data$y < y.hi

  f.data <- map.data[filter, ]

  distances <- stats::dist(f.data)
  tree <- stats::hclust(distances)
  clusters <- stats::cutree(tree, k = k)
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

  do.call(rbind, coords)
}

deltaRank <- function(vals) {
  idx <- seq_along(vals$coord)[-length(vals$coord)]
  delta <- vapply(idx, function(i) {
    abs(vals$coord[i] - vals$coord[i + 1])
  }, numeric(1L))
  delta.rank <- data.frame(delta = sort(unique(delta)))
  delta.rank$rank <- 1:nrow(delta.rank)
  delta.rank
}

coordsByDelta <- function(vals, delta.rank) {
  idx <- seq_along(vals$coord)[-length(vals$coord)]
  delta <- vapply(idx, function(i) {
    abs(vals$coord[i] - vals$coord[i + 1])
  }, numeric(1L))
  dat <- data.frame(coordA = vals$coord[-length(vals$coord)], delta = delta)
  lapply(rev(delta.rank$delta), function(d) {
    dat[dat$delta == d, ]
  })
}
