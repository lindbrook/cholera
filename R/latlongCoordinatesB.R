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

  # filter out frame "shadow"

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

    max.col <- max(row.element.ct$Freq)
    max.row <- max(col.element.ct$Freq)
    row.id <- kmeansRectanlge(row.element.ct$Freq)
    col.id <- kmeansRectanlge(col.element.ct$Freq)

    if (length(row.id) == max.row & length(col.id) != max.col) {
      col.delta <- max.col - col.element.ct$Freq
      col.candidate <- which(col.delta %in% min(col.delta[col.delta != 0]))
      col.adjacent <- any(col.candidate - col.id == 1)
      if (col.adjacent) col.id <- c(col.id, col.candidate)

      # flat rectangle check
      if (length(unique(col.element.ct$Freq)) == 1) {
        v.dist <- vapply(col.element.ct$Var1, function(x) {
          stats::dist(rect.data[rect.data$x == paste(x), "y"])
        }, numeric(1L))
        v.dist.table <- table(v.dist)
        obs.dist <- as.numeric(names(v.dist.table))
        col.id <- which(v.dist == paste(obs.dist[which.min(obs.dist)]))
      }
    } else if (length(row.id) != max.row & length(col.id) == max.col) {
      row.delta <- max.row - row.element.ct$Freq
      row.candidate <- which(row.delta %in% min(row.delta[row.delta != 0]))
      row.adjacent <- any(row.candidate - row.id == 1)
      if (row.adjacent) row.id <- c(row.id, row.candidate)
    } else if (length(row.id) != max.row & length(col.id) != max.col) {
      row.delta <- max.row - row.element.ct$Freq
      row.candidate <- which(row.delta %in% min(row.delta[row.delta != 0]))
      row.adjacent <- any(row.candidate - row.id == 1)
      if (row.adjacent) row.id <- c(row.id, row.candidate)
      col.delta <- max.col - col.element.ct$Freq
      col.candidate <- which(col.delta %in% min(col.delta[col.delta != 0]))
      col.adjacent <- any(col.candidate - col.id == 1)
      if (col.adjacent) col.id <- c(col.id, col.candidate)

      # complete flat rectangle with 2 rows and n cols
      flat.rect.candidate <- row.element.ct$Freq == max.col
      two.max.rows <- sum(flat.rect.candidate) == 2
      if (two.max.rows) {
        new.row.id <- which(flat.rect.candidate)
        max.row.ys <- row.element.ct[new.row.id, "Var1"]
        max.row.ys.in.rect <- paste(rect.data$y) %in% paste(max.row.ys)
        complete.rect <- sum(max.row.ys.in.rect) == length(max.row.ys) * max.col
        if (complete.rect) {
          row.id <- new.row.id # identical(new.row.id, row.id)
          col.id <- which(row.element.ct$Var %in% max.row.ys)
        }
      }
    }

    lon <- mean(col.element.ct[col.id, "Var1"])
    lat <- mean(row.element.ct[row.id, "Var1"])
    data.frame(id = i, lon = lon, lat = lat)
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
