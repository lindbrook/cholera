#' Compute latitude and longitude from geo TIFF (in progress).
#'
#' Georeferenced TIFF file from QGIS3.10.
#' @param geotiff Object. GeoTIFF file.
#' @param dataset Object. data set or layer.
#' @param cutpoint Numeric. Cutpoint for agglomerative hierarchical cluster analysis.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @note This function documents the code that generates "roads2" latitude and longitude estimates.
#' @export

latitudeLongitude <- function(geotiff, dataset = "roads", cutpoint = 0.000006,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)
  vars1 <- c("x", "y")
  vars2 <- c("longitude", "latitude")
  dat <- pointsFromGeoTIFF(geotiff)

  # clean TIFF #

  if (dataset == "roads") {
    f.data <- dat[dat$road.segments4_modified != 0 &
                  dat$road.segments4_modified != 255, ]

  } else if (dataset == "fatalities") {
    f.data <- dat[dat$fatalities_modified != 0 &
                  dat$fatalities_modified != 255, ]

  } else if (dataset == "fatalities.address") {
    f.data <- dat[dat$fatalities.address_modified != 0 &
                  dat$fatalities.address_modified != 255, ]

  } else if (dataset == "pumps") {
    f.data <- dat[dat$pumps_modified != 0 & dat$pumps_modified != 255, ]

  } else if (dataset == "pumps.vestry") {
    f.data <- dat[dat$pumps.vestry_modified != 0 &
                  dat$pumps.vestry_modified != 255, ]

  } else {
    msg1 <- 'dataset must be "roads", "fatalities", "fatalities.address",'
    msg2 <- '"pumps", or "pumps.vestry".'
    stop(paste(msg1, msg2))
  }

  f.data <- f.data[order(f.data$x, f.data$y), vars1]

  # cluster analysis #
  distances <- stats::dist(f.data)
  tree <- stats::hclust(distances)
  clusters <- stats::cutree(tree, h = cutpoint)
  cluster.id <- unique(clusters)

  # estimate center of a point's points (pixels?)
  pts <- lapply(cluster.id, function(grp) names(clusters[clusters == grp]))
  centroids <- lapply(pts, function(p) data.frame(t(colMeans(f.data[p, ]))))
  centroids <- do.call(rbind, centroids)


  # rotate native coordinates to "match" georeferenced coordinates #
  if (dataset == "roads") {
    # unique road segment endpoints #
    rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
    rd$point.id <- paste0(rd$x, "-", rd$y)
    rd.duplicate <- rd[duplicated(rd$point.id), ]
    rd.unique <- rd[!duplicated(rd$point.id), ]

    coordinates <- parallel::mclapply(rd.unique$id, rotatePoint,
      mc.cores = cores)

  } else if (dataset == "fatalities") {
    coordinates <- parallel::mclapply(cholera::fatalities$case, function(x) {
      rotatePoint(x, point.type = "fatalities")
    }, mc.cores = cores)

  } else if (dataset == "fatalities.address") {
    id <- cholera::fatalities.address$anchor
    coordinates <- parallel::mclapply(id, function(x) {
      rotatePoint(x, point.type = "fatalities.address")
    }, mc.cores = cores)

  } else if (dataset == "pumps") {
    coordinates <- parallel::mclapply(cholera::pumps$id, function(x) {
      rotatePoint(x, point.type = "pumps")
    }, mc.cores = cores)

  } else if (dataset == "pumps.vestry") {
    coordinates <- parallel::mclapply(cholera::pumps.vestry$id, function(x) {
      rotatePoint(x, point.type = "pumps.vestry")
    }, mc.cores = cores)
  }

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

  # data "fixes": shifts rather than proximity #

  if (dataset == "roads") {
    missing.centroids <- sort(setdiff(seq_len(nrow(centroids)),
      unique(translation$centroid)))

    sel <- duplicated(translation$centroid)
    duplicate.centroids <- translation$centroid[sel]

    id <- vapply(duplicate.centroids, function(x) {
      dat <- translation[translation$centroid == x, ]

      sel <- coordinates.scaled $id %in% dat$id
      obs.data <- rbind(coordinates.scaled[sel, vars1],
                        centroids.scaled[unique(dat$centroid), vars1])

      d <- vapply(missing.centroids, function(y) {
        c.data <- centroids.scaled[y, vars1]
        sum(sqrt((c.data$x - obs.data$x)^2 + (c.data$y - obs.data$y)^2))
      }, numeric(1L))

      which.min(d)
    }, integer(1L))

    translation.duplicates <- lapply(duplicate.centroids, function(x) {
      dat <- translation[translation$centroid == x, ]
    })

    ## audit plots ##
    
    # invisible(lapply(seq_along(translation.duplicates), function(i) {
    #   dat <- translation.duplicates[[i]]
    #   if (i < length(translation.duplicates)) {
    #     auditTranslationPlot(dat$id[1], dat$id[2], dat$centroid[1],
    #       missing.centroids[id][i], coordinates.scaled, centroids.scaled)
    #   } else {
    #     auditTranslationPlot(dat$id[1], dat$id[2], dat$centroid[1],
    #       missing.centroids[id][i], coordinates.scaled, centroids.scaled,
    #       pos.coord = 2, pos.cent = 4)
    #   }
    # }))

    translation[translation$id == 102, "centroid"] <- 101
    translation[translation$id == 113, "centroid"] <- 225
    translation[translation$id == 116, "centroid"] <- 304
    translation[translation$id == 1101, "centroid"] <- 9

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
    roads2 <- roads2[order(roads2$id), ]

    # usethis::use_data(roads2)

  } else if (dataset == "fatalities") {
    # setdiff(fatalities$case, unique(translation$centroid))
    # 495

    # translation$centroid[duplicated((translation$centroid))]
    # 500

    # subset(translation, centroid == 500)
    #      id centroid           d
    # 86   86      500 0.007725832
    # 125 125      500 0.013241356

    # auditTranslationPlot(86, 125, 495, 500)
    translation[translation$id == 86, "centroid"] <- 495

    fatalities[, c("x2", "y2")] <- NA

    for (case in fatalities$case) {
      c.id <- translation[translation$id == case, "centroid"]
      fatalities[fatalities$case == case, c("x2", "y2")] <- centroids[c.id, ]
    }

    fatalities2 <- fatalities
    # usethis::use_data(fatalities2)
    # usethis::use_data(roads2, overwrite = TRUE)


  } else if (dataset == "fatalities.address") {
    # setdiff(fatalities.address$anchor, unique(translation$centroid))
    # setdiff(fatalities.address$anchor, fatalities.address$anchor[translation$centroid])

    vars2 <- c("longitude", "latitude")

    fatalities.address[, vars2] <- NA

    for (id in fatalities.address$anchor) {
      c.id <- translation[translation$id == id, "centroid"]
      fatalities.address[fatalities.address$anchor == id, vars2] <- centroids[c.id, ]
    }

    fatalities.address2 <- fatalities.address
    # usethis::use_data(fatalities.address2)
    # usethis::use_data(fatalities.address2, overwrite = TRUE)



  } else if (dataset == "pumps") {
    # setdiff(pumps$id, unique(translation$centroid))
    # translation$centroid[duplicated((translation$centroid))]

    vars2 <- c("longitude", "latitude")

    pumps[, vars2] <- NA

    for (id in pumps$id) {
      c.id <- translation[translation$id == id, "centroid"]
      pumps[pumps$id == id, vars2] <- centroids[c.id, ]
    }

  } else if (dataset == "pumps.vestry") {
    # setdiff(pumps.vestry$id, unique(translation$centroid))
    # translation$centroid[duplicated((translation$centroid))]

    vars2 <- c("longitude", "latitude")

    pumps.vestry[, vars2] <- NA

    for (id in pumps.vestry$id) {
      c.id <- translation[translation$id == id, "centroid"]
      pumps.vestry[pumps.vestry$id == id, vars2] <- centroids[c.id, ]
    }

    pumps.vestry2 <- pumps.vestry
    # usethis::use_data(pumps.vestry2)
    # usethis::use_data(pumps.vestry2, overwrite = TRUE)

  }

}

#' Extract points from GeoTiff (prototype).
#'
#' @param x Object. GeoTIFF.
#' @noRd

pointsFromGeoTIFF <- function(x) {
  ras <- raster::raster(x)
  pts <- raster::rasterToPoints(ras)
  data.frame(pts)
}

#' Rotate points (prototype).
#'
#' @param id Numeric. Road segment endpoint ID.
#' @param dataset Character. "roads", "fatalities", "fatalities.address", "pumps", or "pumps.vestry".
#' @param unique.coords Logical. Use unique coordinates.
#' @noRd

rotatePoint <- function(id = 1, dataset = "roads", unique.coords = TRUE) {
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rd <- rd[order(rd$x, rd$y), ]

  if (unique.coords) {
    rd$point.id <- paste0(rd$x, "-", rd$y)
    rd <- rd[!duplicated(rd$point.id), ]
  }

  center <- data.frame(x = mean(range(rd$x)), y = mean(range(rd$y)))

  if (dataset == "roads") {
    points.data <- rbind(center, rd[rd$id == id, c("x", "y")])
  } else if (dataset == "fatalities") {
    sel <- cholera::fatalities$case == id
    points.data <- rbind(center, cholera::fatalities[sel, c("x", "y")])
  } else if (dataset == "fatalities.address") {
    sel <- cholera::fatalities.address$anchor == id
    points.data <- rbind(center, cholera::fatalities.address[sel, c("x", "y")])
  } else if (dataset == "pumps") {
    sel <- cholera::pumps$id == id
    points.data <- rbind(center, cholera::pumps[sel, c("x", "y")])
  } else if (dataset == "pumps.vestry") {
    sel <- cholera::pumps.vestry$id == id
    points.data <- rbind(center, cholera::pumps.vestry[sel, c("x", "y")])
  } else {
    msg1 <- 'dataset must be "roads", "fatalities", "fatalities.address",'
    msg2 <- '"pumps", or "pumps.vestry".'
    stop(paste(msg1, msg2))
  }

  theta <- theta(points.data)
  h <- stats::dist(points.data)
  theta.delta <- referenceRadians()

  if (points.data$x[1] - points.data$x[2] >= 0) {
    x.prime <- c(center$x - cos(theta - theta.delta) * h)
    y.prime <- c(center$y - sin(theta - theta.delta) * h)
  } else {
    x.prime <- c(center$x + cos(theta - theta.delta) * h)
    y.prime <- c(center$y + sin(theta - theta.delta) * h)
  }

  data.frame(x = x.prime, y = y.prime, row.names = NULL)
}

#' Estimate rotation of georeferencing (radians).
#'
#' QGIS georeferencing realigns map: left side approximately parallel to y-axis
#' @param id1 Numeric. Road segment endpoint ID. Margaret Street.
#' @param id2 Numeric. Road segment endpoint ID. Phoenix Yard.
#' @note These two points are the first two observations on the top left side.
#' @noRd

referenceRadians <- function(id1 = 66, id2 = 171) {
  rd <- cholera::roads
  # rd[order(rd$x, rd$y), ] # first two observations on top left side
  x1 <- rd[rd$id == id1, "x"]
  y1 <- rd[rd$id == id1, "y"]
  x2 <- rd[rd$id == id2, "x"]
  y2 <- rd[rd$id == id2, "y"]
  atan((x1 - x2) / (y2 - y1))
}

theta <- function(points.data) {
  ols <- stats::lm(y ~ x, data = points.data)
  segment.slope <- stats::coef(ols)[2]
  atan(segment.slope)
}

auditTranslationPlot <- function(coord1, coord2, cent1, cent2,
  coordinates.scaled, centroids.scaled, pos.coord = 1, pos.cent = 3) {

  coords <- coordinates.scaled[coordinates.scaled$id %in% c(coord1, coord2), ]
  a <- coords[coords$id %in% c(coord1, coord2), c("x", "y")]
  b <- centroids.scaled[c(cent1, cent2), ]
  plot(rbind(a, b), pch = NA, asp = 1)
  points(a, pch = 0)
  text(a, labels = coords$id, pos = pos.coord, cex = 0.75)
  points(b, col = "red")
  text(b, labels = row.names(b), pos = pos.cent, cex = 0.75, col = "red")
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
