#' Convert case IDs to numeric.
#'
#' @param x Object. table() object.
#' @noRd

caseNumber <- function(x) as.numeric(names(x))

#' Decompose geo-cartesian distances into horizontal and vertical components.
#'
#' Compute geo-cartesian distance from origin to pump and translate into
#` horizontal (East-West) and vertical (North-South) components.
#' @param dat Object. Data.
#' @param case.address Logical. Use fatalities.address$anchor
#' @importFrom geosphere distGeo
#' @noRd

geoCartesian <- function(dat = cholera::pumps, case.address = FALSE) {
  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  if (case.address) {
    dat <- cholera::fatalities.address
    names(dat)[names(dat) == "anchor"] <- "id"
  }
  do.call(rbind, lapply(dat$id, function(x) {
    tmp <- dat[dat$id == x, c("lon", "lat")]
    x.proj <- c(tmp$lon, origin$lat)
    y.proj <- c(origin$lon, tmp$lat)
    m.lon <- geosphere::distGeo(y.proj, tmp)
    m.lat <- geosphere::distGeo(x.proj, tmp)
    data.frame(id = x, x = m.lon, y = m.lat)
  }))
}

#' Compute geo-cartesian coordinates from longitude/latitude.
#'
#' Compute geo-cartesian distance from origin to pump and translate into
#` horizontal (East-West) and vertical (North-South) components.
#' @param case.data Object. Data.
#' @importFrom geosphere distGeo
#' @noRd

geoCartesianCoord <- function(case.data) {
  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  x.proj <- c(case.data$lon, origin$lat)
  y.proj <- c(origin$lon, case.data$lat)
  m.lon <- geosphere::distGeo(y.proj, case.data)
  m.lat <- geosphere::distGeo(x.proj, case.data)
  data.frame(x = m.lon, y = m.lat)
}

#' Compute rectangle vertices.
#'
#' @param x Object. Points/pixel count.
#' @noRd

kmeansRectangle <- function(x) {
  if (length(unique(x)) > 1) {
    km <- stats::kmeans(x, 2)
    km.df <- data.frame(ct = x, cluster = km$cluster)
    sel <- km.df[km.df$cluster == which.max(km$centers), ]
    as.integer(row.names(sel))
  } else seq_along(x)
}

#' Convert meters-North to latitude.
#'
#' @param coords Object. Data frame of coordinates.
#' @param origin Object. Bottom left corner of map.
#' @param topleft Object. Top left corner of map.
#' @param delta Numeric. Increment between simulated values.
#' @importFrom geosphere distGeo
#' @noRd

meterLatitude <- function(coords, origin, topleft, delta = 0.000025) {
  lat <- seq(origin$lat, topleft$lat, delta)

  meters.north <- vapply(lat, function(y) {
    geosphere::distGeo(origin, cbind(origin$lon, y))
  }, numeric(1L))

  loess.lat <- stats::loess(lat ~ meters.north,
    control = stats::loess.control(surface = "direct"))

  y.unique <- sort(unique(coords$y))

  est.lat <- vapply(y.unique, function(m) {
    stats::predict(loess.lat, newdata = data.frame(meters.north = m))
  }, numeric(1L))

  data.frame(m = y.unique, lat = est.lat)
}

#' Convert meters-East and meters-North to longitude and latitude.
#'
#' @param coords Object. Data frame of coordinates.
#' @param delta Numeric. Increment between simulated values.
#' @importFrom geosphere distGeo
#' @noRd

meterLatLong <- function(coords, delta = 0.000025) {
  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  topleft <- data.frame(lon = min(cholera::roads$lon),
                        lat = max(cholera::roads$lat))
  bottomright <- data.frame(lon = max(cholera::roads$lon),
                            lat = min(cholera::roads$lat))

  est.lat <- meterLatitude(coords, origin, topleft)

  # uniformly spaced points along x-axis (longitude)
  lon <- seq(origin$lon, bottomright$lon, delta)

  # a set of horizontal distances (East-West) for each estimated latitude
  meters.east <- lapply(est.lat$lat, function(y) {
    y.axis.origin <- cbind(origin$lon, y)
    vapply(lon, function(x) {
      geosphere::distGeo(y.axis.origin, cbind(x, y))
    }, numeric(1L))
  })

  loess.lon <- lapply(meters.east, function(m) {
    dat <- data.frame(lon = lon, m)
    stats::loess(lon ~ m, data = dat,
      control = stats::loess.control(surface = "direct"))
  })

  y.unique <- sort(unique(coords$y))

  # estimate longitudes, append estimated latitudes
  out <- lapply(seq_along(y.unique), function(i) {
    if (nrow(coords) > 1) dat <- coords[coords$y == y.unique[i], ]
    else dat <- coords
    loess.fit <- loess.lon[[i]]
    dat$lon <- vapply(dat$x, function(x) {
      stats::predict(loess.fit, newdata = data.frame(m = x))
    }, numeric(1L))
    dat$lat <- est.lat[est.lat$m == y.unique[i], "lat"]
    dat
  })

  if (length(out) == 1) {
    out <- out[[1]]
  } else {
    out <- do.call(rbind, out)
  }
  out
}

#' Extract points from GeoTiff.
#'
#' @param x Object. GeoTIFF.
#' @importFrom terra as.data.frame
#' @importFrom terra rast
#' @noRd

pointsFromGeoTIFF <- function(x) {
  ras <- terra::rast(x)
  terra::as.data.frame(ras, xy = TRUE)
}

#' Index of subsets.
#'
#' @param max.ct Integer. Upper count of observations.
#' @param bin.size Integer. bin size size of subgroups.
#' @noRd

pointIndex <- function(max.ct = 321, bin.size = 50) {
  alpha <- seq(1, max.ct, bin.size)
  omega <- c(alpha[-1] - 1, max.ct)
  data.frame(start = alpha, stop = omega)
}

#' Estimate of georeferencing rotation (radians).
#'
#' QGIS georeferencing realigns map: left side approximately parallel to y-axis.
#' @param id1 Numeric. Road segment endpoint ID. Margaret Street.
#' @param id2 Numeric. Road segment endpoint ID. Phoenix Yard.
#' @note The two default points are the first two observations on the top left.
#' @noRd

referenceRadian <- function(id1 = 66, id2 = 171) {
  rd <- cholera::roads
  # rd[order(rd$x, rd$y), ] # first two observations on top left side
  x1 <- rd[rd$id == id1, "x"]
  y1 <- rd[rd$id == id1, "y"]
  x2 <- rd[rd$id == id2, "x"]
  y2 <- rd[rd$id == id2, "y"]
  atan((x1 - x2) / (y2 - y1))
}

#' Compute radians between observed point and centroid of 'roads'.
#'
#' @param points.data Object. Data frame of centroid and point.
#' @noRd

radians <- function(points.data) {
  ols <- stats::lm(y ~ x, data = points.data)
  segment.slope <- stats::coef(ols)[2]
  atan(segment.slope)
}

#' Rotate points (prototype).
#'
#' @param id Numeric. Road segment endpoint ID.
#' @param dataset Character. "roads", "fatalities", "fatalities.address", "pumps", "pumps.vestry", "ortho.proj", "ortho.proj.pump", or "ortho.proj.pump.vestry".
#' @noRd

rotatePoint <- function(id = 1, dataset = "roads") {
  rd <- cholera::roads
  center <- data.frame(x = mean(range(rd$x)), y = mean(range(rd$y)))
  oldvars <- c("x.proj", "y.proj")
  newvars <- c("x", "y")

  if (dataset == "roads") {
    points.data <- rbind(center, rd[rd$id == id, newvars])
  } else if (dataset == "fatalities") {
    sel <- cholera::fatalities$case == id
    points.data <- rbind(center, cholera::fatalities[sel, newvars])
  } else if (dataset == "fatalities.address") {
    sel <- cholera::fatalities.address$anchor == id
    points.data <- rbind(center, cholera::fatalities.address[sel, newvars])
  } else if (dataset == "pumps") {
    sel <- cholera::pumps$id == id
    points.data <- rbind(center, cholera::pumps[sel, newvars])
  } else if (dataset == "pumps.vestry") {
    sel <- cholera::pumps.vestry$id == id
    points.data <- rbind(center, cholera::pumps.vestry[sel, newvars])
  } else if (dataset == "voronoi.polygons") {
    tmp <- do.call(rbind, cholera::voronoi.polygons)
    points.data <- rbind(center, tmp[tmp$vertex == id, newvars])
  } else if (dataset == "voronoi.polygons.vestry") {
    tmp <- do.call(rbind, cholera::voronoi.polygons.vestry)
    points.data <- rbind(center, tmp[tmp$vertex == id, newvars])
  } else if (dataset == "landmarks") {
    sel <- which(cholera::landmarks$case == id)
    points.data <- rbind(center, cholera::landmarks[sel, newvars])
  } else if (dataset == "ortho.proj") {
    sel <- which(cholera::ortho.proj$case == id)
    nm.sel <- names(cholera::ortho.proj) %in% oldvars
    ortho.projB <- cholera::ortho.proj
    names(ortho.projB)[nm.sel] <- newvars
    points.data <- rbind(center, ortho.projB[sel, newvars])
  } else if (dataset == "ortho.proj.pump") {
    sel <- which(cholera::ortho.proj.pump$pump.id == id)
    nm.sel <- names(cholera::ortho.proj.pump) %in% oldvars
    ortho.projB <- cholera::ortho.proj.pump
    names(ortho.projB)[nm.sel] <- newvars
    points.data <- rbind(center, ortho.projB[sel, newvars])
  } else if (dataset == "ortho.proj.pump.vestry") {
    sel <- which(cholera::ortho.proj.pump.vestry$pump.id == id)
    nm.sel <- names(cholera::ortho.proj.pump.vestry) %in% oldvars
    ortho.projB <- cholera::ortho.proj.pump.vestry
    names(ortho.projB)[nm.sel] <- newvars
    points.data <- rbind(center, ortho.projB[sel, newvars])
  } else {
    msg1 <- 'dataset must be "roads", "fatalities", "fatalities.address",'
    msg2 <- '"pumps", "pumps.vestry", "landmarks", "ortho.proj",'
    msg3 <- '"ortho.proj.pump", "ortho.proj.pump.vestry". "voronoi.polygons",'
    msg4 <- 'or "voronoi.polygons.vestry".'
    stop(paste(msg1, msg2, msg3, msg4))
  }

  theta <- radians(points.data)
  h <- stats::dist(points.data)
  theta.delta <- referenceRadian()

  if (points.data$x[1] - points.data$x[2] >= 0) {
    x.prime <- c(center$x - cos(theta - theta.delta) * h)
    y.prime <- c(center$y - sin(theta - theta.delta) * h)
  } else {
    x.prime <- c(center$x + cos(theta - theta.delta) * h)
    y.prime <- c(center$y + sin(theta - theta.delta) * h)
  }

  data.frame(x = x.prime, y = y.prime, row.names = NULL)
}

#' Compute estimated length of 'road.segments' in meters.
#'
#' @param dat Object. R data frame of road segments
#' @param latlong Logical. Data use longitude/latitude or native map coordinates.
#' @return An R data frame.
#' @importFrom geosphere distGeo
#' @noRd

segmentDistance <- function(dat, latlong = FALSE) {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")
  vapply(seq_len(nrow(dat)), function(i) {
    p1 <- dat[i, paste0(vars, 1)]
    p2 <- dat[i, paste0(vars, 2)]
    names(p1) <- vars
    names(p2) <- vars
    if (latlong) {
      geosphere::distGeo(p1, p2)
    } else unitMeter(stats::dist(rbind(p1, p2)))
  }, numeric(1L))
}

#' Create subsetted PDFs (prototype).
#'
#' Reduce over-printing of points.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param dataset Object. A 'cholera' dataset e.g., roads.
#' @noRd

subsetPDF <- function(path, dataset = "fatalities.address") {
  rng <- mapRange()

  if (dataset == "roads") {
    dat <- cholera::roads
    dat$point.id <- paste0(dat$x, "-", dat$y)
    dat <- dat[!duplicated(dat$point.id), ]
    file.nm <- "road"
  } else if (dataset == "fatalities") {
    dat <- cholera::fatalities
    file.nm <- "fatality"
  } else if (dataset == "fatalities.address") {
    dat <- cholera::fatalities.address
    file.nm <- "address"
  } else if (dataset == "pumps") {
    dat <- cholera::pumps
    file.nm <- "pumps"
  } else if (dataset == "pumps.vestry") {
    dat <- cholera::pumps.vestry
    file.nm <- "pumps.vestry"
  } else {
    msg1 <- 'dataset must be "roads", "fatalities", "fatalities.address", '
    msg2 <- '"pumps" or "pumps.vestry".'
    stop(msg1, msg2, call. = FALSE)
  }

  if (dataset %in% c("roads", "fatalities", "fatalities.address")) {
    idx <- pointIndex(nrow(dat))
    num.id <- seq_len(nrow(idx))

    if (any(num.id >= 10)) {
      num.id <- c(paste0("0", num.id[num.id < 10]), num.id[num.id >= 10])
    } else {
      num.id <- paste0("0", num.id)
    }

    invisible(lapply(seq_along(num.id), function(i) {
      pre <- paste0(file.nm, ".")
      post <- ".pdf"
      grDevices::pdf(file = paste0(path, pre, num.id[i], post))
      plot(dat$x, dat$y, pch = NA, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
        xlim = rng$x, ylim = rng$y, bty = "n", asp = 1)
      sel <- idx[i, "start"]:idx[i, "stop"]
      points(dat[sel, c("x", "y")], pch = 15, cex = 0.2)
      grDevices::dev.off()
    }))
  } else {
    pre <- file.nm
    post <- ".01.pdf"
    grDevices::pdf(file = paste0(path, pre, post))
    plot(dat$x, dat$y, pch = NA, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
      xlim = rng$x, ylim = rng$y, bty = "n", asp = 1)
    points(dat[, c("x", "y")], pch = 15, cex = 0.2, asp = 1)
    grDevices::dev.off()
  }
}

#' Angle between road segments.
#'
#' For latitude longitude audit.
#' @param id1 Character. First segment ID.
#' @param id2 Character. Second segment ID.
#' @noRd

segmentTheta <- function(id1 = "297-1", id2 = "290-1") {
  seg <- cholera::road.segments
  coordA <- "x"
  coordB <- "y"

  seg.data <- lapply(c(id1, id2), function(z) {
    dat <- seg[seg$id == z, ]
    data.frame(x = unlist(dat[, paste0(coordA, 1:2)]),
               y = unlist(dat[, paste0(coordB, 1:2)]))
  })

  ols <- lapply(seg.data, function(data) stats::lm(y ~ x, data = data))

  rads <- vapply(ols, function(x) {
    segment.slope <- stats::coef(x)[2]
    atan(segment.slope)
  }, numeric(1L))

  thetas <- rads * 180 / pi

  if (all(sign(thetas) == 1 | sign(thetas) == -1)) {
    180 - abs(thetas[1] - thetas[2])
  } else {
    sum(abs(thetas))
  }
}

#' Standardize data.
#'
#' @param dat Object. Data frame.
#' @param center Numeric. Mean.
#' @param spread Numeric. Standard deviation.
#' @noRd

std <- function(dat, center, spread) (dat - center) / spread

#' Unstandardize data.
#'
#' @param dat Object. Data frame.
#' @param center Numeric. Mean.
#' @param spread Numeric. Standard deviation.
#' @noRd

unstd <- function(x, center, spread) x * spread + center
