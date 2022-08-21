#' Compute Georeferenced Latitude and Longitude of vertices of Voronoi polygons.
#'
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @export
#' @examples
#' snowMap(latlong = TRUE)
#' cells <- latlongVoronoiC()
#' invisible(lapply(cells, function(x) polygon(x[, c("lon", "lat")])))

latlongVoronoiC <- function(pump.select = NULL, vestry = FALSE) {
  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  topleft <- data.frame(lon = min(cholera::roads$lon),
                        lat = max(cholera::roads$lat))
  bottomright <- data.frame(lon = max(cholera::roads$lon),
                            lat = min(cholera::roads$lat))
  topright <- data.frame(lon = max(cholera::roads$lon),
                         lat = max(cholera::roads$lat))

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  if (!is.null(pump.select)) {
    if (is.numeric(pump.select) == FALSE) {
      stop("pump.select must be numeric.", call. = FALSE)
    }
    p.count <- nrow(pump.data)
    p.ID <- seq_len(p.count)
    if (any(abs(pump.select) %in% p.ID == FALSE)) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ', p.count,
        call. = FALSE)
    }
    msg1 <- 'If specified,'
    msg2 <- "'pump.select' must include at least 2 different pumps."
    if (length(unique(p.ID[pump.select])) < 2) {
      stop(paste(msg1, msg2), call. = FALSE)
    }
  }

  # compute geodesic distance from origin to pump and decompose result to
  # horizontal (East-West) and vertical (North-South) components.
  pump.meters <- do.call(rbind, lapply(pump.data$id, function(p) {
    pmp <- pump.data[pump.data$id == p, c("lon", "lat")]
    x.proj <- c(pmp$lon, origin$lat)
    y.proj <- c(origin$lon, pmp$lat)
    m.lon <- geosphere::distGeo(y.proj, pmp)
    m.lat <- geosphere::distGeo(x.proj, pmp)
    data.frame(pump = p, x = m.lon, y = m.lat)
  }))

  # Voronoi cells

  height <- geosphere::distGeo(origin, topleft)
  width <- geosphere::distGeo(origin, bottomright)
  bounding.box <- c(0, width, 0, height)

  if (is.null(pump.select)) {
    cells <- voronoiPolygons(pump.meters[, c("x", "y")], rw = bounding.box)
  } else {
    cells <- voronoiPolygons(pump.meters[pump.select, c("x", "y")],
      rw = bounding.box)
  }

  # cells DF

  cells.df <- do.call(rbind, cells)
  cells.lat <- sort(unique(cells.df$y), decreasing = TRUE) # unique latitudes
  tmp <- row.names(cells.df)
  ids <- do.call(rbind, strsplit(tmp, "[.]"))
  cells.df$cell <- as.numeric(ids[, 2])
  cells.df$vertex <- as.numeric(ids[, 3])
  row.names(cells.df) <- NULL

  est.lonlat <- meterLatLong(cells.df, origin, topleft, bottomright)
  split(est.lonlat, est.lonlat$cell)
}

#' Convert meters-North to latitude.
#'
#' @param cells.df Object. Data frame of coordinates of Voronoi cells
#' @param origin Object. Bottom left corner of map.
#' @param topleft Object. Top left corner of map.
#' @param delta Numeric. Increment between simulated values.
#' @noRd

meterLatitude <- function(cells.df, origin, topleft, delta = 0.000025) {
  lat <- seq(origin$lat, topleft$lat, delta)

  meters.north <- vapply(lat, function(y) {
    geosphere::distGeo(origin, cbind(origin$lon, y))
  }, numeric(1L))

  loess.lat <- stats::loess(lat ~ meters.north,
    control = stats::loess.control(surface = "direct"))

  y.unique <- sort(unique(cells.df$y))

  est.lat <- vapply(y.unique, function(m) {
    stats::predict(loess.lat, newdata = data.frame(meters.north = m))
  }, numeric(1L))

  data.frame(m = y.unique, lat = est.lat)
}

#' Convert meters-East to longitude.
#'
#' @param est.latitude Object. Estimated latitudes from meters-North.
#' @param cells.df Object. Data frame of coordinates of Voronoi cells
#' @param origin Object. Bottom left corner of map.
#' @param topleft Object. Top left corner of map.
#' @param bottomright Object. Bottom right corner of map.
#' @param delta Numeric. Increment between simulated values.
#' @noRd

meterLatLong <- function(cells.df, origin, topleft, bottomright,
  delta = 0.000025) {

  est.lat <- meterLatitude(cells.df, origin, topleft)

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

  y.unique <- sort(unique(cells.df$y))

  # estimate longitudes, append estimated latitudes
  est.lonlat <- do.call(rbind, lapply(seq_along(y.unique), function(i) {
    dat <- cells.df[cells.df$y == y.unique[i], ]
    loess.fit <- loess.lon[[i]]
    dat$lon <- vapply(dat$x, function(x) {
      stats::predict(loess.fit, newdata = data.frame(m = x))
    }, numeric(1L))
    dat$lat <- est.lat[est.lat$m == y.unique[i], "lat"]
    dat
  }))

  est.lonlat[order(est.lonlat$cell, est.lonlat$vertex), ]
}
