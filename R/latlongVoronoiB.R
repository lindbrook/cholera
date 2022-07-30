#' Compute Georeferenced Latitude and Longitude of vertices of Voronoi polygons.
#'
#' @export
#' @examples
#' snowMap(latlong = TRUE)
#' cells <- latlongVoronoiB()
#' invisible(lapply(cells, function(x) polygon(x[, c("lon", "lat")])))

latlongVoronoiB <- function() {
  origin <- matrix(c(min(cholera::roads$lon), min(cholera::roads$lat)),
    nrow = 1, ncol = 2)
  topleft <- matrix(c(min(cholera::roads$lon), max(cholera::roads$lat)),
    nrow = 1, ncol = 2)
  bottomright <- matrix(c(max(cholera::roads$lon), min(cholera::roads$lat)),
    nrow = 1, ncol = 2)
  topright <- matrix(c(max(cholera::roads$lon), max(cholera::roads$lat)),
    nrow = 1, ncol = 2)

  pump.meters <- do.call(rbind, lapply(cholera::pumps$id, function(p) {
    pmp <- as.matrix(cholera::pumps[cholera::pumps$id == p, c("lon", "lat")])
    x.proj <- matrix(c(pmp[, "lon"], origin[, 2]), nrow = 1, ncol = 2)
    y.proj <- matrix(c(origin[, 1], pmp[, "lat"]), nrow = 1, ncol = 2)
    m.lon <- sp::spDistsN1(y.proj, pmp, longlat = TRUE) * 1000L
    m.lat <- sp::spDistsN1(x.proj, pmp, longlat = TRUE) * 1000L
    data.frame(pump = p, x = m.lon, y = m.lat)
  }))

  # Voronoi cells

  height <- sp::spDistsN1(origin, topleft, longlat = TRUE) * 1000L
  width <- sp::spDistsN1(origin, bottomright, longlat = TRUE) * 1000L
  bounding.box <- c(0, width, 0, height)

  cells <- voronoiPolygons(pump.meters[, c("x", "y")], rw = bounding.box)

  # cells DF

  cells.df <- do.call(rbind, cells)
  cells.lat <- sort(unique(cells.df$y), decreasing = TRUE) # unique latitudes
  tmp <- row.names(cells.df)
  ids <- do.call(rbind, strsplit(tmp, "[.]"))
  cells.df$cell <- as.numeric(ids[, 2])
  cells.df$vertex <- as.numeric(ids[, 3])
  row.names(cells.df) <- NULL

  ## cell vertices estimate latitudes (y) ##

  # uniformly spaced points along y-axis (latitude)
  lat.val <- seq(origin[, 2], topleft[, 2], 0.000025)

  meter.latitudes <- vapply(lat.val, function(y) {
    sp::spDistsN1(origin, cbind(origin[, 1], y), longlat = TRUE) * 1000L
  }, numeric(1L))

  lat <- lat.val
  m <- meter.latitudes

  # vertical distances are uniform between lines of latitues
  ols.lat <- stats::lm(lat ~ m)

  y.unique <- sort(unique(cells.df$y))

  est.lat <- vapply(y.unique, function(m) {
    stats::predict(ols.lat, newdata = data.frame(m = m))
  }, numeric(1L))

  est.latitudes <- data.frame(m = y.unique, lat = est.lat)

  ## cell vertices estimate longitudes (x) ##

  # unique estimated vertices latitudes
  lat.val.est <- est.latitudes$lat

  # uniformly spaced points along x-axis (longitude)
  lon.val <- seq(origin[, 1], bottomright[, 1], 0.000025)

  # horizontal distances along x-axis decrease with increasing latitude.
  meter.longitudes <- lapply(lat.val.est, function(y) {
    y.axis.origin <- cbind(origin[, 1], y)
    vapply(lon.val, function(x) {
      sp::spDistsN1(y.axis.origin, cbind(x, y), longlat = TRUE) * 1000L
    }, numeric(1L))
  })

  # use stats::loess as function to account for changing, nonlinear horiz.
  # distanaces
  loess.lon <- lapply(meter.longitudes, function(m) {
    dat <- data.frame(lon = lon.val, m)
    stats::loess(lon ~ m, data = dat,
      control = stats::loess.control(surface = "direct"))
  })

  # estimate longitudes, append estimated latitudes
  est.lonlat <- do.call(rbind, lapply(seq_along(y.unique), function(i) {
    dat <- cells.df[cells.df$y == y.unique[i], ]
    loess.fit <- loess.lon[[i]]
    dat$lon <- vapply(dat$x, function(x) {
      stats::predict(loess.fit, newdata = data.frame(m = x))
    }, numeric(1L))
    dat$lat <- est.latitudes[est.latitudes$m == y.unique[i], "lat"]
    dat
  }))

  est.lonlat <- est.lonlat[order(est.lonlat$cell, est.lonlat$vertex), ]
  split(est.lonlat, est.lonlat$cell)
}
