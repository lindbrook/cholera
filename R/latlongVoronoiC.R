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

  # compute geodesic distance from origin to pump and decompose result into
  # horizontal (East-West) and vertical (North-South) components.
  pump.meters <- geodesicMeters(pump.data)

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
  est.lonlat <- est.lonlat[order(est.lonlat$cell, est.lonlat$vertex), ]
  split(est.lonlat, est.lonlat$cell)
}
