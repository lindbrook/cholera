#' Create PDFs of Voronoi Polygon Vertices.
#'
#' For QGIS georeferencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @noRd

voronoiPDF <- function(path, vestry = FALSE, pch = 46, cex = 1) {
  file.nm <- "voronoi.polygon"
  post <- ".pdf"
  frame.corners <- data.frame(x = range(cholera::frame.data$x),
                              y = range(cholera::frame.data$y))

  if (vestry) {
    dat <- cholera::pumps.vestry
    pre <- paste0(file.nm, ".vestry")
  } else {
    dat <- cholera::pumps
    pre <- paste0(file.nm)
  }

  vertices <- voronoiPolygons(dat[, c("x", "y")], rw.data = frame.corners)

  rng <- mapRange()

  invisible(lapply(seq_along(vertices), function(i) {
    dat <- vertices[[i]]
    i <- ifelse(i < 10, paste0("0", i), paste(i))
    grDevices::pdf(file = paste0(path, pre, i, post))
    plot(dat[, c("x", "y")], pch = pch, cex = cex, xaxt = "n", yaxt = "n",
      xlab = NA, ylab = NA, bty = "n", xlim = rng$x, ylim = rng$y, asp = 1)
    grDevices::dev.off()
  }))
}

#' Compute Vertices of Voronoi Polygons.
#'
#' @param vestry Logical.
#' @noRd
#' @note This documents the computation of the voronoi.polygons and voronoi.polygons.vestry lists.

pumpsVoronoiPolygons <- function(vestry = FALSE) {
  if (vestry) {
    vars <- !names(cholera::pumps.vestry) %in% c("lon", "lat") # post-fix
    dat <- cholera::pumps.vestry[, vars]
  } else {
    vars <- !names(cholera::pumps) %in% c("lon", "lat") # post-fix
    dat <- cholera::pumps[, vars]
  }

  frame.corners <- data.frame(x = range(cholera::frame.data$x),
                              y = range(cholera::frame.data$y))

  vars <- c("x", "y")
  vertices <- cholera::voronoiPolygons(dat[, vars], rw.data = frame.corners)

  v.id <- seq_len(nrow(do.call(rbind, vertices)))
  row.ct <- vapply(vertices, nrow, integer(1L))
  end.pt <- cumsum(row.ct)
  start.pt <- c(0, end.pt[-length(end.pt)]) + 1

  vertices <- lapply(seq_along(vertices), function(i) {
    id <- v.id[start.pt[i]:end.pt[i]]
    data.frame(vertex = id, vertices[[i]])
  })

  id <- seq_len(nrow(dat))
  stats::setNames(vertices, paste0("p", id))
}

#' Compute Georeferenced Latitude and Longitude of vertices of Voronoi polygons.
#'
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @noRd
#' @examples
#' snowMap(latlong = TRUE)
#' cells <- cholera:::latlongVoronoiVertices()$cells
#' invisible(lapply(cells, function(x) polygon(x[, c("lon", "lat")])))

latlongVoronoiVertices <- function(pump.select = NULL, vestry = FALSE) {
  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  topleft <- data.frame(lon = min(cholera::roads$lon),
                        lat = max(cholera::roads$lat))
  bottomright <- data.frame(lon = max(cholera::roads$lon),
                            lat = min(cholera::roads$lat))

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
    pump.id <- selectPump(pump.data, pump.select, vestry)
  } else {
    pump.id <- pump.data$id
  }

  # compute geo-cartesian distance from origin to pump and decompose result into
  # horizontal (East-West) and vertical (North-South) components.
  pump.meters <- geoCartesian(pump.data)[pump.id, ]

  # Voronoi cells and Delaunay triangles #

  height <- geosphere::distGeo(origin, topleft)
  width <- geosphere::distGeo(origin, bottomright)
  bounding.box <- c(0, width, 0, height)

  cells <- voronoiPolygons(pump.meters[, c("x", "y")], rw = bounding.box)
  cells.df <- do.call(rbind, cells)
  cells.lat <- sort(unique(cells.df$y), decreasing = TRUE) # unique latitudes
  tmp <- row.names(cells.df)
  ids <- do.call(rbind, strsplit(tmp, "[.]"))
  cells.df$cell <- as.numeric(ids[, 2])
  cells.df$vertex <- as.numeric(ids[, 3])
  row.names(cells.df) <- NULL
  est.lonlat <- meterLatLong(cells.df)
  est.lonlat <- est.lonlat[order(est.lonlat$cell, est.lonlat$vertex), ]
  cells <- split(est.lonlat, est.lonlat$cell)
  names(cells) <- paste0("p", pump.id)

  triangles <- voronoiPolygons(pump.meters[, c("x", "y")], rw = bounding.box,
    type = "triangles")
  triangles.df <- do.call(rbind, triangles)
  triangles.lat <- sort(unique(triangles.df$y), decreasing = TRUE)
  triangles.df$id <- rep(seq_along(triangles), each = 3)
  triangles.df$vertex <- rep(1:3, length(triangles))
  est.lonlat <- meterLatLong(triangles.df)
  est.lonlat <- est.lonlat[order(est.lonlat$id, est.lonlat$vertex), ]
  triangles <- split(est.lonlat, est.lonlat$id)

  list(cells = cells, triangles = triangles)
}

# voronoi.polygons <- cholera:::latlongVoronoiVertices()
# voronoi.polygons.vestry <- cholera:::latlongVoronoiVertices(vestry = TRUE)
# usethis::use_data(voronoi.polygons, overwrite = TRUE)
# usethis::use_data(voronoi.polygons.vestry, overwrite = TRUE)
