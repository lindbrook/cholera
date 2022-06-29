#' Create PDFs of Voronoi Polygon Vertices.
#'
#' For QGIS georeferencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @export

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

  vertices <- cholera::voronoiPolygons(dat[, c("x", "y")],
    rw.data = frame.corners)

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
#' @export
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
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

latlongVoronoi <- function(path, vestry = FALSE, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  frame.corners <- data.frame(x = range(cholera::frame.data$x),
                              y = range(cholera::frame.data$y))

  if (vestry) {
    dat <- cholera::voronoi.polygons.vestry
    pre <- "voronoi.polygon.vestry"
    dataset <- "voronoi.polygons.vestry"
  } else {
    dat <- cholera::voronoi.polygons
    pre <- "voronoi.polygon"
    dataset <- "voronoi.polygons"
  }

  # reset data
  dat <- lapply(dat, function(x) x[, !names(x) %in% c("lon", "lat")])

  id <- seq_along(dat)
  id <- ifelse(id < 10, paste0("0", id), paste(id))
  post <- "_modified.tif"
  tiffs <- paste0(pre, id, post)
  vars <- c("x", "y")

  vs <- parallel::mclapply(seq_along(dat), function(i) {
    vertices <- dat[[i]]
    tif <- tiffs[i]
    k <- nrow(vertices)

    # just scale computed latlong coordinates
    coords <- latlongCoordinates(paste0(path, tif), k, path)
    coords.scale <- data.frame(id = coords$id, scale(coords[, c("lon", "lat")]))

    # rotate and scale nominal coordinates
    tmp <- lapply(vertices$vertex, rotatePoint, dataset = dataset)
    v.rotate <- do.call(rbind, tmp)
    v.rotate.scale <- data.frame(vertex = vertices$vertex, scale(v.rotate))

    alters <- coords.scale
    names(alters)[-1] <- vars

    match.points <- do.call(rbind, lapply(v.rotate.scale$vertex, function(v) {
      ego <- v.rotate.scale[v.rotate.scale$vertex == v, vars]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, vars]))
      }, numeric(1L))
      data.frame(vertex = v, geo.id = alters$id[which.min(d)])
    }))

    out <- merge(vertices, match.points, by = "vertex")
    out <- merge(out, coords, by.x = "geo.id", by.y = "id")
    out <- out[order(out$vertex), ]
    out$geo.id <- NULL
    row.names(out) <- NULL
    out
  }, mc.cores = cores)

  names(vs) <- paste0("p", seq_len(length(dat)))
  vs
}

# voronoi.polygons <- latlongVoronoi(path)
# voronoi.polygons.vestry <- latlongVoronoi(path, vestry = TRUE)
# usethis::use_data(voronoi.polygons, overwrite = TRUE)
# usethis::use_data(voronoi.polygons.vestry, overwrite = TRUE)
