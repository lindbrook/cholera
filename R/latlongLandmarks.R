#' Compute Georeferenced Latitude and Longitude (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @export

latlongLandmarks <- function(path) {
  tif <- "landmarks_modified.tif"
  dat <- cholera::landmarks
  k <- nrow(dat)
  coords <- latlongCoordinates(paste0(path, tif), k, path)
  coords.scale <- data.frame(id = coords$id, scale(coords[, c("lon", "lat")]))

  tmp <- lapply(dat$case, function(id) rotatePoint(id, dataset = "landmarks"))
  tmp <- do.call(rbind, tmp)
  landmark.rotate.scale <- data.frame(id = dat$case, scale(tmp))

  alters <- coords.scale
  names(alters)[-1] <- c("x", "y")

  match.points <- lapply(landmark.rotate.scale$id, function(id) {
    ego <- landmark.rotate.scale[landmark.rotate.scale$id == id, c("x", "y")]
    d <- vapply(seq_len(nrow(alters)), function(i) {
      stats::dist(rbind(ego, alters[i, c("x", "y")]))
    }, numeric(1L))
    data.frame(landmark.id = id, geo.id = alters$id[which.min(d)])
  })

  match.points <- do.call(rbind, match.points)
  out <- merge(dat, match.points, by.x = "case", by.y = "landmark.id")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$case), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  sel <- !names(out) %in% c("x.proj", "y.proj",  "ortho.dist")
  out[, sel]
}

#' Create PDFs of landmark addreses.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @export

landmarksPDF <- function(path, pch = 15, cex = 0.2) {
  dat <- cholera::landmarks
  file.nm <- "landmarks."
  post <- "pdf"
  rng <- mapRange()
  grDevices::pdf(file = paste0(path, file.nm, post))
  plot(dat$x, dat$y, pch = NA, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
    bty = "n", xlim = rng$x, ylim = rng$y)
  points(dat[, c("x", "y")], pch = pch, cex = cex)
  grDevices::dev.off()
}
