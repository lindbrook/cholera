#' Compute Georeferenced Latitude and Longitude (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param orthogonal Logical. Use orthogonal projection coordinates.
#' @export
#' @note This documents the computation of the latlong version of the landmarks data frame.

latlongLandmarks <- function(path, orthogonal = FALSE) {
  vars <- !names(cholera::pumps.vestry) %in% c("lon", "lat")
  dat <- cholera::landmarks[, vars]
  if (orthogonal) {
    tif <- "ortho.landmark_modified.tif"
  } else {
    tif <- "landmark_modified.tif"
  }

  k <- nrow(dat)
  coords <- latlongCoordinates(paste0(path, tif), k, path)
  coords.scale <- data.frame(id = coords$id, scale(coords[, c("lon", "lat")]))

  tmp <- lapply(dat$case, function(id) rotatePoint(id, dataset = "landmarks"))
  tmp <- do.call(rbind, tmp)
  rotate.scale <- data.frame(id = dat$case, scale(tmp))

  alters <- coords.scale
  names(alters)[-1] <- c("x", "y")

  match.points <- lapply(rotate.scale$id, function(id) {
    ego <- rotate.scale[rotate.scale$id == id, c("x", "y")]
    d <- vapply(seq_len(nrow(alters)), function(i) {
      stats::dist(rbind(ego, alters[i, c("x", "y")]))
    }, numeric(1L))
    data.frame(id = id, geo.id = alters$id[which.min(d)])
  })

  match.points <- do.call(rbind, match.points)
  out <- merge(dat, match.points, by.x = "case", by.y = "id")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$case), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  out
}

# usethis::use_data(landmarks, overwrite = TRUE)

#' Create PDFs of landmarks
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param orthogonal Logical. Use orthogonal projection coordinates.
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @noRd

landmarksPDF <- function(path, orthogonal = FALSE, pch = 15, cex = 0.2) {
  if (orthogonal) {
    coords <- c("x.proj", "y.proj")
    file.nm <- "ortho.landmark"
  } else {
    coords <- c("x", "y")
    file.nm <- "landmark"
  }
  pre <- paste0(file.nm, ".")
  post <- "pdf"
  dat <- cholera::landmarkData()
  rng <- mapRange()
  grDevices::pdf(file = paste0(path, pre, post))
  plot(dat[, coords], pch = NA, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
    bty = "n", xlim = rng$x, ylim = rng$y, asp = 1)
  points(dat[, coords], pch = pch, cex = cex)
  grDevices::dev.off()
}
