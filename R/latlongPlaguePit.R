#' Compute Georeferenced Latitude and Longitude (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @noRd
#' @note This documents the computation of the latlong version of the plague.pit data frame.

latlongPlaguePit <- function(path) {
  # reset (delete) lon-lat for recomputation  
  if (any(c("lon", "lat") %in% names(cholera::plague.pit))) {
    vars <- !names(cholera::plague.pit) %in% c("lon", "lat")
    dat <- cholera::plague.pit[, vars]
  } else dat <- cholera::plague.pit

  k <- nrow(dat)
  tif <- "plague.pit_modified.tif"
  coords <- latlongCoordinates(paste0(path, tif), k, path)
  coords.scale <- data.frame(id = coords$id, scale(coords[, c("lon", "lat")]))

  tmp <- lapply(dat$id, function(id) rotatePoint(id, dataset = "plague.pit"))
  tmp <- do.call(rbind, tmp)
  pit.rotate.scale <- data.frame(pit.id = dat$id, scale(tmp))

  alters <- coords.scale
  names(alters)[-1] <- c("x", "y")

  match.points <- lapply(pit.rotate.scale$pit.id, function(id) {
    ego <- pit.rotate.scale[pit.rotate.scale$pit.id == id, c("x", "y")]
    d <- vapply(seq_len(nrow(alters)), function(i) {
      stats::dist(rbind(ego, alters[i, c("x", "y")]))
    }, numeric(1L))
    data.frame(pit.id = id, geo.id = alters$id[which.min(d)])
  })

  match.points <- do.call(rbind, match.points)
  out <- merge(dat, match.points, by.x = "id", by.y = "pit.id")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$id), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  out
}

#' Create PDF of plague pit.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @noRd

plaguePitPDF <- function(path, pch = 46, cex = 1) {
  file.nm <- "plague.pit"
  pre <- paste0(file.nm, ".")
  post <- "pdf"
  dat <- cholera::plague.pit
  
  rng <- mapRange()
  grDevices::pdf(file = paste0(path, pre, post))
  plot(dat$x, dat$y, pch = NA, xaxt = "n", yaxt = "n",
    xlab = NA, ylab = NA, bty = "n", xlim = rng$x, ylim = rng$y, asp = 1)
  points(dat[, c("x", "y")], pch = pch, cex = cex)
  grDevices::dev.off()
}