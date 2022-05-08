#' Compute Georeferenced Latitude and Longitude (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @export

latlongPumps <- function(path, vestry = FALSE) {
  if (vestry) {
    tif <- "pumps.vestry_modified.tif"
    k <- 14
    dat <- cholera::pumps.vestry
    dataset <- "pumps.vestry"
  } else {
    tif <- "pumps_modified.tif"
    k <- 13
    dat <- cholera::pumps
    dataset <- "pumps"
  }

  coords <- latlongCoordinates(paste0(path, tif), k, path)
  coords.scale <- data.frame(id = coords$id, scale(coords[, c("lon", "lat")]))

  tmp <- lapply(dat$id, function(id) rotatePoint(id, dataset = dataset))
  tmp <- do.call(rbind, tmp)
  pumps.rotate.scale <- data.frame(pump.id = dat$id, scale(tmp))

  alters <- coords.scale
  names(alters)[-1] <- c("x", "y")

  match.points <- lapply(pumps.rotate.scale$pump.id, function(id) {
    ego <- pumps.rotate.scale[pumps.rotate.scale$pump.id == id, c("x", "y")]
    d <- vapply(seq_len(nrow(alters)), function(i) {
      stats::dist(rbind(ego, alters[i, c("x", "y")]))
    }, numeric(1L))
    data.frame(pump.id = id, geo.id = alters$id[which.min(d)])
  })

  match.points <- do.call(rbind, match.points)
  out <- merge(dat, match.points, by.x = "id", by.y = "pump.id")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$id), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  out
}

#' Create PDFs of pumps.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @export

pumpsPDF <- function(path, vestry = FALSE, pch = 46, cex = 1) {
  file.nm <- "pump"

  if (vestry) pre <- paste0(file.nm, ".vestry.")
  else pre <- paste0(file.nm, ".")

  post <- "pdf"
  if (vestry) dat <- cholera::pumps.vestry
  else dat <- cholera::pumps

  framework <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rng <- mapRange()

  grDevices::pdf(file = paste0(path, pre, post))
  plot(framework$x, framework$y, pch = NA, xaxt = "n", yaxt = "n",
    xlab = NA, ylab = NA, bty = "n", xlim = rng$x, ylim = rng$y, asp = 1)
  points(dat[, c("x", "y")], pch = pch, cex = cex)
  grDevices::dev.off()
}
