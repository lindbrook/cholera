#' Compute Georeferenced Latitude and Longitude (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @export

latlongPumps <- function(path, vestry = FALSE) {
  if (vestry) {
    tif <- "pump.vestry_modified.tif"
    k <- 14
    dat <- cholera::pumps.vestry
    dataset <- "pumps.vestry"
  } else {
    tif <- "pump_modified.tif"
    k <- 13
    dat <- cholera::pumps
    dataset <- "pumps"
  }

  coords <- latlongCoordinatesB(paste0(path, tif), k)
  coords.scale <- data.frame(id = coords$id, scale(coords[, c("long", "lat")]))

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
