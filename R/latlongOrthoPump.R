#' Compute orthogonal projection of pumps (prototype).
#'
#' Projection from fatality address to nearest road segment.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @export

latlongOrthoPump <- function(path, vestry = FALSE) {
  # source("~/Documents/Data IV/cholera/R/latlongAuxilliaryFunctions.R")
  rd <- cholera::roads
  addr <- cholera::fatalities.address

  if (vestry) {
    pre <- "ortho.proj.pump.vestry"
    ortho <- cholera::ortho.proj.pump.vestry
  } else {
    pre <- "ortho.proj.pump"
    ortho <- cholera::ortho.proj.pump
  }

  post <- "_modified.tif"
  tif <- paste0(path, pre, post)
  k <- nrow(ortho)
  coords <- latlongCoordinates(tif, k, path)

  ortho.rotate <- do.call(rbind, lapply(ortho$pump.id, function(x) {
    rotatePoint(x, dataset = pre)
  }))

  ortho.rotate.scale <- data.frame(id = coords$id, scale(ortho.rotate))
  coords.scale <- data.frame(id = coords$id, scale(coords[, c("lon", "lat")]))

  match.points <- do.call(rbind, lapply(seq_len(nrow(coords)), function(i) {
    pmp <- ortho.rotate.scale[i, ]
    alters <- coords.scale
    names(alters)[-1] <- c("x", "y")
    out <- lapply(pmp$id, function(id) {
      ego <- pmp[pmp$id == id, c("x", "y")]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, c("x", "y")]))
      }, numeric(1L))
      data.frame(id = id, geo.id = alters$id[which.min(d)])
    })
    do.call(rbind, out)
  }))

  out <- merge(ortho, match.points, by.x = "pump.id", by.y = "id")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$pump.id), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  out
}
