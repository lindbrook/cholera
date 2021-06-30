#' Compute latitude and longitude (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @export

latitudeLongitudeAddress <- function(path, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  idx <- pointIndex(nrow(cholera::fatalities.address))

  coords <- parallel::mclapply(seq_len(nrow(idx)), function(i) {
    pre <- paste0(path, "address.0")
    post <- "_modified.tif"
    tif <- paste0(pre, i, post)
    k <- idx[i, "stop"] - idx[i, "start"] + 1
    latlongCoordinates(tif, k)
  }, mc.cores = cores)

  coords <- lapply(seq_along(coords), function(i) {
    tmp <- coords[[i]]
    geo.id <- idx[i, "start"]:idx[i, "stop"]
    tmp$id <- geo.id
    tmp
  })

  address.groups <- lapply(seq_along(idx$start), function(i) {
    sel <- idx[i, "start"]:idx[i, "stop"]
    cholera::fatalities.address[sel, ]
  })

  address.rotate.scale <- parallel::mclapply(address.groups, function(x) {
    tmp <- lapply(x$anchor, function(y) {
      rotatePoint(y, dataset = "fatalities.address")
    })
    tmp <- do.call(rbind, tmp)
    data.frame(id = x$anchor, scale(tmp))
  }, mc.cores = cores)

  coords.scale <- lapply(coords, function(x){
    data.frame(id = x$id, scale(x[, c("long", "lat")]))
  })

  match.points <- parallel::mclapply(seq_along(coords.scale), function(i) {
    addr <- address.rotate.scale[[i]]
    alters <- coords.scale[[i]]
    names(alters)[-1] <- c("x", "y")

    out <- lapply(addr$id, function(id) {
      ego <- addr[addr$id == id, c("x", "y")]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, c("x", "y")]))
      }, numeric(1L))
      data.frame(id = id, geo.id = alters$id[which.min(d)])
    })

    do.call(rbind, out)
  }, mc.cores = cores)

  match.points <- do.call(rbind, match.points)
  coords <- do.call(rbind, coords)

  out <- merge(cholera::fatalities.address, match.points, by.x = "anchor",
    by.y = "id")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$anchor), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  out
}
