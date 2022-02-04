#' Compute latitude and longitude for orthogonal case projection (address).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @export

latlongOrthoAddress <- function(path, multi.core = TRUE) {
  pre <- paste0(path, "ortho.address")
  post <- "_modified.tif"
  cores <- multiCore(multi.core)

  ortho.partitions <- partitionOrthoAddresses()
  ks <- vapply(ortho.partitions, length, integer(1L))

  coords <- parallel::mclapply(seq_along(ortho.partitions), function(i) {
    tif <- paste0(pre, i, post)
    k <- ks[i]
    latlongCoordinates(tif, k, path)
  }, mc.cores = cores)

  stop <- cumsum(ks)
  start <- c(1, stop[-length(stop)] + 1)
  k.idx <- data.frame(start = start, stop = stop, row.names = NULL)

  coords <- lapply(seq_along(coords), function(i) {
    tmp <- coords[[i]]
    tmp$id <- k.idx[i, "start"]:k.idx[i, "stop"]
    tmp
  })

  address.groups <- lapply(ortho.partitions, function(case) {
    cholera::ortho.proj[cholera::ortho.proj$case %in% case, ]
  })

  address.rotate.scale <- parallel::mclapply(address.groups, function(x) {
    tmp <- lapply(x$case, function(y) rotatePoint(y, dataset = "ortho.proj"))
    tmp <- do.call(rbind, tmp)
    data.frame(id = x$case, scale(tmp))
  }, mc.cores = cores)

  coords.scale <- lapply(coords, function(x){
    data.frame(id = x$id, scale(x[, c("lon", "lat")]))
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

#' Create PDFs of orthogonal projection addresses.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @export

latlongOrthoAddressPDF <- function(path, pch = 46, cex = 1) {
  file.nm <- "ortho.address"
  post <- ".pdf"

  parts <- partitionOrthoAddresses()
  framework <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rng <- mapRange()

  invisible(lapply(seq_along(parts), function(i) {
    dat <- cholera::ortho.proj[cholera::ortho.proj$case %in% parts[[i]], ]
    # all(parts[[i]] %in% cholera::ortho.proj$case)
    grDevices::pdf(file = paste0(path, file.nm, i, post))
    plot(framework$x, framework$y, pch = NA, xaxt = "n", yaxt = "n",
      xlab = NA, ylab = NA, bty = "n", xlim = rng$x, ylim = rng$y)
    points(dat[, c("x.proj", "y.proj")], pch = pch, cex = cex)
    grDevices::dev.off()
  }))
}
