#' Compute latitude and longitude of non-address fatalities (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @noRd
#' @note This documents the computation of the latlong version of the fatalities data frame.

latlongFatalities <- function(path, multi.core = FALSE) {
  # reset (delete) lon-lat for recomputation
  sel <- !names(cholera::fatalities) %in% c("lon", "lat")
  fatalities.original <- cholera::fatalities[, sel]

  pre <- paste0(path, "fatalities.v")
  post <- "_modified.tif"
  cores <- multiCore(multi.core)

  fatality.partitions <- partitionFatalities()

  # values of k for stats::hclust()
  ks <- vapply(fatality.partitions, length, integer(1L))

  coords <- parallel::mclapply(seq_along(fatality.partitions), function(i) {
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

  fatality.groups <- lapply(fatality.partitions, function(case) {
    fatalities.original[fatalities.original$case %in% case, ]
  })

  fatality.rotate.scale <- parallel::mclapply(fatality.groups, function(x) {
    tmp <- lapply(x$case, function(y) rotatePoint(y, dataset = "fatalities"))
    tmp <- do.call(rbind, tmp)
    data.frame(id = x$case, scale(tmp))
  }, mc.cores = cores)

  coords.scale <- lapply(coords, function(x){
    data.frame(id = x$id, scale(x[, c("lon", "lat")]))
  })

  match.points <- parallel::mclapply(seq_along(coords.scale), function(i) {
    fatal <- fatality.rotate.scale[[i]]
    alters <- coords.scale[[i]]
    names(alters)[-1] <- c("x", "y")
    out <- lapply(fatal$id, function(id) {
      ego <- fatal[fatal$id == id, c("x", "y")]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, c("x", "y")]))
      }, numeric(1L))
      data.frame(id = id, geo.id = alters$id[which.min(d)])
    })
    do.call(rbind, out)
  }, mc.cores = cores)

  match.points <- do.call(rbind, match.points)
  coords <- do.call(rbind, coords)

  sel <- fatalities.original$case %in% unlist(fatality.partitions)
  fatality.data <- fatalities.original[sel, ]

  out <- merge(fatality.data, match.points, by.x = "case", by.y = "id")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$case), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  out
}
