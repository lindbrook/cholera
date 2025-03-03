#' Compute latitude and longitude for unique road segment endpoints (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @noRd
#' @note This documents the computation of the lat-long version of the roads data frame.

latlongRoads <- function(path, multi.core = FALSE) {
  cores <- multiCore(multi.core)

  # match road IDs used to create the georeferenced TIFs
  partition.rds <- partitionRoads()

  coords <- parallel::mclapply(seq_along(partition.rds), function(i) {
    ids <- partition.rds[[i]]
    nm <- names(partition.rds[i])
    tif <- paste0(path, "roads.", nm, "_modified.tif")
    k <- length(ids)
    geo.coords <- latlongCoordinates(tif, k, path)

    # reset (delete) lon-lat for recomputation 
    vars <- !names(cholera::roads) %in% c("lon", "lat")
    nominal.coords <- cholera::roads[cholera::roads$id %in% ids, vars]

    # rotate nominal coords to approximate and "match" georeferenced coords
    nominal.rotate <- lapply(ids, rotatePoint)
    nominal.rotate <- do.call(rbind, nominal.rotate)
    nominal.rotate.scale <- data.frame(id = ids, scale(nominal.rotate))

    geo.scale <- data.frame(id = geo.coords$id,
      scale(geo.coords[, c("lon", "lat")]))

    alters <- geo.scale
    names(alters)[-1] <- c("x", "y")

    translation <- do.call(rbind, lapply(ids, function(id) {
      ego <- nominal.rotate.scale[nominal.rotate.scale$id == id, c("x", "y")]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, c("x", "y")]))
      }, numeric(1L))
      data.frame(id = id, geo.id = alters$id[which.min(d)])
    }))

    ## classification error diagnostic ##
    # vars <- c("x", "y")
    # plot(nominal.rotate.scale[, vars], pch = 16, cex = 0.5)
    # points(alters[, vars], pch = 16, cex = 0.5, col = "red")
    # dups <- duplicated(translation$geo.id)
    # if (any(dups)) {
    #   title(main = paste("err =", summary(dups)[2]))
    # } else {
    #   title(main = "OK")
    # }

    geo.coords <- merge(geo.coords, translation, by.x = "id", by.y = "geo.id")
    names(geo.coords)[c(1, length(names(geo.coords)))] <- c("geo.id", "id")
    merge(nominal.coords, geo.coords[, -1], by = "id")
  }, mc.cores = cores)

  coords <- do.call(rbind, coords)
  # coords <- coords[, c(names(cholera::roads), c("lon", "lat"))]
  coords$id2 <- paste0(coords$x, "_&_", coords$y)

  # post-fix
  vars <- !names(cholera::roads) %in% c("lon", "lat")
  rd0 <- cholera::roads[cholera::roads$name != "Map Frame", vars]

  rd0 <- rd0[duplicated(rd0[, c("x", "y")]), ]
  rd0$id2 <- paste0(rd0$x, "_&_", rd0$y)
  rd0 <- merge(rd0, coords[, c("lon", "lat", "id2")], all.x = TRUE, by = "id2")

  out <- rbind(coords, rd0)
  out$id2 <- NULL
  row.names(out) <- NULL
  out <- out[order(out$id), ]
  out
}
