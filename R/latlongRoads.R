#' Compute latitude and longitude for unique road segment endpoints (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @export
#' @note This documents the computation of the lat-long version of the roads data frame.

latlongRoads <- function(path, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  partition.rds <- partitionRoads()

  coords <- parallel::mclapply(seq_along(partition.rds), function(i) {
    ids <- partition.rds[[i]]
    nm <- names(partition.rds[i])
    tif <- paste0(path, "roads.", nm, "_modified.tif")
    k <- length(ids)
    geo.coords <- latlongCoordinates(tif, k, path)

    # post-fix
    vars <- !names(cholera::roads) %in% c("lon", "lat")
    nom.coords <- cholera::roads[cholera::roads$id %in% ids, vars]

    # rotate nominal coords to approximate georeferenced coords
    nom.rotate <- lapply(ids, rotatePoint)
    nom.rotate <- do.call(rbind, nom.rotate)
    nom.rotate.scale <- data.frame(id = ids, scale(nom.rotate))

    geo.scale <- data.frame(id = geo.coords$id,
      scale(geo.coords[, c("lon", "lat")]))

    alters <- geo.scale
    names(alters)[-1] <- c("x", "y")

    ## classification error diagnostic ##
    # vars <- c("x", "y")
    # plot(nom.rotate.scale[, vars], pch = 16, cex = 0.5)
    # points(alters[, vars], pch = 16, cex = 0.5, col = "red")
    # summary(duplicated(translation$geo.id))

    translation <- do.call(rbind, lapply(ids, function(id) {
      ego <- nom.rotate.scale[nom.rotate.scale$id == id, c("x", "y")]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, c("x", "y")]))
      }, numeric(1L))
      data.frame(id = id, geo.id = alters$id[which.min(d)])
    }))

    geo.coords <- merge(geo.coords, translation, by.x = "id", by.y = "geo.id")
    names(geo.coords)[c(1, length(names(geo.coords)))] <- c("geo.id", "id")
    merge(nom.coords, geo.coords[, -1], by = "id")
  }, mc.cores = cores)

  coords <- do.call(rbind, coords)
  # coords <- coords[, c(names(cholera::roads), c("lon", "lat"))]
  coords$id2 <- paste0(coords$x, "-", coords$y)

  # post-fix
  vars <- !names(cholera::roads) %in% c("lon", "lat")
  rd0 <- cholera::roads[cholera::roads$name != "Map Frame", vars]

  rd0 <- rd0[duplicated(rd0[, c("x", "y")]), ]
  rd0$id2 <- paste0(rd0$x, "-", rd0$y)
  rd0 <- merge(rd0, coords[, c("lon", "lat", "id2")], all.x = TRUE, by = "id2")

  out <- rbind(coords, rd0)
  out$id2 <- NULL
  row.names(out) <- NULL
  out <- out[order(out$id), ]
  out
}
