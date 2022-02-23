#' Compute latitude and longitude for unique road segment endpoints (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @export

latlongRoads <- function(path, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  partition.rds <- partitionRoads()

  coords <- parallel::mclapply(seq_along(partition.rds), function(i) {
    ids <- partition.rds[[i]]
    nm <- names(partition.rds[i])
    tif <- paste0(path, "roads.", nm, "_modified.tif")
    k <- length(ids)
    geo.coords <- latlongCoordinates(tif, k, path)
    nom.coords <- cholera::roads[cholera::roads$id %in% ids, ]

    # rotate nominal coords to approximate georeferenced coords
    nom.rotate <- lapply(ids, rotatePoint)
    nom.rotate <- do.call(rbind, nom.rotate)
    nom.rotate.scale <- data.frame(id = ids, scale(nom.rotate))

    geo.scale <- data.frame(id = geo.coords$id,
      scale(geo.coords[, c("lon", "lat")]))

    alters <- geo.scale
    names(alters)[-1] <- c("x", "y")

    ## translation diagnostic (classification error) ##

    # There should be a one-to-one correspondence between native and
    # georeferenced points.

    # translation[duplicated(translation$geo.id), ]
    # #      id geo.id
    # # 42   99      2
    # # 201 579    205
    #
    # translation[duplicated(translation$id), ]
    # # [1] id     geo.id
    # # <0 rows> (or 0-length row.names)
    #
    # subset(translation, geo.id == 2)
    # #    id geo.id
    # # 2  26      2
    # # 42 99      2
    #
    # subset(translation, geo.id == 205)
    # #      id geo.id
    # # 197 566    205
    # # 201 579    205

    # vars <- c("x", "y")
    # plot(nom.rotate.scale[, vars], pch = 16, cex = 0.5)
    # points(alters[, vars], pch = 16, cex = 0.5, col = "red")
    # points(nom.rotate.scale[nom.rotate.scale$id == 566, vars], col = "green")
    # points(nom.rotate.scale[nom.rotate.scale$id == 579, vars], col = "green")
    # points(nom.rotate.scale[nom.rotate.scale$id ==  26, vars], col = "green")
    # points(nom.rotate.scale[nom.rotate.scale$id ==  99, vars], col = "green")

    # data1 <- rbind(nom.rotate.scale[nom.rotate.scale$id %in% c(566, 579), ],
    #                alters[alters$id %in% c(205, 211), ])
    # plot(data1[, vars], pch = NA)
    # text(data1[1:2, vars], labels = data1$id[1:2])
    # text(data1[3:4, vars], labels = data1$id[3:4], col = "red")

    # data2 <- rbind(nom.rotate.scale[nom.rotate.scale$id %in% c(26, 99), ],
    #                alters[alters$id %in% 1:2, ])
    # plot(data2[, vars], pch = NA)
    # text(data2[1:2, vars], labels = data2$id[1:2])
    # text(data2[3:4, vars], labels = data2$id[3:4], col = "red")

    translation <- lapply(ids, function(id) {
      ego <- nom.rotate.scale[nom.rotate.scale$id == id, c("x", "y")]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, c("x", "y")]))
      }, numeric(1L))
      data.frame(id = id, geo.id = alters$id[which.min(d)])
    })

    translation <- do.call(rbind, translation)

    # translation manual fixes
    translation[translation$id == 26, "geo.id"] <- 1L
    translation[translation$id == 579, "geo.id"] <- 211L

    geo.coords <- merge(geo.coords, translation, by.x = "id", by.y = "geo.id")
    names(geo.coords)[c(1, length(names(geo.coords)))] <- c("geo.id", "id")
    merge(nom.coords, geo.coords[, -1], by = "id")
  }, mc.cores = cores)

  coords <- do.call(rbind, coords)
  coords <- coords[, c(names(cholera::roads), c("lon", "lat"))]
  coords$id2 <- paste0(coords$x, "-", coords$y)

  rd0 <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rd0 <- rd0[duplicated(rd0[, c("x", "y")]), ]
  rd0$id2 <- paste0(rd0$x, "-", rd0$y)
  rd0 <- merge(rd0, coords[, c("lon", "lat", "id2")], all.x = TRUE, by = "id2")

  out <- rbind(coords, rd0)
  out$id2 <- NULL
  row.names(out) <- NULL
  out <- out[order(out$id), ]
  out
}
