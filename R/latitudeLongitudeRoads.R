#' Compute latitude and longitude for unique road segment endpoints (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @export

latitudeLongitudeRoads <- function(path, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  dat <- cholera::roads[cholera::roads$name != "Map Frame", ]
  dat$point.id <- paste0(dat$x, "-", dat$y)
  intersections <- table(dat$point.id)
  dat <- dat[!duplicated(dat$point.id), ]

  pts <- list(one = cholera::rd.sample$one,
              two = names(intersections[intersections == 2]),
              three = cholera::rd.sample$three,
              four = names(intersections[intersections == 4]))

  idx1 <- pointIndex(length(cholera::rd.sample$one), 25)
  idx3 <- pointIndex(length(cholera::rd.sample$three), 25)

  k <- list(one = idx1$stop - idx1$start + 1,
            two = length(intersections[intersections == 2]),
            three = idx3$stop - idx3$start + 1,
            four = length(intersections[intersections == 4]))

  p1 <- lapply(seq_along(idx1$start), function(i) {
    pts$one[idx1[i, "start"]:idx1[i, "stop"]]
  })

  p3 <- lapply(seq_along(idx3$start), function(i) {
    pts$three[idx3[i, "start"]:idx3[i, "stop"]]
  })

  post <- "_modified.tif"
  pre.single <- paste0("road.", paste0("0", c(2, 4)))
  pre.multi <- paste0("road.", paste0("0", c(1, 3)))
  sub.id <- lapply(k[c("one", "three")], function(x) letters[seq_along(x)])

  multi.tiff <- lapply(seq_along(pre.multi), function(i) {
    lttr <- sub.id[[i]]
    paste0(path, pre.multi[i], lttr, post)
  })

  single.tiff <- paste0(path, pre.single, post)

  tiff <- c(multi.tiff[[1]], single.tiff[1], multi.tiff[[2]], single.tiff[2])

  k <- unlist(k)

  geo.coords <- parallel::mclapply(seq_along(tiff), function(i) {
    latlongCoordinates(tiff[i], k[i])
  }, mc.cores = cores)

  start <- c(1, cumsum(k)[-length(k)] + 1)
  stop <- cumsum(k)
  idx <- data.frame(start = start, stop = stop)

  geo.coords <- lapply(seq_along(geo.coords), function(i) {
    tmp <- geo.coords[[i]]
    tmp$geo.id <- idx[i, "start"]:idx[i, "stop"]
    tmp$id <- NULL
    tmp
  })

  endpt <- c(p1, list(pts$two), p3, list(pts$four))

  rds <- lapply(seq_along(endpt), function(i) {
    dat[dat$point.id %in% endpt[[i]], ]
  })

  rds.rotate.scale <- parallel::mclapply(rds, function(x) {
    tmp <- lapply(x$point.id, function(y) {
      rotatePoint(y, dataset = "roads")
    })
    tmp <- do.call(rbind, tmp)
    if (nrow(x) > 1) {
      data.frame(point.id = x$point.id, scale(tmp))
    } else {
      data.frame(point.id = x$point.id, tmp)
    }
  }, mc.cores = cores)

  geo.coords.scale <- lapply(geo.coords, function(x){
    data.frame(geo.id = x$geo.id, scale(x[, c("long", "lat")]))
  })

  strata.ct <- vapply(rds, nrow, integer(1L))

  # exclude singletons, scale() returns NA
  multi.id <- seq_along(strata.ct)[strata.ct != 1]
  single.id <- which(strata.ct == 1)

  match.points <- parallel::mclapply(multi.id, function(i) {
    rd <- rds.rotate.scale[[i]]
    alters <- geo.coords.scale[[i]]
    names(alters)[-1] <- c("x", "y")

    out <- lapply(rd$point.id, function(z) {
      ego <- rd[rd$point.id == z, c("x", "y")]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, c("x", "y")]))
      }, numeric(1L))
      data.frame(point.id = z, geo.id = alters$geo.id[which.min(d)])
    })

    do.call(rbind, out)
  }, mc.cores = cores)

  match.points <- do.call(rbind, match.points)
  singleton <- data.frame(point.id = rds[[single.id]]$point.id,
                          geo.id = geo.coords[[single.id]]$geo.id)
  match.points <- rbind(match.points, singleton)

  geo.coords <- do.call(rbind, geo.coords)

  out <- merge(dat, match.points, by = "point.id")
  out <- merge(out, geo.coords, by = "geo.id")
  out <- out[order(out$geo.id), ]
  out$geo.id <- NULL
  out$point.id <- NULL
  out
}

#' Create PDFs road segment endpoints stratified by number of intersections.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @export

subsetRoadsPDF <- function(path) {
  file.nm <- "road"
  pre <- paste0(file.nm, ".")
  post <- ".pdf"

  framework <- cholera::roads[cholera::roads$name != "Map Frame", ]
  dat <- framework
  dat$point.id <- paste0(dat$x, "-", dat$y)
  intersections <- table(dat$point.id)

  # > table(intersections)
  # intersections
  #   1   2   3   4
  # 276  10 221  44

  one <- cholera::rd.sample$one
  three <- cholera::rd.sample$three
  idx1 <- pointIndex(length(one), 25)
  idx3 <- pointIndex(length(three), 25)
  intersection.ct <- sort(unique(intersections))

  invisible(lapply(seq_along(intersection.ct), function(ct) {
    file.num <- paste0("0", ct)

    if (ct %in% c(1, 3)) {
      if (ct == 1) {
        idx <- idx1
        pts <- one
      } else if (ct == 3) {
        idx <- idx3
        pts <- three
      }

      sub.id <- letters[seq_along(idx$start)]

      lapply(seq_along(idx$start), function(i) {
        sel <- pts[idx[i, "start"]:idx[i, "stop"]]
        nm <- paste0(path, pre, paste0(file.num, sub.id[i]), post)
        grDevices::pdf(file = nm)
        plot(framework$x, framework$y, pch = NA, xaxt = "n", yaxt = "n",
          xlab = NA, ylab = NA, bty = "n")
        points(dat[dat$point.id %in% sel, c("x", "y")], pch = 15, cex = 0.2)
        grDevices::dev.off()
      })
    } else {
      grDevices::pdf(file = paste0(path, pre, file.num, post))
      plot(framework$x, framework$y, pch = NA, xaxt = "n", yaxt = "n",
        xlab = NA, ylab = NA, bty = "n")
      sel <- names(intersections[intersections == ct])
      points(dat[dat$point.id %in% sel, c("x", "y")], pch = 15, cex = 0.2)
      grDevices::dev.off()
    }
  }))
}
