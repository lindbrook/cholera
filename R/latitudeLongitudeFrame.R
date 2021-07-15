#' Compute latitude and longitude for unique road segment endpoints (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @export

latitudeLongitudeFrame <- function(path, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  dat <- cholera::roads[cholera::roads$name == "Map Frame", ]
  dat$point.id <- paste0(dat$x, "-", dat$y)
  dat <- dat[!duplicated(dat$point.id), ]

  pts <- cholera::frame.sample
  idx <- pointIndex(length(pts), 25)
  num.id <- seq_len(nrow(idx))

  k <- idx$stop - idx$start + 1

  p <- lapply(seq_along(idx$start), function(i) {
    pts[idx[i, "start"]:idx[i, "stop"]]
  })

  post <- "_modified.tif"
  pre <- paste0("frame.", paste0("0", seq_len(nrow(idx))))
  tiff <- paste0(path, pre, post)

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

  rds <- lapply(p, function(x) dat[dat$id %in% x, ])

  rds.rotate.scale <- parallel::mclapply(rds, function(x) {
    tmp <- lapply(x$point.id, function(y) {
      rotatePoint(y, dataset = "roads")
    })
    tmp <- do.call(rbind, tmp)
    data.frame(point.id = x$point.id, scale(tmp))
  }, mc.cores = cores)

  geo.coords.scale <- lapply(geo.coords, function(x) {
     data.frame(geo.id = x$geo.id, scale(x[, c("long", "lat")]))
  })

  match.points <- parallel::mclapply(seq_along(geo.coords.scale), function(i) {
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
  geo.coords <- do.call(rbind, geo.coords)

  out <- merge(dat, match.points, by = "point.id")
  out <- merge(out, geo.coords, by = "geo.id")
  out <- out[order(out$geo.id), ]
  out$geo.id <- NULL

  dat <- cholera::roads[cholera::roads$name == "Map Frame", ]
  dat$point.id <- paste0(dat$x, "-", dat$y)
  out <- merge(dat, out[, c("long", "lat", "point.id")], by = "point.id")
  out$point.id <- NULL
  out <- out[order(out$id), ]
  row.names(out) <- NULL
  out
}

#' Create PDFs of map frame points.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param cex Numeric.
#' @export

subsetFramePDF <- function(path, cex = 0.2) {
  file.nm <- "frame"
  pre <- paste0(file.nm, ".")
  post <- ".pdf"

  pts <- cholera::frame.sample
  idx <- pointIndex(length(pts), 25)
  num.id <- seq_len(nrow(idx))

  if (any(num.id >= 10)) {
    num.id <- c(paste0("0", num.id[num.id < 10]), num.id[num.id >= 10])
  } else {
    num.id <- paste0("0", num.id)
  }

  dat0 <- cholera::roads[cholera::roads$name != "Map Frame", ]
  dat <- cholera::roads[cholera::roads$name == "Map Frame", ]

  invisible(lapply(seq_along(num.id), function(i) {
    grDevices::pdf(file = paste0(path, pre, num.id[i], post))
    plot(dat0$x, dat0$y, pch = NA, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
      bty = "n")
    sel <- idx[i, "start"]:idx[i, "stop"]
    points(dat[dat$id %in% pts[sel], c("x", "y")], pch = 15, cex = cex)
    grDevices::dev.off()
  }))
}

#' Sample for map frame segment endpoints.
#'
#' @export

mapFrameSamples <- function() {
  dat <- cholera::roads[cholera::roads$name == "Map Frame", ]
  dat$point.id <- paste0(dat$x, "-", dat$y)
  dat <- dat[!duplicated(dat$point.id), ]
  sample(dat$id)
}

# frame.sample <- mapFrameSamples()
# usethis::use_data(frame.sample, overwrite = TRUE)
