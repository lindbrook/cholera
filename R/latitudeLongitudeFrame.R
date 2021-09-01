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
#' @param path Character. e.g., "~/Documents/Data/".
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @export

subsetFramePDF <- function(path, pch = 15, cex = 0.2) {
  file.nm <- "frame"
  pre <- paste0(file.nm, ".")
  post <- ".pdf"

  pts <- cholera::frame.sample
  num.id <- seq_along(pts)

  if (any(num.id >= 10)) {
    num.id <- c(paste0("0", num.id[num.id < 10]), num.id[num.id >= 10])
  } else {
    num.id <- paste0("0", num.id)
  }

  dat <- cholera::roads[cholera::roads$name == "Map Frame", ]
  rng <-  mapRange()

  invisible(lapply(seq_along(num.id), function(i) {
    grDevices::pdf(file = paste0(path, pre, num.id[i], post))
    plot(dat$x, dat$y, pch = NA, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
      bty = "n", xlim = rng$x, ylim = rng$y)
    points(dat[dat$id %in% pts[[i]], c("x", "y")], pch = pch, cex = cex)
    grDevices::dev.off()
  }))
}

#' Sample for map frame segment endpoints.
#'
#' @param cutpoint Numeric.
#' @export

mapFrameSamples <- function(cutpoint = 0.1) {
  data1 <- cholera::roads[cholera::roads$name == "Map Frame", ]
  data1$point.id <- paste0(data1$x, "-", data1$y)
  data1 <- data1[!duplicated(data1$point.id), ]

  center <- data.frame(x = mean(range(data1$x)), y = mean(range(data1$y)))

  data2 <- data1
  data2$x <- data2$x - center$x
  data2$y <- data2$y - center$y

  theta <- vapply(data2$id, function(id) {
    pt.data <- data2[data2$id == id, c("x", "y")]
    angle <- atan(pt.data$y/pt.data$x) * 180L / pi
    if (all(sign(pt.data) == c(-1, 1)) | all(sign(pt.data) == c(-1, -1))) {
      180L + angle
    } else if (all(sign(pt.data) == c(1, -1))) {
      360L + angle
    } else if (all(sign(pt.data) == c(1, 1))) {
      angle
    }
  }, numeric(1L))

  data2 <- data2[order(theta), ]
  vars <- c("x", "y")

  # Distance between neighboring points on map frame

  point.delta <- vapply(seq_along(data2$x)[-length(data2$x)], function(i) {
    stats::dist(rbind(data2[i + 1, vars], data2[i, vars]))
  }, numeric(1L))

  point.delta0 <- stats::dist(rbind(data2[nrow(data2), vars], data2[1, vars]))
  point.delta <- c(point.delta, point.delta0)

  # Identify "overlapping" points

  pair.ego <- which(point.delta < cutpoint)
  pair.alter <- pair.ego + 1

  pairs <- lapply(seq_along(pair.ego), function(i) {
    data2[c(pair.ego[i], pair.alter[i]), ]
  })

  pair.id <- lapply(pairs, function(x) x$id)
  single.id <- setdiff(data2$id, unlist(pair.id))

  # Triplet check

  idx <- data.frame(t(utils::combn(length(pair.id), 2)))
  names(idx) <- c("v1", "v2")

  triplet.test <- vapply(seq_len(nrow(idx)), function(i) {
    any(pair.id[[idx[i, "v1"]]] %in% pair.id[[idx[i, "v2"]]])
  }, logical(1L))

  triplet.sel <- unlist(idx[which(triplet.test), ])
  pair.sel <- setdiff(seq_along(pair.id), triplet.sel)

  # Data components

  overlapping <- c(pair.id[pair.sel], list(unique(unlist(pair.id[triplet.sel]))))
  singles <- setdiff(data2$id, unlist(overlapping))
  pairs <- pair.id[pair.sel]
  triplet <- unique(unlist(pair.id[triplet.sel]))

  # Assemble data

  init.data <- matrix(unlist(pairs[1:3]), ncol = 3, byrow = TRUE)
  dataA <- rbind(triplet, init.data)

  sgl.smpl <- sample(singles)
  p.data <- c(pairs[[4]], sgl.smpl)
  dataB <- matrix(p.data[1:45], ncol = 3, byrow = TRUE)

  smpl <- c(data.frame(rbind(dataA, dataB)))

  leftovers <- p.data[46:length(p.data)]

  dataC <- lapply(seq_along(leftovers), function(i) {
    c(smpl[[i]], leftovers[i])
  })

  smpl[1:2] <- dataC
  stats::setNames(smpl, paste0("s", 1:3))
}

# frame.sample <- mapFrameSamples()
# usethis::use_data(frame.sample, overwrite = TRUE)
