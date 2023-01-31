#' Compute latitude and longitude for unique road segment endpoints (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @noRd

latlongFrame <- function(path, multi.core = TRUE) {
  cores <- multiCore(multi.core)

  pts <- partitionFrame()
  k <- vapply(pts, length, integer(1L))
  geo.id <- geoID(k)

  post <- "_modified.tif"
  pre <- paste0("frame", seq_along(pts))
  tiff <- paste0(path, pre, post)

  geo.coords <- parallel::mclapply(seq_along(tiff), function(i) {
    latlongCoordinates(tiff[i], k[i], path)
  }, mc.cores = cores)

  geo.coords <- lapply(seq_along(geo.coords), function(i) {
    tmp <- geo.coords[[i]]
    tmp$geo.id <- geo.id[[i]]
    tmp$id <- NULL
    row.names(tmp) <- NULL
    tmp
  })

  dat0 <- cholera::roads[cholera::roads$name == "Map Frame", ]
  dat0$point.id <- paste0(dat0$x, "-", dat0$y)
  dat <- dat0[!duplicated(dat0$point.id), ]

  frm <- lapply(pts, function(x) dat[dat$id %in% x, ])

  frm.rotate.scale <- parallel::mclapply(frm, function(x) {
    tmp <- lapply(x$id, function(y) {
      rotatePoint(y, dataset = "roads")
    })
    tmp <- do.call(rbind, tmp)
    data.frame(point.id = x$point.id, scale(tmp))
  }, mc.cores = cores)

  geo.coords.scale <- lapply(geo.coords, function(x) {
     data.frame(geo.id = x$geo.id, scale(x[, c("lon", "lat")]))
  })

  match.points <- parallel::mclapply(seq_along(geo.coords.scale), function(i) {
    rd <- frm.rotate.scale[[i]]
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
  translation <- merge(match.points, geo.coords, by = "geo.id")

  out <- merge(dat0, translation, by = "point.id")
  out$geo.id <- NULL
  out$point.id <- NULL
  out <- out[order(out$id), ]
  row.names(out) <- NULL
  out
}

geoID <- function(k) {
  id <- cumsum(k)
  lapply(seq_along(id), function(i) {
    if (i == 1) 1:id[i]
    else (id[i - 1] + 1):id[i]
  })
}

#' Network graph of frame points with selected inter-point distance.
#'
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An 'igraph' object.
#' @noRd

thresholdFrameGraph <- function(inter.point.dist = 0.15) {
  dat <- cholera::roads[cholera::roads$name == "Map Frame", ]
  dat$point.id <- paste0(dat$x, "-", dat$y)
  dat <- dat[!duplicated(dat$point.id), ]
  idx <- data.frame(t(utils::combn(dat$id, 2)))
  names(idx) <- c("v1", "v2")
  d <- stats::dist(dat[, c("x", "y")])
  frame.pt.dist <- data.frame(idx, d = c(d))
  frame.pt.dist <- frame.pt.dist[frame.pt.dist$d <= inter.point.dist, ]
  edge.list <- frame.pt.dist[, c("v1", "v2")]
  plot(igraph::graph_from_data_frame(edge.list, directed = FALSE),
    vertex.size = 0)
}

#' Partition overlapping frame points.
#'
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An R list.
#' @noRd

partitionFrame <- function(inter.point.dist = 0.15) {
  dat <- cholera::roads[cholera::roads$name == "Map Frame", ]
  dat$point.id <- paste0(dat$x, "-", dat$y)
  dat <- dat[!duplicated(dat$point.id), ]

  idx <- data.frame(t(utils::combn(dat$id, 2)))
  names(idx) <- c("v1", "v2")
  d <- stats::dist(dat[, c("x", "y")])
  frame.pt.dist <- data.frame(idx, d = c(d))
  frame.pt.dist <- frame.pt.dist[frame.pt.dist$d <= inter.point.dist, ]

  closed.triad <- c(1154, 1158, 1159)

  triad.sel <- vapply(seq_along(frame.pt.dist$v1), function(i) {
    any(closed.triad %in% frame.pt.dist[i,  c("v1", "v2")])
  }, logical(1L))

  tmp <- frame.pt.dist[!triad.sel, c("v1", "v2")]
  tmp <- lapply(seq_along(tmp$v1), function(i) unlist(tmp[i, ]))
  tmp <- do.call(c, tmp)

  list(set1 = c(closed.triad[1], unname(tmp[seq_along(tmp) %% 3 == 1])),
       set2 = c(closed.triad[2], unname(tmp[seq_along(tmp) %% 3 == 2])),
       set3 = c(closed.triad[3], unname(tmp[seq_along(tmp) %% 3 == 0])),
       set4 = setdiff(dat$id, c(closed.triad, tmp)))
}

#' Create PDFs of frame points.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @noRd

framePartitionPDF <- function(path, pch = 46, cex = 1) {
  file.nm <- "frame"
  post <- ".pdf"
  rng <- mapRange()
  frame.partitions <- partitionFrame()
  invisible(lapply(seq_along(frame.partitions), function(i) {
    grDevices::pdf(file = paste0(path, file.nm, i, post))
    dat <- cholera::roads[cholera::roads$id %in% frame.partitions[[i]], ]
    plot(dat$x, dat$y, pch = pch, xaxt = "n", yaxt = "n", cex = cex, xlab = NA,
      ylab = NA, bty = "n", xlim = rng$x, ylim = rng$y, asp = 1)
    grDevices::dev.off()
  }))
}
