#' Compute Georeferenced Latitude and Longitude (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @noRd
#' @note This documents the computation of the latlong version of the landmarks data frame.

latlongLandmarks <- function(path) {
  lndmrks <- landmarkData()
  dat <- lndmrks[-grep("Square", lndmrks$name), ]
  dat <- dat[dat$name != "The Pantheon", ]
  k <- nrow(dat)

  # nominal coordinates

  tif <- "landmark_modified.tif"
  coords <- latlongCoordinates(paste0(path, tif), k, path)
  coords.scale <- data.frame(id = coords$id, scale(coords[, c("lon", "lat")]))

  tmp <- lapply(dat$case, function(id) rotatePoint(id, dataset = "landmarks"))
  tmp <- do.call(rbind, tmp)
  rotate.scale <- data.frame(id = dat$case, scale(tmp))

  alters <- coords.scale
  names(alters)[-1] <- c("x", "y")

  match.points <- lapply(rotate.scale$id, function(id) {
    ego <- rotate.scale[rotate.scale$id == id, c("x", "y")]
    d <- vapply(seq_len(nrow(alters)), function(i) {
      stats::dist(rbind(ego, alters[i, c("x", "y")]))
    }, numeric(1L))
    data.frame(id = id, geo.id = alters$id[which.min(d)])
  })

  match.points <- do.call(rbind, match.points)
  out <- merge(dat, match.points, by.x = "case", by.y = "id")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$case), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  a <- out

  # orthogonal projections

  tif <- "ortho.landmark_modified.tif"
  coords <- latlongCoordinates(paste0(path, tif), k, path)
  coords.scale <- data.frame(id = coords$id, scale(coords[, c("lon", "lat")]))

  tmp <- lapply(dat$case, function(id) rotatePoint(id, dataset = "landmarks"))
  tmp <- do.call(rbind, tmp)
  rotate.scale <- data.frame(id = dat$case, scale(tmp))

  alters <- coords.scale
  names(alters)[-1] <- c("x", "y")

  match.points <- lapply(rotate.scale$id, function(id) {
    ego <- rotate.scale[rotate.scale$id == id, c("x", "y")]
    d <- vapply(seq_len(nrow(alters)), function(i) {
      stats::dist(rbind(ego, alters[i, c("x", "y")]))
    }, numeric(1L))
    data.frame(id = id, geo.id = alters$id[which.min(d)])
  })

  match.points <- do.call(rbind, match.points)
  out <- merge(dat, match.points, by.x = "case", by.y = "id")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$case), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  b <- out

  # assembly

  rd.geo <- roadSegments(latlong = TRUE)
  rd.nom <- roadSegments(latlong = FALSE)

  one <- paste0(c("lon", "lat"), 1)
  two <- paste0(c("lon", "lat"), 2)

  out <- cbind(a, stats::setNames(b[, c("lon", "lat")],
    c("lon.proj", "lat.proj")))

  # use latlong segment coordinates for coordinates of Squares

  sq <- lndmrks[grep("Square", lndmrks$name), ]

  out.geo <- lapply(seq_len(nrow(sq)), function(i) {
    sel1 <- signif(rd.nom$x1) %in% signif(sq[i, ]$x) &
            signif(rd.nom$y1) %in% signif(sq[i, ]$y)

    sel2 <- signif(rd.nom$x2) %in% signif(sq[i, ]$x) &
            signif(rd.nom$y2) %in% signif(sq[i, ]$y)

    nodes.select <- rd.geo[sel1 | sel2, ]

    ones <- vapply(seq_len(nrow(nodes.select)), function(i) {
      paste0(nodes.select[i, ]$lon1, "_&_", nodes.select[i, ]$lat1)
    }, character(1L))

    twos <- vapply(seq_len(nrow(nodes.select)), function(i) {
      paste0(nodes.select[i, ]$lon2, "_&_", nodes.select[i, ]$lat2)
    }, character(1L))

    endpt.census <- table(c(ones, twos))
    node.candidate <- names(which.max(endpt.census))
    col1.rows <- which(ones %in% node.candidate)
    col2.rows <- which(twos %in% node.candidate)

    if (length(col1.rows) > 0) {
      geo.coords <- nodes.select[col1.rows[1], c("lon1", "lat1")]
    } else if (length(col2.rows) > 0) {
      geo.coords <- nodes.select[col2.rows[1], c("lon2", "lat2")]
    } else stop("err.")

    geo.coords <- stats::setNames(geo.coords, c("lon", "lat"))
    cbind(sq[i, ], geo.coords)
  })

  out.geo <- do.call(rbind, out.geo)
  out.geo$lon.proj <- out.geo$lon
  out.geo$lat.proj <- out.geo$lat

  out <- rbind(out, out.geo[, names(out)])

  # use Winsley Street latlong coordinates for coordinates of The Pantheon

  pantheon <- lndmrks[lndmrks$name == "The Pantheon", ]

  sel1 <- signif(rd.nom$x1) %in% signif(pantheon$x) &
          signif(rd.nom$y1) %in% signif(pantheon$y)

  sel2 <- signif(rd.nom$x2) %in% signif(pantheon$x) &
          signif(rd.nom$y2) %in% signif(pantheon$y)

  nodes.select <- rd.geo[sel1 | sel2, ]

  if (sum(sel1) > sum(sel2)) sel <- one
  else if (sum(sel2) > sum(sel1)) sel <- two
  else stop("err.")

  pantheon.coords <- nodes.select[-grep("Oxford", nodes.select$name), sel]
  pantheon.coords <- stats::setNames(pantheon.coords, c("lon", "lat"))

  out.pantheon <- cbind(pantheon, pantheon.coords)
  out.pantheon$lon.proj <- out.pantheon$lon
  out.pantheon$lat.proj <- out.pantheon$lat

  out <- rbind(out, out.pantheon[, names(out)])
  out <- out[order(out$case), ]
  row.names(out) <- NULL
  out
}

# usethis::use_data(landmarks, overwrite = TRUE)

#' Create PDFs of landmarks
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param orthogonal Logical. Use orthogonal projection coordinates.
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @noRd

landmarksPDF <- function(path, orthogonal = FALSE, pch = 15, cex = 0.2) {
  if (orthogonal) {
    coords <- c("x.proj", "y.proj")
    file.nm <- "ortho.landmark"
  } else {
    coords <- c("x", "y")
    file.nm <- "landmark"
  }
  pre <- paste0(file.nm, ".")
  post <- "pdf"
  dat <- landmarkData()

  # Exclude Squares from georeferencing
  dat <- dat[-grep("Square", dat$name), ]

  # Exclude The Pantheon from georeferencing
  dat <- dat[dat$name != "The Pantheon", ]

  rng <- mapRange()
  grDevices::pdf(file = paste0(path, pre, post))
  plot(dat[, coords], pch = NA, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
    bty = "n", xlim = rng$x, ylim = rng$y, asp = 1)
  points(dat[, coords], pch = pch, cex = cex)
  grDevices::dev.off()
}
