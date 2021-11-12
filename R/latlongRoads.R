#' Compute latitude and longitude for unique road segment endpoints (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @export

latlongRoads <- function(path, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  endpt.ids <- partitionRoadEndpoints()

  coords <- parallel::mclapply(seq_along(endpt.ids), function(i) {
    ids <- endpt.ids[[i]]
    nm <- unlist(strsplit(names(endpt.ids[i]), "set"))[2]
    tif <- paste0(path, "roads", nm, "_modified.tif")
    k <- length(ids)
    geo.coords <- latlongCoordinatesC(tif, k, path)
    nom.coords <- cholera::roads[cholera::roads$id %in% ids, ]

    # rotate nominal coords to approximate georeferenced coords
    nom.rotate <- lapply(ids, rotatePoint)
    nom.rotate <- do.call(rbind, nom.rotate)
    nom.rotate.scale <- data.frame(id = ids, scale(nom.rotate))

    geo.scale <- data.frame(id = geo.coords$id,
      scale(geo.coords[, c("lon", "lat")]))

    alters <- geo.scale
    names(alters)[-1] <- c("x", "y")

    translation <- lapply(ids, function(id) {
      ego <- nom.rotate.scale[nom.rotate.scale$id == id, c("x", "y")]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, c("x", "y")]))
      }, numeric(1L))
      data.frame(id = id, geo.id = alters$id[which.min(d)])
    })

    translation <- do.call(rbind, translation)
    geo.coords <- merge(geo.coords, translation, by.x = "id", by.y = "geo.id")
    names(geo.coords)[c(1, length(names(geo.coords)))] <- c("geo.id", "id")
    merge(nom.coords, geo.coords[, -1], by = "id")
  }, mc.cores = cores)

  coords <- do.call(rbind, coords)
  coords <- coords[, c(names(cholera::roads), c("lon", "lat"))]
  coords$id2 <- paste0(coords$x, "-", coords$y)

  rds <- cholera::roads[cholera::roads$name != "Map Frame", ]
  duplicates <- duplicated(rds[, c("x", "y")])
  rds.dup <- rds[duplicates, ]
  rds.dup$id2 <- paste0(rds.dup$x, "-", rds.dup$y)

  rds.dup <- lapply(unique(rds.dup$id2), function(x) {
    cbind(rds.dup[rds.dup$id2 == x, ], coords[coords$id2 == x, c("lon", "lat")])
  })

  rds.dup <- do.call(rbind, rds.dup)
  rds.dup$id2 <- NULL
  coords$id2 <- NULL
  coords <- rbind(coords, rds.dup)
  coords[order(coords$id), ]
}

#' Partition road endpoints to avoid over-printing of points (prototype).
#'
#' @export

partitionRoadEndpoints <- function() {
  rds <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rds$id2 <- paste0(rds$x, "-", rds$y)

  duplicates <- duplicated(rds[, c("x", "y")])
  rds.sel <- rds[!duplicates, ]
  rds.dup <- rds[duplicates, ]

  # First pass nominal map distance ≤ 0.1 #

  idx <- data.frame(t(utils::combn(rds.sel$id, 2)))
  names(idx) <- c("v1", "v2")

  ds <- stats::dist(rds.sel[, c("x", "y")])
  endpts <- data.frame(idx, d = c(ds))

  setA <- endpts[endpts$d <= 0.1, ]

  # multiple end points "opposite" side
  opp.side <- intersect(setA$v1, setA$v2)

  # multiple end points "same" side
  table.v1 <- table(setA$v1)
  table.v2 <- table(setA$v2)

  if (any(table.v1 != 1) & any(table.v2 != 1)) {
    one <- as.numeric(names(table.v1[table.v1 != 1]))
    two <- as.numeric(names(table.v2[table.v2 != 1]))
    same.side <- unique(c(one, two))
  } else if (any(table.v1 != 1) & !any(table.v2 != 1)) {
    same.side <- unique(as.numeric(names(table.v1[table.v1 != 1])))
  } else if (!any(table.v1 != 1) & any(table.v2 != 1)) {
    same.side <- as.numeric(names(table.v2[table.v2 != 1]))
  } else {
    same.side <- NULL
  }

  intermediates <- c(opp.side, same.side)
  multi.obs <- setA[setA$v1 %in% intermediates | setA$v2 %in% intermediates, ]
  setA.pairs <- setA[!row.names(setA) %in% row.names(multi.obs), ]

  if (length(intermediates) > 1) {
    even <- intermediates[seq_along(intermediates) %% 2 == 0]
    odd <- intermediates[seq_along(intermediates) %% 2 != 0]
    setA.lst <- list(v1 = c(setA.pairs$v1, odd), v2 = c(setA.pairs$v2, even))
  } else {
    setA.lst <- list(v1 = c(setA.pairs$v1, intermediates), v2 = setA.pairs$v2)
  }

  # Second pass nominal map distance ≤ 0.16 (apparent gap at 0.16) #

  rds.sel2 <- rds.sel[!rds.sel$id %in% unlist(setA.lst), ]

  idx <- data.frame(t(utils::combn(rds.sel2$id, 2)))
  names(idx) <- c("v1", "v2")

  ds <- stats::dist(rds.sel2[, c("x", "y")])
  endpts2 <- data.frame(idx, d = c(ds))

  setB <- endpts2[endpts2$d <= 0.16, ]

  # multiple end points opposite side
  opp.side <- intersect(setB$v1, setB$v2)

  # multiple end points same side
  table.v1 <- table(setB$v1)
  table.v2 <- table(setB$v2)

  if (any(table.v1 != 1) & any(table.v2 != 1)) {
    one <- as.numeric(names(table.v1[table.v1 != 1]))
    two <- as.numeric(names(table.v2[table.v2 != 1]))
    same.side <- unique(c(one, two))
  } else if (any(table.v1 != 1) & !any(table.v2 != 1)) {
    same.side <- unique(as.numeric(names(table.v1[table.v1 != 1])))
  } else if (!any(table.v1 != 1) & any(table.v2 != 1)) {
    same.side <- as.numeric(names(table.v2[table.v2 != 1]))
  } else {
    same.side <- NULL
  }

  intermediates <- c(opp.side, same.side)
  multi.obs <- setB[setB$v1 %in% intermediates | setB$v2 %in% intermediates, ]

  # Extract bridge node of open triads
  multi.obsA <- selectBridgeNode(1:2, multi.obs)
  multi.obsB <- selectBridgeNode(3:4, multi.obs)
  multi.obsC <- selectBridgeNode(5:6, multi.obs)

  # 4 vertices in simple transitive relationship (string of pearls)
  multi.obsD <- unique(unlist(multi.obs[8:10, c("v1", "v2")]))
  pt.idx <- seq_along(multi.obsD)
  even <- multi.obsD[pt.idx %% 2 == 0]
  odd <- multi.obsD[pt.idx %% 2 != 0]

  # OK multi.obs[7, ] just split pair
  setB.pairs <- setB[!row.names(setB) %in% row.names(multi.obs[-7, ]), ]

  setB.lst <- list(v1 = c(setB.pairs$v1, odd, multi.obsB),
                   v2 = c(setB.pairs$v2, even, multi.obsA, multi.obsC))

  # Sufficiently distant endpoints
  rds.sel3 <- rds.sel2[!rds.sel2$id %in% unlist(setB.lst), ]

  list(setA1 = setA.lst$v1, setA2 = setA.lst$v2,
       setB1 = setB.lst$v1, setB2 = setB.lst$v2,
       setC0 = rds.sel3$id)
}

#' Compute nominal distance between road endpoints.
#'
#' @param a Numeric. Road ID.
#' @param b Numeric. Road ID.
#' @noRd

endptDist <- function(a = 602, b = 624, meters = FALSE) {
  d <- stats::dist(cholera::roads[cholera::roads$id %in% c(a, b), c("x", "y")])
  ifelse(meters, cholera::unitMeter(d), d)
}

#' Select bridge node of an opent triplet
#'
#' @param row.ids Numeric. Pair of rows to select.
#' @param dat Object. Data frame of multi-observations (non-pairs).
#' @noRd

selectBridgeNode <- function(row.ids = 1:2, dat) {
  tmp <- table(unlist(dat[row.ids, c("v1", "v2")]))
  as.numeric(names(tmp[tmp != 1]))
}

#' Create PDFs of road endpoints partition (prototype).
#'
#' For georeferencing in QGIS .
#' @param path Character. e.g., "~/Documents/Data/".
#' @export

pdfPartitionRoadEndpoints <- function(path) {
  pt.ids <- partitionRoadEndpoints()
  rng <- cholera::mapRange()
  invisible(lapply(names(pt.ids), function(nm) {
    pre <- "roads"
    file.id <- unlist(strsplit(nm, "set"))[2]
    post <- ".pdf"
    file.nm <- paste0(path, pre, file.id, post)
    dat <- cholera::roads[cholera::roads$id %in% pt.ids[[nm]], c("x", "y")]
    grDevices::pdf(file = file.nm)
    plot(dat, pch = 46, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
      xlim = rng$x, ylim = rng$y, bty = "n", asp = 1)
    grDevices::dev.off()
  }))
}
