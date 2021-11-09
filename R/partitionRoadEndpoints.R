#' Partition road endpoints to avoid over-printing of points (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

partitionRoadEndpoints <- function(path, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  rds <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rds$id2 <- paste0(rds$x, "-", rds$y)

  duplicates <- duplicated(rds[, c("x", "y")])
  rds.sel <- rds[!duplicates, ]
  rds.dup <- rds[duplicates, ]

  # First pass nominal map distance ≤ 0.1 #

  idx <- data.frame(t(utils::combn(rds.sel$id, 2)))
  names(idx) <- c("v1", "v2")

  ds <- parallel::mclapply(seq_along(idx$v1), function(i) {
    stats::dist(rbind(rds.sel[rds.sel$id == idx[i, "v1"], c("x", "y")],
                      rds.sel[rds.sel$id == idx[i, "v2"], c("x", "y")]))
  }, mc.cores = cores)

  endpts <- data.frame(idx, d = unlist(ds))

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

  ds <- parallel::mclapply(seq_along(idx$v1), function(i) {
    stats::dist(rbind(rds.sel2[rds.sel2$id == idx[i, "v1"], c("x", "y")],
                      rds.sel2[rds.sel2$id == idx[i, "v2"], c("x", "y")]))
  }, mc.cores = 4L)

  endpts2 <- data.frame(idx, d = unlist(ds))

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
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

pdfPartitionRoadEndpoints <- function(path, multi.core = TRUE) {
  pt.ids <- partitionRoadEndpoints(path, multi.core = multi.core)
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
