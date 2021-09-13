#' Compute latitude and longitude for fatalities cases (prototype).
#'
#' Non-anchor points: in stack, not at base of stack.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @export

latitudeLongitudeFatality <- function(path, multi.core = TRUE) {
  cores <- multiCore(multi.core)

  partitioned.fatalities <- partitionFatalities()
  partition.ct <- vapply(partitioned.fatalities, length, integer(1L))

  idx <- lapply(seq_along(partition.ct), function(i) {
    x <- partition.ct[i]
    q <- round(quantile(1:x, probs = c(0.25, 0.5, 0.75)))
    list(s1 = 1:q[1],
         s2 = (q[1] + 1):q[2],
         s3 = (q[2] + 1):q[3],
         s4 = (q[3] + 1):x)
  })

  split.partitions <- lapply(seq_along(partitioned.fatalities), function(i) {
    dat <- partitioned.fatalities[[i]]
    indices <- idx[[i]]
    lapply(seq_along(indices), function(i) dat[indices[[i]]])
  })

  split.partitions <- do.call(c, split.partitions)
  k <- vapply(split.partitions, length, integer(1L))
  num.id <- seq_along(split.partitions)

  if (any(num.id >= 10)) {
    num.id <- c(paste0("0", num.id[num.id < 10]), num.id[num.id >= 10])
  } else {
    num.id <- paste0("0", num.id)
  }

  pre <- paste0(path, "fatality.")
  post <- "_modified.tif"

  coords <- parallel::mclapply(seq_along(split.partitions), function(i) {
    tif <- paste0(pre, num.id[i], post)
    latlongCoordinatesB(tif, k[i])
  }, mc.cores = cores)

  start <- c(1, cumsum(k)[-length(k)] + 1)
  stop <- cumsum(k)
  idx <- data.frame(start = start, stop = stop)

  coords <- lapply(seq_along(coords), function(i) {
    tmp <- coords[[i]]
    tmp$id <- idx[i, "start"]:idx[i, "stop"]
    tmp
  })

  address.groups <- lapply(seq_along(partitioned.fatalities), function(i) {
    sel <- cholera::fatalities$case %in% partitioned.fatalities[[i]]
    cholera::fatalities[sel, ]
  })

  addr.grp.ct <- vapply(address.groups, nrow, integer(1L))
  ag <- address.groups[addr.grp.ct > 1]

  address.rotate.scale <- parallel::mclapply(ag, function(x) {
    tmp <- lapply(x$case, function(y) {
      rotatePoint(y, dataset = "fatalities")
    })
    tmp <- do.call(rbind, tmp)
    if (nrow(x) > 1) data.frame(case = x$case, scale(tmp))
    else data.frame(case = x$case, tmp)
  }, mc.cores = cores)

  coords.ct <- vapply(address.groups, nrow, integer(1L))
  cs <- coords[coords.ct > 1]

  coords.scale <- lapply(cs, function(x){
    data.frame(id = x$id, scale(x[, c("long", "lat")]))
  })

  match.points <- parallel::mclapply(seq_along(coords.scale), function(i) {
    addr <- address.rotate.scale[[i]]
    alters <- coords.scale[[i]]
    names(alters)[-1] <- c("x", "y")

    out <- lapply(addr$case, function(case) {
      ego <- addr[addr$case == case, c("x", "y")]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, c("x", "y")]))
      }, numeric(1L))
      data.frame(case = case, geo.id = alters$id[which.min(d)])
    })

    do.call(rbind, out)
  }, mc.cores = cores)

  match.points <- do.call(rbind, match.points)
  coords <- do.call(rbind, coords)

  out <- merge(cholera::fatalities, match.points, by = "case")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$case), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  out
}

partitionIndex <- function(x, subparts) {
  size <- 1 / subparts
  p <- seq(size, 1 - size, size)
  q <- round(stats::quantile(1:x, probs = p))
  start <- c(1, q + 1)
  stop <- c(q, x)
  lapply(seq_along(start), function(i) start[i]:stop[i])
}

#' Case by strata and strata numeric ID.
#'
#' Cases at address, excluding anchor; in the stack, excluding the base fatality.
#' @export

stratifiedFatalities <- function() {
  stack.cases.ct <- stackCaseCount()

  # exclude stack.cases.ct == 1, which are included in fatalities.address!
  strata <- stack.cases.ct[stack.cases.ct > 1]

  strata.anchor.case <- lapply(sort(unique(strata)), function(s) {
    stratum <- strata[strata == s]
    lapply(seq_along(stratum), function(i) {
      a <- as.numeric(names(stratum[i]))
      cases <- cholera::anchor.case[cholera::anchor.case$anchor == a, ]
      cases <- cases[cases$anchor != cases$case, "case"]
      if (any(nchar(cases) == 2)) {
        cases[nchar(cases) == 2] <- paste0(0, cases[nchar(cases) == 2])
      } else if (any(nchar(cases) == 1)) {
        cases[nchar(cases) == 1] <- paste0("00", cases[nchar(cases) == 1])
      }
      paste0(a, ".", cases)
    })
  })

  stratified <- lapply(seq_len(max(strata)), function(s) {
    out <- lapply(strata.anchor.case, function(x) {
      vapply(x, function(y) y[s], character(1L))
    })
    out <- unlist(out)
    out[!is.na(out)]
  })

  strata.cases <- lapply(stratified, function(x) {
    tmp <- strsplit(x, "[.]")
    vapply(tmp, function(x) as.numeric(x[2]), numeric(1L))
  })

  list(cases = strata.cases, num.id = seq_len(max(strata)))
}

#' Count of non-anchor cases by anchor.
#'
#' Cases at address, excluding anchor; in the stack, excluding the base case.
#' @export

stackCaseCount <- function() {
  sel <- cholera::anchor.case$anchor != cholera::anchor.case$case
  in_stack.cases <- cholera::anchor.case[sel, ]
  table(in_stack.cases$anchor)
}

# vapply(stratified.cases, length, integer(1L))

#' Create PDFs of stratified non-anchor cases.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param subparts Integer. Subdivide the three primary partitions in number of subparts.
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @export

subsetFatalitiesPDF <- function(path, subparts = 4, pch = 15, cex = 0.2) {
  file.nm <- "fatality"
  pre <- paste0(file.nm, ".")
  post <- ".pdf"
  dat <- cholera::fatalities

  partitioned.cases <- partitionFatalities()
  partition.ct <- vapply(partitioned.cases, length, integer(1L))
  idx <- lapply(partition.ct, function(x) partitionIndex(x, subparts))

  subdivided.partitions <- lapply(seq_along(partitioned.cases), function(i) {
    dat <- partitioned.cases[[i]]
    indices <- idx[[i]]
    lapply(seq_along(indices), function(i) dat[indices[[i]]])
  })

  subdivided.partitions <- do.call(c, subdivided.partitions)
  num.id <- seq_along(subdivided.partitions)

  if (any(num.id >= 10)) {
    num.id <- c(paste0("0", num.id[num.id < 10]), num.id[num.id >= 10])
  } else {
    num.id <- paste0("0", num.id)
  }

  framework <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rng <- mapRange()

  invisible(lapply(seq_along(subdivided.partitions), function(i) {
    grDevices::pdf(file = paste0(path, pre, num.id[i], post))
    plot(framework$x, framework$y, pch = NA, xaxt = "n", yaxt = "n",
      xlab = NA, ylab = NA, bty = "n", xlim = rng$x, ylim = rng$y)
    sel <- dat$case %in% subdivided.partitions[[i]]
    points(dat[sel, c("x", "y")], pch = pch, cex = cex)
    grDevices::dev.off()
  }))
}
