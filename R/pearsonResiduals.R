#' Compute Pearson Residuals (prototype)
#'
#' @param x An object created by \code{neighborhoodEuclidean()}, \code{neighborhoodVoronoi()} or \code{neighborhoodWalking()}.
#' @return An R vector.
#' @export
#' @examples
#' \donttest{
#' pearsonResiduals(neighborhoodEuclidean())
#' pearsonResiduals(neighborhoodVoronoi())
#' pearsonResiduals(neighborhoodWalking())
#' }

pearsonResiduals <- function(x) UseMethod("pearsonResiduals", x)

pearsonResiduals.default <- function(x) NULL

#' @export
pearsonResiduals.euclidean <- function(x) {
  obs <- unclass(table(x$nearest.pump))
  exp <- unclass(table(neighborhoodEuclidean(pump.select = x$pump.id,
    case.set = "expected", multi.core = x$cores)$nearest.pump))

  exp.data <- data.frame(pump.id = as.numeric(names(exp)),
                         exp.ct = exp,
                         Percent = round(100 * exp / sum(exp), 2))

  obs.data <- data.frame(pump.id = as.numeric(names(obs)),
                         Count = obs)

  output <- merge(obs.data, exp.data, by = "pump.id")
  output$exp.ct <- NULL
  output$Expected <- sum(output$Count) * output$Percent / 100
  output$Pearson <- pearson(output)
  output
}

#' @export
pearsonResiduals.voronoi <- function(x) {
  census <- x$statistic.data
  count <- vapply(census, sum, numeric(1L))
  output <- data.frame(pump.id = x$pump.id,
                       Count = count,
                       Percent = round(100 * count / sum(count), 2))
  output <- merge(output, x$expected.data[, c("pump", "pct")],
    by.x = "pump.id", by.y = "pump")
  output$Expected <- output$pct * sum(output$Count)
  output$pct <- NULL
  output$Pearson <- pearson(output)
  output
}

#' @export
pearsonResiduals.walking <- function(x) {
  dat <- expectedWalkingLength(x)
  obs <- dat$obs
  exp <- dat$exp
  exp.data <- data.frame(pump.id = as.numeric(names(exp)),
                         exp.ct = exp,
                         Percent = round(100 * exp / sum(exp), 2))

  obs.data <- data.frame(pump.id = as.numeric(names(obs)),
                         Count = obs)

  output <- merge(obs.data, exp.data, by = "pump.id")
  output$exp.ct <- NULL
  output$Expected <- sum(output$Count) * output$Percent / 100
  output$Pearson <- pearson(output)
  output[!is.na(output$Pearson), ]
}

pearson <- function(x) {
  (x$Count - x$Expected) / sqrt(x$Expected)
}

#' Compute Total Length of Roads by Neighborhood (prototype)
#'
#' @param x An object created by \code{neighborhoodWalking()}.
#' @noRd

expectedWalkingLength <- function(x) {
  dat <- observedExpected(x, neighborhoodPathData(x))
  observed.wholes <- dat$observed.wholes
  expected.wholes <- dat$expected.wholes
  obs.splits <- dat$exp.splits
  obs.splits.pump <- dat$exp.splits.pump
  obs.splits.segs <- dat$exp.splits.segs
  exp.splits <- dat$exp.splits
  exp.splits.pump <- dat$exp.splits.pump
  exp.splits.segs <- dat$exp.splits.segs

  observed.wholes.total.length <- lapply(observed.wholes, function(x) {
    sum(vapply(x, segmentLength, numeric(1L)))
  })

  observed.splits.total.length <- lapply(seq_along(exp.splits), function(i) {
    dat <- exp.splits[[i]]
    lst <- list(rbind(dat[1, c("x", "y")], dat[2, c("x", "y")]),
                rbind(dat[3, c("x", "y")], dat[4, c("x", "y")]))
    data.frame(dist = vapply(lst, stats::dist, numeric(1L)),
               pump = exp.splits.pump[[i]])
  })

  observed.splits.lengths <- do.call(rbind, observed.splits.total.length)
  observed.splits.pumps <- sort(unique(observed.splits.lengths$pump))

  observed.splits.total.length <- lapply(observed.splits.pumps, function(p) {
    sum(observed.splits.lengths[observed.splits.lengths$pump == p, "dist"])
  })

  names(observed.splits.total.length) <- observed.splits.pumps

  neighs <- names(observed.wholes.total.length)

  observed.total.length <- vapply(neighs, function(nm) {
    if (nm %in% names(observed.splits.total.length)) {
      observed.wholes.total.length[[nm]] + observed.splits.total.length[[nm]]
    } else {
      observed.wholes.total.length[[nm]]
    }
  }, numeric(1L))

  expected.wholes.total.length <- lapply(expected.wholes, function(x) {
    sum(vapply(x, segmentLength, numeric(1L)))
  })

  expected.splits.total.length <- lapply(seq_along(exp.splits), function(i) {
    dat <- exp.splits[[i]]
    lst <- list(rbind(dat[1, c("x", "y")], dat[2, c("x", "y")]),
                rbind(dat[3, c("x", "y")], dat[4, c("x", "y")]))
    data.frame(dist = vapply(lst, stats::dist, numeric(1L)),
               pump = exp.splits.pump[[i]])
  })

  expected.splits.lengths <- do.call(rbind, expected.splits.total.length)
  expected.splits.pumps <- sort(unique(expected.splits.lengths$pump))

  expected.splits.total.length <- lapply(expected.splits.pumps, function(p) {
     sum(expected.splits.lengths[expected.splits.lengths$pump == p, "dist"])
  })

  names(expected.splits.total.length) <- expected.splits.pumps

  neighs <- names(expected.wholes.total.length)

  expected.total.length <- vapply(neighs, function(nm) {
    if (nm %in% names(expected.splits.total.length)) {
      expected.wholes.total.length[[nm]] + expected.splits.total.length[[nm]]
    } else {
      expected.wholes.total.length[[nm]]
    }
  }, numeric(1L))

  list(obs = observed.total.length, exp = expected.total.length)
}
