#' Compute Pearson Residuals (prototype)
#'
#' @param x An object created by \code{neighborhoodEuclidean()}, \code{neighborhoodVoronoi()} or \code{neighborhoodWalking()}.
#' @return An R vector.
#' @export
#' @examples
#' \dontrun{
#' pearsonResiduals(neighborhoodEuclidean())
#' pearsonResiduals(neighborhoodVoronoi())
#' pearsonResiduals(neighborhoodWalking())
#' }

pearsonResiduals <- function(x) UseMethod("pearsonResiduals", x)

#' @export
pearsonResiduals.default <- function(x) NULL

#' @export
pearsonResiduals.euclidean <- function(x) {
  obs <- unclass(table(x$nearest.pump))
  exp <- unclass(table(neighborhoodEuclidean(pump.select = x$pump.select,
    case.set = "expected", multi.core = x$cores)$nearest.pump))

  exp.data <- data.frame(Pump = as.numeric(names(exp)),
                         exp.ct = exp,
                         Percent = round(100 * exp / sum(exp), 2))

  obs.data <- data.frame(Pump = as.numeric(names(obs)),
                         Count = obs)

  output <- merge(obs.data, exp.data, by = "Pump", all.y = TRUE)
  output$Count[is.na(output$Count)] <- 0
  output$exp.ct <- NULL
  output$Expected <- sum(output$Count) * output$Percent / 100
  output$Pearson <- pearson(output)
  output
}

#' @export
pearsonResiduals.voronoi <- function(x) {
  census <- x$statistic.data
  count <- vapply(census, sum, numeric(1L))
  output <- data.frame(Pump = x$pump.id,
                       Count = count,
                       Percent = round(100 * count / sum(count), 2),
                       row.names = NULL)
  output <- merge(output, x$expected.data[, c("pump", "pct")], by.x = "Pump",
    by.y = "pump")
  output$Expected <- output$pct * sum(output$Count)
  output$pct <- NULL
  output$Pearson <- pearson(output)
  output
}

#' @export
pearsonResiduals.walking <- function(x) {
  vars <- c("x", "y")
  exp <- neighborhoodWalking(x$p.sel, case.set = "expected")

  neigh.seg.ds <- lapply(exp$same_pump.road_segs, function(neigh) {
    neigh <- cholera::road.segments[cholera::road.segments$id %in% neigh, ]
    vapply(neigh$id, function(s) {
      seg <- neigh[neigh$id == s, ]
      dat <- rbind(stats::setNames(seg[, paste0(vars, 1)], vars),
                   stats::setNames(seg[, paste0(vars, 2)], vars))
      stats::dist(dat)
    }, numeric(1L))
  })

  whole.segs <- vapply(neigh.seg.ds, sum, numeric(1L))

  split.segs <- vapply(exp$diff_pump.road_segs, function(neigh) {
    sum(neigh$d)
  }, numeric(1L))

  if (2L %in% x$p.sel) {
    split.segs <- c(split.segs, 0)
    names(split.segs)[length(split.segs)] <- "2"
    split.segs <- split.segs[order(as.integer(names(split.segs)))]
  }

  exp.d <- whole.segs + split.segs

  obs.d <- vapply(x$neigh.edges, function(neigh) {
    sum(x$edges[x$edges$id2 %in% neigh, "d"])
  }, numeric(1L))

  names(obs.d) <- substr(names(obs.d), 2, length(obs.d))

  exp.data <- data.frame(Pump = as.numeric(names(exp.d)), exp.d = exp.d,
    Percent = round(100 * exp.d / sum(exp.d), 2))

  obs.data <- data.frame(Pump = as.numeric(names(obs.d)), Observed = obs.d)

  output <- merge(obs.data, exp.data, by = "Pump", all.y = TRUE)
  if (any(is.na(output$Observed))) output$Observed[is.na(output$Observed)] <- 0
  output$Expected <- sum(output$Observed) * output$Percent / 100
  output$Pearson <- (output$Observed - output$Expected) / sqrt(output$Expected)
  output$Pearson <- round(output$Pearson, 2)
  output$exp.d <- NULL
  output
}

pearson <- function(x) {
  (x$Count - x$Expected) / sqrt(x$Expected)
}
