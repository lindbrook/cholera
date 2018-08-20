#' Compute Pearson Residuals (prototype)
#'
#' @param x An object created by \code{neighborhoodEuclidean()}, \code{neighborhoodVoronoi()} or \code{neighborhoodWalking()}.
#' @seealso \code{\link{neighborhoodVoronoi}}, \code{\link{neighborhoodVoronoi}}, \code{\link{neighborhoodEuclidean}},
#' @return An R vector.
#' @export
#' @examples
#' \dontrun{
#'
#' pearsonResiduals(neighborhoodEuclidean())
#' pearsonResiduals(neighborhoodVoronoi())
#' pearsonResiduals(neighborhoodWalking())
#' }

pearsonResiduals <- function(x) UseMethod("pearsonResiduals", x)

pearsonResiduals.default <- function(x) NULL

#' @export
pearsonResiduals.euclidean <- function(x) {
  obs <- unclass(table(x$nearest.pump))
  exp <- unclass(table(cholera::neighborhoodEuclidean(pump.select = x$pump.id,
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
  output <- data.frame(pump.id = as.numeric(names(count)),
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
pearsonResiduals.walking <- function(x) NULL

pearson <- function(x) {
  (x$Count - x$Expected) / sqrt(x$Expected)
}
