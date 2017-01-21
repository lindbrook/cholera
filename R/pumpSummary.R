#' Summary statistics for Voronoi neighborhoods.
#'
#' @param obj An object of class "voronoi" created by neighborhoodVoronoi().
#' @param statistic Character. "address" summarizes the number of addresses in pump neighbohoods. "fatality", the default, summarizes the number of fatalities in pump neighborhoods.
#' @return A data frame with observed and expected counts, observed percentage, and the Pearson residual, (observed - expected) / sqrt(expected).
#' @seealso \code{addVoronoi()}
#' \code{plot.voronoi()}
#' @export
#' @examples
#' dat <- neighborhoodVoronoi()
#' pumpSummary(dat)
#'
#' pumpSummary(dat, statistic = "address")

pumpSummary <- function(obj, statistic) UseMethod("pumpSummary", obj)

pumpSummary.default <- function(obj, statistic) NULL

#' @export
pumpSummary.voronoi <- function(obj, statistic = "fatality") {
  if (class(obj) != "voronoi") {
    stop('Input object\'s class needs to be "voronoi".')
  }

  statistic <- match.arg(statistic, choices = c("address", "fatality"))

  if (statistic == "address") {
    census <- lapply(obj$coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
        cholera::fatalities.address$y,cell$x, cell$y)
    })

    count <- vapply(census, sum, numeric(1L))

    output <- data.frame(pump.id = as.numeric(names(count)),
                         Count = count,
                         Percent = round(100 * count / sum(count), 2))

    output <- merge(output, obj$expected.data[, c("pump", "pct")],
      by.x = "pump.id", by.y = "pump")

    output$Expected <- output$pct * sum(output$Count)
    output$pct <- NULL
    output$Pearson <- (output$Count - output$Expected) / sqrt(output$Expected)
    output
  } else if (statistic == "fatality") {
    census <- lapply(obj$coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.unstacked$x,
        cholera::fatalities.unstacked$y,cell$x, cell$y)
    })

    count <- vapply(census, sum, numeric(1L))

    output <- data.frame(pump.id = as.numeric(names(count)),
                         Count = count,
                         Percent = round(100 * count / sum(count), 2))

    output <- merge(output, obj$expected.data[, c("pump", "pct")],
      by.x = "pump.id", by.y = "pump")

    output$Expected <- output$pct * sum(output$Count)
    output$pct <- NULL
    output$Pearson <- (output$Count - output$Expected) / sqrt(output$Expected)
    output
  }

  output
}
