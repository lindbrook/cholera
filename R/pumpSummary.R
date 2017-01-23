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

pumpSummary.walking <- function(obj) {
  if (class(obj) != "walking") {
    stop('Input object\'s class needs to be "walking".')
  }

  pump.distances <- obj$distance
  tot.distances <- colSums(pump.distances)
  names(tot.distances) <- obj$pump
  total <- tot.distances[is.infinite(tot.distances) == FALSE]

  pct <- total / sum(total)
  pct <- c(pct, 0)
  names(pct)[length(pct)] <- names(which(is.infinite(tot.distances)))

  pct <- data.frame(pct)
  pct$pump.id <- rownames(pct)

  nearest.pump <- obj$pump[apply(pump.distances, 1, which.min)]
  count <- as.data.frame(table(nearest.pump), stringsAsFactors = FALSE)
  temp <- merge(count, pct, by.x = "nearest.pump", by.y = "pump.id",
    all.y = TRUE)

  temp$nearest.pump <- as.numeric(substr(temp$nearest.pump, 2, nchar(temp$nearest.pump)))

  temp$Freq[is.na(temp$Freq)] <- 0
  names(temp) <- c("pump.id", "Count", "exp.pct")

  expected <- temp$exp.pct * sum(temp$Count)

  output <- data.frame(temp[, c("pump.id", "Count")],
    Percent = round(100 * temp$Count / sum(temp$Count), 2),
    Expected = expected)
  output[order(output$pump.id), ]

}


# id <- apply(pump.distances, 1, which.min)
#
# lapply(id, function(i) pump.distances[, i])
#
#
# apply(pump.distances, 1, function(x) x[id])
#
# d.mat <- matrix(0, nrow(pump.distances), 2)
#
# for (i in seq_len(nrow(pump.distances))) {
#   temp <- pump.distances[i, ]
#
#   for (j in id) {
#     d.mat[i, ] <- temp[c(6, j)]
#   }
# }

## cases

# nearest.pump <- pump.names[apply(pump.distances, 1, which.min)]
# output <- lapply(unique(nearest.pump), function(x) which(nearest.pump == x))
# names(output) <- unique(nearest.pump)
