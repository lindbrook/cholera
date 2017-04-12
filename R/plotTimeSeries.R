#' Plot time series data.
#'
#' Vestry report data.
#' @param statistic Character. "deaths" or "fatal.attacks".
#' @param pump.handle Logical. Annotate date of removal of Broad Street pump handle.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param ... Additional plotting parameters.
#' @return A base R graphics plot.
#' @seealso \code{
#' \link{time.series.snow}},
#' \code{\link{time.series.vestry}},
#' \code{vignette}("time.series")
#' @export
#' @examples
#' plotTimeSeries()


plotTimeSeries <- function(statistic = "fatal.attacks", pump.handle = TRUE,
  vestry = FALSE, ...) {

  if (all(statistic %in% c("deaths", "fatal.attacks")) == FALSE) {
    stop('"statistic" must either be "deaths" or "fatal.attacks".')
  }

  if (vestry) {
    time.series <- cholera::time.series.vestry
  } else {
    time.series <- cholera::time.series.snow
  }

  plot(time.series$date, time.series[, statistic], type = "o", xlab = "Date",
    ylab = statistic)

  if (pump.handle) pumpHandle()
}

pumpHandle <- function() {
  abline(v = as.Date("1854-09-08"), col = "red", lty = "dotted")
  axis(3, at = as.Date("1854-09-08"), labels = "Sep 08", cex.axis = 0.8,
    line = -0.5, col.axis = "red", col.ticks = "red")
  title(main = "Removal of the Broad Street Pump Handle")
}
