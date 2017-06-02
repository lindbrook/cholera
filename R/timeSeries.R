#' Aggregate time series fatality data from the Vestry report.
#'
#' For use in integrating pumps and cases into road network needed to compute walking neighbohoods.
#' @param vestry Logical. TRUE returns the data from the Vestry committee (Appendix B). FALSE (default) returns John Snow's contribution to the report (p.117).
#' @return A R list with two objects: "data" and "source" ("snow" or "vestry").
#' \itemize{
#'   \item{\code{date}: Calendar date.}
#'   \item{\code{fatal.attacks}: measure of fatality.}
#'   \item{\code{deaths}: measure of fatality.}
#' }
#' @section Note: This function documents the code that generates \code{\link[cholera]{time.series.snow}} and \code{\link[cholera]{time.series.vestry}}.
#' @export

timeSeries <- function(vestry = FALSE) {
  if (vestry) {
    month <- c("July", rep("Aug", length(3:31)), rep("Sep", length(1:30)),
      "Oct")

    day <- c(26, 3:31, 1:30, 1)

    month.num <- c(7, rep(8, length(3:31)), rep(9, length(1:30)), 10)

    ascertained <- c(rep(0, 3), rep(1, 3), rep(0, 3), 1, 2, 2, 0, 1, 2, 2, 1,
      3, 0, 3, 1, 0, 3, 0, 0, 1, 0, 2, 2, 4, 72, 127, 76, 71, 45, 40, 34, 30,
      24, 18, 15, 7, 13, 6, 8, 6, 5, 4, 4, 1, 0, 3, 3, 0, 1, 2, 0, 2, 1, 0, 1)

    daily.fatal.attacks <- c(1, 1, 0, 1, 0, 1, rep(0, 3), 2, 3, 0, 3, 0, 3, 1,
      2, 2, 1, 2, 0, 2, 0, 0, 1, 0, 3, 2, 3, 34, 142, 128, 62, 55, 26, 28, 22,
      14, 6, 2, 3, 1, 3, 0, 1, 3, 4, 0, 1, 0, 0, 2, 0, 1, 0, 1, 0, 2, 0, 0, 1)

    calendar.date <- as.Date(paste0(1854, "-", month.num, "-", day))

    dat <- data.frame(date = calendar.date,
                      fatal.attacks = daily.fatal.attacks,
                      deaths = ascertained)

    output <- list(data = dat, source = "vestry")

  } else {
    yr <- 1854
    mo <- c(rep("08", length(19:31)), rep("09", length(1:30)))
    day <- c(19:31, 1:30)

    # +45 unknown
    fatal.attacks <- c(rep(1, 3), 0, rep(1, 2), 0, rep(1, 4), 8, 56, 143, 116,
      54, 46, 36, 20, 28, 12, 11, 5, 5, 1, 4, 0, 1, 4, 2, 3, 0, 0, 2, rep(1, 6),
      rep(0, 3))

    deaths <- c(1, 0, 2, 0, 0, 2, 0, 0, 1, 0, 1, 2, 3, 70, 127, 76, 71, 45, 37,
      32, 30, 24, 18, 15, 6, 13, 6, 8, 6, 5, 2, 3, 0, 0, 2, 3, 0, 0, 2, 0, 2,
      1, 0)

    dat <- data.frame(date = as.Date(paste0(yr, "-", mo, "-", day)),
                      fatal.attacks = fatal.attacks, deaths)

    output <- list(data = dat, source = "snow")
  }

  class(output) <- "time.series"
  output
}

#' Plot aggregate time series data from Vestry report.
#'
#' Plot aggregate fatality data and indicates the date of the removal of the handle of the Broad Street pump.
#' @param x An object of class "time.series": \code{\link[cholera]{time.series.snow}} or \code{\link[cholera]{time.series.vestry}}.
#' @param statistic Character. Fatality measure: either "fatal.attacks" (default) or "deaths".
#' @param pump.handle Logical. Indicate date of removal of Broad Street pump handle.
#' @param ... Additional plotting parameters.
#' @seealso \code{\link{timeSeries}}
#' @examples
#' # plot(time.series.snow)

plot.time.series <- function(x, statistic = "fatal.attacks",
  pump.handle = TRUE, ...) {

  if (class(x) != "time.series") {
    stop('Input object\'s class needs to be "time.series".')
  }

  if (all(statistic %in% c("deaths", "fatal.attacks")) == FALSE) {
    stop('"statistic" must either be "deaths" or "fatal.attacks".')
  }

  if (x$source == "vestry") {
    time.series <- cholera::time.series.vestry$data
  } else if (x$source == "snow") {
    time.series <- cholera::time.series.snow$data
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
