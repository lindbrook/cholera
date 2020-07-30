#' Aggregate time series fatality data from the Vestry report.
#'
#' @param vestry Logical. \code{TRUE} returns the data from the Vestry committee (Appendix B, p. 175). \code{FALSE} returns John Snow's contribution to the report (p.117).
#' @return A R list with two objects: "data" and "source" ("snow" or "vestry").
#' \itemize{
#'   \item{\code{date}: Calendar date.}
#'   \item{\code{day}: Day of the week.}
#'   \item{\code{deaths}: Measure of fatality.}
#'   \item{\code{fatal.attacks}: Measure of fatality.}
#' }
#' @note The "snow" data appears on p. 117 of the report; the "vestry" data appear in Appendix B on p.175.
#' @seealso \code{\link{plot.time_series}}, \code{\link{print.time_series}}, \code{vignette("time.series")}
#' @export
#' @examples
#' timeSeries(vestry = TRUE)
#' plot(timeSeries())

timeSeries <- function(vestry = FALSE) {
  if (vestry) {
    month <- c("July", rep("Aug", length(3:31)), rep("Sep", length(1:30)),
      "Oct")

    day <- c(26, 3:31, 1:30, 1)

    month.num <- c(7, rep(8, length(3:31)), rep(9, length(1:30)), 10)

    deaths <- c(rep(0, 3), rep(1, 3), rep(0, 3), 1, 2, 2, 0, 1, 2, 2, 1,
      3, 0, 3, 1, 0, 3, 0, 0, 1, 0, 2, 2, 4, 72, 127, 76, 71, 45, 40, 34, 30,
      24, 18, 15, 7, 13, 6, 8, 6, 5, 4, 4, 1, 0, 3, 3, 0, 1, 2, 0, 2, 1, 0, 1)

    fatal.attacks <- c(1, 1, 0, 1, 0, 1, rep(0, 3), 2, 3, 0, 3, 0, 3, 1,
      2, 2, 1, 2, 0, 2, 0, 0, 1, 0, 3, 2, 3, 34, 142, 128, 62, 55, 26, 28, 22,
      14, 6, 2, 3, 1, 3, 0, 1, 3, 4, 0, 1, 0, 0, 2, 0, 1, 0, 1, 0, 2, 0, 0, 1)

    calendar.date <- as.Date(paste0(1854, "-", month.num, "-", day))

    output <- list(data = data.frame(date = calendar.date,
      day = weekdays(calendar.date), deaths, fatal.attacks))

  } else {
    # Note: entry for an additional 45 fatal attacks with "Date unknown".
    yr <- 1854
    mo <- c(rep("08", length(19:31)), rep("09", length(1:30)))
    day <- c(19:31, 1:30)

    deaths <- c(1, 0, 2, 0, 0, 2, 0, 0, 1, 0, 1, 2, 3, 70, 127, 76, 71, 45, 37,
      32, 30, 24, 18, 15, 6, 13, 6, 8, 6, 5, 2, 3, 0, 0, 2, 3, 0, 0, 2, 0, 2,
      1, 0)

    fatal.attacks <- c(rep(1, 3), 0, rep(1, 2), 0, rep(1, 4), 8, 56, 143, 116,
      54, 46, 36, 20, 28, 12, 11, 5, 5, 1, 4, 0, 1, 4, 2, 3, 0, 0, 2, rep(1, 6),
      rep(0, 3))

    calendar.date <- as.Date(paste0(yr, "-", mo, "-", day))

    output <- list(data = data.frame(date = calendar.date,
      day = weekdays(calendar.date), deaths, fatal.attacks))
  }

  class(output) <- "time_series"
  output
}

#' Plot aggregate time series data from Vestry report.
#'
#' Plot aggregate fatality data and indicates the date of the removal of the handle of the Broad Street pump.
#' @param x An object of class "time_series" from timeSeries().
#' @param statistic Character. Fatality measure: either "fatal.attacks" or "deaths".
#' @param pump.handle Logical. Indicate date of removal of Broad Street pump handle.
#' @param main Character. Title of graph.
#' @param type Character. R plot type.
#' @param xlab Character. x-axis label.
#' @param ylab Character. y-axis label.
#' @param ... Additional plotting parameters.
#' @seealso \code{\link{timeSeries}}
#' @export
#' @examples
#' plot(timeSeries())
#' plot(timeSeries(), statistic = "deaths")
#' plot(timeSeries(), bty = "n", type = "h", lwd = 4)

plot.time_series <- function(x, statistic = "fatal.attacks",
  pump.handle = TRUE, main = "Removal of the Broad Street Pump Handle",
  type = "o", xlab = "Date", ylab = "Fatalities", ...) {

  if (class(x) != "time_series") stop('x\'s class needs to be "time_series".')

  if (all(statistic %in% c("deaths", "fatal.attacks")) == FALSE) {
    stop('statistic must either be "deaths" or "fatal.attacks".')
  }

  dat <- x$data
  plot(dat$date, dat[, statistic], type = type, xlab = xlab, ylab = ylab, ...)
  title(main = main, ...)
  if (pump.handle) pumpHandle()
}

#' Print summary data for timeSeries().
#'
#' Return summary results.
#' @param x An object of class "time_series" created by timeSeries().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export
#' @examples
#' timeSeries()
#' print(timeSeries())

print.time_series <- function(x, ...) {
  if (class(x) != "time_series") {
    stop('x\'s class needs to be "time_series".')
  }
  print(x$data)
}

pumpHandle <- function() {
  abline(v = as.Date("1854-09-08"), col = "red", lty = "dotted")
  axis(3, at = as.Date("1854-09-08"), labels = "Sep 08", cex.axis = 0.8,
    line = -0.5, col.axis = "red", col.ticks = "red")
}
