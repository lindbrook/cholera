#' Weather data recorded in Oxford (Met Office UK).
#'
#' Add and use last day of month as unit of observation to oxford.weather.
#' @note December 1860 observation is dropped due to missing "tmin" value.
#' @return An R data frame.
#' @export

oxfordWeather <- function() {
  ox <- cholera::oxford.weather
  class(ox) <- c("oxfordWeather", class(ox))
  ox
}

#' Plot method for oxfordWeather().
#'
#' @param x object.
#' @param statistic Character.
#' @param month Character. "august" or "september".
#' @param end.year Numeric.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.oxfordWeather <- function(x, statistic = "temperature",
  month = "september", end.year = NULL, ...) {

  month <- tolower(month)
  if (!is.null(end.year)) {
    if (end.year > min(x$year) & end.year <= max(x$year)) {
      x <- x[x$year <= end.year, ]
    } else stop(min(x$year), " < end.yr <= ", max(x$year))
  }
  if (statistic == "temperature") temperaturePlot(x, month)
  else if (statistic == "rain") rainPlot(x, month)
  else stop('statistic must be "temperature" or "rain".', call. = FALSE)
}

#' @importFrom tools toTitleCase

rainPlot <- function(x, month) {
  if (month == "august") outbreak.mo <- as.Date("1854-08-01")
  else if (month == "september") outbreak.mo <- as.Date("1854-09-01")
  else stop('month must be "august" or "september".', call. = FALSE)

  outbreak.sel <- x$date == outbreak.mo
  mo <- as.numeric(format(outbreak.mo, "%m"))
  yr <- as.numeric(format(outbreak.mo, "%Y"))

  rain.outbreak <- x[x$year == yr & x$mo == mo, "rain"]
  rain.mo <- x[x$mo == mo, "rain"]

  percentile <- 100 * mean(rain.outbreak > rain.mo)
  ttl <- paste0("Monthly Rainfall in Oxford UK - ", tools::toTitleCase(month))

  plot(x$date, x$rain, xlab = "Year", ylab = "mm", col = "gray",
    pch = NA, main = ttl)
  points(x[x$year != yr & x$mo == mo, "date"],
         x[x$year != yr & x$mo == mo, "rain"], pch = 16)
  points(x[x$year == yr & x$mo == mo, "date"],
         x[x$year == yr & x$mo == mo, "rain"], col = "red", pch = 16)
  axis(3, at = outbreak.mo, labels = "Soho outbreak", padj = 0.9,
    cex.axis = 3/4, col.axis = "red", col.ticks = "red")
  abline(v = outbreak.mo, col = "red")
  abline(h = x[outbreak.sel, "rain"], col = "red")
  axis(4, at = x[outbreak.sel, "rain"],  col.axis = "red", col = "red",
    labels = paste0(round(percentile), "th %"))
  rug(rain.mo, side = 4)
  lines(stats::lowess(x[x$mo == mo, "date"], rain.mo), lty = "dashed", lwd = 2)
}

#' @importFrom tools toTitleCase

temperaturePlot <- function(x, month) {
  if (month == "august") {
    outbreak <- as.Date("1854-09-01") - 1
    suffix <- "-08-31"
    temp.mo.hi <- x[x$year == 1854 & x$mo == 8, "tmax"]
    temp.mo.lo <- x[x$year == 1854 & x$mo == 8, "tmin"]
    temp.hi <- x[x$mo == 8, "tmax"]
    temp.lo <- x[x$mo == 8, "tmin"]
  } else if (month == "september") {
    outbreak <- as.Date("1854-10-01") - 1
    suffix <- "-09-30"
    temp.mo.hi <- x[x$year == 1854 & x$mo == 9, "tmax"]
    temp.mo.lo <- x[x$year == 1854 & x$mo == 9, "tmin"]
    temp.hi <- x[x$mo == 9, "tmax"]
    temp.lo <- x[x$mo == 9, "tmin"]
  } else stop('month must be "august" or "september".', call. = FALSE)

  percentile.hi <- 100 * mean(temp.mo.hi > temp.hi)
  percentile.lo <- 100 * mean(temp.mo.lo > temp.lo, na.rm = TRUE)

  sel <- x$date %in% as.Date(paste0(unique(x$year), suffix))
  mo.data <- x[sel, ]

  ttl <- paste0("Monthly High and Low Temperatures in Oxford UK (",
    tools::toTitleCase(month), ")")

  plot(mo.data$date, mo.data$tmax, pch = NA, xlab = "Year", ylab = "Celsius",
    ylim = range(mo.data$tmax, mo.data$tmin), main = ttl)
  axis(3, at = outbreak, labels = "Soho Outbreak", cex.axis = 3/4, padj = 0.9)
  axis(4, at = temp.mo.hi, cex.axis = 0.9, padj = -0.9,
    labels = paste0(round(percentile.hi), "th %"))
  axis(4, at = temp.mo.lo, cex.axis = 0.9, padj = -0.9,
    labels = paste0(round(percentile.lo), "th %"))

  invisible(lapply(seq_len(nrow(mo.data )), function(i) {
    segments(mo.data$date[i], mo.data$tmax[i],
             mo.data$date[i], mo.data$tmin[i], lwd = 0.5)
  }))

  points(mo.data$date, mo.data$tmax, col = "red")
  points(mo.data$date, mo.data$tmin, col = "blue")
  segments(mo.data[mo.data$date == outbreak, "date"],
           mo.data[mo.data$date == outbreak, "tmax"],
           mo.data[mo.data$date == outbreak, "date"],
           mo.data[mo.data$date == outbreak, "tmin"], lwd = 2)
  points(mo.data[mo.data$date == outbreak, "date"],
         mo.data[mo.data$date == outbreak, "tmax"], col = "red", pch = 16)
  points(mo.data[mo.data$date == outbreak, "date"],
         mo.data[mo.data$date == outbreak, "tmin"], col = "blue", pch = 16)
  lines(stats::lowess(mo.data$date, mo.data$tmax), col = "red", lwd = 1.5)
  lines(stats::lowess(mo.data$date, mo.data$tmin), col = "blue", lwd = 1.5)
  rug(mo.data$tmax, side = 4, col = "red")
  rug(mo.data$tmin, side = 4, col = "blue")
  abline(h = mo.data[mo.data$date == outbreak, "tmax"], col = "red",
    lty = "dashed", lwd = 1.5)
  abline(h = mo.data[mo.data$date == outbreak, "tmin"], col = "blue",
    lty = "dashed", lwd = 1.5)
}
