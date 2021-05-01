#' Weather data recorded in Oxford (Met Office UK).
#'
#' Add and use last day of month as unit of observation to oxford.weather.
#' @note December 1860 observation is dropped due to missing "tmin" value.
#' @return An R data frame.
#' @export

oxfordWeather <- function() {
  ox <- cholera::oxford.weather
  ox$date <- monthEndDate()
  class(ox) <- c("oxfordWeather", class(ox))
  ox
}

# Compute last day of month.
monthEndDate <- function(start.yr = 1853, end.yr = NULL) {
  if (is.null(end.yr)) end.yr <- max(cholera::oxford.weather$year)
  start <- paste0(start.yr, "-1-1")
  end <- paste0(end.yr, "-12-31")
  cal <- seq.Date(as.Date(start), as.Date(end), by = "day")
  cal <- data.frame(date = cal, year = as.numeric(format(cal, "%Y")),
    month = as.numeric(format(cal, "%m")), day = as.numeric(format(cal, "%d")))
  last.day.month <- lapply(start.yr:end.yr, function(yr) {
    tmp <- cal[cal$year == yr, ]
    out <- lapply(1:12, function(mo) max(tmp[tmp$month == mo, "date"]))
    do.call(c, out)
  })
  do.call(c, last.day.month)
}

#' Plot method for oxfordWeather().
#'
#' @param x object.
#' @param statistic Character.
#' @param month Character. "august" or "september".
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.oxfordWeather <- function(x, statistic = "temperature",
  month = "september", ...) {

  outbreak <- as.Date("1854-10-01") - 1
  sept <- as.Date(paste0(unique(x$year), "-09-30"))
  if (statistic == "temperature") temperaturePlot(x, month)
  else if (statistic == "rain") rainPlot(x, month)
  else stop('statistic must be "temperature" or "rain".', call. = FALSE)
}

rainPlot <- function(x, month) {
  if (month == "august") {
    outbreak <- as.Date("1854-09-01") - 1
    suffix <- "-08-31"
    mo.col <- "blue"
  } else if (month == "september") {
    outbreak <- as.Date("1854-10-01") - 1
    suffix <- "-09-30"
    mo.col <- "red"
  } else stop('month must be "august" or "september".', call. = FALSE)

  mo.sel <- x$date == outbreak
  mos <- x$date %in% as.Date(paste0(unique(x$year), suffix))
  ttl <- paste0("Monthly Rainfall in Oxford UK (", tools::toTitleCase(month),
    ")")

  plot(x$date, x$rain, xlab = "Year", ylab = "millimeters", col = "gray",
    main = ttl)
  points(x[mos, "date"], x[mos, "rain"], col = mo.col, pch = 16)
  axis(3, at = outbreak, labels = format(outbreak, "%b %Y"), padj = 0.9,
    col.ticks = mo.col, cex.axis = 0.9, col.axis = mo.col)
  abline(v = outbreak, col = mo.col)
  abline(h = x[mo.sel, "rain"], col = mo.col)
  axis(4, at = x[mo.sel, "rain"], labels = round(x[mo.sel, "rain"], 1),
    col.axis = mo.col, col = mo.col)
  rug(x[mos, "rain"], side = 4, col = mo.col)
  lines(stats::lowess(x[mos, "date"], x[mos, "rain"]), col = mo.col,
      lty = "dashed", lwd = 2)
}

temperaturePlot <- function(x, month) {
  if (month == "august") {
    outbreak <- as.Date("1854-09-01") - 1
    suffix <- "-08-31"
  } else if (month == "september") {
    outbreak <- as.Date("1854-10-01") - 1
    suffix <- "-09-30"
  } else stop('month must be "august" or "september".', call. = FALSE)

  sel <- x$date %in% as.Date(paste0(unique(x$year), suffix))
  mo.data <- x[sel, ]
  ttl <- paste0("Monthly High and Low Temperatures in Oxford UK (",
    tools::toTitleCase(month), ")")

  plot(mo.data$date, mo.data$tmax, pch = NA, xlab = "Year",
    ylab = "Celsius", ylim = range(mo.data$tmax, mo.data$tmin), main = ttl)
  axis(3, at = outbreak, labels = format(outbreak, "%b %Y"), padj = 0.9)
  invisible(lapply(seq_len(nrow(mo.data )), function(i) {
    segments(mo.data$date[i], mo.data$tmax[i],
             mo.data$date[i], mo.data$tmin[i], col = "gray")
  }))
  points(mo.data$date, mo.data$tmax, col = "red")
  points(mo.data$date, mo.data$tmin, col = "blue")
  points(mo.data[mo.data$date == outbreak, "date"],
         mo.data[mo.data$date == outbreak, "tmax"], col = "red", pch = 16)
  points(mo.data[mo.data$date == outbreak, "date"],
         mo.data[mo.data$date == outbreak, "tmin"], col = "blue", pch = 16)
  lines(stats::lowess(mo.data$date, mo.data$tmax), col = "red", lty = "dashed",
    lwd = 1.5)
  lines(stats::lowess(mo.data$date, mo.data$tmin), col = "blue",
    lty = "dashed", lwd = 1.5)
  rug(mo.data$tmax, side = 4, col = "red")
  rug(mo.data$tmin, side = 4, col = "blue")
  abline(v = as.Date("1854-09-30"), lty = "dotted")
  abline(h = mo.data[mo.data$date == outbreak, "tmax"], col = "red",
    lwd = 2)
  abline(h = mo.data[mo.data$date == outbreak, "tmin"], col = "blue",
    lwd = 2)
}
