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

  if (statistic == "temperature") {
    temps <- data.frame(date = rep(x$date, 2),
                        hi.lo = unlist(x[, c("tmax", "tmin")]))
    plot(x$date, x$tmax, pch = NA, ylim = range(x[, c("tmax", "tmin")]),
      xlab = "Year", ylab = "Celsius")
    title(main = "Monthly High and Low Temperatures, Oxford UK")
    sel <- x$date %in% sept
    points(x[!sel, "date"], x[!sel, "tmin"], pch = 0)
    points(x[!sel, "date"], x[!sel, "tmax"], pch = 16, col = "gray")
    points(x[sel, "date"], x[sel, "tmin"], pch = 15, col = "red")
    points(x[sel, "date"], x[sel, "tmax"], pch = 16, col = "red")
    invisible(lapply(seq_len(nrow(x)), function(i) {
      segments(x$date[i], x$tmax[i], x$date[i], x$tmin[i], col = "gray")
    }))
    invisible(lapply(which(sel), function(i) {
      segments(x$date[i], x$tmax[i], x$date[i], x$tmin[i], col = "red", lwd = 2)
    }))
    abline(v = sept, col = "red", lty = "dotted")
    abline(h = x[x$date %in% outbreak, "tmax"], col = "red")
    axis(3, at = outbreak, labels = format(outbreak, "%b %Y"), padj = 0.9,
      col.ticks = "red", cex.axis = 0.9)
    axis(3, at = sept[-2], labels = NA, padj = 0.9, col.ticks = "red",
      cex.axis = 0.75)
  } else if (statistic == "rain") {
    rainPlot(x, month)
  } else stop('statistic must be "temperature" or "rain".', call. = FALSE)
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
}
