#' Weather data recorded in Oxford (Met Office UK).
#'
#' Add and use last day of month as unit of observation to oxford.weather.
#' @note December 1860 observation is dropped due to missing "tmin" value.
#' @return An R data frame.
#' @export
#' @examples
#' plot(oxfordWeather())

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
#' @examples
#' plot(oxfordWeather())
#' plot(oxfordWeather(), "rain")

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
    if (month == "august") {
      august <- as.Date("1854-09-01") - 1
      aug.sel <- x$date == august
      augs <- x$date %in% as.Date(paste0(unique(x$year), "-08-31"))
      plot(x$date, x$rain, xlab = "Year", ylab = "millimeters", col = "gray",
        main = "Monthly Rainfall in Oxford UK (August)")
      points(x[augs, "date"], x[augs, "rain"], col = "blue", pch = 16)
      axis(3, at = august, labels = format(august, "%b %Y"), padj = 0.9,
        col.ticks = "blue", cex.axis = 0.9, col.axis = "blue")
      abline(v = august, col = "blue", lty = "solid")
      abline(h = x[aug.sel, "rain"], col = "blue", lty = "solid")
      axis(4, at = x[aug.sel, "rain"], labels = round(x[aug.sel, "rain"], 1),
        col.axis = "blue", col = "blue")
      rug(x[augs, "rain"], side = 4, col = "blue")
    } else if (month == "september") {
      september <- as.Date("1854-10-01") - 1
      sep.sel <- x$date == september
      seps <- x$date %in% as.Date(paste0(unique(x$year), "-09-30"))
      plot(x$date, x$rain, xlab = "Year", ylab = "millimeters", col = "gray",
        main = "Rainfall (September)")
      points(x[seps, "date"], x[seps, "rain"], col = "red", pch = 16)
      axis(3, at = september, labels = format(september, "%b %Y"), padj = 0.9,
        col.ticks = "red", cex.axis = 0.9, col.axis = "red")
      abline(v = september, col = "red", lty = "solid")
      abline(h = x[sep.sel, "rain"], col = "red", lty = "solid")
      axis(4, at = x[sep.sel, "rain"], labels = round(x[sep.sel, "rain"], 1),
        col.axis = "red", col = "red")
      rug(x[seps, "rain"], side = 4, col = "red")
    }
  } else stop('statistic must be "temperature" or "rain".')
}
