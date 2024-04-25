#' Weather data recorded in Oxford (Met Office UK).
#'
#' Add and use last day of month as unit of observation to oxford.weather.
#' @note December 1860 observation is dropped due to missing "tmin" value.
#' @return An R data frame.
#' @export

oxfordWeather <- function() {
  ox <- cholera::oxford.weather
  ox$date <- as.Date(paste0(ox$year, "-", ox$mo, "-01"))
  class(ox) <- c("oxfordWeather", class(ox))
  ox
}

#' Plot method for oxfordWeather().
#'
#' @param x object.
#' @param statistic Character.
#' @param month Character. "august" or "september".
#' @param end.year Numeric.
#' @param unit.observation Character. "day" or "month".
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.oxfordWeather <- function(x, statistic = "temperature",
  month = "september", end.year = NULL, unit.observation = "month", ...) {

  month <- tolower(month)

  if (!month %in% c("august", "september")) {
    stop('month must be "august" or "september".', call. = FALSE)
  }

  if (!unit.observation %in% c("day", "month")) {
    stop('unit.observation must be "day" or "month".', call. = FALSE)
  }

  if (!is.null(end.year)) {
    if (end.year > min(x$year) & end.year <= max(x$year)) {
      x <- x[x$year <= end.year, ]
    } else stop(min(x$year), " < end.yr <= ", max(x$year))
  }

  if (statistic == "temperature") temperaturePlot(x, month, unit.observation)
  else if (statistic == "rain") rainPlot(x, month, unit.observation)
  else stop('statistic must be "temperature" or "rain".', call. = FALSE)
}

#' @importFrom tools toTitleCase

rainPlot <- function(x, month, unit.observation) {
  if (month == "august") outbreak.mo <- as.Date("1854-08-01")
  else if (month == "september") outbreak.mo <- as.Date("1854-09-01")

  outbreak.sel <- x$date == outbreak.mo
  mo <- as.numeric(format(outbreak.mo, "%m"))
  yr <- as.numeric(format(outbreak.mo, "%Y"))
  rain.outbreak <- x[x$year == yr & x$mo == mo, "rain"]

  if (unit.observation == "month") {
    rain.mo <- x[x$month == mo, "rain"]
    percentile <- 100 * mean(rain.outbreak > rain.mo)
    ttl <- paste0("Total Monthly Rainfall Oxford UK - ",
                  paste0(tools::toTitleCase(month), "(s)"))
  } else if (unit.observation == "day") {
    percentile <- 100 * mean(rain.outbreak > x$rain)
    ttl <- paste("Total Monthly Rainfall Oxford UK -",
                 tools::toTitleCase(month))
  }

  plot(x$date, x$rain, xlab = "Year", ylab = "mm", pch = NA, main = ttl)

  vars <- c("date", "rain")

  if (unit.observation == "month") {
    points(x[x$year != yr & x$mo == mo, vars])
    points(x[x$year == yr & x$mo == mo, vars], col = "red", pch = 16)
    points(x[x$year == yr & x$mo == mo, vars])
  } else if (unit.observation == "day") {
    points(x[x$mo != mo, vars], col = "gray")
    points(x[x$year == yr & x$mo == mo, vars], pch = 16, col = "red")
    points(x[x$year == yr & x$mo == mo, vars])
  }

  axis(3, at = outbreak.mo, labels = "Soho outbreak", padj = 0.9,
    cex.axis = 3/4, col.axis = "red", col.ticks = "red")
  abline(v = outbreak.mo, col = "red")
  abline(h = x[outbreak.sel, "rain"], col = "red")
  axis(4, at = x[outbreak.sel, "rain"],  col.axis = "red", col = "red",
    labels = paste0(round(percentile), "th %"))

  if (unit.observation == "month") {
    rug(rain.mo, side = 4)
    lines(stats::lowess(x[x$mo == mo, "date"], rain.mo), lty = "dashed",
          lwd = 2)
  } else if (unit.observation == "day") {
    rug(x$rain, side = 4)
    lines(stats::lowess(x[, c("date", "rain")]), lty = "dashed", lwd = 2)
  }
}

#' @importFrom tools toTitleCase

temperaturePlot <- function(x, month, unit.observation) {
  if (month == "august") outbreak.mo <- as.Date("1854-08-01")
  else if (month == "september") outbreak.mo <- as.Date("1854-09-01")

  outbreak.sel <- x$date == outbreak.mo
  mo <- as.numeric(format(outbreak.mo, "%m"))
  yr <- as.numeric(format(outbreak.mo, "%Y"))
  temp.outbreak.hi <- x[x$year == yr & x$month == mo, "tmax"]
  temp.outbreak.lo <- x[x$year == yr & x$month == mo, "tmin"]

  if (unit.observation == "month") {
    mo.data <- x[x$month == mo, ]
    temp.hi <- x[x$month == mo, "tmax"]
    temp.lo <- x[x$month == mo, "tmin"]
    percentile.hi <- 100 * mean(temp.outbreak.hi > temp.hi)
    percentile.lo <- 100 * mean(temp.outbreak.lo > temp.lo)
    ttl <- paste0("Monthly Avg Hi and Lo Temperatures in Oxford - ",
                  paste0(tools::toTitleCase(month), "(s)"))
  } else if (unit.observation == "day") {
    percentile.hi <- 100 * mean(temp.outbreak.hi > x$tmax)
    percentile.lo <- 100 * mean(temp.outbreak.lo > x$tmin, na.rm = TRUE)
    ttl <- paste0("Monthly Avg Hi and Lo Temperatures in Oxford - ",
                  tools::toTitleCase(month))
  }

  plot(x$date, x$tmax, pch = NA, xlab = "Year", ylab = "Celsius",
    ylim = range(c(x$tmax, x$tmin), na.rm = TRUE), main = ttl)


  axis(3, at = outbreak.mo, labels = "Soho Outbreak", cex.axis = 3/4,
    padj = 0.9)
  axis(4, at = temp.outbreak.hi, cex.axis = 0.9, padj = -0.9,
    labels = paste0(round(percentile.hi), "th %"))
  axis(4, at = temp.outbreak.lo, cex.axis = 0.9, padj = -0.9,
    labels = paste0(round(percentile.lo), "th %"))

  if (unit.observation == "month") {
    invisible(lapply(seq_len(nrow(mo.data)), function(i) {
      segments(mo.data$date[i], mo.data$tmax[i],
               mo.data$date[i], mo.data$tmin[i], lwd = 0.5)
    }))
    points(mo.data$date, mo.data$tmax, col = "red")
    points(mo.data$date, mo.data$tmin, col = "blue")

  } else if (unit.observation == "day") {
    invisible(lapply(seq_len(nrow(x)), function(i) {
      segments(x$date[i], x$tmax[i],
               x$date[i], x$tmin[i], col = "lightgray")
    }))
  }

  segments(x[x$date == outbreak.mo, "date"],
           x[x$date == outbreak.mo, "tmax"],
           x[x$date == outbreak.mo, "date"],
           x[x$date == outbreak.mo, "tmin"], lwd = 2.5)
  points(x[x$date == outbreak.mo, "date"],
         x[x$date == outbreak.mo, "tmax"], col = "red", pch = 16)
  points(x[x$date == outbreak.mo, "date"],
         x[x$date == outbreak.mo, "tmin"], col = "blue", pch = 16)

  if (unit.observation == "month") {
    lines(stats::lowess(mo.data$date, mo.data$tmin), col = "blue", lwd = 1.5)
    lines(stats::lowess(mo.data$date, mo.data$tmax), col = "red", lwd = 1.5)
    rug(mo.data$tmax, side = 4, col = "red")
    rug(mo.data$tmin, side = 4, col = "blue")
    abline(h = mo.data[mo.data$date == outbreak.mo, "tmax"], col = "red",
      lty = "dashed", lwd = 1.5)
    abline(h = mo.data[mo.data$date == outbreak.mo, "tmin"], col = "blue",
      lty = "dashed", lwd = 1.5)
  } else if (unit.observation == "day") {
    lines(stats::lowess(x$date, x$tmax), col = "red", lwd = 1.5)

    # interpolation for tmin based on tmax
    missing <- which(is.na(x$tmin))
    delta <- x[missing, "tmax"] - min(x[c(missing - 1, missing + 1), "tmax"])
    distance <- abs(x[missing - 1, "tmax"] - x[missing + 1, "tmax"])
    fraction <- delta / distance
    missing.data.length <- abs(x[missing - 1, "tmin"] - x[missing + 1, "tmin"])
    delta.fix <- fraction * missing.data.length
    x2 <- x
    abs.fix <- delta.fix + min(x2[c(missing - 1, missing + 1), "tmin"])
    x2[missing, "tmin"] <- abs.fix
    lines(stats::lowess(x2$date, x2$tmin), col = "blue", lwd = 1.5)

    rug(x$tmax, side = 4, col = grDevices::adjustcolor("red", alpha.f = 0.125))
    rug(x$tmin, side = 4, col = grDevices::adjustcolor("blue", alpha.f = 0.125))
    abline(h = x[x$date == outbreak.mo, "tmax"], col = "red", lty = "dashed",
      lwd = 1.5)
    abline(h = x[x$date == outbreak.mo, "tmin"], col = "blue", lty = "dashed",
      lwd = 1.5)
  }
}
