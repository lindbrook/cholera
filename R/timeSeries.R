#' John Snow and Committee data from Vestry report.
#'
#' Long, ggplot2-style data layout.
#' @export

timeSeries <- function() {
  snow <- snowTimeSeries()
  vestry <- vestryTimeSeries()
  
  stat <- c(rep("deaths", nrow(snow)), rep("fatal.attacks", nrow(snow)))
  s2 <- data.frame(snow[, c("date", "day", "source")], statistic = stat,
    count = c(snow$deaths, snow$fatal.attacks))

  stat <- c(rep("deaths", nrow(vestry)),
            rep("fatal.attacks", nrow(vestry)))
  v2 <- data.frame(vestry[, c("date", "day", "source")], statistic = stat,
    count = c(vestry$deaths, vestry$fatal.attacks))
  
  dat <- rbind(s2, v2)
  dat$data <- paste0(dat$source, "-", dat$statistic)
  output <- list(data = dat)
  class(output) <- "time_series"
  output
}

#' Plot aggregate time series data from Vestry report.
#'
#' Plot aggregate fatality data and indicates the date of the removal of the handle of the Broad Street pump.
#' @param x Data frame of four fatality datasets.
#' @param all.data Logical. Use all 4 datasets.
#' @param statistic Character. Fatality measure:  "fatal.attacks" or "deaths".
#' @param vestry Logical. TRUE = Vestry Report; FALSE = John Snow.
#' @param multi.plot Logical. Multiple data sets in single plot. Meaningful only when `all.data = TRUE`.
#' @param main Character. Title of graph.
#' @param points Logical. Plot points.
#' @param pump.handle Logical. Plot date of removal of Broad Street pump handle.
#' @param weekend Logical. Highlight weekend.
#' @param xlab Character. x-axis label.
#' @param ylab Character. y-axis label.
#' @param ... Additional plotting parameters.
#' @export

plot.time_series <- function(x, all.data = FALSE, statistic = "fatal.attacks", 
  vestry = FALSE, multi.plot = FALSE, 
  main = "Removal of the Broad Street Pump Handle", points = TRUE,
  pump.handle = TRUE, weekend = FALSE, xlab = "Date", ylab = NULL, ...) {
  
  dat <- x$data
  vars <- c("date", "count")
  
  if (all.data) {
    xlim <- range(dat$date)
    ylim <- range(dat$count)

    if (multi.plot == TRUE) {
      if (is.null(ylab)) {
        if (statistic == "deaths") ylab <- "Deaths"
        else if (statistic == "fatal.attacks") ylab  <- "Fatal Attacks"
      }
      
      plot(dat$date, dat$count, pch = NA, xlab = "Date", ylab = "Count",
        xlim = xlim, ylim = ylim)
      data.nm <- unique(dat$data)
      data.col <- c("black", "#1B9E77", "#E7298A", "#1F78B4")
      names(data.col) <- data.nm

      invisible(lapply(data.nm, function(d) {
        sel <- dat$data == d
        lines(dat[sel, vars], col = data.col[d])
        wknd <- dat$day %in% c("Saturday", "Sunday")

        if (points & weekend) {
          points(dat[sel & !wknd, vars], col = data.col[d])
          points(dat[sel & wknd, vars], pch = 16, col = "red")
          legend(x = "topright",
               legend = "Weekend",
               col = "red",
               pch = 16,
               bg = "white",
               cex = 3/4,
               bty = "n",
               title = NULL)
        } else if (!points & weekend) {
          points(dat[sel & wknd, vars], pch = 16, col = "red")
          legend(x = "topright",
             legend = "Weekend",
             col = "red",
             pch = 16,
             bg = "white",
             cex = 3/4,
             bty = "n",
             title = NULL)
        } else if (points & !weekend) {
          points(dat[sel, vars], col = data.col[d])
        }
      }))
      
      legend(x = "topleft",
             legend = data.nm,
             col = data.col,
             lty = "solid",
             bg = "white",
             cex = 3/4,
             bty = "n",
             title = NULL)
      
      title(main = main)
      if (pump.handle) pumpHandle()

    } else {
      grDevices::devAskNewPage(ask = TRUE)
    
      invisible(lapply(unique(dat$data), function(d) {
        sel <- dat$data == d
        ylab <- unique(dat[sel, "statistic"])
        
        if (weekend) {
          plot(dat[sel, "date"], dat[sel, "count"], pch = NA, type = "l", 
            xlab = "Date", ylab = ylab, xlim = xlim, ylim = ylim)
          wknd <- dat$day %in% c("Saturday", "Sunday")
          points(dat[sel & wknd, vars], pch = 16, col = "red")
          if (points) points(dat[sel & !wknd, vars])

          legend(x = "topleft",
                 legend = "Weekend",
                 col = "red",
                 pch = 16,
                 bg = "white",
                 cex = 3/4,
                 bty = "n",
                 title = NULL)
        } else {
          plot(dat[sel, "date"], dat[sel, "count"], type = "l", xlab = xlab,
            ylab = ylab, xlim = xlim, ylim = ylim)
          if (points) points(dat[sel, vars])
        }

        source <- tools::toTitleCase(unlist(strsplit(d, "-"))[1])
        title(main = paste0(main, ": ", source))
        if (pump.handle) pumpHandle()
      }))
      
      grDevices::devAskNewPage(ask = FALSE)
    }
    
  } else if (all.data == FALSE) {
    prefix <- ifelse(vestry, "vestry", "snow")
    sel <- dat$data == paste0(prefix, "-", statistic)
    
    if (statistic == "fatal.attacks") {
      ylab <- "Fatal Attacks"
    } else if (statistic == "deaths") {
      ylab <- "Deaths"
    }

    if (weekend) {
      wknd <- dat$day %in% c("Saturday", "Sunday")
      plot(dat[sel, "date"], dat[sel, "count"], pch = NA, type = "l", 
        ylab = ylab)
      points(dat[sel & wknd, vars], pch = 16, col = "red")
      if (points) points(dat[sel & !wknd, vars])
      
      legend(x = "topleft",
                 legend = "Weekend",
                 col = "red",
                 pch = 16,
                 bg = "white",
                 cex = 3/4,
                 bty = "n",
                 title = NULL)
    } else {
      plot(dat[sel, "date"], dat[sel, "count"], type = "l", xlab = xlab,
        ylab = ylab)
      if (points) points(dat[sel, vars])
    }
    title(main = paste0(main, ": ", tools::toTitleCase(prefix)))
    if (pump.handle) pumpHandle()
  }
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
  if (!inherits(x, "time_series")) {
    stop('x\'s class needs to be "time_series".')
  }
  print(x$data)
}

vestryTimeSeries <- function() {
  month <- c("July", rep("Aug", length(3:31)), rep("Sep", length(1:30)), "Oct")
  day <- c(26, 3:31, 1:30, 1)
  month.num <- c(7, rep(8, length(3:31)), rep(9, length(1:30)), 10)

  deaths <- c(rep(0, 3), rep(1, 3), rep(0, 3), 1, 2, 2, 0, 1, 2, 2, 1,
    3, 0, 3, 1, 0, 3, 0, 0, 1, 0, 2, 2, 4, 72, 127, 76, 71, 45, 40, 34, 30,
    24, 18, 15, 7, 13, 6, 8, 6, 5, 4, 4, 1, 0, 3, 3, 0, 1, 2, 0, 2, 1, 0, 1)

  fatal.attacks <- c(1, 1, 0, 1, 0, 1, rep(0, 3), 2, 3, 0, 3, 0, 3, 1,
    2, 2, 1, 2, 0, 2, 0, 0, 1, 0, 3, 2, 3, 34, 142, 128, 62, 55, 26, 28, 22,
    14, 6, 2, 3, 1, 3, 0, 1, 3, 4, 0, 1, 0, 0, 2, 0, 1, 0, 1, 0, 2, 0, 0, 1)

  calendar.date <- as.Date(paste0(1854L, "-", month.num, "-", day))

  data.frame(date = calendar.date, day = weekdays(calendar.date), deaths,
    fatal.attacks, source = "vestry")
}

snowTimeSeries <- function() {
  # Note: entry for an additional 45 fatal attacks with "Date unknown".
  yr <- 1854L
  mo <- c(rep("08", length(19:31)), rep("09", length(1:30)))
  day <- c(19:31, 1:30)

  deaths <- c(1, 0, 2, 0, 0, 2, 0, 0, 1, 0, 1, 2, 3, 70, 127, 76, 71, 45, 37,
    32, 30, 24, 18, 15, 6, 13, 6, 8, 6, 5, 2, 3, 0, 0, 2, 3, 0, 0, 2, 0, 2,
    1, 0)

  fatal.attacks <- c(rep(1, 3), 0, rep(1, 2), 0, rep(1, 4), 8, 56, 143, 116,
    54, 46, 36, 20, 28, 12, 11, 5, 5, 1, 4, 0, 1, 4, 2, 3, 0, 0, 2, rep(1, 6),
    rep(0, 3))

  calendar.date <- as.Date(paste0(yr, "-", mo, "-", day))

  data.frame(date = calendar.date, day = weekdays(calendar.date), deaths,
    fatal.attacks, source = "snow")
}

pumpHandle <- function(col = "black") {
  abline(v = as.Date("1854-09-08"), col = col)
  axis(3, at = as.Date("1854-09-08"), labels = "Sep 08", cex.axis = 0.8,
    line = -0.5, col.axis = col, col.ticks = col)
}
