#' Average Winter Temperatures.
#'
#' Gareth Stedman Jones Appendix 2, Table 12, p.384.
#' @export
#' @examples
#' plot(winterTemperatures(), "1859-6-1")

winterTemperatures <- function() {
  # Appendix 2, Table 12 London 1850-1900: Mean Winter Temperatures
  # (Farenheit), p.384.
  # Taken from A. J. Drummond. 1943. "Cold Winters at Kew Observatory".
  # _Quarterly Journal of Royal Meteorological Society_, vol. 69. p. 28.

  Year <- seq(1850, 1900)

  Dec <- c(39.0, 40.2, 41.0, 48.2, 34.8, 40.9, 36.6, 40.4, 45.2, 41.0, 36.7,
    36.4, 40.5, 44.5, 43.3, 39.1, 43.5, 44.0, 37.8, 46.8, 38.6, 34.4, 38.7,
    43.3, 40.9, 33.6, 39.1, 44.5, 41.0, 33.9, 32.7, 43.3, 39.8, 40.3, 40.7,
    41.6, 38.7, 36.5, 38.2, 40.6, 37.7, 30.0, 40.8, 36.6, 40.2, 42.1, 40.4,
    40.1, 40.7, 45.5, 37.0)

  Jan <- c(34.5, 43.2, 41.5, 43.3, 39.8, 35.6, 39.9, 37.0, 31.2, 40.9, 40.1,
    33.7, 39.6, 42.6, 35.8, 37.2, 43.8, 34.3, 38.6, 42.1, 39.8, 33.8, 41.8,
    42.7, 42.1, 44.1, 37.1, 43.3, 40.8, 32.6, 33.2, 31.8, 40.6, 41.8, 43.9,
    37.1, 36.3, 35.7, 38.0, 36.9, 43.7, 34.3, 37.1, 35.7, 38.9, 34.1, 40.9,
    35.9, 43.4, 42.8, 40.4)

  Feb <- c(44.3, 40.5, 40.7, 33.8, 39.8, 29.6, 42.5, 38.5, 35.6, 42.6, 36.0,
    42.3, 42.2, 43.0, 36.5, 37.4, 41.3, 45.7, 44.2, 46.2, 36.8, 42.9, 45.0,
    35.5, 39.3, 35.8, 41.5, 44.5, 42.5, 38.7, 41.9, 38.2, 42.5, 43.0, 42.2,
    43.9, 33.9, 38.8, 35.6, 37.3, 37.8, 37.8, 39.2, 41.6, 41.9, 29.4, 40.4,
    43.6, 41.2, 41.5, 38.4)

  dec.date <- as.Date(paste0(Year - 1, "-12-31"), optional = TRUE)
  jan.date <- as.Date(paste0(Year, "-01-31"), optional = TRUE)
  leap.day <- vapply(Year, leapDay, numeric(1L))
  feb.date <- as.Date(paste0(Year , "-02-", leap.day))

  if (any(is.na(dec.date)) | any(is.na(jan.date))) {
    stop("Invalid Date(s).")
  } else {
    out <- data.frame(date = c(dec.date, jan.date, feb.date),
                      temp = c(Dec, Jan, Feb))
    out <- out[order(out$date), ]
    row.names(out) <- NULL
    out$id <- rep(1:(length(Year)), each = 3)
    class(out) <- c("winterTemperatures", class(out))
    out
  }
}

#' Plot method for winterTemperatures().
#'
#' @param x object.
#' @param end.date Date. "yyyy-mm-dd" or NULL.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @examples
#' plot(winterTemperatures())

plot.winterTemperatures <- function(x, end.date = "1859-6-1", ...) {
  if (!is.null(end.date)) {
    end.date <- as.Date(end.date, optional = TRUE)
    if (!is.na(end.date)) {
      t.data <- x[x$date < end.date, ]
    } else stop("Not a valid date.")
  } else t.data <- x

  plot(t.data$date, t.data$temp, xlab = "Date",
    ylab = "Farenheit",
    main = "Winter Temperatures (Kew Observatory)")
  invisible(lapply(unique(t.data$id), function(z) {
    tmp <- t.data[t.data$id == z, ]
    segments(tmp[1, "date"], tmp[1, "temp"], tmp[2, "date"], tmp[2, "temp"])
    segments(tmp[2, "date"], tmp[2, "temp"], tmp[3, "date"], tmp[3, "temp"])
  }))
  lines(stats::lowess(t.data$date, t.data$temp, f = 1/3), col = "dodgerblue",
    lwd = 2)
  abline(v = as.Date("1854-09-01"), col = "red")
  axis(3, at = as.Date("1854-09-01"), labels = "1854-09-01", padj = 0.9,
    col = "red", col.axis = "red")
}

# Compute Leap Days.
leapDay <- function(x) {
  yr_004 <- x %% 4 == 0
  yr_100 <- x %% 100 == 0
  yr_400 <- x %% 400 == 0
  if (yr_004) {
    if (!yr_100 & !yr_400) last.day.feb <- 29
    else if (yr_100 & !yr_400) last.day.feb <- 28
    else last.day.feb <- 29
  } else last.day.feb <- 28
  last.day.feb
}
