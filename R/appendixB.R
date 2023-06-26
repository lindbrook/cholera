#' Appendix B of Vestry Report.
#'
#' @note p.177 of Vestry Report.
#' @noRd

appendixB <- function() {
  ## Appendix B data p.177 of Vestry Report ##

  obs.ct <- 72L # number of observations (rows or streets)

  ## Table Data ##

  jul_26 <- c(rep(0, 68), 1, rep(0, 3))
  aug_03 <- c(rep(0, 29), 1, rep(0, 42))
  aug_04 <- rep(0, obs.ct)
  aug_05 <- c(rep(0, 39), 1, rep(0, 32))
  aug_06 <- rep(0, obs.ct)

  aug_07 <- c(rep(0, 61), 1, rep(0, 10))
  aug_08 <- rep(0, obs.ct)
  aug_09 <- rep(0, obs.ct)
  aug_10 <- rep(0, obs.ct)
  aug_11 <- c(rep(0, 31), 1, rep(0, 9), 1, rep(0, 30))

  aug_12 <- c(rep(0, 27), 1, rep(0, 33), 1, rep(0, 4), 1, rep(0, 5))
  aug_13 <- rep(0, obs.ct)
  aug_14 <- c(rep(0, 28), 1, rep(0, 10), 1, rep(0, 4), 1, rep(0, 27))
  aug_15 <- rep(0, obs.ct)
  aug_16 <- c(rep(0, 10), 1, rep(0, 9), 1, rep(0, 44), 1, rep(0, 6))

  aug_17 <- c(rep(0, 3), 1, rep(0, 68))
  aug_18 <- c(rep(0, 28), 1, rep(0, 37), 1, rep(0, 5))
  aug_19 <- c(rep(0, 10), 1, rep(0, 17), 1, rep(0, 43))
  aug_20 <- c(rep(0, 28), 1, rep(0, 43))
  aug_21 <- c(rep(0, 39), 1, rep(0, 25), 1, rep(0, 6))

  aug_22 <- rep(0, obs.ct)
  aug_23 <- c(rep(0, 28), 1, rep(0, 6), 1, rep(0, 36))
  aug_24 <- rep(0, obs.ct)
  aug_25 <- rep(0, obs.ct)
  aug_26 <- c(rep(0, 62), 1, rep(0, 9))

  aug_27 <- rep(0, obs.ct)
  aug_28 <- c(rep(0, 27), rep(1, 2), rep(0, 32), 1, rep(0, 10))
  aug_29 <- c(rep(0, 46), 1, rep(0, 5), 1, rep(0, 19))
  aug_30 <- c(rep(0, 27), 1, rep(0, 3), 1, rep(0, 12), 1, rep(0, 27))
  aug_31 <- c(3, rep(0, 7), 1, rep(0, 2), 2, 1, 0, 2, 1, rep(0, 3), rep(1, 2),
    rep(0, 2), 2, rep(1, 2), 3, 6, 2, 1, rep(0, 6), 1, 0, 1, 2, rep(0, 4), 1,
    rep(0, 5), 1, rep(0, 21))

  sep_01 <- c(11, rep(0, 3), 1, 2, 3, 0, 1, 0, rep(5, 2), 9, 1, 3, rep(4, 2), 1,
    rep(0, 2), 5, rep(3, 2), 1, 4, 6, 4, 26, 7, 3, 0, 3, rep(0, 3), 5, 1,
    rep(0, 2), 3, 0, 1, rep(0, 5), 1, 0, 6, 3, 2, 0, 1, 2, rep(0, 3), 1,
    rep(0, 5), 1, rep(0, 7))

  sep_02 <- c(3, rep(0, 3), 1, 2, 1, 0, 1, 0, 6, 3, 4, 0, rep(1, 2), rep(2, 2),
    rep(0, 2), 5, 0, rep(1, 2), 3, 5, 3, 24, 6, 5, 2, 4, rep(0, 2), 2, 8, 2, 6,
    0, 6, rep(0, 6), 1, rep(0, 2), 4, 7, 0, 1, 0, 2, 1, rep(0, 3), 1, rep(0, 3),
    1, rep(0, 8))

  sep_03 <- c(4, rep(0, 3), rep(1, 2), rep(0, 2), rep(1, 2), rep(2, 2), 4, 0,
    rep(1, 2), rep(0, 4), 1, 0, 3, 1, 2, rep(1, 2), 9, rep(0, 2), 3, 1, 0, 1,
    rep(0, 3), 1, 0, 1, 0, 1, rep(0, 5), rep(1, 2), 6, 3, 0, 3, 1, 2,
    rep(0, 12), 1, rep(0, 4))

  sep_04 <- c(3, rep(0, 3), rep(1, 2), rep(0, 2), 1, rep(0, 2), 2, 3, rep(2, 2),
    0, 3, rep(0, 3), 2, 0, 1, 0, 3, 1, 2, 8, 3, rep(1, 2), rep(0, 2), 1,
    rep(0, 3), 2, 0, 1, rep(0, 8), 2, rep(1, 2), 2, 1, 0, 2, 1, rep(0, 4), 1,
    rep(0, 11))

  sep_05 <- c(rep(0, 4), 1, rep(0, 3), 3, 0, 3, rep(0, 3), 1, rep(0, 8), 1,
    rep(0, 3), 6, 1, rep(2, 2), rep(0, 2), 1, rep(0, 3), 1, rep(0, 11),
    rep(1, 2), rep(0, 3), rep(1, 2), rep(0, 16))

  sep_06 <- c(rep(0, 4), rep(1, 2), rep(0, 3), 2, rep(1, 2), rep(0, 5), 1,
    rep(0, 2), 3, rep(0, 3), 1, rep(0, 2), 5, 3, rep(0, 12), 1, rep(0, 5), 1, 0,
    3, rep(0, 4), 3, rep(0, 5), 1, rep(0, 11))

  sep_07 <- c(rep(0, 8), 1, 0, rep(1, 2), rep(0, 2), 1, 0, 1, 0, 1, rep(0, 2),
    rep(1, 3), 0, 1, rep(0, 2), 3, 0, rep(1, 2), rep(0, 4), 1, rep(0, 3), 1,
    rep(0, 2), rep(1, 2), rep(0, 4), 1, rep(0, 2), 1, rep(0, 17), 1, 0)

  sep_08 <- c(rep(0, 10), rep(1, 2), rep(0, 2), 1, 0, 2, rep(0, 3), 1,
    rep(0, 5), 1, 2, 1, rep(0, 8), 1, rep(0, 4), 1, rep(0, 11), 2, rep(0, 17))

  sep_09 <- c(rep(0, 20), 1, rep(0, 6), 1, rep(0, 3), 1, rep(0, 18), 1,
    rep(0, 3), 2, rep(0, 17))

  sep_10 <- c(rep(0, 21), 1, rep(0, 4), 1, rep(0, 45))
  sep_11 <- c(rep(0, 39), 1, rep(0, 10), 2, rep(0, 21))
  sep_12 <- c(rep(0, 14), 1, rep(0, 57))
  sep_13 <- c(rep(0, 10), 1, rep(0, 12), 1, rep(0, 15), 1, rep(0, 32))
  sep_14 <- rep(0, obs.ct)

  sep_15 <- c(rep(0, 14), 1, rep(0, 57))
  sep_16 <- c(rep(0, 11), 1, rep(0, 8), 1, rep(0, 48), 1, rep(0, 2))
  sep_17 <- c(rep(0, 10), 1, rep(0, 2), 1, rep(0, 47), 1, rep(0, 9), 1)
  sep_18 <- rep(0, obs.ct)
  sep_19 <- c(rep(0, 20), 1, rep(0, 51))

  sep_20 <- rep(0, obs.ct)
  sep_21 <- rep(0, obs.ct)
  sep_22 <- c(rep(0, 18), 1, 0, 1, rep(0, 51))
  sep_23 <- rep(0, obs.ct)
  sep_24 <- c(rep(0, 20), 1, rep(0, 51))

  sep_25 <- rep(0, obs.ct)
  sep_26 <- c(rep(0, 13), 1, rep(0, 58))
  sep_27 <- rep(0, obs.ct)
  sep_28 <- c(rep(0, 2), 1, rep(0, 27), 1, rep(0, 41))
  sep_29 <- rep(0, obs.ct)

  sep_30 <- rep(0, obs.ct)
  oct_01 <- c(rep(0, 63), 1, rep(0, 8))

  #

  # Daily (column) totals from Appendix B
  daily.tot <- c(1, 1, 0, 1, 0, 1, 0, 0, 0, 2,
                 3, 0, 3, 0, 3, 1, 2, 2, 1, 2,
                 0, 2, 0, 0, 1, 0, 3, 2, 3, 34,
                 142, 128, 62, 55, 26, 28, 22, 14, 6, 2,
                 3, 1, 3, 0, 1, 3, 4, 0, 1, 0,
                 0, 2, 0, 1, 0, 1, 0, 2, 0, 0,
                 1)

  # Road (row) totals from Appendix B
  road.tot <- c(24, 0, rep(1, 2), 6, 7, 4, 0, 9, 3, 23, 18, 21, 5, 14, 7, 12, 4,
    2, 1, 24, 5, 9, 8, 14, rep(15, 2), 90, 32, 13, 10, 12, 0, 3, 2, 14, 5, 11,
    1, 18, 1, 4, rep(1, 2), 4, 0, 2, rep(3, 2), 22, 19, 4, 7, 2, 16, 3,
    rep(0, 2), rep(1, 2), 2, 4, 1, 2, 1, rep(2, 2), rep(1, 5))

  ## Dates ##

  aug.dates <- vapply(3:31, function(x) {
    if (nchar(x) == 1) paste0("aug_0", x)
    else paste0("aug_", x)
  }, character(1L))

  sep.dates <- vapply(1:30, function(x) {
    if (nchar(x) == 1) paste0("sep_0", x)
    else paste0("sep_", x)
  }, character(1L))

  vec.dates <- c("jul_26", aug.dates, sep.dates, "oct_01")

  #

  jul <- as.Date("1854-07-26")
  aug_oct <- seq(as.Date("1854-08-03"), as.Date("1854-10-01"), by = 1)
  cal.dates <- c(jul, aug_oct)

  ## check for uniform length of dates (columns) ##
  # date.list <- vector("list", length(vec.dates))
  # test <- lapply(seq_along(vec.dates), function(i) {
  #   date.list[[i]] <- get(vec.dates[i])
  # })
  # all(vapply(test, length, numeric(1L)) == 72L)  # OK

  ## Data Assembly ##

  appendix <- data.frame(matrix(0, obs.ct, length(vec.dates)))

  for (i in seq_along(vec.dates)) {
    appendix[, i] <- get(vec.dates[i])
  }

  names(appendix) <- format(cal.dates, "%b-%d")
  appendix
}

## row (street) count audit ##
# app <- appendixB()
# road.tmp <- rowSums(app)
# row.test <- data.frame(obs = road.tot, calc = road.tmp)
# row.test[row.test$obs != row.test$calc, ]
