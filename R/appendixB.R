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

  # Supplementary Rows (by date) #

  tot.daily.fatalities <- c(1, 1, 0, 1, 0, 1, 0, 0, 0, 2,
                            3, 0, 3, 0, 3, 1, 2, 2, 1, 2,
                            0, 2, 0, 0, 1, 0, 3, 2, 3, 34,
                            142, 128, 62, 55, 26, 28, 22, 14, 6, 2,
                            3, 1, 3, 0, 1, 3, 4, 0, 1, 0,
                            0, 2, 0, 1, 0, 1, 0, 2, 0, 0,
                            1)

  Daily.Deaths.streets.above <- c(rep(0, 3), rep(1, 3), rep(0, 3), 1, rep(2, 2),
    0, 1, rep(2, 2), 1, 3, 0, 3, 1, 0, 3, rep(0, 2), 1, 0, rep(2, 2), 4, 72,
    127, 76, 71, 45, 40, 34, 30, 24, 18, 15, 7, 13, 6, 8, 6, 5, rep(4, 2), 1, 0,
    rep(3, 2), 0, 1, 2, 0, 2, 1, 0, 1) # 652

  Daily.Deaths.rest.of.london <- c(20, rep(78, 2), 100, 103, 77, 107, 96, 83,
    104, 96, 106, 116, 89, 113, 123, 96, rep(118, 2), 128, 130, 131, 137, 118,
    100, 121, 141, 135, 185, 207, 317, 332, 253, 231, 222, 219, 201, 185, 235,
    238, 218, 239, 190, 152, 200, 217, 185, 175, 204, 166, 142, 156, 134, 120,
    103, 104, 90, 78, 74, 66, 69)

  Daily.Deaths.all.of.london <- c(20, rep(78, 2), 101, 104, 78, 107, 96, 83,
    105, 98, 108, 116, 90, 115, 125, 97, 121, 118, rep(131, 3), 140, 118, 100,
    122, 141, 137, 187, 211, 389, 159, 329, 305, 267, 250, 235, 215, 259, 256,
    233, 246, 203, 158, 208, 223, 190, 179, 208, 167, 142, 159, 137, 129, 104,
    106, 90, 80, 75, 66, 70)

  # Supplementary Columns (by road) #

  zone <- c(0, rep(1, 4), rep(2, 38), rep(3, 14), rep(4, 14))

  known.attack <- c(24, 0, rep(1, 2), 6, 7, 4, 0, 9, 3, 23, 18, 21, 5, 14, 7,
    12, 4, 2, 1, 24, 5, 9, 8, 14, rep(15, 2), 90, 32, 13, 10, 12, 0, 3, 2, 14,
    5, 11, 1, 18, 1, 4, rep(1, 2), 4, 0, 2, rep(3, 2), 22, 19, 4, 7, 2, 16, 3,
    rep(0, 2), rep(1, 2), 2, 4, 1, 2, 1, rep(2, 2), rep(1, 5))

  total.ascertained <- c(43, rep(1, 3), 7, 9, 4, 1, 10, 5, 25, 23, 22, 5, 22, 9,
    15, 4, 3, 1, 28, 8, 13, 8, 15, rep(17, 2), 90, 49, 14, 12, 16, 2, 3, 2, 18,
    5, 15, 1, 24, 1, 4, rep(1, 2), 4, 1, 2, 3, 5, 22, 26, 4, 7, 2, 20, 5,
    rep(1, 2), 2, 1, 2, 5, 1, 2, 1, rep(2, 3), rep(1, 3), 2)

  population <- c(592, 415, 55, 68, 443, 602, 444, 27, 525, 58, 976, 327, 802,
    403, 0, 160, 769, 95, 26, 56, 535, 78, 70, 120, 230, 150, 179, 896, 951,
    176, 325, 223, 60, 54, 38, 965, 137, 367, 58, 587, 15, 1005, 1092, 410, 233,
    245, 106, 105, 68, 528, 620, 530, 623, 109, 650, 224, 0, 76, 348, 81, 810,
    610, 94, 161, 200, 151, rep(0, 2), 130, 26, 318, 163)

  mortality.pct <- c(7.2, 0.2, 1.8, 1.4, 1.5, 1.4, 0.9, 3.7, 1.9, 8.6, 2.5, 7.0,
    2.7, 1.0, 0, 5.6, 1.9, 4.2, 11.5, 1.7, 5.2, 10.2, 18.5, 6.6, 6.5, 11.3,
    9.4, 10.0, 5.1, 7.9, 3.6, 7.1, 3.3, 5.5, 5.2, 1.8, 3.6, 4.0, 1.7, 4.0, 6.6,
    0.3, 0.1, 0.2, 1.7, 0.4, 1.8, 2.8, 7.3, rep(4.1, 2), 0.7, 1.1, 1.8, 3.0,
    2.6, 0, 1.3, 0.5, 1.2, 0.2, 0.8, 1.0, 1.2, 0.5, 1.3, rep(0, 2), 0.7, 3.8,
    0.3, 1.2)

  inhabited.houses <- c(32, 57, 9, 5, 55, 62, 27, 3, 33, 5, 63, 14, 64, 1, 0, 9,
    61, 9, 7, 4, 35, 7, 3, 5, 11, 7, 14, 49, 52, 7, 18, 11, 6, 3, 4, 58, 16, 14,
    4, 48, 3, 63, 127, 18, 12, 24, 10, 7 ,10, 41, 35, 57, 39, 12, 29, 11, 0, 12,
    25, 8, 63, 51, 6, 13, 8, 18, rep(0, 2), 26, 3, 45, 19)

  fatality.houses <- c(21, rep(1, 3), 4, 7, 2, 1, 7, 4, 15, 9, 12, 1, 0, 5, 10,
    rep(3, 2), 1, 14, 4, rep(3, 2), 8, 6, 7, 35, 23, 6, 7, 8, 1, rep(2, 2), 13,
    3, 5, 1, 18, 1, 3, rep(1, 2), 3, 1, 2, 3, 4, 13, 14, 1, 6, 2, 7, 4, 0, 1, 2,
    1, 2, 4, 1, 2, 1, rep(2, 3), rep(1, 4))

  rd.name.short <- c("St Anns Court", "Oxford Street", "Marlborough Mews", 
    "Marlborough Street", "Great Marlborough Street", "Wardour Street (North)",
    "Noel Street", "Phillips Court", "Portland Street", "Portland Mews", 
    "Berwick Street (North)", "Bentinck Street", "Poland Street", 
    "St James Workhouse (inmates)", "St James Workhouse (casualties)", 
    "Dufours Place", "Wardour Street (South)", "Edward Street", "Duck Lane", 
    "Tyler Court", "Berwick Street (South)", "Kemps Court", "Hopkins Street", 
    "Husband Street",  "New Street", "Pulteney Court", "Cambridge Street", 
    "Broad Street", "Marshall Street", "South Row", "West Street", 
    "Marlborough Row", "Tyler Court", "Marlborough Court", "Cross Court", 
    "Carnaby Street", "Tyler Street", "Cross Street", "George Place", 
    "Silver Street", "Naylors Yard", "King Street", "Regent Street", 
    "Heddon Street", "Heddon Court", "Leicester Street", "Upper John Street", 
    "Upper James Street", "Bridle Lane", "Great Pulteney Street", 
    "Little Windmill Street", "Brewer Street", "Little Pulteney Street", 
    "William and Mary Yard", "Peter Street", "Greens Court", "Pulteney Place", 
    "Walkers Court", "Great Crown Court", "George Court", "Rupert Street", 
    "Great Windmill Street", "Queens Head Court", "Ham Yard", "Angel Court", 
    "Swallow Street", "Picadilly", "Jermyn Street", "St James's Market", 
    "Eagle Place", "Duke Street", "Crown Court")

  rd.name.check <- c(rep(1, 7), 0, rep(1, 61), rep(0, 2), 1)

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
