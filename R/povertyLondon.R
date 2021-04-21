#' Poverty and Born in London.
#'
#' Gareth Stedman Jones, p. 132. Census and Charles Booth Data, 1881.
#' @export

povertyLondon <- function() {
  # London registration districts: proportion of poverty (Booth) and proportion
  # born in London (1881 Census).

  district <- c("Holborn", "St. George's-in-the-East", "Bethnal Green",
    "St. Saviour's", "St. Olave's", "Shoreditch", "Whitechapel", "Stepney",
    "Greenwich", "Poplar", "Westminster", "City", "Islington", "St. Pancras",
    "Camberwell", "Wandsworth", "Marylebone", "St. Giles", "Mile End",
    "Lambeth", "Woolwich", "Fulham", "Kensington", "Chelsea", "Strand",
    "Hackney", "Paddington", "St. George's, Hanover Sq.", "Lewisham",
    "Hampstead")

  poverty <- c(48.9, 48.9, 44.6, 43.4, 42.2, 40.2, 39.2, 38.0, 36.8, 36.5,
    35.0, 31.5, 31.2, 30.4, 28.6, 27.4, 27.4, 26.7, 26.1, 26.1, 24.7, 24.7,
    24.7, 24.5, 23.9, 23.1, 21.7, 21.6, 18.1, 13.5)

  london.born <- c(70.8, 71.38, 83.57, 69.80, 72.60, 76.27, 64.17, 71.55,
    65.5, 66.47, 56.8, 60.5, 63.0, 61.0, 66.5, 58.4, 50.8, 62.8, 73.65, 62.9,
    55.5, 61.1, 43.8, 56.1, 57.0, 67.3, 47.2, 49.5, 54.6, 50.8)

  out <- data.frame(district = district,
                    poverty = poverty,
                    london.born = london.born)
  class(out) <- c("povertyLondon", class(out))
  out
}

#' Plot method for povertyLondon().
#'
#' @param x object.
#' @param district Character. Selected district(s).
#' @param ... Additional plotting parameters.
#' @export

plot.povertyLondon <- function(x, district = c("City", "Westminster",
  "Marylebone", "St. Giles"), ...) {

  sel <- x[x$district %in% district, ]
  tmp <- x[!x$district %in% district, ]
  poor <- x[x$poverty == max(x$poverty), ]
  rich <- x[x$poverty == min(x$poverty), ]
  native <- x[x$london.born == max(x$london.born), ]
  nonnative <- x[x$london.born == min(x$london.born), ]

  plot(x$london.born, x$poverty, pch = NA, xlab = "Percent London Born",
    ylab = "Percent Poverty", main = "1881: Census and Charles Booth Data")
  points(tmp$london.born, tmp$poverty)
  points(sel$london.born, sel$poverty, pch = 0, col = "red")
  lines(stats::lowess(x$london.born, x$poverty), col = "dodgerblue")
  text(sel$london.born[-4], sel$poverty[-4], labels = sel$district[-4], pos = 2,
    col = "red", cex = 0.75)
  text(sel$london.born[4], sel$poverty[4], labels = sel$district[4], pos = 4,
    col = "red", cex = 0.75)
  text(poor$london.born[1], poor$poverty[1], labels = poor$district[1],
    cex = 0.75, pos = 2, col = "gray")
  text(poor$london.born[2], poor$poverty[2], labels = poor$district[2],
    cex = 0.75, pos = 4, col = "gray")
  text(rich$london.born, rich$poverty, labels = rich$district, cex = 0.75,
    pos = 4, col = "gray")
  text(native$london.born, native$poverty, labels = native$district, cex = 0.75,
    pos = 2, col = "gray")
  text(nonnative$london.born, nonnative$poverty, labels = nonnative$district,
    cex = 0.75, pos = 4, col = "gray")
  abline(h = mean(x$poverty), lty = "dotted")
  abline(v = mean(x$london.born), lty = "dotted")
  textDistrict("Hackney", x)
  textDistrict("Mile End", x)
}

textDistrict <- function(district = "Hackney", x) {
  tmp <- x[x$district == district, ]
  text(tmp$london.born, tmp$poverty, cex = 0.75, pos = 1, labels = district,
    col = "gray")
}
