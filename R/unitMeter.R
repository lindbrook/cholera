#' Convert nominal map distance to meters or yards.
#'
#' A best guess estimate.
#' @param x Numeric. Nominal map distance.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" uses the map's nominal scale. See \code{vignette("roads")} for information on conversion.
#' @export

unitMeter <- function(x, distance.unit = "meter") {
  if (is.numeric(x) == FALSE) stop('x must be numeric.')

  sel <- cholera::road.segments$name == "Carnaby Street"
  carnaby <- cholera::road.segments[sel, ]
  carnaby <- carnaby[-c(8:9), ]

  carnaby.ft <- 463
  foot.unit <- carnaby.ft / stLength(carnaby)
  yard.unit <- foot.unit / 3
  meter.unit <- foot.unit / 3.281

  if (distance.unit == "meter") {
    x * meter.unit
  } else if (distance.unit == "yard") {
    x * yard.unit
  } else if (distance.unit == "native") {
    x
  } else stop('distance.unit must be "meter", "yard" or "native".')
}

#' Compute total length of roads.
#'
#' @param dat Object. data frame.
#' @noRd

stLength <- function(dat) {
  d <- vapply(seq_len(nrow(dat)), function(i) {
    p1 <- dat[i, c("x1", "y1")]
    p2 <- dat[i, c("x2", "y2")]
    vars <- c("x", "y")
    names(p1) <- vars
    names(p2) <- vars
    coords <- rbind(p1, p2)
    stats::dist(coords)
  }, numeric(1L))
  sum(d)
}
