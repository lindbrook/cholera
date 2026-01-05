#' Horizontal/vertical and diaganol distances between regular cases.
#'
#' @param latlong Logical. Use estimated longitude and latitude.
#' @noRd

cardinalDistances <- function(latlong = FALSE) {
  if (latlong) {
    dat <- cholera::latlong.regular.cases[, c("x", "y")]
  } else {
    dat <- cholera::regular.cases
  }
  horizontal <- c(stats::dist(dat[c(1, 2), ])) # East-West
  diag.id <- which(dat$x == dat$x[2] & dat$y == unique(dat$y)[2])
  diagonal <- c(stats::dist(dat[c(1, diag.id), ])) # Origin-Northeast
  stats::setNames(c(horizontal, diagonal), c("horiz", "diag"))
}
