#' Compute road length.
#'
#' Computes length of selected road.
#' @param road.name Character. Road name.
#' @param unit Character. Unit of measurement.
#' @return An R vector of length one.
#' @export

roadLength <- function(road.name = "Oxford Street", unit = NULL) {
  real.road.names <- unique(cholera::roads$name)

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  dat <- cholera::road.segments[cholera::road.segments$name == road.name, ]

  distances <- vapply(dat$id, function(i) {
    stats::dist(rbind(as.matrix(dat[dat$id == i, c("x1", "y1")]),
                      as.matrix(dat[dat$id == i, c("x2", "y2")])))
  }, numeric(1L))

  if (is.null(unit)) {
    sum(distances)
  } else if (unit == "yard") {
    sum(distances) * 177 / 3
  } else if (unit == "meter") {
    sum(distances) * 54
  }
}
