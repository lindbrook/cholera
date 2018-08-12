#' Compute length of road segment.
#'
#' @param id Character. A concatenation of a street's numeric ID, a whole number between 1 and 528, and a second number used to identify the sub-segments.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @return  An R vector of length one.
#' @seealso \code{\link{roads}}, \code{\link{road.segments}}, \code{\link{streetNameLocator}}, \code{\link{streetNumberLocator}}, \code{vignette("roads")}
#' @export
#' @examples
#' segmentLength("242-1")
#' segmentLength("242-1", unit = "yard")

segmentLength <- function(id , unit = "meter") {
  if (is.character(id) == FALSE) {
    stop('id\'s type must be character.')
  }

  if (id %in% cholera::road.segments$id == FALSE) {
    stop("Invalid segment ID.")
  }

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('unit must be "meter", "yard" or "native".')
  }

  dat <- cholera::road.segments[cholera::road.segments$id == id, ]

  distances <- vapply(dat$id, function(i) {
    stats::dist(rbind(as.matrix(dat[dat$id == i, c("x1", "y1")]),
                      as.matrix(dat[dat$id == i, c("x2", "y2")])))
  }, numeric(1L))

  if (unit == "native") {
    sum(distances)
  } else if (unit == "yard") {
    cholera::unitMeter(sum(distances), "yard")
  } else if (unit == "meter") {
    cholera::unitMeter(sum(distances), "meter")
  }
}
