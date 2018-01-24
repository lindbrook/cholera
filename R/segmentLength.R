#' Compute length of road segment.
#'
#' @param id Character. A concatenation of a street's numeric ID, a whole number between 1 and 528, and a second number to identify the segment.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale.
#' @return  An R vector of length one.
#' @seealso \code{\link{roads}}, code{\link{road.segments}}, \code{\link{streetNameLocator}}, \code{\link{streetNumberLocator}}, \code{vignette("road.names")}
#' @export
#' @examples
#' segmentLength("242-1")
#' segmentLength("242-1", unit = "meter")

segmentLength <- function(id , unit = NULL) {
  if (is.character(id) == FALSE) {
    stop('"id" type must be character.')
  }

  if (id %in% cholera::road.segments$id == FALSE) {
    stop("Invalid segment ID.")
  }

  dat <- cholera::road.segments[cholera::road.segments$id == id, ]

  distances <- vapply(dat$id, function(i) {
    stats::dist(rbind(as.matrix(dat[dat$id == i, c("x1", "y1")]),
                      as.matrix(dat[dat$id == i, c("x2", "y2")])))
  }, numeric(1L))

  if (is.null(unit)) {
    sum(distances)
  } else if (unit == "yard") {
    cholera::unitMeter(sum(distances), "yard")
  } else if (unit == "meter") {
    cholera::unitMeter(sum(distances), "meter")
  }
}
