#' Compute length of selected street.
#'
#' @param road Character or Numeric. Road name or number. For names, the function tries to correct for case and to remove extra spaces.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @return  An R vector of length one.
#' @seealso \code{\link{roads}}, code{\link{road.segments}}, \code{\link{streetNameLocator}}, \code{\link{streetNumberLocator}}, \code{vignette("roads")}
#' @export
#' @examples
#' streetLength("Oxford Street")
#' streetLength("oxford street")
#' streetLength("oxford street", unit = "yard")

streetLength <- function(road = "Oxford Street", unit = "meter") {
  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('unit must be "meter", "yard" or "native".')
  }

  if (is.character(road)) {
    real.road.names <- unique(cholera::roads$name)
    if (road %in% real.road.names == FALSE) {
      case.name <- caseAndSpace(road)
      if (case.name %in% real.road.names == FALSE) {
        error.msg <- paste("Invalid road name.",
          'Check spelling or see list of road names in vignette("roads").')
        stop(error.msg)
      } else nm <- case.name
    } else nm <- road

    dat <- cholera::road.segments[cholera::road.segments$name == nm, ]

  } else if (is.numeric(road)) {
    if (road %in% unique(cholera::roads$street) == FALSE) {
      stop("If numeric, road must be 1 >= x <= 528.")
    }
    dat <- cholera::road.segments[cholera::road.segments$street == road, ]
  }

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
