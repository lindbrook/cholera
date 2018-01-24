#' Compute length of selected street.
#'
#' @param road Character or Numeric. Road name or number. For names, the function tries to correct for case and to remove extra spaces.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale.
#' @return  An R vector of length one.
#' @seealso \code{\link{roads}}, code{\link{road.segments}}, \code{\link{streetNameLocator}}, \code{\link{streetNumberLocator}}, \code{vignette("road.names")}
#' @export
#' @examples
#' streetLength("Oxford Street")
#' streetLength("oxford street")
#' streetLength("oxford street", unit = "meter")

streetLength <- function(road = "Oxford Street", unit = NULL) {
  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  if (is.character(road)) {
    real.road.names <- unique(cholera::roads$name)
    if (road %in% real.road.names == FALSE) {
      case.name <- caseAndSpace(road)
      if (case.name %in% real.road.names == FALSE) {
        error.msg <- paste("Invalid road name.",
          'Check spelling or see list of road names in vignette("road.names").')
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

  if (is.null(unit)) {
    sum(distances)
  } else if (unit == "yard") {
    cholera::unitMeter(sum(distances), "yard")
  } else if (unit == "meter") {
    cholera::unitMeter(sum(distances), "meter")
  }
}
