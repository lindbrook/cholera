#' Compute length of selected street.
#'
#' @param road Character or Numeric. Road name or number. For names, the function tries to correct for case and to remove extra spaces.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @return An R vector of length one.
#' @export
#' @examples
#' streetLength("Oxford Street")
#' streetLength("oxford street")
#' streetLength("oxford street", distance.unit = "yard")

streetLength <- function(road = "Oxford Street", distance.unit = "meter") {
  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
    stop('distance.unit must be "meter", "yard" or "native".')
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

  if (distance.unit == "native") {
    sum(distances)
  } else if (distance.unit == "yard") {
    unitMeter(sum(distances), "yard")
  } else if (distance.unit == "meter") {
    unitMeter(sum(distances), "meter")
  }
}
