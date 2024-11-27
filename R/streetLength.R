#' Compute length of selected street.
#'
#' @param road Character or Numeric. Road name or number. For names, the function tries to correct for case and to remove extra spaces.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @return An R vector of length one.
#' @importFrom geosphere distGeo
#' @export
#' @examples
#' streetLength("Oxford Street")
#' streetLength("oxford street")
#' streetLength("oxford street", distance.unit = "yard")

streetLength <- function(road = "Oxford Street", distance.unit = "meter",
  latlong = FALSE) {

  if (!latlong & !distance.unit %in% c("meter", "yard", "native")) {
    msg1 <- 'If latlong = FALSE,'
    msg2 <- 'distance.unit must be "meter", "yard" or "native".'
    stop(paste(msg1, msg2), call. = FALSE)
  } else if (latlong & !distance.unit %in% c("meter", "yard")) {
    stop('With latlong = TRUE, distance.unit must be "meter" or "yard".',
      call. = FALSE)
  }

  if (latlong) {
    rd.segs <- roadSegments(latlong = latlong)
  } else {
    rd.segs <- cholera::road.segments
  }

  if (is.character(road)) {
    real.road.names <- unique(cholera::roads$name)
    if (road %in% real.road.names == FALSE) {
      case.name <- caseAndSpace(road)
      if (case.name %in% real.road.names == FALSE) {
        error.msg <- paste("Invalid road name.",
          'Check spelling or see list of road names in vignette("roads").')
        stop(error.msg, call. = FALSE)
      } else nm <- case.name
    } else nm <- road

    dat <- rd.segs[rd.segs$name == nm, ]

  } else if (is.numeric(road)) {
    if (road %in% unique(cholera::roads$street) == FALSE) {
      stop("If numeric, road must be 1 >= x <= 528.", call. = FALSE)
    }
    dat <- rd.segs[rd.segs$street == road, ]
  }

  if (latlong) {
    p1 <- dat[, grep(1, names(dat))]
    p2 <- dat[, grep(2, names(dat))]
    distances <- geosphere::distGeo(p1, p2)
    if (distance.unit == "meter") {
      sum(distances)
    } else if (distance.unit == "yard") {
      cholera::meter.to.yard * sum(distances)
    }
  } else {
    distances <- vapply(dat$id, function(i) {
      stats::dist(rbind(as.matrix(dat[dat$id == i, c("x1", "y1")]),
                        as.matrix(dat[dat$id == i, c("x2", "y2")])))
    }, numeric(1L))

    if (distance.unit == "native") {
      sum(distances)
    } else {
      unitMeter(sum(distances), distance.unit)
    }
  }
}
