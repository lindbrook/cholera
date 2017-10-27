#' Compute street length.
#'
#' Computes length of selected named street.
#' @param road.name Character. Road name.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale.
#' @return  An R vector of length one.
#' @seealso \code{\link{roads}}, code{\link{road.segments}}, \code{\link{streetNameLocator}}, \code{\link{streetNumberLocator}}, \code{vignette("road.names")}
#' @export
#' @examples
#' streetLength("Oxford Street")
#' streetLength("oxford street")
#' streetLength("oxford street", unit = "meter")

streetLength <- function(road.name = "Oxford Street", unit = NULL) {
  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  real.road.names <- unique(cholera::roads$name)

  if (is.character(road.name) == FALSE) {
    stop("Road name must be a character string.")
  } else if (road.name %in% real.road.names == FALSE) {
    case.name <- caseAndSpace(road.name)
    if (case.name %in% real.road.names == FALSE) {
      error.msg <- paste("Invalid road name.",
        'Check spelling or see list of road names in vignette("road.names").')
      stop(error.msg)
    } else nm <- case.name
  } else nm <- road.name

  dat <- cholera::road.segments[cholera::road.segments$name == nm, ]

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
