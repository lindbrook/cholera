#' Convert nominal map distance to yards or meters.
#'
#' A best guess estimate.
#' @param x Numeric. Nominal map distance.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @export

unitMeter <- function(x, unit = NULL) {
  if (is.numeric(x) == FALSE) {
    stop('"x" must be numeric.')
  }

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  if (is.null(unit)) {
    x
  } else {
    if (unit == "yard") {
      x * 177 / 3
    } else if (unit == "meter") {
      x * 54
    }
  }
}
