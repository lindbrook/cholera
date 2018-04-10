#' Convert nominal map distance to yards or meters.
#'
#' A best guess estimate.
#' @param x Numeric. Nominal map distance.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param yard.unit Numeric. Estimate of yards per map unit: 177 / 3.
#' @param meter.unit Numeric. Estimate of meters per map unit: 54.
#' @export

unitMeter <- function(x, unit = NULL, yard.unit = 177 / 3, meter.unit = 54) {
  if (is.numeric(x) == FALSE) {
    stop('"x" must be numeric.')
  }

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE) {
      stop('If specified, "unit" must be "meter", "yard".')
    }
  }

  if (is.null(unit)) {
    x
  } else {
    if (unit == "yard") x * yard.unit
    else if (unit == "meter") x * meter.unit
  }
}
