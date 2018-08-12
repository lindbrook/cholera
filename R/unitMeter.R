#' Convert nominal map distance to meters or yards.
#'
#' A best guess estimate.
#' @param x Numeric. Nominal map distance.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param yard.unit Numeric. Estimate of yards per map unit.
#' @param meter.unit Numeric. Estimate of meters per map unit:.
#' @export

unitMeter <- function(x, unit = "meter", yard.unit = 177 / 3, meter.unit = 54) {
  if (is.numeric(x) == FALSE) {
    stop('x must be numeric.')
  }

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('unit must be "meter", "yard" or "native".')
  }

  if (unit == "meter") {
    x * meter.unit
  } else if (unit == "yard") {
    x * yard.unit
  } else if (unit == "native") {
    x
  }
}
