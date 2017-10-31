#' Compute Euclidean distance between a case and a pump.
#'
#' @param case Numeric or Integer. Case must be a whole number between 1 and 578. To compute distance, the function uses the coordinates of the case's "address" (i.e., the "anchor case").
#' @param pump.select Numeric or Integer. 1 to 13 for "vestry = FALSE"; 1 to 14 for "vestry = TRUE".
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale.
#' @return A base R data frame.
#' @export
#' @examples
#' euclideanDistance(1, 7)

euclideanDistance <- function(case, pump.select, vestry = FALSE, unit = NULL) {

  if (case %in% 1:578 == FALSE) {
   stop('"case" must be between 1 and 578.')
  }

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  if (vestry) {
    if (case %in% 1:14 == FALSE) {
     stop('If "vestry = TRUE", "case" must be between 1 and 14.')
   } else {
     p.data <- cholera::pumps.vestry
   }
  } else {
    if (case %in% 1:13 == FALSE) {
     stop('If "vestry = FALSE", "case" must be between 1 and 13.')
   } else {
     p.data <- cholera::pumps
   }
  }

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  id <- cholera::anchor.case[cholera::anchor.case$case == case, "anchor.case"]
  c.data <- cholera::fatalities[cholera::fatalities$case == id, c("x", "y")]

  d <- c(stats::dist(rbind(p.data[pump.select, c("x", "y")], c.data)))

  if (is.null(unit)) {
    data.frame(case = case,
               pump = pump.select,
               pump.name = p.data[pump.select, "street"],
               distance = d)
  } else if (unit == "yard") {
    data.frame(case = case,
               pump = pump.select,
               pump.name = p.data[pump.select, "street"],
               distance = d * 177 / 3)
  } else if (unit == "meter") {
    data.frame(case = case,
               pump = pump.select,
               pump.name = p.data[pump.select, "street"],
               distance = d * 54)
  }
}
