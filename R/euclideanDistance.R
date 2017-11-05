#' Compute Euclidean distance from selected case to nearest or selected pump.
#'
#' @param case Numeric or Integer. Case must be a whole number between 1 and 578. To compute distance, the function uses the coordinates of the case's "address" (i.e., its "anchor case").
#' @param pump.select Numeric or Integer vector. For "vestry = FALSE", 1 >= |pump.select| <= 13. For "vestry = TRUE", 1 >= |pump.select| <= 14. Negative values are excluded from consideration. Default is NULL, which returns the closest pump.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @return A base R data frame.
#' @seealso \code{\link{fatalities}}, \code{\link{fatalities.address}}, \code{\link{fatalities.unstacked}}, \code{vignette("pump.neighborhoods")}
#' @export
#' @examples
#' euclideanDistance(1)
#' euclideanDistance(1, 2)
#' euclideanDistance(1, -7)

euclideanDistance <- function(case, pump.select = NULL, vestry = FALSE,
  unit = NULL) {

  if (case %in% 1:578 == FALSE) {
   stop('"case" must be between 1 and 578.')
  }

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  if (!is.null(pump.select)) {
    if (vestry) {
      if (abs(pump.select) %in% 1:14 == FALSE) {
        stop('If "vestry = TRUE", 1 >= |pump.select| <= 14.')
      } else {
        p.data <- cholera::pumps.vestry[pump.select, ]
      }
    } else {
      if (abs(pump.select) %in% 1:13 == FALSE) {
        stop('If "vestry = FALSE", 1 >= |pump.select| <= 13.')
      } else {
        p.data <- cholera::pumps[pump.select, ]
      }
    }
  } else {
    if (vestry) {
      p.data <- cholera::pumps.vestry
    } else {
      p.data <- cholera::pumps
    }
  }

  id <- cholera::anchor.case[cholera::anchor.case$case == case, "anchor.case"]
  c.data <- cholera::fatalities[cholera::fatalities$case == id, c("x", "y")]

  d <- vapply(p.data$id, function(i) {
    c(stats::dist(rbind(p.data[p.data$id == i, c("x", "y")], c.data)))
  }, numeric(1L))

  out <- data.frame(case,
                    distance = d[which.min(d)],
                    pump = p.data[which.min(d), "id"],
                    pump.name = p.data[which.min(d), "street"],
                    stringsAsFactors = FALSE)

  if (!is.null(unit)) {
    if (unit == "yard") {
      out$distance <- out$distance * 177 / 3
    } else if (unit == "meter") {
      out$distance <- out$distance * 54
    }
  }

  out
}
