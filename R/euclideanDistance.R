#' Compute Euclidean distance to nearest pump or selected pump.
#'
#' @param case Numeric or Integer. Case must be a whole number between 1 and 578. To compute distance, the function uses the coordinates of the case's "address" (i.e., its "anchor case").
#' @param pump.select Numeric or Integer. 1 to 13 for "vestry = FALSE"; 1 to 14 for "vestry = TRUE". Defult is NULL: finds closest pump.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale.
#' @return A base R data frame.
#' @seealso \code{\link{fatalities}}, \code{\link{fatalities.address}}, \code{\link{fatalities.unstacked}}, \code{vignette("pump.neighborhoods")}
#' @export
#' @examples
#' euclideanDistance(1)
#' euclideanDistance(1, 2)

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
      if (pump.select %in% 1:14 == FALSE) {
        stop('If "vestry = TRUE", "pump.select" must be between 1 and 14.')
      } else {
        p.data <- cholera::pumps.vestry
      }
    } else {
      if (pump.select %in% 1:13 == FALSE) {
        stop('If "vestry = FALSE", "pump.select" must be between 1 and 13.')
      } else {
        p.data <- cholera::pumps
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

  if (is.null(pump.select) == FALSE) {
    d <- c(stats::dist(rbind(p.data[pump.select, c("x", "y")], c.data)))
    out <- data.frame(case,
                      distance = d,
                      pump = p.data[p.data$id == pump.select, "id"],
                      pump.name = p.data[p.data$id == pump.select, "street"],
                      stringsAsFactors = FALSE)
  } else {
    ds <- vapply(p.data$id, function(i) {
      c(stats::dist(rbind(p.data[p.data$id == i, c("x", "y")], c.data)))
    }, numeric(1L))
    out <- data.frame(case,
                      distance = ds[which.min(ds)],
                      pump = p.data[which.min(ds), "id"],
                      pump.name = p.data[which.min(ds), "street"],
                      stringsAsFactors = FALSE)
  }

  if (!is.null(unit)) {
    if (unit == "yard") {
      out$distance <- out$distance * 177 / 3
    } else if (unit == "meter") {
      out$distance <- out$distance * 54
    }
  }

  out
}
