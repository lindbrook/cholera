#' Compute the Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is NULL: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Default is 5 km/hr.
#' @note The function uses a case's "address" (i.e., "anchor" case of a stack) to compute distance. Time is computed using distanceTime().
#' @return An R data frame.
#' @export
#' @examples
#' # path from case 1 to nearest pump.
#' euclideanDistance(1)
#'
#' # path from case 1 to pump 6.
#' euclideanDistance(1, 6)
#'
#' # exclude pump 7 from consideration.
#' euclideanDistance(1, -7)
#'
#' # path from case 1 to case 6.
#' euclideanDistance(1, 6, type = "cases")
#'
#' # path from pump 1 to pump 6.
#' euclideanDistance(1, 6, type = "pumps")

euclideanDistance <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, vestry = FALSE, unit = "meter", time.unit = "second",
  walking.speed = 5) {

  arguments <- list(origin = origin,
                    destination = destination,
                    type = type,
                    observed = observed,
                    vestry = vestry,
                    unit = unit,
                    time.unit = "second",
                    walking.speed = 5)

  x <- do.call(euclideanPath, arguments)
  x$data
}
