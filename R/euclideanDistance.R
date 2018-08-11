#' Compute the Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @note The function uses a case's "address" (i.e., "anchor" case of a stack) to compute distance. Time is computed using \code{distanceTime()}.
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

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('type must be "case-pump", "cases" or "pumps".')
  }

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('unit must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('time.unit must be "hour", "minute" or "second".')
  }


  obs.ct <- nrow(cholera::fatalities)
  exp.ct <- nrow(cholera::regular.cases)

  if (observed) ct <- obs.ct else ct <- exp.ct

  if (vestry) {
    p.data <- cholera::ortho.proj.pump.vestry
    p.data$street <- cholera::pumps.vestry$street
  } else {
    p.data <- cholera::ortho.proj.pump
    p.data$street <- cholera::pumps$street
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  # ----- #

  if (type == "case-pump") {
    if (origin %in% seq_len(ct) == FALSE) {
      txt1 <- 'With type = "case-pump" and observed = '
      txt2 <- 'origin must be between 1 and '
      stop(txt1, observed, ", ", txt2, ct, ".")
    }

    if (is.null(destination) == FALSE) {
      if (any(abs(destination) %in% p.ID == FALSE)) {
        stop('With vestry = ', vestry, ', 1 >= |destination| <= ', p.count)
      }
    }

  } else if (type == "cases") {
    if (any(abs(c(origin, destination)) %in% seq_len(ct) == FALSE)) {
      txt1 <- 'With type = "cases" and observed = '
      txt2 <- ', the absolute values of origin and of destination must be '
      txt3 <- 'between 1 and '
      stop(txt1, observed, txt2, txt3, ct, ".")
    }

  } else if (type == "pumps") {
    if (origin %in% p.ID == FALSE) {
      stop('With vestry = ', vestry, ', 1 >= |origin| <= ', p.count, ".")
    }

    if (!is.null(destination)) {
      if (any(abs(destination) %in% p.ID == FALSE)) {
        stop('With vestry = ', vestry, ', 1 >= |destination| <= ', p.count,
             ".")
       }
     }
  }

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
