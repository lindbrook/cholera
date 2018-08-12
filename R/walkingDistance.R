#' Compute the shortest walking distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}, which returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of nodes.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. "unit" is meaningful only when "weighted" is \code{TRUE}. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @note The function uses a case's "address" (i.e., "anchor" case of a stack) to compute distance. Time is computed using \code{distanceTime()}. Adam and Eve Court, and Falconberg Court and Falconberg Mews, are disconnected from the larger road network and form two isolated subgraphs. This has two consequences: first, only cases on Adam and Eve Court can reach pump 2 and those cases cannot reach any other pump; second, cases on Falconberg Court and Mews cannot reach any pump. Unreachable pumps will return distances of "Inf".
#' @return An R data frame.
#' @seealso \code{\link{fatalities}}, \code{vignette("pump.neighborhoods")}
#' @export
#' @examples
#' \dontrun{
#'
#' # distance from case 1 to nearest pump.
#' walkingDistance(1)
#'
#' # distance from case 1 to pump 6.
#' walkingDistance(1, 6)
#'
#' # exclude pump 7 from consideration.
#' walkingDistance(1, -7)
#'
#' # distance from case 1 to case 6.
#' walkingDistance(1, 6, type = "cases")
#'
#' # distance from pump 1 to pump 6.
#' walkingDistance(1, 6, type = "pumps")
#' }

walkingDistance <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, weighted = TRUE, vestry = FALSE, unit = "meter",
  time.unit = "second", walking.speed = 5) {

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('type must be "case-pump", "cases" or "pumps".')
  }

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('unit must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('time.unit must be "hour", "minute" or "second".')
  }

  if (observed) {
    node.data <- cholera::neighborhoodData(vestry)
  } else {
    node.data <- cholera::neighborhoodData(vestry, case.set = "expected")
  }

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g

  obs.ct <- nrow(cholera::fatalities)
  exp.ct <- nrow(cholera::regular.cases)

  if (observed) ct <- obs.ct else ct <- exp.ct

  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  if (type == "case-pump") {
    if (origin %in% seq_len(ct) == FALSE) {
      txt1 <- 'With type = "case-pump" and observed = '
      txt2 <- 'origin must be between 1 and '
      stop(txt1, observed, ", ", txt2, ct, ".")
    }

    if (is.null(destination) == FALSE) {
      if (any(abs(destination) %in% p.ID == FALSE)) {
        stop('With vestry = ', vestry, ', 1 >= |destination| <= ', p.count, ".")
      }
    }

  } else if (type == "cases") {
    if (any(abs(c(origin, destination)) %in% seq_len(ct) == FALSE)) {
      txt1 <- 'With type = "cases" and observed = '
      txt2 <- ', the absolute value of origin and destination must be '
      txt3 <- 'between 1 and '
      stop(txt1, observed, txt2, txt3, ct, ".")
    }

  } else if (type == "pumps") {
    if (any(abs(c(origin, destination)) %in% p.ID == FALSE)) {
      txt1 <- 'With type = "pumps" and vestry = '
      txt2 <- ', origin and destination must whole number(s) 1 >= |x| <= '
      stop(txt1, vestry, txt2, p.count, ".")
    }
  }

  arguments <- list(origin = origin,
                    destination = destination,
                    type = type,
                    observed = observed,
                    weighted = weighted,
                    vestry = vestry,
                    unit = unit,
                    time.unit = "second",
                    walking.speed = 5)

  x <- do.call(walkingPath, arguments)
  x$data
}
