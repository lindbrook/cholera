#' Compute the shortest walking distance between cases and/or pumps.
#'
#' @param origin Numeric or Character. Numeric ID of case or pump. Character landmark name.
#' @param destination Numeric or Character. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}, which returns closest pump or "anchor" case. Character landmark name.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of nodes.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. "unit" is meaningful only when "weighted" is \code{TRUE}. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @note The function uses a case's "address" (i.e., "anchor" case of a stack) to compute distance. Time is computed using \code{distanceTime()}. Adam and Eve Court, and Falconberg Court and Falconberg Mews, are disconnected from the larger road network and form two isolated subgraphs. This has two consequences: first, only cases on Adam and Eve Court can reach pump 2 and those cases cannot reach any other pump; second, cases on Falconberg Court and Mews cannot reach any pump. Unreachable pumps will return distances of "Inf".
#' @return An R data frame.
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
#'
#' # for multiple cases.
#' lapply(1:3, walkingDistance)
#' }

walkingDistance <- function(origin = 1, destination = NULL, type = "case-pump",
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

  if (observed) {
    ct <- nrow(cholera::fatalities)
  } else {
    ct <- nrow(cholera::regular.cases)
  }

  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  # ----- #

  if (type == "case-pump") {
    if (is.numeric(origin)) {
      if (is.numeric(origin)) {
        if (origin %in% seq_len(ct) == FALSE) {
          txt1 <- 'With type = "case-pump" and observed = '
          txt2 <- ', origin must be between 1 and '
          stop(txt1, observed, ", ", txt2, ct, ".")
        }
      }
    } else if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      origin.test <- origin %in% cholera::landmarks.squares$name == FALSE &
                     origin %in% cholera::landmarks$name == FALSE
      if (origin.test) stop("Use a valid landmark name.")
    }

    if (!is.null(destination)) {
      if (is.numeric(destination)) {
        if (any(abs(destination) %in% p.ID == FALSE)) {
          txt1 <- 'With type = "case-pump" and vestry = '
          txt2 <- ', destination must a whole number 1 >= |x| <= '
          stop(txt1, vestry, txt2, p.count, ".")
        }
      }
    }

  # ----- #

  } else if (type == "cases") {
    if (is.numeric(origin)) {
      if (origin %in% seq_len(ct) == FALSE) {
        txt1 <- 'With type = "cases" and observed = '
        txt2 <- ', the origin and destination must be '
        txt3 <- 'between 1 and '
        stop(txt1, observed, txt2, txt3, ct, ".")
      }
    } else if (is.character(origin)) {
      origin <- caseAndSpace(origin)
      origin.test <- origin %in% cholera::landmarks.squares$name == FALSE &
                     origin %in% cholera::landmarks$name == FALSE
      if (origin.test) stop("Use a valid landmark name.")
    }

    if (is.null(destination) == FALSE) {
      if (is.numeric(destination)) {
        if (abs(destination) %in% seq_len(ct) == FALSE) {
          txt1 <- 'With type = "cases" and observed = '
          txt2 <- ', the absolute value of destination must be '
          txt3 <- 'between 1 and '
          stop(txt1, observed, txt2, txt3, ct, ".")
        }
      } else if (is.character(destination)) {
        destination <- caseAndSpace(destination)
        A <- destination %in% cholera::landmarks.squares$name == FALSE
        B <- destination %in% cholera::landmarks$name == FALSE
        destination.test <- A & B
        if (destination.test) stop("Use a valid landmark name.")
      }
    }

  # ----- #

  } else if (type == "pumps") {
    if (origin %in% p.ID == FALSE) {
      stop('With vestry = ', vestry, ', origin <= ', p.count, ".")
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
                    weighted = weighted,
                    vestry = vestry,
                    unit = unit,
                    time.unit = "second",
                    walking.speed = 5)

  output <- do.call(walkingPath, arguments)
  class(output) <- "walking_distance"
  output
}

#' Print method for walkingDistance().
#'
#' Summary output.
#' @param x An object of class "walking_distance" created by walkingDistance().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export
#' @examples
#' \dontrun{
#'
#' walkingDistance()
#' print(walkingDistance())
#' }

print.walking_distance <- function(x, ...) {
  if (class(x) != "walking_distance") {
    stop('"x"\'s class must be "walking_distance".')
  }

  print(x[["data"]])
}

#' Plot method for walkingDistance().
#'
#' plot output.
#' @param x An object of class "walking_distance" created by walkingDistance().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export

plot.walking_distance <- function(x, ...) {
  if (class(x) != "walking_distance") {
    stop('"x"\'s class must be "walking_distance".')
  }

  message("To plot path, use plot(walkingPath()).")
}
