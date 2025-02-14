#'  Extract numeric case IDs by pump neighborhood.
#'
#' @param x An object created by \code{neighborhoodEuclidean()}, \code{neighborhoodVoronoi()} or \code{neighborhoodWalking()}.
#' @param case Character. "address" or "fatality"
#' @return An R list of numeric ID of cases by pump neighborhoods.
#' @export
#' @examples
#' \dontrun{
#' pumpCase(neighborhoodEuclidean())
#' pumpCase(neighborhoodVoronoi())
#' pumpCase(neighborhoodWalking())
#' }

pumpCase <- function(x, case) UseMethod("pumpCase", x)

#' @export
pumpCase.default <- function(x, case) NULL

#' @export
pumpCase.euclidean <- function(x, case = "anchor") {
  pumps <- sort(unique(x$nearest.pump))
  out <- lapply(pumps, function(p) {
    cholera::fatalities$case[x$nearest.pump == p]
  })
  if (case == "anchor") {
    out <- lapply(out, function(x) {
      x[x %in% cholera::fatalities.address$anchor]
    })
  } else if (case != "fatality") stop('case must be "anchor" or "fatality".')
  stats::setNames(out, paste0("p", pumps))
}

#' @export
pumpCase.euclidean_latlong <- function(x, case = "anchor") {
  census <- lapply(x$statistic.data, function(x) which(x == 1))
  if (case == "anchor") {
    census <- lapply(census, function(x) {
      x[x %in% cholera::fatalities.address$anchor]
    })
  } else if (case != "fatality") stop('case must be "anchor" or "fatality".')
  census
}

#' @export
pumpCase.voronoi <- function(x, case = "anchor") {
  output <- x$statistic.data
  if (case == "orthogonal") {
    lapply(output, function(x) cholera::ortho.proj$case[x == 1])
  } else if (case == "anchor") {
    lapply(output, function(x) cholera::fatalities.address$anchor[x == 1])
  } else if (case == "fatality") {
    case.pump <- lapply(output, function(x) cholera::fatalities$case[x == 1])
    lapply(case.pump, function(vec) {
      sort(cholera::anchor.case[cholera::anchor.case$anchor %in% vec, "case"])
    })
  }
}

#' @export
pumpCase.walking <- function(x, case = "anchor") {
  if (case == "fatality") {
    lapply(x$case.pump, function(vec) {
      sort(cholera::anchor.case[cholera::anchor.case$anchor %in% vec, "case"])
    })
  } else if (case == "anchor") {
    x$case.pum
  } else stop('case must either be "anchor" or "fatality"', call. = FALSE)
}
