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
pumpCase.euclidean <- function(x, case = "address") {
  pumps <- sort(unique(x$nearest.pump))
  out <- lapply(pumps, function(p) {
    x$anchors[x$nearest.pump == p]
  })

  stats::setNames(out, paste0("p", pumps))
}

#' @export
pumpCase.voronoi <- function(x, case = "orthogonal") {
  output <- x$statistic.data
  if (x$location == "orthogonal") {
    lapply(output, function(x) cholera::ortho.proj$case[x == 1])
  } else if (x$location == "nominal") {
    lapply(output, function(x) cholera::fatalities.address$anchor[x == 1])
  }
}

#' @export
pumpCase.walking <- function(x, case = "address") {
  if (case == "address") {
    x$cases
  } else if (case == "fatality") {
    lapply(x$cases, function(dat) {
      cholera::anchor.case[cholera::anchor.case$anchor %in% dat, "case"]
    })
  } else stop('case must either be "address" or "fatality"')
}
