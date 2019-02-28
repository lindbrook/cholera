#'  Extract numeric case IDs by pump neighborhood.
#'
#' @param x An object created by \code{neighborhoodEuclidean()}, \code{neighborhoodVoronoi()} or \code{neighborhoodWalking()}.
#' @return An R list of numeric ID of cases by pump neighborhoods.
#' @export
#' @examples
#' \dontrun{
#'
#' pumpCase(neighborhoodEuclidean())
#' pumpCase(neighborhoodVoronoi())
#' pumpCase(neighborhoodWalking())
#' }

pumpCase <- function(x) UseMethod("pumpCase", x)

pumpCase.default <- function(x) NULL

#' @export
pumpCase.euclidean <- function(x) {
  pumps <- sort(unique(x$nearest.pump))
  out <- lapply(pumps, function(p) {
    x$anchors[x$nearest.pump == p]
  })

  stats::setNames(out, paste0("p", pumps))
}

#' @export
pumpCase.voronoi <- function(x) {
  output <- x$statistic.data
  out <- lapply(output, function(x) cholera::fatalities.address$anchor[x == 1])

  if (is.null(x$pump.select)) {
    if (x$vestry == TRUE) {
      stats::setNames(out, paste0("p", seq_len(nrow(cholera::pumps.vestry))))
    } else {
      stats::setNames(out, paste0("p", seq_len(nrow(cholera::pumps))))
    }
  } else {
    stats::setNames(out, paste0("p", x$pump.id))
  }
}

#' @export
pumpCase.walking <- function(x) {
  output <- x$cases
  stats::setNames(output, paste0("p", names(output)))
}
