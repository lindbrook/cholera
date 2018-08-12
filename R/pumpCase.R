#'  Extract numeric case IDs by pump neighborhood.
#'
#' @param obj An object created by \code{neighborhoodEuclidean()}, \code{neighborhoodVoronoi()} or \code{neighborhoodWalking()}.
#' @seealso \code{\link{neighborhoodVoronoi}}, \code{\link{neighborhoodVoronoi}}, \code{\link{neighborhoodEuclidean}},
#' @return An R list of numeric ID of cases by pump neighborhoods.
#' @export
#' @examples
#' \dontrun{
#'
#' pumpCase(neighborhoodEuclidean())
#' pumpCase(neighborhoodVoronoi())
#' pumpCase(neighborhoodWalking())
#' }

pumpCase <- function(obj) UseMethod("pumpCase", obj)

pumpCase.default <- function(obj) NULL

#' @export
pumpCase.euclidean <- function(obj) {
  if (class(obj) != "euclidean") {
    stop('obj\'s class needs to be "euclidean".')
  }

  pumps <- sort(unique(obj$nearest.pump))
  out <- lapply(pumps, function(p) {
    obj$anchors[obj$nearest.pump == p]
  })

  stats::setNames(out, paste0("p", pumps))
}

#' @export
pumpCase.voronoi <- function(obj) {
  if (class(obj) != "voronoi") {
    stop('obj\'s class needs to be "voronoi".')
  }

  output <- obj$statistic.data
  out <- lapply(output, function(x) {
    cholera::fatalities.address$anchor.case[x == 1]
  })

  if (is.null(obj$pump.select)) {
    if (obj$vestry == TRUE) {
      stats::setNames(out, paste0("p", seq_len(nrow(cholera::pumps.vestry))))
    } else {
      stats::setNames(out, paste0("p", seq_len(nrow(cholera::pumps))))
    }
  } else {
    stats::setNames(out, paste0("p", obj$pump.id))
  }
}

#' @export
pumpCase.walking <- function(obj) {
  if (class(obj) != "walking") {
    stop('obj\'s class needs to be "walking".')
  }

  output <- obj$cases
  stats::setNames(output, paste0("p", names(output)))
}
