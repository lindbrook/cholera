#' Extract numeric case IDs by neighborhood.
#'
#' @param obj An object created by neighborhoodVoronoi() or neighborhoodWalking().
#' @return A list of pump neighborhoods with the numeric ID of observed cases.
#' @export
#' @examples
#' pumpCases(neighborhoodVoronoi())
#' pumpCases(neighborhoodWalking())

pumpCases <- function(obj) UseMethod("pumpCases", obj)

pumpCases.default <- function(obj) NULL

#' @export
pumpCases.voronoi <- function(obj) {
  if (class(obj) != "voronoi") {
    stop('Input object\'s class needs to be "voronoi".')
  }

  output <- obj$statistic.data

  if (is.null(obj$selection)) {
    if (obj$vestry == TRUE) {
      stats::setNames(output, paste0("p", 1:14))
    } else {
      stats::setNames(output, paste0("p", 1:13))
    }
  } else {
    stats::setNames(output, paste0("p", obj$selection))
  }
}

#' @export
pumpCases.walking <- function(obj) {
  if (class(obj) != "walking") {
    stop('Input object\'s class needs to be "walking".')
  }

  output <- obj$pump.case
  
  if (obj$vestry == TRUE) {
    stats::setNames(output, paste0("p", 1:14))
  } else {
    stats::setNames(output, paste0("p", 1:13))
  }
}
