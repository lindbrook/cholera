#'  Numeric case IDs by pump neighborhood.
#'
#' @param obj An object created by neighborhoodVoronoi() or neighborhoodWalking().
#' @seealso \code{\link{neighborhoodVoronoi}}, \code{\link{neighborhoodWalking}},
#' @return A list of pump neighborhoods with the numeric ID of cases.
#' @export
#' @examples
#' # pumpCase(neighborhoodVoronoi())
#' # pumpCase(neighborhoodWalking())

pumpCase <- function(obj) UseMethod("pumpCase", obj)

pumpCase.default <- function(obj) NULL

#' @export
pumpCase.voronoi <- function(obj) {
  if (class(obj) != "voronoi") {
    stop('Input object\'s class needs to be "voronoi".')
  }

  output <- obj$statistic.data
  out <- lapply(output, function(x) {
    cholera::fatalities.address$anchor.case[x == 1]
  })

  if (is.null(obj$pump.select)) {
    if (obj$vestry == TRUE) {
      stats::setNames(out, paste0("p", 1:14))
    } else {
      stats::setNames(out, paste0("p", 1:13))
    }
  } else {
    stats::setNames(out, paste0("p", obj$pump.select))
  }
}

#' @export
pumpCase.walking <- function(obj) {
  if (class(obj) != "walking") {
    stop('Input object\'s class needs to be "walking".')
  }

  output <- obj$cases
  stats::setNames(output, paste0("p", names(output)))
}
