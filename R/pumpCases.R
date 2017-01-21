#' Numerical case IDs by Voronoi neighborhood.
#'
#' @param obj An object of class "voronoi" created by neighborhoodVoronoi().
#' @param statistic Character. "address" summarizes the number of addresses in pump neighbohoods. "fatality", the default, summarizes the number of fatalities in pump neighborhoods.
#' @return A list of pump neighborhoods with the numeric ID of observed cases.
#' @export
#' @examples
#' dat <- neighborhoodVoronoi()
#' pumpCases(dat)
#'
#' pumpCases(dat, statistic = "address")

pumpCases <- function(obj, statistic) UseMethod("pumpCases", obj)

pumpCases.default <- function(obj, statistic) NULL

#' @export
pumpCases.voronoi <- function(obj, statistic = "fatality") {
  if (class(obj) != "voronoi") {
    stop('Input object\'s class needs to be "voronoi".')
  }

  statistic <- match.arg(statistic, choices = c("address", "fatality"))

  if (statistic == "address") {
    output <- lapply(obj$coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
        cholera::fatalities.address$y, cell$x, cell$y)
    })
  } else if (statistic == "fatality") {
    output <- lapply(obj$coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.unstacked$x,
        cholera::fatalities.unstacked$y, cell$x, cell$y)
    })
  }

  lapply(output, function(x) which(x == 1))
}
