#' Add Euclidean path from case/landmark to nearest or selected pump. (prototype)
#'
#' @param origin Numeric. Vector of origin(s) (numeric ID or character name landmark/pump ).
#' @param destination Numeric. Vector of destination(s) (numeric or landmark/pump name).
#' @param type Character. Path case to pump. FALSE is all other combinations of cases, landmarks and pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param latlong Logical.
#' @param case.set Character. "observed" or "expected".
#' @param location Character. For cases and pumps. "nominal", "anchor" or "orthogonal".
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param distance.unit Character. Unit of distance: "meter" or "yard".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param include.landmarks Logical. Include landmarks as cases.
#' @param long.title Logical. Tile with names.
#' @param mileposts Logical. Plot mile/time posts.
#' @param milepost.unit Character. "distance" or "time".
#' @param milepost.interval Numeric. Mile post interval unit of distance (yard or meter) or unit of time (seconds).
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @export

addEuclideanPath <- function(origin = 1, destination = NULL,
  type = "case-pump", vestry = FALSE, latlong = FALSE, case.set = "observed",
  location = "nominal", weighted = TRUE, distance.unit = "meter",
  time.unit = "second", walking.speed = 5, include.landmarks = TRUE,
  long.title = FALSE, mileposts = TRUE, milepost.unit = "distance",
  milepost.interval = NULL, alpha.level = 1) {

  args <- list(origin = origin,
               destination = destination,
               type = type,
               vestry = vestry,
               latlong = latlong,
               case.set = case.set,
               location = location,
               weighted = weighted,
               distance.unit = distance.unit,
               time.unit = time.unit,
               walking.speed = walking.speed,
               include.landmarks = include.landmarks)

  x <- do.call("euclideanPath", args)

  plot(x, add = TRUE, long.title = long.title, mileposts = mileposts,
    milepost.unit = milepost.unit, milepost.interval = milepost.interval,
    alpha.level = alpha.level)
}
