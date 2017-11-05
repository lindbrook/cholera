#' Compute Euclidean distance between pumps or between cases.
#'
#' For case to pump(s), use \code{\link{euclideanDistance}}.
#' @param origin Numeric or Integer. Numeric ID of origin.
#' @param destination Numeric or Integer. Numeric ID of destination.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @param type Character "cases" or "pumps"
#' @note For "type = "cases"", "origin" and "destination" must be a number between 1 and 578. To compute distance, the function uses the coordinates of the case's "address" (i.e., its "anchor case"). For type = "pumps", "origin" and "destination" must be numbers between 1 and 14 for vestry = TRUE; for vestry = FALSE
#' @return An R vector.
#' @export
#' @examples
#' # Pairwise Euclidean distance between pumps.
#'
#' # pairs <- combn(cholera::pumps$id, 2, simplify = FALSE)
#' #
#' # vapply(pairs, function(x) {
#' #   euclideanDistance2(x[1], x[2])
#' # }, numeric(1L))

euclideanDistance2 <- function(origin, destination, vestry = FALSE,
  unit = NULL, type = "pumps") {

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  if (type == "pumps") {
    if (vestry) {
      if (any(c(origin, destination) %in% 1:14 == FALSE)) {
        stop('If "vestry = TRUE", 1 >= pump.select <= 14.')
      } else if (all(c(origin, destination) %in% 1:13)) {
        p.data <- cholera::pumps.vestry
      }
    } else {
      if (any(c(origin, destination) %in% 1:13 == FALSE)) {
        stop('If "vestry = FALSE", 1 >= pump.select <= 13.')
      } else if (all(c(origin, destination) %in% 1:13)) {
        p.data <- cholera::pumps
      }
    }

    dat <- rbind(p.data[p.data$id == origin, c("x", "y")],
                 p.data[p.data$id == destination, c("x", "y")])

  } else if (type == "cases") {
    if (any(c(origin, destination) %in% 1:578 == FALSE)) {
       stop('"origin" and "destination" must be between 1 and 578.')
    }

    id1 <- cholera::anchor.case[cholera::anchor.case$case == origin,
      "anchor.case"]
    id2 <- cholera::anchor.case[cholera::anchor.case$case == destination,
      "anchor.case"]
    case1 <- cholera::fatalities[cholera::fatalities$case == id1, c("x", "y")]
    case2 <- cholera::fatalities[cholera::fatalities$case == id2, c("x", "y")]
    dat <- rbind(case1, case2)
  }

  if (!is.null(unit)) {
    if (unit == "yard") {
      c(stats::dist(dat)) * 177 / 3
    } else if (unit == "meter") {
      c(stats::dist(dat)) * 54
    }
  } else c(stats::dist(dat))
}
