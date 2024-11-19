#' Generate simulated "regular" fatalities.
#'
#' Places regularly spaced "simulated" or "expected" cases across the face of the map..
#' @param simulated.obs Numeric. Number of "regular" sample cases.
#' @importFrom pracma cart2pol
#' @importFrom sp Polygon
#' @importFrom sp spsample
#' @noRd

regularCases <- function(simulated.obs = 20000L) {
  rd <- cholera::roads[!cholera::roads$street %in% cholera::border, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]

  ## order vertices for polygon functions ##
  map.frame.centered <- data.frame(x = map.frame$x - mean(map.frame$x),
                                   y = map.frame$y - mean(map.frame$y))

  idx <- order(apply(map.frame.centered, 1, pracma::cart2pol)[1, ])
  map.frame <- map.frame[idx, ]

  vars <- c("x", "y")

  sp.frame <- sp::spsample(sp::Polygon(map.frame[, vars]), n = simulated.obs,
    type = "regular")
  regular.cases <- data.frame(sp.frame@coords)
  names(regular.cases) <- vars
  regular.cases
}
