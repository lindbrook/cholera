#' Street names (alphabetized).
#'
#' Unique road names from Snow's cholera map.
#' @note See vignette("roads"), and roads and road.segment data frames.
#' @return An R character vector.
#' @export

streetNames <- function() {
  rd.nm <- sort(unique(cholera::roads$name))
  rd.nm[rd.nm != "Map Frame"]
}
