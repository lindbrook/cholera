#' Highlight road by name.
#'
#' @param road.name Character vector. The functions tries to correct for case and to remove extra spaces.
#' @return A base R graphics segment(s).
#' @export
#' @examples
#' snowMap()
#' streetHighlight("Broad Street")

streetHighlight <- function(road.name) {
  real.road.names <- unique(cholera::roads$name)
  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)

  if (is.character(road.name) == FALSE) {
    stop("Road name must be a character string.")
  } else if (road.name %in% real.road.names == FALSE) {
    case.name <- caseAndSpace(road.name)
    if (case.name %in% real.road.names == FALSE) {
      txt1 <- "Invalid road name. Check spelling or"
      txt2 <- 'see list of road names in vignette("roads").'
      error.msg <- paste(txt1, txt2)
      stop(error.msg)
    } else name <- case.name
  } else name <- road.name

  selected.road <- cholera::roads[cholera::roads$name == name, "street"]
  invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
    lwd = 3))
}
