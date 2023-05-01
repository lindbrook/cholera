#' Highlight road by name.
#'
#' @param road.name Character vector. The function tries to correct for case and remove extra spaces (includes "Map Frame").
#' @param col Character. Highlight color.
#' @param lwd Numeric. Line width.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @return A base R graphics segment(s).
#' @export
#' @examples
#' snowMap()
#' streetHighlight("Broad Street")

streetHighlight <- function(road.name, col = "red", lwd = 3, latlong = FALSE) {
  real.road.names <- unique(cholera::roads$name)

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  roads.list <- split(cholera::roads[, vars], cholera::roads$street)

  if (is.character(road.name) == FALSE) {
    stop("Road name must be a character string.", call. = FALSE)
  } else if (road.name %in% real.road.names == FALSE) {
    case.name <- caseAndSpace(road.name)
    if (case.name %in% real.road.names == FALSE) {
      txt1 <- "Invalid road name. Check spelling or"
      txt2 <- 'see list of road names in vignette("roads").'
      error.msg <- paste(txt1, txt2)
      stop(error.msg, call. = FALSE)
    } else name <- case.name
  } else name <- road.name

  selected.road <- cholera::roads[cholera::roads$name == name, "street"]
  invisible(lapply(roads.list[paste(selected.road)], lines, col = col,
    lwd = lwd))
}
