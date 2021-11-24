#' Highlight road by street name.
#'
#' @param road.name Character vector. Note that \code{streetNameLocator}() tries to correct for case and to remove extra spaces.
#' @param col Character. Highlight color.
#' @param lwd Numeric. Line width.
#' @return A base R graphics segment(s).
#' @export

roadHighlight <- function(road.name, col = "red", lwd = 1) {
  real.road.names <- streetNames()
  vars <- c("x", "y")
  if (!is.character(road.name)) {
    stop("Road name must be a character string.", call. = FALSE)
  } else if (road.name %in% real.road.names == FALSE) {
    case.name <- caseAndSpace(road.name)
    if (case.name %in% real.road.names == FALSE) {
      txt1 <- "Invalid road name. Check spelling or"
      txt2 <- 'see list of road names in streetNames() or vignette("roads").'
      error.msg <- paste(txt1, txt2)
      stop(error.msg, call. = FALSE)
    } else name <- case.name
  } else name <- road.name
  selected.road <- unique(cholera::roads[cholera::roads$name == name, "street"])
  sel <- cholera::roads$street %in% selected.road
  roads.list <- split(cholera::roads[sel, vars], selected.road)
  invisible(lapply(roads.list, lines, col = col, lwd = lwd))
}
