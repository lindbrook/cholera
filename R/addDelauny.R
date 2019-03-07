#' Add Delauny triangles.
#'
#' @param pump.select Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Exclusion (negative selection) is possible (e.g., -6).
#' @param vestry Logical. \code{FALSE} for original 13 pumps. TRUE for 14 pumps in Vestry Report.
#' @param color Character. Color of triangle edges.
#' @param line.type Character. Type of line for triangle edges.
#' @note This function uses \code{deldir::deldir()}.
#' @import graphics
#' @export
#' @examples
#' snowMap()
#' addDelauny()

addDelauny <- function(pump.select = NULL, vestry = FALSE, color = "black",
  line.type = "solid") {

  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  if (is.null(pump.select)) {
    pump.data <- p.data[, c("x", "y")]
  } else {
    if (is.numeric(pump.select) == FALSE) stop("pump.select must be numeric.")
    if (any(abs(pump.select) %in% p.ID == FALSE)) {
      stop('With vestry = ', vestry, ", 1 >= |pump.select| <= ", p.count, ".")
    }
    pump.data <- cholera::pumps[pump.select, c("x", "y")]
  }

  dat <- deldir::deldir(pump.data, rw = c(range(cholera::roads$x),
    range(cholera::roads$y)), suppressMsge = TRUE)

  plot(dat, add = TRUE, wline = "triang", wpoints = "none", col = color,
    lty = line.type)
}
