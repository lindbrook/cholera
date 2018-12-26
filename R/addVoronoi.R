#' Add Voronoi cells.
#'
#' @param pump.select Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Exclusion (negative selection) is possible (e.g., -6).
#' @param vestry Logical. \code{FALSE} for original 13 pumps. TRUE for 14 pumps in Vestry Report.
#' @param case.location Character. For observed = FALSE: "address" or "nominal". "nominal" is the x-y coordinate of \code{regular.cases}.
#' @param color Character. Color of cell edges.
#' @param line.type Character. Type of line for cell edges.
#' @param ... Additional plotting parameters.
#' @note This function uses \code{deldir::deldir()}.
#' @import graphics
#' @export
#' @examples
#' snowMap()
#' addVoronoi()

addVoronoi <- function(pump.select = NULL, vestry = FALSE,
  case.location = "address", color = "black", line.type = "solid", ...) {

  if (case.location == "address") {
    if (vestry) {
      p.data <- cholera::ortho.proj.pump.vestry
      p.data$street <- cholera::pumps.vestry$street
    } else {
      p.data <- cholera::ortho.proj.pump
      p.data$street <- cholera::pumps$street
      names(p.data)[names(p.data) %in% c("x.proj", "y.proj")] <- c("x", "y")
    }
  } else if (case.location == "nominal") {
    if (vestry) {
      p.data <- cholera::pumps.vestry
    } else {
      p.data <- cholera::pumps
    }
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

  plot(dat, add = TRUE, wline = "tess", wpoints = "none", col = color,
    lty = line.type, ...)
}
