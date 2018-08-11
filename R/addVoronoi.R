#' Add Voronoi cells.
#'
#' @param pump.select Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Exclusion (negative selection) is possible (e.g., -6).
#' @param vestry Logical. \code{FALSE} for original 13 pumps. TRUE for 14 pumps in Vestry Report.
#' @param color Character. Color of cell edges.
#' @param line.type Character. Type of line for cell edges.
#' @param ... Additional plotting parameters.
#' @seealso \code{\link{snowMap}},
#' \code{\link{addIndexCase}},
#' \code{\link{addKernelDensity}},
#' \code{\link{addLandmarks}},
#' \code{\link{addPlaguePit}},
#' \code{\link{addSnow}},
#' \code{\link{addWhitehead}}
#' @note This function uses deldir::deldir().
#' @import graphics
#' @export
#' @examples
#' snowMap()
#' addVoronoi()

addVoronoi <- function(pump.select = NULL, vestry = FALSE, color = "black",
  line.type = "solid", ...) {

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

  cells <- deldir::deldir(pump.data, rw = c(range(cholera::roads$x),
    range(cholera::roads$y)), suppressMsge = TRUE)

  plot(cells, add = TRUE, wline = "tess", wpoints = "none", col = color,
    lty = line.type)
}
