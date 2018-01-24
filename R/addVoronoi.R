#' Add Voronoi cells.
#'
#' Uses deldir::deldir().
#' @param pump.select Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Exclusion (negative selection) is possible (e.g., -6).
#' @param vestry Logical. FALSE for original 13 pumps. TRUE for 14 pumps in Vestry Report.
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
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addVoronoi()

addVoronoi <- function(pump.select = NULL, vestry = FALSE, color = "black",
  line.type = "solid", ...) {

  if (is.null(pump.select)) {
    if (vestry) {
      dat <- cholera::pumps.vestry[, c("x", "y")]
    } else {
      dat <- cholera::pumps[, c("x", "y")]
    }
  } else {
    if (vestry) {
      if (is.numeric(pump.select) == FALSE |
          any(abs(pump.select) %in% 1:14) == FALSE) {

        stop('With "vestry = TRUE", 1 >= |selection| <= 14')
      } else {
        dat <- cholera::pumps.vestry[pump.select, c("x", "y")]
      }
    } else {
      if (is.numeric(pump.select) == FALSE |
          any(abs(pump.select) %in% 1:13) == FALSE) {

        stop('With "vestry = FALSE", 1 >= |selection| <= 13')
      } else {
        dat <- cholera::pumps[pump.select, c("x", "y")]
      }
    }
  }

  cells <- deldir::deldir(dat, rw = c(range(cholera::roads$x),
    range(cholera::roads$y)), suppressMsge = TRUE)

  plot(cells, add = TRUE, wline = "tess", wpoints = "none", col = color,
       lty = line.type)
}
