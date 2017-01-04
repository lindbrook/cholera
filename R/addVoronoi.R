#' Add Voronoi cells.
#'
#' Computes and draws Voronoi cells using deldir::deldir().

#' @param select Numeric. Default is NULL and all pumps are used. Ortherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param vestry Logical. FALSE for original 13 pumps. TRUE for 14 pumps in Vestry Report.
#' @param col Character. Color of borders.
#' @param lty Character. Type of line for borders.
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addVoronoi()

addVoronoi <- function(select = NULL, vestry = FALSE, col = "black",
  lty = "solid") {

  if (is.null(select)) {
    if (vestry) {
      dat <- cholera::pumps.vestry[, c("x", "y")]
    } else {
      dat <- cholera::pumps[, c("x", "y")]
    }
  } else {
    if (vestry) {
      if (is.numeric(select) == FALSE | any(abs(select) %in% 1:14) == FALSE) {
        stop('With "vestry = TRUE", 1 >= |selection| <= 14')
      } else {
        dat <- cholera::pumps.vestry[select, c("x", "y")]
      }
    } else {
      if (is.numeric(select) == FALSE | any(abs(select) %in% 1:13) == FALSE) {
        stop('With "vestry = FALSE", 1 >= |selection| <= 13')
      } else {
        dat <- cholera::pumps[select, c("x", "y")]
      }
    }
  }

  cells <- deldir::deldir(dat, rw = c(range(cholera::roads$x),
    range(cholera::roads$y)), suppressMsge = TRUE)
  plot(cells, add = TRUE, wline = "tess", wpoints = "none", col = col,
       lty = lty)
}
