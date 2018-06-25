#' Highlight index case at 40 Broad Street.
#'
#' @param cex Numeric. Size of point.
#' @param col Character. Color of point.
#' @param pch Numeric. Type of of point.
#' @param add.label Logical. Add text annotation: "40 Broad Street"
#' @param text.size Numeric. Size of text label.
#' @param ... Additional plotting parameters.
#' @seealso \code{\link{snowMap}},
#' \code{\link{addKernelDensity}},
#' \code{\link{addLandmarks}},
#' \code{\link{addPlaguePit}},
#' \code{\link{addSnow}},
#' \code{\link{addVoronoi}},
#' \code{\link{addWhitehead}}
#' @return Add base R point and (optionally) text to a graphics plot.
#' @export
#' @examples
#' segmentLocator("216-1")
#' addIndexCase()

addIndexCase <- function(cex = 2, col = "red", pch = 1, add.label = FALSE,
  text.size = 0.5, ...) {

  index.case <- cholera::fatalities[cholera::fatalities$case == 32, ]

  if (add.label) {
    text(index.case$x, index.case$y, labels = "40 Broad\nStreet",
      cex = text.size)
  }

  points(index.case$x, index.case$y, col = col, cex = cex, pch = pch)
}
