#' Highlight index case at 40 Broad Street.
#'
#' @param latlong Logical.
#' @param cex Numeric. Size of point.
#' @param col Character. Color of point.
#' @param pch Numeric. Type of of point.
#' @param add.label Logical. Add text annotation: "40 Broad Street"
#' @param text.size Numeric. Size of text label.
#' @return Add base R point and (optionally) text to a graphics plot.
#' @export
#' @examples
#' segmentLocator("216-1")
#' addIndexCase()

addIndexCase <- function(latlong = FALSE, cex = 2, col = "red", pch = 1,
  add.label = FALSE, text.size = 0.5) {

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  index.case <- cholera::fatalities[cholera::fatalities$case == 32, vars]
  points(index.case[, vars], col = col, cex = cex, pch = pch)

  if (add.label) {
    text(index.case[, vars], labels = "40 Broad\nStreet", cex = text.size)
  }
}
