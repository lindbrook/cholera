#' Add Golden and Soho Squares to plot.
#'
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param text.size Numeric. cex for text labels.
#' @param text.col Character. col for text labels.
#' @return Base R points and text.
#' @import graphics
#' @export
#' @examples
#' snowMap()
#' addLandmarkSquares()

addLandmarkSquares <- function(latlong = FALSE, text.size = 0.5,
  text.col = "black") {

  if (latlong) {
    vars <- c("lon", "lat")

    # Golden Square and Soho Square #
    sel <- cholera::landmarks$name %in% c("Golden Square-N", "Golden Square-S")
    golden.NS <- cholera::landmarks[sel, vars]

    sel <- cholera::landmarks$name %in% c("Golden Square-E", "Golden Square-W")
    golden.EW <- cholera::landmarks[sel, vars]

    sel <- cholera::landmarks$name %in% c("Soho Square-N", "Soho Square-S2")
    soho.NS <- cholera::landmarks[sel, vars]

    sel <- cholera::landmarks$name %in% c("Soho Square-E", "Soho Square-W")
    soho.EW <- cholera::landmarks[sel, vars]

    golden <- squareCenter(golden.NS, golden.EW)
    text(golden, labels = "Golden\nSquare", cex = text.size, col = text.col)

    soho <- squareCenter(soho.NS, soho.EW)
    text(soho, labels = "Soho\nSquare", cex = text.size, col = text.col)
  } else {
    # Soho Square
    soho.sq <- data.frame(x = 18.07044, y = 15.85703)
    text(soho.sq$x, soho.sq$y, labels = "Soho\nSquare", cex = text.size,
      col = text.col)

    # Golden Square
    golden.sq <- data.frame(x = 11.90927, y = 8.239483)
    text(golden.sq$x, golden.sq$y, labels = "Golden\nSquare", cex = text.size,
      col = text.col)
  }
}
