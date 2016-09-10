#' Locate fatality by numerical case ID.
#'
#' Plots John Snow's map of the 1854 London cholera outbreak and highlights the
#' selected case.
#' @param case Whole number between 1 and 578. Need not be of integer type.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom. Default value is 2.
#' @param stacked Logical. TRUE uses \code{fatalities} ("stacked"); FALSE uses \code{fatalities.address} ("unstacked").
#' @return A base R graphics plot.
#' @seealso \code{fatalities}
#'
#' \code{fatalities.address}
#'
#' \url{http://r.789695.n4.nabble.com/no-visible-binding-for-global-variable-for-data-sets-in-a-package-td4696053.html}
#' @import graphics
#' @export
#' @examples
#' fatalityLocator(290)
#' fatalityLocator(290, zoom = TRUE)
#' fatalityLocator(290, stacked = FALSE)
#' fatalityLocator(290, zoom = TRUE, stacked = FALSE)

fatalityLocator <- function(case, zoom = FALSE, radius = 2, stacked = TRUE) {
  if (is.numeric(case) == FALSE) {
    stop("case must be numeric.")
  }

  if (case %in% unique(cholera::fatalities$case) == FALSE) {
    stop("case must lie between 1 and 578.")
  }

  if (zoom == FALSE) {
    x.rng <- range(cholera::roads$x)
    y.rng <- range(cholera::roads$y)
  } else {
    x.rng <- c(cholera::fatalities[cholera::fatalities$case == case, "x"] -
                 radius,
               cholera::fatalities[cholera::fatalities$case == case, "x"] +
                 radius)
    y.rng <- c(cholera::fatalities[cholera::fatalities$case == case, "y"] -
                 radius,
               cholera::fatalities[cholera::fatalities$case == case, "y"] +
                 radius)
  }

  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)

  if (stacked == TRUE) {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = 15, cex = 0.5, col = "gray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(HistData::Snow.pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(HistData::Snow.pumps[, c("x", "y")], label = HistData::Snow.pumps$pump,
      pos = 1)
    points(cholera::fatalities[cholera::fatalities$case == case, c("x", "y")],
      col = "red", lwd = 2)
    title(main = paste("Case #", case))
  } else {
    case.b <- cholera::anchor.case[cholera::anchor.case$case == case,
      "anchor.case"]
    roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)
    plot(cholera::fatalities.address[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = 15,cex = 0.5, col = "gray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(HistData::Snow.pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(HistData::Snow.pumps[, c("x", "y")], label = HistData::Snow.pumps$pump,
      pos = 1)
    points(cholera::fatalities.address[cholera::fatalities.address$anchor.case
      == case.b, c("x", "y")], col = "red", lwd = 2)
    title(main = paste("Case #", case))
  }
}
