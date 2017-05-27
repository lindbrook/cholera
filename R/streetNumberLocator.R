#' Locate road by its numerical ID.
#'
#' Plots John Snow's map of the 1854 London cholera outbreak and highlights the
#' selected road. See cholera::roads for numerical IDs and \code{vignette}("road.names") for details.
#' @param road.number Numeric or integer. A whole number between 1 and 528.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom. For values <= 5, the anchor case number is plotted.
#' @return A base R graphics plot.
#' @seealso \code{\link{roads}}, \code{\link{road.segments}}, \code{\link{streetNameLocator}}, \code{vignette("road.names")}
#' @import graphics
#' @export
#' @examples
#' streetNumberLocator(243)
#' streetNumberLocator(243, zoom = TRUE)
#' streetNumberLocator(243, zoom = TRUE, radius = 0)

streetNumberLocator <- function(road.number, zoom = FALSE, radius = 1) {
  if (is.numeric(road.number) == FALSE) {
    stop("road.number must be numeric.")
  }

  if (road.number %in% unique(cholera::roads$street) == FALSE) {
    stop("road.number must lie between 1 and 528.")
  }

  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)
  rng <- lapply(cholera::roads[cholera::roads$street == road.number,
       c("x", "y")], range)
  x.rng <- c(min(rng$x) - radius, max(rng$x) + radius)
  y.rng <- c(min(rng$y) - radius, max(rng$y) + radius)

  if (zoom == FALSE) {
    plot(cholera::fatalities[, c("x", "y")], xlim = range(cholera::roads$x),
      ylim = range(cholera::roads$y), pch = 15, cex = 0.5, col = "gray",
      asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = paste0("p", cholera::pumps$id),
      pos = 1, col = "blue")
    invisible(lapply(roads.list[paste(road.number)], lines, col = "red",
      lwd = 3))
    st.name <- unique(cholera::roads[cholera::roads$street == road.number,
      "name"])
    title(main = paste0(st.name, ": 'Street' # ", road.number))

   } else if (zoom & radius <= 5) {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = NA, asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    text(cholera::fatalities.address[, c("x", "y")], labels =
      cholera::fatalities.address$anchor.case, cex = 0.5)
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = paste0("p", cholera::pumps$id),
      pos = 1)
    invisible(lapply(roads.list[paste(road.number)], lines, col = "red",
      lwd = 3))
    st.name <- unique(cholera::roads[cholera::roads$street == road.number,
      "name"])
    title(main = paste0(st.name, ": 'Street' # ", road.number))

  } else {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = 15, cex = 0.5, col = "gray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = paste0("p", cholera::pumps$id),
      pos = 1)
    invisible(lapply(roads.list[paste(road.number)], lines, col = "red",
      lwd = 3))
    st.name <- unique(cholera::roads[cholera::roads$street == road.number,
      "name"])
    title(main = paste0(st.name, ": 'Street' # ", road.number))
  }
}
