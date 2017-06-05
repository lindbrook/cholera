#' Locate road segment by its character ID.
#'
#' Plots John Snow's map of the 1854 London cholera outbreak and highlights the
#' selected road segment. With Dodson and Tobler's data, a street (e.g., Broad Street) is often comprised of multiple straight line segments. To identify each segment individually, an additional identifying number is appended to form a text string (e.g., "116-2").
#' @param id Character. A concatenation of a street's numeric ID, a whole number between 1 and 528, and a second number to identify the segment.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom. For values <= 5, the anchor case number is plotted.
#' @seealso \code{\link{road.segments}}
#' @return A base R graphics plot.
#' @import graphics
#' @export
#' @examples
#' segmentLocator("190-1")
#' segmentLocator("216-1")

segmentLocator <- function(id, zoom = TRUE, radius = 0.5) {
  if (is.character(id) == FALSE) {
    stop('"id" must be a character.')
  }

  if (id %in% cholera::road.segments$id == FALSE) {
    stop("Invalid segment ID.")
  }

  st <- cholera::road.segments[cholera::road.segments$id == id, ]

  x.rng <- c(min(st[, c("x1", "x2")]) - radius,
             max(st[, c("x1", "x2")]) + radius)

  y.rng <- c(min(st[, c("y1", "y2")]) - radius,
             max(st[, c("y1", "y2")]) + radius)

  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)

  if (!zoom) {
    plot(cholera::fatalities[, c("x", "y")], xlim = range(cholera::roads$x),
      ylim = range(cholera::roads$y), pch = 15, cex = 0.5, col = "gray",
      asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = cholera::pumps$id, pos = 1,
      col = "blue")
    segments(st$x1, st$y1, st$x2, st$y2, col = "red", lwd = 3)
    title(main = paste0(st$name, ": Segment # ", id))

   } else if (zoom & radius <= 5) {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = NA, asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    text(cholera::fatalities.address[, c("x", "y")], labels =
      cholera::fatalities.address$anchor.case, cex = 0.75)
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = paste0("p", cholera::pumps$id),
      pos = 1, col = "blue")
    segments(st$x1, st$y1, st$x2, st$y2, col = "red", lwd = 3)
    title(main = paste0(st$name, ": Segment # ", id))

  } else {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = 15, cex = 0.5, col = "gray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = paste0("p", cholera::pumps$id),
      pos = 1, col = "blue")
    segments(st$x1, st$y1, st$x2, st$y2, col = "red", lwd = 3)
    title(main = paste0(st$name, ": Segment # ", id))
  }
}
