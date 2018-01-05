#' Locate road by its numerical ID.
#'
#' Plots John Snow's map of the 1854 London cholera outbreak and highlights the
#' selected road and its cases. See cholera::roads for numerical IDs and \code{vignette}("road.names") for details.
#' @param road.number Numeric or integer. A whole number between 1 and 528.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom. For values <= 5, the numeric ID of all cases or just the anchor case is plotted.
#' @param all.cases Logical. When zoom = TRUE and radius <= 5, all.cases = TRUE plots the numeric ID of all cases; when all.cases = FALSE only the numeric ID of the anchor case is shown.
#' @return A base R graphics plot.
#' @seealso \code{\link{roads}}, \code{\link{road.segments}}, \code{\link{streetNameLocator}}, \code{vignette("road.names")}
#' @import graphics
#' @export
#' @examples
#' streetNumberLocator(243)
#' streetNumberLocator(243, zoom = TRUE)
#' streetNumberLocator(243, zoom = TRUE, radius = 0)

streetNumberLocator <- function(road.number, zoom = FALSE, radius = 1,
  all.cases = FALSE) {

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

    id <- cholera::road.segments[cholera::road.segments$street == road.number,
      "id"]
    seg.ortho <- cholera::ortho.proj[cholera::ortho.proj$road.segment %in% id, ]
    seg.anchors <- cholera::fatalities.address$anchor.case %in% seg.ortho$case
    seg.cases <- cholera::fatalities$case %in% seg.ortho$case

    if (all.cases) {
      text(cholera::fatalities[!seg.cases, c("x", "y")],
        labels = cholera::fatalities$case[!seg.cases], cex = 0.5)
      if (any(seg.cases)) {
        text(cholera::fatalities[seg.cases, c("x", "y")],
          labels = cholera::fatalities$case[seg.cases], cex = 0.5,
          col = "red")
      }
    } else {
      text(cholera::fatalities.address[!seg.anchors, c("x", "y")],
        labels = cholera::fatalities.address$anchor.case[!seg.anchors],
        cex = 0.5)
      if (any(seg.anchors)) {
        text(cholera::fatalities.address[seg.anchors, c("x", "y")],
          labels = cholera::fatalities.address$anchor.case[seg.anchors],
          cex = 0.5, col = "red")
      }
    }

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
