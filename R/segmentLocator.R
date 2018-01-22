#' Locate road segment by ID.
#'
#' Highlights the selected road segment and its cases.
#' @param id Character. A concatenation of a street's numeric ID, a whole number between 1 and 528, and a second number to identify the segment.
#' @param zoom Logical. Default is FALSE.
#' @param radius Numeric. Controls the degree of zoom. For values <= 5, the numeric ID of all cases or just the anchor case is plotted.
#' @param cases Character. Plot cases: NULL, "anchors" or "all".
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale.
#' @param title Logical. Print title.
#' @param subtitle Logical. Print subtitle.
#' @seealso \code{\link{road.segments}}
#' @return A base R graphics plot.
#' @import graphics
#' @section Notes: With Dodson and Tobler's data, a street (e.g., Broad Street) is often comprised of multiple straight line segments. To identify each segment individually, an additional number is appended to form a text string ID (e.g., "116-2").  See cholera::road.segments.
#' @export
#' @examples
#' segmentLocator("190-1")
#' segmentLocator("216-1")
#' segmentLocator("216-1", unit = "meter")

segmentLocator <- function(id, zoom = FALSE, radius = 0.5, cases = "anchors",
  unit = NULL, title = TRUE, subtitle = TRUE) {

  if (is.character(id) == FALSE) {
    stop('"id" must be a character.')
  }

  if (id %in% cholera::road.segments$id == FALSE) {
    stop("Invalid segment ID. See cholera::road.segments.")
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

  } else {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = NA, asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))

    if (is.null(cases) == FALSE) {
      seg.ortho <- cholera::ortho.proj[cholera::ortho.proj$road.segment == id, ]
      seg.anchors <- cholera::fatalities.address$anchor.case %in% seg.ortho$case
      seg.cases <- cholera::fatalities$case %in% seg.ortho$case

      if (cases == "all") {
        text(cholera::fatalities[!seg.cases, c("x", "y")],
          labels = cholera::fatalities$case[!seg.cases], cex = 0.5)
        if (any(seg.cases)) {
          text(cholera::fatalities[seg.cases, c("x", "y")],
            labels = cholera::fatalities$case[seg.cases], cex = 0.5,
            col = "red")
        }
      } else if (cases == "anchors") {
        text(cholera::fatalities.address[!seg.anchors, c("x", "y")],
          labels = cholera::fatalities.address$anchor.case[!seg.anchors],
          cex = 0.5)
        if (any(seg.anchors)) {
          text(cholera::fatalities.address[seg.anchors, c("x", "y")],
            labels = cholera::fatalities.address$anchor.case[seg.anchors],
            cex = 0.5, col = "red")
        }
      }
    }
  }

  points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
  text(cholera::pumps[, c("x", "y")], label = cholera::pumps$id, pos = 1,
    col = "blue")
  segments(st$x1, st$y1, st$x2, st$y2, col = "red", lwd = 3)

  if (title) title(main = paste0(st$name, ": Segment # ", id))

  if (subtitle) {
    if (is.null(unit)) {
      title(sub = paste(round(segmentLength(id, unit = unit), 2), "units"))
    } else if (unit == "meter") {
      title(sub = paste(round(segmentLength(id, unit = unit), 2), "meters"))
    } else if (unit == "yard") {
      title(sub = paste(round(segmentLength(id, unit = unit), 2), "yards"))
    }
  }
}
