#' Highlight segment by ID.
#'
#' @param id Character. A concatenation of a street's numeric ID, a whole number between 1 and 528, and a second number to identify the segment.
#' @param highlight Logical. Color segment.
#' @param col Character. Highlight color.
#' @param angled Logical. Rotate segment ID label.
#' @return A base R graphics segment(s).
#' @export
#' @examples
#' streetNameLocator("Soho Square", zoom = TRUE, highlight = FALSE)
#' ids <- road.segments[road.segments$name == "Soho Square", "id"]
#' invisible(lapply(ids, function(x) segmentHighlight(x, highlight = FALSE)))

segmentHighlight <- function(id, highlight = TRUE, col = "red",
  angled = FALSE) {

  if (is.character(id) == FALSE) stop('id\'s type must be character.',
    call. = FALSE)

  if (id %in% cholera::road.segments$id == FALSE) {
    stop("Invalid segment ID. See cholera::road.segments.", call. = FALSE)
  }

  st <- cholera::road.segments[cholera::road.segments$id == id, ]
  if (highlight) segments(st$x1, st$y1, st$x2, st$y2, col = col, lwd = 3)

  seg.data <- data.frame(x = unlist(st[, c("x1", "x2")]),
                         y = unlist(st[, c("y1", "y2")]),
                         row.names = NULL)

  intercept.slope <- stats::coef(stats::lm(y ~ x, data = seg.data))
  x.prime <- mean(seg.data$x)
  y.prime <- x.prime * intercept.slope["x"] + intercept.slope["(Intercept)"]

  if (angled) {
    angle <- atan(intercept.slope["x"]) * 180L / pi
    text(x.prime, y.prime, labels = id, srt = angle, col = col)
  } else {
    text(x.prime, y.prime, labels = id, col = col)
  }
}
