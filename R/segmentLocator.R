#' Locate road segment by ID.
#'
#' Highlights the selected road segment and its cases.
#' @param id Character. A concatenation of a street's numeric ID, a whole number between 1 and 528, and a second number to identify the segment.
#' @param zoom Logical or Numeric. A numeric value >= 0 controls the degree of zoom. The default is 0.5.
#' @param cases Character. Plot cases: \code{NULL}, "address" or "fatality".
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param add.title Logical. Print title.
#' @param add.subtitle Logical. Print subtitle.
#' @param highlight Logical. Highlight selected road and its cases.
#' @return A base R graphics plot.
#' @import graphics
#' @note With Dodson and Tobler's data, a street (e.g., Broad Street) is often comprised of multiple straight line segments. To identify each segment individually, an additional number is appended to form a text string ID (e.g., "116-2").  See \code{cholera::road.segments}.
#' @export
#' @examples
#' segmentLocator("190-1")
#' segmentLocator("216-1")
#' segmentLocator("216-1", distance.unit = "yard")

segmentLocator <- function(id = "216-1", zoom = 0.5, cases = "address",
  distance.unit = "meter", time.unit = "second", walking.speed = 5,
  add.title = TRUE, add.subtitle = TRUE, highlight = TRUE) {

  if (is.character(id) == FALSE) {
    stop('id\'s type must be a character.')
  }

  if (id %in% cholera::road.segments$id == FALSE) {
    stop("Invalid segment ID. See cholera::road.segments.")
  }

  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
    stop('distance.unit must be "meter", "yard" or "native".')
  }

  st <- cholera::road.segments[cholera::road.segments$id == id, ]

  if (is.logical(zoom)) {
    if (zoom) {
      padding <- 0.1
      x.rng <- c(min(st[, c("x1", "x2")]) - padding,
                 max(st[, c("x1", "x2")]) + padding)
      y.rng <- c(min(st[, c("y1", "y2")]) - padding,
                 max(st[, c("y1", "y2")]) + padding)
    } else {
      x.rng <- range(cholera::roads$x)
      y.rng <- range(cholera::roads$y)
    }
  } else if (is.numeric(zoom)) {
    if (zoom >= 0) {
      x.rng <- c(min(st[, c("x1", "x2")]) - zoom,
                 max(st[, c("x1", "x2")]) + zoom)
      y.rng <- c(min(st[, c("y1", "y2")]) - zoom,
                 max(st[, c("y1", "y2")]) + zoom)
    } else stop("If numeric, zoom must be >= 0.")
  } else stop("zoom must either be logical or numeric.")

  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)

  if ((is.logical(zoom) & zoom == TRUE) | is.numeric(zoom)) {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = NA, asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))

    if (is.null(cases) == FALSE) {
      seg.ortho <- cholera::ortho.proj[cholera::ortho.proj$road.segment == id, ]
      seg.anchors <- cholera::fatalities.address$anchor %in% seg.ortho$case
      seg.cases <- cholera::fatalities$case %in% seg.ortho$case

      if (cases == "fatality") {
        text(cholera::fatalities[!seg.cases, c("x", "y")],
          labels = cholera::fatalities$case[!seg.cases], cex = 0.5)
        if (any(seg.cases)) {
          if (highlight) {
            text(cholera::fatalities[seg.cases, c("x", "y")],
              labels = cholera::fatalities$case[seg.cases], cex = 0.5,
              col = "red")
          } else {
            text(cholera::fatalities[seg.cases, c("x", "y")],
              labels = cholera::fatalities$case[seg.cases], cex = 0.5)
          }

        }
      } else if (cases == "address") {
        text(cholera::fatalities.address[!seg.anchors, c("x", "y")],
          labels = cholera::fatalities.address$anchor[!seg.anchors], cex = 0.5)
        if (any(seg.anchors)) {
          if (highlight) {
            text(cholera::fatalities.address[seg.anchors, c("x", "y")],
              labels = cholera::fatalities.address$anchor[seg.anchors],
              cex = 0.5, col = "red")
          } else {
            text(cholera::fatalities.address[seg.anchors, c("x", "y")],
              labels = cholera::fatalities.address$anchor[seg.anchors],
              cex = 0.5)
          }
        }
      }
    }
  } else {
    plot(cholera::fatalities[, c("x", "y")], xlim = range(cholera::roads$x),
      ylim = range(cholera::roads$y), pch = 15, cex = 0.5, col = "gray",
      asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
  }

  points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
  text(cholera::pumps[, c("x", "y")], label = cholera::pumps$id, pos = 1,
    col = "blue")

  if (highlight) segments(st$x1, st$y1, st$x2, st$y2, col = "red", lwd = 3)
  if (add.title) title(main = paste0(st$name, ": Segment # ", id))

  segment.length <- segmentLength(id, distance.unit)

  est.time <- distanceTime(segment.length, distance.unit = distance.unit,
    time.unit = time.unit, walking.speed = walking.speed)

  if (time.unit == "hour") {
    nominal.time <- paste(round(est.time, 1), "hr.")
  } else if (time.unit == "minute") {
    nominal.time <- paste(round(est.time, 1), "mins.")
  } else if (time.unit == "second") {
    nominal.time <- paste(round(est.time, 1), "secs.")
  }

  if (add.subtitle) {
    if (distance.unit == "native") {
      title(sub = paste(round(segment.length, 2), "units;", nominal.time, "@",
        walking.speed, "km/hr"))
    } else if (distance.unit == "meter") {
      title(sub = paste(round(segment.length, 2), "meters;", nominal.time, "@",
        walking.speed, "km/hr"))
    } else if (distance.unit == "yard") {
      title(sub = paste(round(segment.length, 2), "yards;", nominal.time, "@",
        walking.speed, "km/hr"))
    }
  }
}
