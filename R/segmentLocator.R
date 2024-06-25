#' Locate road segment by ID.
#'
#' Highlights the selected road segment and its cases.
#' @param id Character. A concatenation of a street's numeric ID, a whole number between 1 and 528, and a second number to identify the segment.
#' @param zoom Logical or Numeric. Numeric values pad segment end points in meters (appox). Negative values zoom out. Positive values zoom in.
#' @param cases Character. Plot cases: \code{NULL}, "address" or "fatality".
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param add.title Logical. Print title.
#' @param add.subtitle Logical. Print subtitle.
#' @param highlight Logical. Highlight selected road and its cases.
#' @param cex.text Numeric.
#' @return A base R graphics plot.
#' @import graphics
#' @note With Dodson and Tobler's data, a street (e.g., Broad Street) is often comprised of multiple straight line segments. To identify each segment individually, an additional number is appended to form a text string ID (e.g., "116-2").  See \code{cholera::road.segments}.
#' @export
#' @examples
#' segmentLocator("190-1")
#' segmentLocator("216-1")
#' segmentLocator("216-1", distance.unit = "yard")

segmentLocator <- function(id = "216-1", zoom = FALSE, cases = "address",
  distance.unit = "meter", time.unit = "second", walking.speed = 5,
  add.title = TRUE, add.subtitle = TRUE, highlight = TRUE, cex.text = 0.67) {

  if (is.null(id) == FALSE) {
    if (is.character(id) == FALSE) {
      stop('id must be a character.', call. = FALSE)
    } else if (id %in% cholera::road.segments$id == FALSE) {
      stop("Invalid segment ID. See cholera::road.segments.", call. = FALSE)
    }
  }

  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
    stop('distance.unit must be "meter", "yard" or "native".', call. = FALSE)
  }

  if (is.null(id) | isFALSE(zoom)) {
    xlim <- range(cholera::roads$x)
    ylim <- range(cholera::roads$y)
  } else {
    if (isTRUE(zoom) | zoom == 0) {
      sel <- cholera::road.segments$id %in% id
      xlim <- range(cholera::road.segments[sel, paste0("x", 1:2)])
      ylim <- range(cholera::road.segments[sel, paste0("y", 1:2)])
    } else if (zoom != 0) {
      sel <- cholera::road.segments$id %in% id
      nom.seg <- cholera::road.segments[sel, ]
      vars <- c("x", "y")

      seg.data <- rbind(stats::setNames(nom.seg[, paste0(vars, 1)], vars),
                        stats::setNames(nom.seg[, paste0(vars, 2)], vars))

      ols <- stats::lm(y ~ x, data = seg.data)
      segment.slope <- stats::coef(ols)[2]
      theta <- atan(segment.slope)

      pad <- abs(zoom) / unitMeter(1)
      delta.x <- pad * cos(theta)
      delta.y <- pad * sin(theta)

      x.range <- range(seg.data$x)
      y.range <- range(seg.data$y)

      if (zoom < 0) {
        xlim <- c(x.range[1] - delta.x, x.range[2] + delta.x)
        ylim <- c(y.range[1] - delta.y, y.range[2] + delta.y)
      } else if (zoom > 0) {
        xlim <- c(x.range[1] + delta.x, x.range[2] - delta.x)
        ylim <- c(y.range[1] + delta.y, y.range[2] - delta.y)
      }

      xlim.delta <- xlim[2] - xlim[1]
      ylim.delta <- ylim[2] - ylim[1]

      if (xlim.delta <= 0 | ylim.delta <= 0) {
        xlim <- x.range
        ylim <- y.range
        message("Note: zoom = ",  zoom, " too far! Use smaller.")
      }
    }
  }

  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)

  if ((is.logical(zoom) & zoom == TRUE) | is.numeric(zoom)) {
    plot(cholera::fatalities[, c("x", "y")], xlim = xlim, ylim = ylim, pch = NA,
      asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))

    if (is.null(cases) == FALSE) {
      seg.ortho <- cholera::ortho.proj[cholera::ortho.proj$road.segment == id, ]
      seg.anchors <- cholera::fatalities.address$anchor %in% seg.ortho$case
      seg.cases <- cholera::fatalities$case %in% seg.ortho$case

      if (cases == "fatality") {
        text(cholera::fatalities[!seg.cases, c("x", "y")],
          labels = cholera::fatalities$case[!seg.cases], cex = cex.text)
        if (any(seg.cases)) {
          if (highlight) {
            text(cholera::fatalities[seg.cases, c("x", "y")],
              labels = cholera::fatalities$case[seg.cases], cex = cex.text,
              col = "red")
          } else {
            text(cholera::fatalities[seg.cases, c("x", "y")],
              labels = cholera::fatalities$case[seg.cases], cex = cex.text)
          }
        }
      } else if (cases == "address") {
        text(cholera::fatalities.address[!seg.anchors, c("x", "y")],
          labels = cholera::fatalities.address$anchor[!seg.anchors],
          cex = cex.text)
        if (any(seg.anchors)) {
          if (highlight) {
            text(cholera::fatalities.address[seg.anchors, c("x", "y")],
              labels = cholera::fatalities.address$anchor[seg.anchors],
              cex = cex.text, col = "red")
          } else {
            text(cholera::fatalities.address[seg.anchors, c("x", "y")],
              labels = cholera::fatalities.address$anchor[seg.anchors],
              cex = cex.text)
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

  if (!is.null(id)) {
    if (highlight) {
      lapply(id, function(seg) {
        tmp <- cholera::road.segments[cholera::road.segments$id == seg, ]
        segments(tmp$x1, tmp$y1, tmp$x2, tmp$y2, col = "red", lwd = 3)
      })
    }

    if (add.title) {
      seg.nm <- cholera::road.segments[cholera::road.segments$id == id, "name"]
      title(main = paste0(seg.nm, ": Segment # ", id))
    }

    if (add.subtitle) {
      segment.length <- segmentLength(id, distance.unit)
      est.time <- distanceTime(segment.length, distance.unit = distance.unit,
        time.unit = time.unit, walking.speed = walking.speed)
      nominal.time <- nominalTime(est.time, time.unit)

      if (distance.unit == "native") {
        title(sub = paste(round(segment.length, 2), "units;", nominal.time, "@",
          walking.speed, "km/hr"))
      } else if (distance.unit == "meter") {
        title(sub = paste(round(segment.length, 2), "meters;", nominal.time,
          "@", walking.speed, "km/hr"))
      } else if (distance.unit == "yard") {
        title(sub = paste(round(segment.length, 2), "yards;", nominal.time, "@",
          walking.speed, "km/hr"))
      }
    }
  }
}
