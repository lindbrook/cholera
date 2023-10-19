#' Highlight segment by ID.
#'
#' @param id Character. Segment ID: a concatenation of a street's numeric ID, a whole number between 1 and 528, and a second number to identify the segment.
#' @param highlight Logical. Color segment.
#' @param col Character. Highlight color.
#' @param rotate.label Logical. Rotate segment ID label.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @return A base R graphics segment(s).
#' @importFrom geosphere distGeo
#' @export
#' @examples
#' streetNameLocator("Soho Square", zoom = TRUE, highlight = FALSE)
#' ids <- road.segments[road.segments$name == "Soho Square", "id"]
#' invisible(lapply(ids, function(x) segmentHighlight(x, highlight = FALSE)))

segmentHighlight <- function(id, highlight = TRUE, col = "red",
  rotate.label = FALSE, latlong = FALSE) {

  if (is.character(id) == FALSE) stop('id\'s type must be character.',
    call. = FALSE)

  if (id %in% cholera::road.segments$id == FALSE) {
    stop("Invalid segment ID. See cholera::road.segments.", call. = FALSE)
  }

  if (latlong) {
    rd.segs <- roadSegments(latlong = latlong)
    seg <- rd.segs[rd.segs$id == id, ]

    if (highlight) {
      segments(seg$lon1, seg$lat1, seg$lon2, seg$lat2, col = col, lwd = 3)
    }

    mid.pt <- data.frame(lon = mean(unlist(seg[, c("lon1", "lon2")])),
                         lat = mean(unlist(seg[, c("lat1", "lat2")])))

    if (rotate.label) {
      origin <- data.frame(lon = min(cholera::roads$lon),
                           lat = min(cholera::roads$lat))

      x1.proj <- c(seg$lon1, origin$lat)
      y1.proj <- c(origin$lon, seg$lat1)
      x2.proj <- c(seg$lon2, origin$lat)
      y2.proj <- c(origin$lon, seg$lat2)

      lon1.meters <- geosphere::distGeo(x1.proj, origin)
      lat1.meters <- geosphere::distGeo(y1.proj, origin)
      lon2.meters <- geosphere::distGeo(x2.proj, origin)
      lat2.meters <- geosphere::distGeo(y2.proj, origin)

      cartesian <- data.frame(x = c(lon1.meters, lon2.meters),
                              y = c(lat1.meters, lat2.meters))

      ols <- stats::lm(y ~ x, data = cartesian)
      intercept.slope <- stats::coef(ols)

      angle <- atan(intercept.slope["x"]) * 180L / pi
      text(mid.pt$lon, mid.pt$lat, labels = id, col = col, srt = angle)
    } else {
      text(mid.pt$lon, mid.pt$lat, labels = id, col = col)
    }
  } else {
    seg <- cholera::road.segments[cholera::road.segments$id == id, ]

    if (highlight) segments(seg$x1, seg$y1, seg$x2, seg$y2, col = col, lwd = 3)

    seg.data <- data.frame(x = unlist(seg[, c("x1", "x2")]),
                           y = unlist(seg[, c("y1", "y2")]),
                           row.names = NULL)

    intercept.slope <- stats::coef(stats::lm(y ~ x, data = seg.data))
    x.prime <- mean(seg.data$x)
    y.prime <- x.prime * intercept.slope["x"] + intercept.slope["(Intercept)"]

    if (rotate.label) {
      angle <- atan(intercept.slope["x"]) * 180L / pi
      text(x.prime, y.prime, labels = id, srt = angle, col = col)
    } else {
      text(x.prime, y.prime, labels = id, col = col)
    }
  }
}
