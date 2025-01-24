#' Compute length of road segment.
#'
#' @param id Character. A concatenation of a street's numeric ID, a whole number between 1 and 528, and a second number used to identify the sub-segments.
#' @param latlong Logical.
#' @return An R vector of length one.
#' @noRd
#' @examples
#' segmentLength("242-1")
#' segmentLength("242-1", latlong = TRUE)

segmentLength <- function(id = "216-1", latlong = FALSE) {
  if (is.character(id) == FALSE) {
    stop('id\'s type must be character.', call. = FALSE)
  }
  if (all(id %in% cholera::road.segments$id == FALSE)) {
    stop("No valid segment ID(s).", call. = FALSE)
  } else if (any(id %in% cholera::road.segments$id == FALSE)) {
    message("Invalid segment ID(s) removed.")
    id <- id[id %in% cholera::road.segments$id]
  }

  if (latlong) {
    rd.segs <- roadSegments(latlong = latlong)
    dat <- rd.segs[rd.segs$id %in% id, ]
    ds <- vapply(seq_along(dat$id), function(i) {
      p1 <- dat[i, c("lon1", "lat1")]
      p2 <- dat[i, c("lon2", "lat2")]
      geosphere::distGeo(p1, p2) / unitMeter(1)
    }, numeric(1L))
    names(ds) <- id
  } else {
    dat <- cholera::road.segments[cholera::road.segments$id %in% id, ]
    ds <- vapply(dat$id, function(id) {
      stats::dist(rbind(as.matrix(dat[dat$id == id, c("x1", "y1")]),
                        as.matrix(dat[dat$id == id, c("x2", "y2")])))
    }, numeric(1L))
  }
  ds
}
