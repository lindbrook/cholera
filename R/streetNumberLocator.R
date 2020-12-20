#' Locate road by numerical ID.
#'
#' Highlight a road and its cases. See \code{cholera::roads} for numerical IDs and \code{vignette}("road.names") for details.
#' @param road.number Numeric or integer. A whole number between 1 and 528.
#' @param zoom Logical or Numeric. A numeric value >= 0 controls the degree of zoom. The default is FALSE, which is equivalent to zero.
#' @param cases Character. Plot cases: \code{NULL}, "address" or "fatality".
#' @param token Character. "id" or "point".
#' @param add.title Logical. Include title.
#' @param add.subtitle Logical. Include subtitle with road information.
#' @param add.pump Logical. Include nearby pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param highlight Logical. Highlight selected road and its cases.
#' @param distance.unit Character. Unit of measurement: "meter" or "yard". Default is \code{NULL}, which returns the map's native scale.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @return A base R graphics plot.
#' @import graphics
#' @export
#' @examples
#' streetNumberLocator(243)
#' streetNumberLocator(243, zoom = TRUE)
#' streetNumberLocator(243, zoom = 0.5)

streetNumberLocator <- function(road.number = 216, zoom = FALSE,
  cases = "address", token = "id", add.title = TRUE, add.subtitle = TRUE,
  add.pump = TRUE, vestry = FALSE, highlight = TRUE, distance.unit = "meter",
  time.unit = "second", walking.speed = 5) {

  if (is.numeric(road.number) == FALSE) {
    stop("road.number must be numeric.", call. = FALSE)
  } else {
    if (road.number %in% unique(cholera::roads$street) == FALSE) {
      rd.ct <- length(unique(cholera::roads$street))
      stop("road.number must lie between 1 and ", rd.ct, ".", call. = FALSE)
    }
  }

  if (is.null(cases) == FALSE) {
    if (cases %in% c("address", "fatality") == FALSE) {
      stop('If specified, cases must either be "address" or "fatality".',
        call. = FALSE)
    }
  }

  if (token %in% c("id", "point") == FALSE) {
    stop('token must be "id", or "point".', call. = FALSE)
  }

  if (is.null(distance.unit) == FALSE) {
    if (distance.unit %in% c("meter", "yard") == FALSE) {
      stop('If specified, distance.unit must either be "meter" or "yard".',
        call. = FALSE)
    }
  }

  if (time.unit %in% c("minute", "hour", "second") == FALSE) {
    stop('time.unit must be "hour", "minute" or "second".', call. = FALSE)
  }

  vars <- c("x", "y")
  roads.list <- split(cholera::roads[, vars], cholera::roads$street)
  rng <- lapply(cholera::roads[cholera::roads$street == road.number, vars],
    range)

  if (is.logical(zoom)) {
    if (zoom) {
      radius <- 0.1
      x.rng <- c(min(rng$x) - radius, max(rng$x) + radius)
      y.rng <- c(min(rng$y) - radius, max(rng$y) + radius)
    } else {
      x.rng <- range(cholera::roads$x)
      y.rng <- range(cholera::roads$y)
    }
  } else if (is.numeric(zoom)) {
    if (zoom >= 0) {
      x.rng <- c(min(rng$x) - zoom, max(rng$x) + zoom)
      y.rng <- c(min(rng$y) - zoom, max(rng$y) + zoom)
    } else stop("If numeric, zoom must be >= 0.", call. = FALSE)
  } else stop("zoom must either be logical or numeric.", call. = FALSE)

  plot(cholera::fatalities[, vars], xlim = x.rng, ylim = y.rng,
    pch = NA, asp = 1)
  invisible(lapply(roads.list, lines, col = "gray"))

  if ((is.logical(zoom) & zoom == TRUE) | is.numeric(zoom)) {
    if (is.null(cases) == FALSE) {
      id <- cholera::road.segments[cholera::road.segments$street ==
        road.number, "id"]
      seg.ortho <- cholera::ortho.proj[cholera::ortho.proj$road.segment %in%
        id, ]
      seg.anchors <- cholera::fatalities.address$anchor %in% seg.ortho$case
      seg.cases <- cholera::fatalities$case %in% seg.ortho$case

      if (token == "id") {
        if (cases == "fatality") {
          text(cholera::fatalities[!seg.cases, vars],
            labels = cholera::fatalities$case[!seg.cases], cex = 0.5)
          if (any(seg.cases)) {
            if (highlight) {
              text(cholera::fatalities[seg.cases, vars],
                labels = cholera::fatalities$case[seg.cases], cex = 0.5,
                col = "red")
            } else {
              text(cholera::fatalities[seg.cases, vars],
                labels = cholera::fatalities$case[seg.cases], cex = 0.5)
            }
          }
        } else if (cases == "address") {
          text(cholera::fatalities.address[!seg.anchors, vars],
            labels = cholera::fatalities.address$anchor[!seg.anchors],
            cex = 0.5)
          if (any(seg.anchors)) {
            if (highlight) {
              text(cholera::fatalities.address[seg.anchors, vars],
                labels = cholera::fatalities.address$anchor[seg.anchors],
                cex = 0.5, col = "red")
            } else {
              text(cholera::fatalities.address[seg.anchors, vars],
                labels = cholera::fatalities.address$anchor[seg.anchors],
                cex = 0.5)
            }
          }
        }

      } else if (token == "point") {
        if (cases == "fatality") {
          points(cholera::fatalities[!seg.cases, vars], pch = 15, cex = 0.5)
          if (any(seg.cases)) {
            if (highlight) {
              points(cholera::fatalities[seg.cases, vars], pch = 15, cex = 0.5,
                col = "red")
            } else {
              points(cholera::fatalities[seg.cases, vars], pch = 15, cex = 0.5)
            }
          }
        } else if (cases == "address") {
          points(cholera::fatalities.address[!seg.anchors, vars], pch = 15,
            cex = 0.5)
          if (any(seg.anchors)) {
            if (highlight) {
              points(cholera::fatalities.address[seg.anchors, vars], pch = 15,
                cex = 0.5, col = "red")
            } else {
              points(cholera::fatalities.address[seg.anchors, vars], pch = 15,
                cex = 0.5)
            }
          }
        }
      }
    }
  }

  if (add.pump) {
    if (vestry) {
      points(cholera::pumps.vestry[, vars], pch = 17, cex = 1, col = "blue")
      text(cholera::pumps.vestry[, vars],
        label = paste0("p", cholera::pumps.vestry$id), pos = 1)
    } else {
      points(cholera::pumps[, vars], pch = 17, cex = 1, col = "blue")
      text(cholera::pumps[, vars], label = paste0("p", cholera::pumps$id),
        pos = 1)
    }
  }

  if (highlight) {
    invisible(lapply(roads.list[paste(road.number)], lines, col = "red",
      lwd = 3))
  }

  if (add.title) {
    st.name <- unique(cholera::roads[cholera::roads$street == road.number,
      "name"])
    title(main = paste0(st.name, ": 'Street' # ", road.number))
  }

  if (add.subtitle) {
    street.length <- streetLength(road.number, distance.unit)
    est.time <- distanceTime(street.length, distance.unit = distance.unit,
      time.unit = time.unit, walking.speed = walking.speed)

    if (time.unit == "hour") {
      nominal.time <- paste(round(est.time, 1), "hr")
    } else if (time.unit == "minute") {
      nominal.time <- paste(round(est.time, 1), "min")
    } else if (time.unit == "second") {
      nominal.time <- paste(round(est.time, 1), "sec")
    }

    if (is.null(distance.unit)) {
     subtitle <- paste(round(street.length, 1), "units;", nominal.time)
    } else if (distance.unit == "meter") {
     subtitle <- paste(round(street.length, 1), "m;", nominal.time)
    } else if (distance.unit == "yard") {
     subtitle <- paste(round(street.length, 1), "yd;", nominal.time)
    }

    title(sub = paste(subtitle, "@", walking.speed, "km/hr"))
  }
}
