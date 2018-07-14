#' Locate road by numerical ID.
#'
#' Highlight a road and its cases. See cholera::roads for numerical IDs and \code{vignette}("road.names") for details.
#' @param road.number Numeric or integer. A whole number between 1 and 528.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param cases Character. Plot cases: NULL, "anchors" or "all".
#' @param add.title Logical. Include title.
#' @param add.pump Logical. Include nearby pumps.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param highlight Logical. Highlight selected road.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Default walking speed is 5 km/hr.
#' @return A base R graphics plot.
#' @seealso \code{\link{roads}}, \code{\link{road.segments}}, \code{\link{streetNameLocator}}, \code{vignette("roads")}
#' @import graphics
#' @export
#' @examples
#' streetNumberLocator(243)
#' streetNumberLocator(243, zoom = TRUE)
#' streetNumberLocator(243, zoom = TRUE, radius = 0)

streetNumberLocator <- function(road.number, zoom = FALSE, radius = 1,
  cases = "anchors", add.title = TRUE, add.pump = TRUE, vestry = FALSE,
  highlight = TRUE, unit = "meter", time.unit = "second", walking.speed = 5) {

  if (is.numeric(road.number) == FALSE) {
    stop("road.number must be numeric.")
  } else {
    if (road.number %in% unique(cholera::roads$street) == FALSE) {
      stop("road.number must lie between 1 and 528.")
    }
  }

  if (is.null(cases) == FALSE) {
    if (cases %in% c("anchors", "all") == FALSE) {
      stop('If specified, "cases" must either be "anchors" or "all".')
    }
  }

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE) {
      stop('If specified, "unit" must either be "meter" or "yard".')
    }
  }

  if (time.unit %in% c("minute", "hour", "second") == FALSE) {
    stop('"time.unit" must be "hour", "minute" or "second".')
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

  } else {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = NA, asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))

    if (is.null(cases) == FALSE) {
      id <- cholera::road.segments[cholera::road.segments$street ==
        road.number, "id"]
      seg.ortho <- cholera::ortho.proj[cholera::ortho.proj$road.segment %in%
        id, ]
      seg.anchors <- cholera::fatalities.address$anchor.case %in%
      seg.ortho$case
      seg.cases <- cholera::fatalities$case %in% seg.ortho$case

      if (cases == "all") {
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
      } else if (cases == "anchors") {
        text(cholera::fatalities.address[!seg.anchors, c("x", "y")],
          labels = cholera::fatalities.address$anchor.case[!seg.anchors],
           cex = 0.5)
        if (any(seg.anchors)) {
          if (highlight) {
            text(cholera::fatalities.address[seg.anchors, c("x", "y")],
              labels = cholera::fatalities.address$anchor.case[seg.anchors],
              cex = 0.5, col = "red")
          } else {
            text(cholera::fatalities.address[seg.anchors, c("x", "y")],
              labels = cholera::fatalities.address$anchor.case[seg.anchors],
              cex = 0.5)
          }
        }
      }
    }
  }

  if (add.pump) {
    if (vestry) {
      points(cholera::pumps.vestry[, c("x", "y")], pch = 17, cex = 1,
        col = "blue")
      text(cholera::pumps.vestry[, c("x", "y")],
        label = paste0("p", cholera::pumps.vestry$id), pos = 1)
    } else {
      points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
      text(cholera::pumps[, c("x", "y")],
        label = paste0("p", cholera::pumps$id), pos = 1)
    }
  }

  if (highlight) {
    invisible(lapply(roads.list[paste(road.number)], lines, col = "red",
      lwd = 3))
  }

  street.length <- cholera::streetLength(road.number, unit)

  native.street.length <- cholera::streetLength(road.number, unit = "native")
  est.time <- cholera::distanceTime(native.street.length, unit = time.unit,
    speed = walking.speed)

  if (time.unit == "hour") {
    nominal.time <- paste(round(est.time, 1), "hr")
  } else if (time.unit == "minute") {
    nominal.time <- paste(round(est.time, 1), "min")
  } else if (time.unit == "second") {
    nominal.time <- paste(round(est.time, 1), "sec")
  }

  if (is.null(unit)) {
   subtitle <- paste(round(street.length, 1), "units;", nominal.time)
  } else if (unit == "meter") {
   subtitle <- paste(round(street.length, 1), "m;", nominal.time)
  } else if (unit == "yard") {
   subtitle <- paste(round(street.length, 1), "yd;", nominal.time)
  }

  if (add.title) {
    st.name <- unique(cholera::roads[cholera::roads$street == road.number,
      "name"])
    title(main = paste0(st.name, ": 'Street' # ", road.number),
          sub = paste(subtitle, "@", walking.speed, "km/hr"))
  }
}
