#' Locate road by name.
#'
#' Highlight a road and its cases. See the list of road names in \code{vignette}("road.names").
#' @param road.name Character vector. Note that \code{streetNameLocator}() tries to correct for case and to remove extra spaces.
#' @param zoom Logical.
#' @param cases Character. Plot cases: \code{NULL}, "address" or "fatality".
#' @param token Character. "id" or "point".
#' @param add.title Logical. Include title.
#' @param add.subtitle Logical. Include subtitle with road information.
#' @param add.pump Logical. Include nearby pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param highlight Logical. Highlight selected road and its cases.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @return A base R graphics plot.
#' @export

latlongStreetNameLocator <- function(road.name = "Broad Street", zoom = FALSE,
  cases = "address", token = "id", add.title = TRUE, add.subtitle = TRUE,
  add.pump = TRUE, vestry = FALSE, highlight = TRUE, distance.unit = "meter",
  time.unit = "minute", walking.speed = 5) {

  real.road.names <- streetNames()
  vars <- c("lon", "lat")

  if (!is.null(road.name)) {
    if (is.character(road.name)) {
      name <- vapply(road.name, caseAndSpace, character(1L))
    } else {
      stop("Road name must be a character string.", call. = FALSE)
    }

    if (all(name %in% real.road.names == FALSE)) {
      error.msg <- "Invalid road name(s). See streetNames()."
      stop(error.msg, call. = FALSE)
    } else if (any(name %in% real.road.names == FALSE)) {
      nm.err <- name[!name %in% real.road.names]
      nm.msg <- "Misspelled or invalid road name(s). See streetNames():"
      message(paste(nm.msg, paste(nm.err, collapse = ", ")))
      name <- name[name %in% real.road.names]
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

  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
    stop('distance.unit must be "meter", "yard" or "native".', call. = FALSE)
  }

  if (time.unit %in% c("minute", "hour", "second") == FALSE) {
    stop('time.unit must be "hour", "minute" or "second".', call. = FALSE)
  }

  sel <- cholera::roads$name %in% name
  selected.road <- unique(cholera::roads[sel, "street"])

  roads.list <- split(cholera::roads[, vars], cholera::roads$street)

  if (is.null(road.name) | isFALSE(zoom)) {
    xlim <- range(cholera::roads$lon)
    ylim <- range(cholera::roads$lat)
  } else {
    sel <- cholera::roads$name %in% name
    xlim <- range(cholera::roads[sel, "lon"])
    ylim <- range(cholera::roads[sel, "lat"])
  }

  plot(cholera::fatalities[, vars], xlim = xlim, ylim = ylim, pch = NA,
    asp = 1.6)
  invisible(lapply(roads.list, lines, col = "gray"))

  if (zoom == TRUE) {
    if (is.null(cases) == FALSE) {
      id <- cholera::road.segments[cholera::road.segments$name %in% name, "id"]

      sel <- cholera::latlong.ortho.addr$road.segment %in% id
      seg.ortho <- cholera::latlong.ortho.addr[sel, ]

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
    invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
      lwd = 3))
  }

  if (add.title) {
    if (length(name) == 1) title(main = name)
    else if (length(name) > 1) title(main = paste(name, collapse = ", "))
  }

  if (add.subtitle) {
    if (length(name) == 1) {
      street.length <- streetLength(name, distance.unit, latlong = TRUE)
      est.time <- distanceTime(street.length, distance.unit = distance.unit,
        time.unit = time.unit, walking.speed = walking.speed)

      nominal.time <- nominalTime(est.time, time.unit)

      if (distance.unit == "meter") {
        subtitle <- paste(round(street.length, 1), "m;", nominal.time)
      } else if (distance.unit == "yard") {
        subtitle <- paste(round(street.length, 1), "yd;", nominal.time)
      }

      title(sub = paste(subtitle, "@", walking.speed, "km/hr"))
    }
  }
}
