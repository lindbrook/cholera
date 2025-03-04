#' Plot/Locate road segment by ID.
#'
#' Highlight selected road segment(s) and cases.
#' @param segment.id Character. A vector of segment IDs. See Note.
#' @param zoom Logical or Numeric. Positive value zoom in. Negative values zoom out.
#' @param latlong Logical. Longitude and latitude coordinates
#' @param cases Character. Cases to plot: \code{NULL}, "address" or "fatality".
#' @param token Character. Cases as "id" or "point".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param add.pump Logical. Include pumps.
#' @param add.title Logical. Include title.
#' @param add.subtitle Logical. Include subtitle.
#' @param highlight Logical. Highlight selected segment(s) and cases.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param cex.text Numeric.
#' @return A base R graphics plot.
#' @import graphics
#' @note With Dodson and Tobler's data, a street (e.g., Broad Street) is often comprised of multiple straight line segments. To identify each segment individually, an additional number is appended to form a text string ID (e.g., "116-2"). See \code{cholera::road.segments}.
#' @export
#' @examples
#' segmentLocator("216-1")
#' segmentLocator("216-1", zoom = -10)
#' segmentLocator("216-1", latlong = TRUE, zoom = -10)
#' segmentLocator("216-1", distance.unit = "yard")
#' segmentLocator("216-1", zoom = FALSE)

segmentLocator <- function(segment.id = "216-1", zoom = TRUE, latlong = FALSE,
  cases = "address", token = "id", vestry = FALSE, add.pump = TRUE,
  add.title = TRUE, add.subtitle = TRUE, highlight = TRUE,
  distance.unit = "meter", time.unit = "second", walking.speed = 5,
  cex.text = 0.67) {

  if (latlong) {
    asp  <- 1.6
    ew <- "lon"
    ns <- "lat"
    proj.data <- cholera::latlong.ortho.addr
    rd.segs <- roadSegments(latlong = latlong)
  } else {
    asp  <- 1
    ew <- "x"
    ns <- "y"
    proj.data <- cholera::ortho.proj
    rd.segs <- cholera::road.segments
  }

  vars <- c(ew, ns)

  if (!is.null(segment.id)) {
    if (!is.character(segment.id)) {
      stop('segment.id\'s must be a character vector.', call. = FALSE)
    } else if (all(segment.id %in% rd.segs$id == FALSE)) {
      error.msg <- "Invalid segment ID."
      stop(error.msg, call. = FALSE)
    } else if (any(segment.id %in% rd.segs$id == FALSE)) {
      id.err <- segment.id[!segment.id %in% rd.segs$id]
      id.msg <- "Misspelled or invalid segment."
      message(paste(id.msg, paste(id.err, collapse = ", ")))
      segment.id <- segment.id[segment.id %in% rd.segs$id]
    }
  }

  if (!is.null(cases)) {
    if (cases %in% c("address", "fatality") == FALSE) {
      stop('If specified, cases must either be "address" or "fatality".',
        call. = FALSE)
    }
  }

  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
    stop('distance.unit must be "meter", "yard" or "native".', call. = FALSE)
  }

  if (time.unit %in% c("minute", "hour", "second") == FALSE) {
    stop('time.unit must be "hour", "minute" or "second".', call. = FALSE)
  }

  if (token %in% c("id", "point") == FALSE) {
    stop('token must be "id", or "point".', call. = FALSE)
  }

  if (is.null(segment.id) | isFALSE(zoom)) {
    xlim <- range(cholera::roads[, ew])
    ylim <- range(cholera::roads[, ns])
  } else {
    if (isTRUE(zoom) | zoom == 0) {
      sel <- rd.segs$id %in% segment.id
      xlim <- range(rd.segs[sel, paste0(ew, 1:2)])
      ylim <- range(rd.segs[sel, paste0(ns, 1:2)])
    } else if (zoom != 0) {
      if (latlong) {
        vars <- c("lon", "lat")
        col.vars <- paste0(vars, 1)
        dat <- stats::setNames(rd.segs[, c("id", col.vars)], c("id", vars))
        ones <- geoCartesian(dat)

        col.vars <- paste0(vars, 2)
        dat <- stats::setNames(rd.segs[, c("id", col.vars)], c("id", vars))
        twos <- geoCartesian(dat)

        new.vars <- c("x", "y")
        ones <- stats::setNames(ones, c("id", paste0(new.vars, 1)))
        twos <- stats::setNames(twos, c("id", paste0(new.vars, 2)))
        cartestian.rd.segs <- merge(ones, twos, by = "id")
        cart.seg <- cartestian.rd.segs[cartestian.rd.segs$id %in% segment.id, ]

        cart.x.range <- range(cart.seg[, paste0("x", 1:2)])
        cart.y.range <- range(cart.seg[, paste0("y", 1:2)])

        pad <- c(zoom, -zoom)
        xlim <- cart.x.range + pad
        ylim <- cart.y.range + pad

        xlim.delta <- xlim[2] - xlim[1]
        ylim.delta <- ylim[2] - ylim[1]

        if (xlim.delta <= 0 | ylim.delta <= 0) {
          xlim <- cart.x.range
          ylim <- cart.y.range
          message("Note: zoom = ",  zoom, " too far! Use smaller.")
        }

        range.data <- meterLatLong(data.frame(x = xlim, y = ylim))
        xlim <- range.data$lon
        ylim <- range.data$lat

      } else {
        vars <- c("x", "y")
        nom.seg <- rd.segs[rd.segs$id %in% segment.id, ]
        seg.data <- rbind(stats::setNames(nom.seg[,  paste0(vars, 1)], vars),
                          stats::setNames(nom.seg[,  paste0(vars, 2)], vars))

        ols <- stats::lm(y ~ x, data = seg.data)
        segment.slope <- stats::coef(ols)[2]
        theta <- atan(segment.slope)

        pad <- abs(zoom) / unitMeter(1)
        delta.x <- abs(pad * cos(theta))
        delta.y <- abs(pad * sin(theta))

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
  }

  plot(cholera::fatalities[, vars], xlim = xlim, ylim = ylim, pch = NA,
    asp = asp)
  addFrame(col = "gray", latlong = latlong)
  addRoads(col = "gray", latlong = latlong)

  if (zoom == TRUE | is.numeric(zoom)) {
    if (is.null(cases) == FALSE) {
      seg.ortho <- proj.data[proj.data$road.segment %in% segment.id, ]

      if (latlong) {
        seg.anchors <- seg.ortho$case

        sel <- cholera::anchor.case$anchor %in% seg.anchors
        seg.cases <- cholera::anchor.case[sel, "case"]
      } else {
        seg.cases <- seg.ortho$case

        sel <- cholera::anchor.case$case %in% seg.cases
        seg.anchors <- unique(cholera::anchor.case[sel, "anchor"])
      }

      select.cases <- cholera::fatalities$case %in% seg.cases
      select.anchors <- cholera::fatalities.address$anchor %in% seg.anchors

      if (cases == "fatality") {
        text(cholera::fatalities[!select.cases, vars],
          labels = cholera::fatalities$case[!select.cases], cex = cex.text)
        if (any(select.cases)) {
          if (highlight) {
            text(cholera::fatalities[select.cases, vars],
              labels = cholera::fatalities$case[select.cases], cex = cex.text,
              col = "red")
          } else {
            text(cholera::fatalities[select.cases, vars],
              labels = cholera::fatalities$case[select.cases], cex = cex.text)
          }
        }
      } else if (cases == "address") {
        text(cholera::fatalities.address[!select.anchors, vars],
          labels = cholera::fatalities.address$anchor[!select.anchors],
          cex = cex.text)
        if (any(select.anchors)) {
          if (highlight) {
            text(cholera::fatalities.address[select.anchors, vars],
              labels = cholera::fatalities.address$anchor[select.anchors],
              cex = cex.text, col = "red")
          } else {
            text(cholera::fatalities.address[select.anchors, vars],
              labels = cholera::fatalities.address$anchor[select.anchors],
              cex = cex.text)
          }
        }
      }
    }
  }

  if (add.pump) {
    addPump(col = "blue", pch = 17, latlong = latlong, vestry = vestry)
  }

  if (!is.null(segment.id)) {
     if (highlight) {
      lapply(segment.id, function(seg) {
        tmp <- rd.segs[rd.segs$id == seg, ]
        segments(tmp[, paste0(ew, 1)], tmp[, paste0(ns, 1)],
                 tmp[, paste0(ew, 2)], tmp[, paste0(ns, 2)],
                 col = "red", lwd = 3)
      })
    }

    if (add.title) {
      if (length(segment.id) == 1) {
        sel <- cholera::road.segments$id %in% segment.id
        seg.nm <- cholera::road.segments[sel, "name"]
        title(main = paste0(seg.nm, ": Segment ", segment.id))
      } else if (length(segment.id) > 1) {
        title(main = paste(segment.id, collapse = ", "))
      }
    }

    if (add.subtitle) {
      segment.length <- segmentLength(id = segment.id, latlong = latlong)

      if (length(segment.id) > 1) segment.length <- sum(segment.length)
      
      est.time <- distanceTime(segment.length, distance.unit = distance.unit,
        time.unit = time.unit, walking.speed = walking.speed)
      nominal.time <- nominalTime(est.time, time.unit)
      segment.length <- unitMeter(segment.length, distance.unit = distance.unit)

      if (distance.unit == "meter") {
        subtitle <- paste(round(segment.length, 1), "m;", nominal.time)
      } else if (distance.unit == "yard") {
        subtitle <- paste(round(segment.length, 1), "yd;", nominal.time)
      } else if (distance.unit == "native") {
        subtitle <- paste(round(segment.length, 1), "units;", nominal.time)
      }

      title(sub = paste(subtitle, "@", walking.speed, "km/hr"))
    }
  }
}
