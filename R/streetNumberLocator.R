#' Locate street by its numerical ID.
#'
#' Highlight selected road segment(s) and cases.
#' @param street.number Character. A vector of segment IDs. See Note.
#' @param zoom Logical or Numeric. Positive numbers zoom in; negative numbers zoom out.
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
#' @note See \code{cholera::roads}.
#' @export
#' @examples
#' streetNumberLocator(216)
#' streetNumberLocator(216, zoom = -10)
#' streetNumberLocator(216, latlong = TRUE, zoom = -10)
#' streetNumberLocator(216, distance.unit = "yard")
#' streetNumberLocator(216, zoom = FALSE)

streetNumberLocator <- function(street.number = 216, zoom = TRUE,
  latlong = FALSE, cases = "address", token = "id", vestry = FALSE,
  add.pump = TRUE, add.title = TRUE, add.subtitle = TRUE, highlight = TRUE,
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

  rds <- cholera::roads
  vars <- c(ew, ns)

  if (!is.null(street.number)) {
    st.obs <- street.number %in% rds$street
    if (all(st.obs == FALSE)) {
      error.msg <- "Invalid street number."
      stop(error.msg, call. = FALSE)
    } else if (any(st.obs == FALSE)) {
      no.err <- street.number[!st.obs]
      no.msg <- "Misspelled or invalid sreet."
      message(paste(no.msg, paste(no.err, collapse = ", ")))
      street.number <- street.number[st.obs]
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

  if (is.null(street.number) | isFALSE(zoom)) {
    xlim <- range(rds[, ew])
    ylim <- range(rds[, ns])
  } else if (isTRUE(zoom) | is.numeric(zoom)) {
    sel <- rds$street %in% street.number
    xlim <- range(rds[sel, ew])
    ylim <- range(rds[sel, ns])

    if (zoom != 0) {
      if (latlong) {
        geo.vars <- c("lon", "lat")

        seg.vars <- paste0(geo.vars, 1)
        dat <- stats::setNames(rd.segs[, c("id", seg.vars)], c("id", geo.vars))
        ones <- geoCartesian(dat)

        seg.vars <- paste0(geo.vars, 2)
        dat <- stats::setNames(rd.segs[, c("id", seg.vars)], c("id", geo.vars))
        twos <- geoCartesian(dat)

        new.vars <- c("x", "y")
        ones <- stats::setNames(ones, c("id", paste0(new.vars, 1)))
        twos <- stats::setNames(twos, c("id", paste0(new.vars, 2)))
        cartesian.rds <- merge(ones, twos, by = "id")

        st.seg <- rd.segs[rd.segs$street %in% street.number, "id"]
        cart.rd <- cartesian.rds[cartesian.rds$id %in% st.seg, ]

        cart.x.range <- range(cart.rd[, paste0("x", 1:2)])
        cart.y.range <- range(cart.rd[, paste0("y", 1:2)])

        pad <- c(zoom, -zoom)
        xlim <- cart.x.range + pad
        ylim <- cart.y.range + pad

        xlim.delta <- xlim[2] - xlim[1]
        ylim.delta <- ylim[2] - ylim[1]

        if (xlim.delta <= 0 | ylim.delta <= 0) {
          xlim <- cart.x.range
          ylim <- cart.y.range
          message("Note: zoom = ", zoom, " too far! Use smaller.")
        }

        range.data <- meterLatLong(data.frame(x = xlim, y = ylim))
        xlim <- range.data$lon
        ylim <- range.data$lat

      } else {
        ols <- stats::lm(y ~ x, data = data.frame(x = xlim, y = ylim))
        slope <- stats::coef(ols)[2]
        theta <- atan(slope)

        pad <- abs(zoom) / unitMeter(1)
        delta.x <- abs(pad * cos(theta))
        delta.y <- abs(pad * sin(theta))

        if (zoom < 0) {
          xlim <- c(xlim[1] - delta.x, xlim[2] + delta.x)
          ylim <- c(ylim[1] - delta.y, ylim[2] + delta.y)
        } else if (zoom > 0) {
          xlim <- c(xlim[1] + delta.x, xlim[2] - delta.x)
          ylim <- c(ylim[1] + delta.y, ylim[2] - delta.y)
        }

        ## diagnostic
        # st.data <- rds[rds$street %in% street.number, vars]
        # plot(st.data, asp = 1, xlim = xlim, ylim = ylim)
        # abline(coef(ols)[1], slope, lty = "dotted")
        # points(xy.lim, pch = 15)

        xlim.delta <- xlim[2] - xlim[1]
        ylim.delta <- ylim[2] - ylim[1]

        if (xlim.delta <= 0 | ylim.delta <= 0) {
          sel <- rds$street %in% street.number
          xlim <- range(rds[sel, ew])
          ylim <- range(rds[sel, ns])
          message("Note: zoom = ", zoom, " too far! Use smaller.")
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
      seg.id <- rd.segs[rd.segs$street %in% street.number, "id"]
      seg.ortho <- proj.data[proj.data$road.segment %in% seg.id, ]

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

  if (!is.null(street.number)) {
     if (highlight) {
      st.segs <- rd.segs[rd.segs$street %in% street.number, "id"]
      invisible(lapply(st.segs, function(seg) {
        tmp <- rd.segs[rd.segs$id == seg, ]
        segments(tmp[, paste0(ew, 1)], tmp[, paste0(ns, 1)],
                 tmp[, paste0(ew, 2)], tmp[, paste0(ns, 2)],
                 col = "red", lwd = 3)
      }))
    }

    if (add.title) {
      if (length(street.number) == 1) {
        title(main = street.number)
      } else if (length(street.number) > 1) {
        title(main = paste(street.number, collapse = ", "))
      }
    }

    if (add.subtitle) {
      st.segs <- rd.segs[rd.segs$street %in% street.number, "id"]
      segment.length <- sum(segmentLength(id = st.segs, latlong = latlong))
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
