#' Locate street(s) by name(s).
#'
#' Highlight selected road(s) and cases.
#' @param street.name Character. A street name or vector of street names (e.g., "Broad Street", "Poland Street").
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
#' @note See \code{streetNames()}.
#' @export
#' @examples
#' streetNameLocator("broad street")
#' streetNameLocator("Broad Street", zoom = -10)
#' streetNameLocator("Broad Street", latlong = TRUE, zoom = -10)
#' streetNameLocator("Broad Street", distance.unit = "yard")
#' streetNameLocator("Broad Street", zoom = FALSE)

streetNameLocator <- function(street.name = "Broad Street", zoom = TRUE,
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

  if (!is.null(street.name)) {
    street.name <- vapply(street.name, caseAndSpace, character(1L))
    st.obs <- street.name %in% rds$name
    if (all(st.obs == FALSE)) {
      error.msg <- "Invalid street name."
      stop(error.msg, call. = FALSE)
    } else if (any(st.obs == FALSE)) {
      no.err <- street.name[!st.obs]
      no.msg <- "Misspelled or invalid sreet name."
      message(paste(no.msg, paste(no.err, collapse = ", ")))
      street.name <- street.name[st.obs]
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

  if (is.null(street.name) | isFALSE(zoom)) {
    xlim <- range(rds[, ew])
    ylim <- range(rds[, ns])
  } else if (isTRUE(zoom) | is.numeric(zoom)) {
    sel <- rds$name %in% street.name
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

        st.seg <- rd.segs[rd.segs$name %in% street.name, "id"]
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
        # st.data <- rds[rds$name %in% street.name, vars]
        # plot(st.data, asp = 1, xlim = xlim, ylim = ylim)
        # abline(coef(ols)[1], slope, lty = "dotted")
        # points(xy.lim, pch = 15)

        xlim.delta <- xlim[2] - xlim[1]
        ylim.delta <- ylim[2] - ylim[1]

        if (xlim.delta <= 0 | ylim.delta <= 0) {
          sel <- rds$name %in% street.name
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
      seg.id <- rd.segs[rd.segs$name %in% street.name, "id"]
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

  if (!is.null(street.name)) {
     if (highlight) {
      st.segs <- rd.segs[rd.segs$name %in% street.name, "id"]
      invisible(lapply(st.segs, function(seg) {
        tmp <- rd.segs[rd.segs$id == seg, ]
        segments(tmp[, paste0(ew, 1)], tmp[, paste0(ns, 1)],
                 tmp[, paste0(ew, 2)], tmp[, paste0(ns, 2)],
                 col = "red", lwd = 3)
      }))
    }

    if (add.title) {
      if (length(street.name) == 1) {
        title(main = street.name)
      } else if (length(street.name) > 1) {
        title(main = paste(street.name, collapse = ", "))
      }
    }

    if (add.subtitle) {
      st.segs <- rd.segs[rd.segs$name %in% street.name, "id"]
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

#' @importFrom tools toTitleCase

caseAndSpace <- function(name) {
  valid.names <- unique(cholera::roads$name)
  name.parts <- unlist(strsplit(name, " "))
  extra.spaces <- vapply(name.parts, nchar, integer(1L))

  if (any(extra.spaces == 0)) {
    name.parts <- name.parts[extra.spaces != 0]
    road.name.string <- paste(name.parts, collapse = " ")
  } else {
    road.name.string <- paste(name.parts, collapse = " ")
  }

  if (road.name.string %in% valid.names) {
    string.out <- road.name.string
  } else {
    lo.case <- tolower(road.name.string)

    road.name.string <- unlist(strsplit(lo.case, " "))
    vec.length <- seq_along(road.name.string)
    word.case <- wordCase(road.name.string)
    string.out <- paste0(word.case, collapse = " ")
  }

  if (string.out %in% valid.names) {
    return(string.out)
  } else {

    # ------- tests ------- #

    # George Court (I) #
    test.multiple.name <- vapply(road.name.string, function(x) {
      grepl("(", x, fixed = TRUE)
    }, logical(1L))

    # Macclesfield Street/Gerrard Street #
    test.two.roads <- vapply(road.name.string, function(x) {
      grepl("/", x, fixed = TRUE)
    }, logical(1L))

    # Adam and Eve Court" #
    test.and <- "and" %in% road.name.string

    # Unknown-A1 #
    test.unknown <- grepl("unknown", road.name.string)

    # ------- road strings ------- #

    if (any(test.multiple.name)) {
      multi.name <- vapply(road.name.string, function(x) {
        grepl("(", x, fixed = TRUE)
      }, logical(1L))

      others.position <- which(multi.name == FALSE)
      multi.position <- which(multi.name)
      word.case <- wordCase(road.name.string[others.position])
      multi.case <- toupper(road.name.string[multi.position])
      string.out <- paste(c(word.case, multi.case), collapse = " ")

    } else if (any(test.two.roads)) {
      slash.position <- grep("/", road.name.string)

      if (road.name.string[test.two.roads] == "/") {
        # isolated "/": "Princes Street / Hanover Square"
        slash.names <- c(slash.position - 1, slash.position, slash.position + 1)
        word.case <- wordCase(road.name.string)
        pre <- word.case[vec.length < min(slash.names)]
        delimited <- paste(word.case[slash.names], collapse = "")
        post.select <- vec.length > max(slash.names)

        if (any(post.select)) {
          # "Princes Street/Hanover Square"
          post <- word.case[post.select]
          string.out <- paste(pre, delimited, post, collapse = " ")
        } else {
          # "Richmond Buildings/Mews"
          string.out <- paste(pre, delimited, collapse = " ")
        }

      } else {
        # "Princes Street /Hanover Square", "Princes Street/ Hanover Square"
        word.case <- wordCase(road.name.string)
        pre <- word.case[vec.length < slash.position]
        delimited <- tools::toTitleCase(word.case[slash.position])
        post.select <- vec.length > slash.position

        if (any(post.select)) {
          post <- word.case[post.select]

          if (length(pre) == 1 & sum(post.select) == 2) {
            string.out <- paste(pre, paste0(delimited, post[1]), post[2],
              collapse = " ")
          } else if (length(pre) == 2 & sum(post.select) == 1) {
            string.out <- paste(pre[1], paste0(pre[2], delimited), post,
              collapse = " ")
          } else if (length(pre) == 1 & sum(post.select) == 1) {
            string.out <- paste(pre, paste0(delimited, post), collapse = " ")
          }
        } else {
          string.out <- paste(pre[1], paste0(pre[2], delimited), collapse = " ")
        }
      }

    } else if (test.and) {
      others.position <- which(road.name.string %in% "and" == FALSE)
      and.position <- which(road.name.string == "and")
      word.case <- wordCase(road.name.string[others.position])
      string.out <- paste0(c(word.case[others.position < and.position],
        tolower(road.name.string[and.position]),
        word.case[others.position > and.position]), collapse = " ")

    } else if (any(test.unknown)) {
      dash.position <- grep("-", road.name.string)

      if (road.name.string[dash.position] == "-") {
        # isolated "-": "Unknown - C"
        name.parts <- unlist(strsplit(road.name.string, "-"))
        word.case <- wordCase(name.parts)
        string.out <- paste0(word.case[1], "-", word.case[3])
      } else {
        # "Unknown- C", "Unknown -C"
        word.case <- tools::toTitleCase(road.name.string)
        string.out <- paste(word.case, collapse = "")
      }
    }
  }
  string.out
}

computeCoords <- function(endpt, intercept, slope) {
  y.prime <- slope * endpt$x + intercept
  x.prime <- (y.prime - intercept) / slope
  data.frame(x = x.prime, y = y.prime, row.names = NULL)
}

wordCase <- function(x) {
  # faster than tools::toTitleCase(), bytecode?
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}
