#' Locate water pump by numerical ID.
#'
#' Highlight selected water pump.
#' @param id Numeric or Integer. With \code{vestry = TRUE}, a whole number between 1 and 14. With \code{vestry = FALSE}, a whole number between 1 and 13. See \code{cholera::pumps.vestry} and \code{cholera::pumps} for IDs and details about specific pumps.
#' @param zoom Logical or Numeric. Positive numbers zoom in; negative numbers zoom out.
#' @param vestry Logical. \code{TRUE} for the 14 pumps from Vestry Report. \code{FALSE} for the original 13 pumps.
#' @param add.title Logical. Include title.
#' @param highlight.segment Logical. Highlight case's segment.
#' @param latlong Logical. Use longitude and latitude.
#' @seealso\code{\link{pumpData}}
#' @return A base R graphics plot.
#' @export
#' @examples
#' pumpLocator()
#' pumpLocator(zoom = TRUE)
#' pumpLocator(14, vestry = TRUE, zoom = TRUE)

pumpLocator <- function(id = 7, zoom = FALSE,  vestry = FALSE, add.title = TRUE,
  highlight.segment = TRUE, latlong = FALSE) {

  if (is.numeric(id) == FALSE) stop('id must be numeric.', call. = FALSE)

  if (!vestry & id %in% cholera::pumps$id == FALSE) {
    stop('For original pumps, id must be a whole number between 1 and 13.',
      call. = FALSE)
  }

  if (vestry & id %in% cholera::pumps.vestry$id == FALSE) {
    stop('For vestry pumps, id must be a whole number between 1 and 14.',
      call. = FALSE)
  }

  if (vestry) {
    p.data <- cholera::pumps.vestry
    if (latlong) {
      ortho.data <- cholera::latlong.ortho.pump.vestry
      names(ortho.data)[names(ortho.data) == "id"] <- "pump.id"
    } else {
      ortho.data <- cholera::ortho.proj.pump.vestry
    }
  } else {
    p.data <- cholera::pumps
    if (latlong) {
      ortho.data <- cholera::latlong.ortho.pump
      names(ortho.data)[names(ortho.data) == "id"] <- "pump.id"
    } else {
      ortho.data <- cholera::ortho.proj.pump
    }
  }

  if (latlong) {
    asp  <- 1.6
    vars <- c("lon", "lat")
    ew <- vars[1]
    ns <- vars[2]
    rd.segs <- cholera::roadSegments(latlong = TRUE)
  } else {
    asp  <- 1
    vars <- c("x", "y")
    ew <- vars[1]
    ns <- vars[2]
    rd.segs <- cholera::road.segments
  }

  roads.list <- split(cholera::roads[, vars], cholera::roads$street)
  p.seg <- ortho.data[ortho.data$pump.id == id, "road.segment"]
  seg.data <- rd.segs[rd.segs$id == p.seg, ]

  if (isFALSE(zoom)) {
    xlim <- range(cholera::roads[, ew])
    ylim <- range(cholera::roads[, ns])

  } else if (isTRUE(zoom) | zoom == 0) {
    padding <- ifelse(latlong, 0.0000125, 0.05)
    xlim <- c(min(seg.data[, paste0(ew, 1:2)]) - padding,
              max(seg.data[, paste0(ew, 1:2)]) + padding)
    ylim <- c(min(seg.data[, paste0(ns, 1:2)]) - padding,
              max(seg.data[, paste0(ns, 1:2)]) + padding)

  } else if (is.numeric(zoom) & zoom != 0) {
    if (latlong) {
      geo.vars <- c("lon", "lat")
      new.vars <- c("id", geo.vars)

      seg.vars <- paste0(geo.vars, 1)
      dat <- stats::setNames(rd.segs[, c("id", seg.vars)], new.vars)
      ones <- geoCartesian(dat)

      seg.vars <- paste0(geo.vars, 2)
      dat <- stats::setNames(rd.segs[, c("id", seg.vars)], new.vars)
      twos <- geoCartesian(dat)

      new.vars <- c("x", "y")
      ones <- stats::setNames(ones, c("id", paste0(new.vars, 1)))
      twos <- stats::setNames(twos, c("id", paste0(new.vars, 2)))
      cartestian.rds <- merge(ones, twos, by = "id")

      cart.rd <- cartestian.rds[cartestian.rds$id %in% seg.data$id, ]
      cart.x.range <- range(cart.rd[, paste0("x", 1:2)])
      cart.y.range <- range(cart.rd[, paste0("y", 1:2)])

      padding <- c(zoom, -zoom)
      xlim <- cart.x.range + padding
      ylim <- cart.y.range + padding

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
      xlim <- c(min(seg.data[, paste0(ew, 1:2)]),
                max(seg.data[, paste0(ew, 1:2)]))
      ylim <- c(min(seg.data[, paste0(ns, 1:2)]),
                max(seg.data[, paste0(ns, 1:2)]))

      seg.df <- data.frame(x = xlim, y = ylim)
      ols <- stats::lm(y ~ x, data = seg.df)
      slope <- stats::coef(ols)[2]
      theta <- atan(slope)

      padding <- abs(zoom) / unitMeter(1)
      delta.x <- abs(padding * cos(theta))
      delta.y <- abs(padding * sin(theta))

      if (zoom < 0) {
        xlim <- c(xlim[1] - delta.x, xlim[2] + delta.x)
        ylim <- c(ylim[1] - delta.y, ylim[2] + delta.y)
      } else if (zoom > 0) {
        xlim <- c(xlim[1] + delta.x, xlim[2] - delta.x)
        ylim <- c(ylim[1] + delta.y, ylim[2] - delta.y)
      }

      xlim.delta <- xlim[2] - xlim[1]
      ylim.delta <- ylim[2] - ylim[1]

      if (xlim.delta <= 0 | ylim.delta <= 0) {
        sel <- rd.segs$id %in% seg.data$id
        xlim <- range(rd.segs[sel, paste0(ew, 1:2)])
        ylim <- range(rd.segs[sel, paste0(ns, 1:2)])
        message("Note: zoom = ",  zoom, " too far! Use smaller.")
      }
    }
  }

  plot(cholera::fatalities[, vars], xlim = xlim, ylim = ylim, pch = 15,
    cex = 0.5, col = "lightgray", asp = asp)
  invisible(lapply(roads.list, lines, col = "gray"))
  points(p.data[p.data$id != id, vars], pch = 2, cex = 1, col = "blue")
  points(p.data[p.data$id == id, vars], pch = 17, cex = 1, col = "red")
  text(p.data[p.data$id == id, vars], label = p.data$id[p.data$id == id],
    pos = 1, col = "red")

  if (highlight.segment) {
    segments(seg.data[, paste0(ew, 1)], seg.data[, paste0(ns, 1)],
             seg.data[, paste0(ew, 2)], seg.data[, paste0(ns, 2)],
             col = "red", lwd = 2)
  }

  if (add.title) {
    if (vestry) {
      title(main = paste0("Vestry Pump #", id, "; ", seg.data$name, " ",
        seg.data$id))
    } else {
      title(main = paste0("Pump #", id, "; ", seg.data$name, " ", seg.data$id))
    }
  }
}
