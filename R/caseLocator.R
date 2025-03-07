#' Locate case by numerical ID.
#'
#' Highlight selected observed or simulated case and its home road segment.
#' @param case Numeric or Integer. Whole number between 1 and 578.
#' @param zoom Logical or Numeric. Positive numbers zoom in; negative numbers zoom out.
#' @param observed Logical. \code{TRUE} for observed. \code{FALSE} for simulated.
#' @param latlong Logical. Longitude and latitude coordinates
#' @param add.title Logical. Include title.
#' @param highlight.segment Logical. Highlight case's segment.
#' @param data Logical. Output data.
#' @param add Logical. Add to existing plot or separate plot.
#' @param col Character. Point color.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @return A base R graphics plot.
#' @export
#' @examples
#' caseLocator(290)
#' caseLocator(290, zoom = TRUE)
#' caseLocator(290, observed = FALSE)
#' caseLocator(290, latlong = TRUE, zoom = TRUE)

caseLocator <- function(case = 1, zoom = FALSE, observed = TRUE,
  latlong = FALSE, add.title = TRUE, highlight.segment = TRUE, data = FALSE,
  add = FALSE, col = "red", vestry = FALSE) {

  if (latlong) {
    asp  <- 1.6
    ew <- "lon"
    ns <- "lat"
    proj.data <- cholera::latlong.ortho.addr
    rd.segs <- roadSegments(latlong = latlong)
    reg.data <- cholera::latlong.regular.cases
    sim.proj.data <- cholera::latlong.sim.ortho.proj
    if (isFALSE(case %in% proj.data$case)) {
      case0 <- case
      case <- cholera::anchor.case[cholera::anchor.case$case == case, "anchor"]
    }
  } else {
    asp  <- 1
    ew <- "x"
    ns <- "y"
    proj.data <- cholera::ortho.proj
    rd.segs <- cholera::road.segments
    reg.data <- cholera::regular.cases
    sim.proj.data <- cholera::sim.ortho.proj
  }

  vars <- c(ew, ns)

  if (vestry) pmp <- cholera::pumps.vestry
  else pmp <- cholera::pumps

  if (!is.numeric(case)) stop("case must be numeric.", call. = FALSE)

  if (observed) {
    if (case %in% unique(cholera::fatalities$case) == FALSE) {
      stop("Observed case must be a whole number between 1 and 578.",
        call. = FALSE)
    }
  } else {
    if (case %in% seq_len(nrow(reg.data)) == FALSE) {
      reg.obs.ct <- format(nrow(reg.data), big.mark = ",")
      stop("Simulated case must be a whole number between 1 and ", reg.obs.ct,
        ".", call. = FALSE)
    }
  }

  if (observed) {
    case.seg <- proj.data[proj.data$case == case, "road.segment"]
    if (exists("case0")) {
      case.data <- cholera::fatalities[cholera::fatalities$case == case0, vars]
    } else {
      case.data <- cholera::fatalities[cholera::fatalities$case == case, vars]
    }
  } else {
    case.seg <- sim.proj.data[sim.proj.data$case == case, "road.segment"]
    case.data <- reg.data[case, vars]
  }

  seg.data <- rd.segs[rd.segs$id == case.seg, ]

  if (add == TRUE) {
    points(case.data, col = col, lwd = 2)
  } else {
    if (data == TRUE) {
      list(case = case, segment.data = seg.data)
    } else {
      if (isFALSE(zoom)) {
        xlim <- range(cholera::roads[, ew])
        ylim <- range(cholera::roads[, ns])
      } else if (isTRUE(zoom) | is.numeric(zoom)) {
        sel <- rd.segs$id %in% case.seg
        xlim <- range(c(unlist(rd.segs[sel, paste0(ew, 1:2)]), case.data[, ew]))
        ylim <- range(c(unlist(rd.segs[sel, paste0(ns, 1:2)]), case.data[, ns]))

        if (zoom != 0) {
          if (latlong) {
            geo.vars <- c("lon", "lat")

            seg.vars <- paste0(geo.vars, 1)
            dat <- stats::setNames(rd.segs[, c("id", seg.vars)],
                                   c("id", geo.vars))
            ones <- geoCartesian(dat)

            seg.vars <- paste0(geo.vars, 2)
            dat <- stats::setNames(rd.segs[, c("id", seg.vars)],
                                   c("id", geo.vars))
            twos <- geoCartesian(dat)

            new.vars <- c("x", "y")
            ones <- stats::setNames(ones, c("id", paste0(new.vars, 1)))
            twos <- stats::setNames(twos, c("id", paste0(new.vars, 2)))
            cartestian.rds <- merge(ones, twos, by = "id")

            st.seg <- rd.segs[rd.segs$id %in% case.seg, "id"]
            cart.rd <- cartestian.rds[cartestian.rds$id %in% st.seg, ]

            cart.case <- geoCartesianCoord(case.data)

            cart.x.range <- range(cart.rd[, paste0("x", 1:2)], cart.case$x)
            cart.y.range <- range(cart.rd[, paste0("y", 1:2)], cart.case$y)

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
            xs <- c(seg.data[, paste0(ew, 1)], seg.data[, paste0(ew, 2)])
            ys <- c(seg.data[, paste0(ns, 1)], seg.data[, paste0(ns, 2)])
            seg.df <- data.frame(x = xs, y = ys)

            ols <- stats::lm(y ~ x, data = seg.df)
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

            xlim.delta <- xlim[2] - xlim[1]
            ylim.delta <- ylim[2] - ylim[1]

            if (xlim.delta <= 0 | ylim.delta <= 0) {
              sel <- rd.segs$id %in% case.seg
              xlim <- range(rd.segs[sel, paste0(ew, 1:2)])
              ylim <- range(rd.segs[sel, paste0(ns, 1:2)])
              message("Note: zoom = ",  zoom, " too far! Use smaller.")
            }
          }
        }
      }

      plot(cholera::fatalities[, vars], xlim = xlim, ylim = ylim, pch = 15,
        cex = 0.5, col = "gray", asp = asp)
      addRoads(latlong = latlong)
      addFrame(latlong = latlong)

      points(pmp[, vars], pch = 17, cex = 1, col = "blue")
      text(pmp[, vars], label = pmp$id, pos = 1)

      if (observed) {
        points(case.data, col = col, lwd = 2)

        if (isTRUE(zoom) | is.numeric(zoom)) {
          if (highlight.segment) {
            segments(seg.data[, paste0(ew, 1)], seg.data[, paste0(ns, 1)],
                     seg.data[, paste0(ew, 2)], seg.data[, paste0(ns, 2)],
                     col = "red", lwd = 2)
          }
        }

        if (add.title) {
          if (exists("case0")) {
            title(main = paste0("Obs Case ", case0, "; ", seg.data$name, " ",
                                seg.data$id))
          } else {
            title(main = paste0("Obs Case ", case, "; ", seg.data$name, " ",
                                seg.data$id))
          }
        }
      } else {
        points(reg.data[case, vars], col = col, lwd = 2)

        if (isTRUE(zoom) | is.numeric(zoom)) {
          if (highlight.segment) {
            segments(seg.data[, paste0(ew, 1)], seg.data[, paste0(ns, 1)],
                     seg.data[, paste0(ew, 2)], seg.data[, paste0(ns, 2)],
                     col = "red", lwd = 2)
          }
        }

        if (add.title) {
          title(main = paste0("Sim Case ", case, "; ", seg.data$name, " ",
                              seg.data$id))
        }
      }
    }
  }
}
