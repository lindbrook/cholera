#' Compute latitude and longitude for orthogonal case projection (address).
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @noRd

latlongOrthoAddress <- function(multi.core = TRUE) {
  cores <- multiCore(multi.core)

  geo.addr <- geodesicMeters(dat = cholera::fatalities.address,
    case.address = TRUE)

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  geo.rd <- data.frame(street = rd$street, geodesicMeters(rd))

  geo.rd.segs <- lapply(unique(geo.rd$street), function(st) {
    dat <- geo.rd[geo.rd$street == st, ]
    names(dat)[names(dat) %in% c("x", "y")] <- c("x1", "y1")
    seg.data <- dat[-1, c("x1", "y1")]
    names(seg.data) <- c("x2", "y2")
    dat <- cbind(dat[-nrow(dat), ], seg.data)
    dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
    dat
  })

  geo.rd.segs <- do.call(rbind, geo.rd.segs)
  seg.endpts <- c("x1", "y1", "x2", "y2")

  # classification errors due to bar orientation
  road.segment.fix <- roadSegmentFix()

  # check for segments with multiple cases
  multi.audit <- vapply(road.segment.fix, function(x) {
    sum(x %in% cholera::fatalities.address$anchor)
  }, integer(1L))

  no.multi <- all(multi.audit %in% c(0, 1))

  if (no.multi) {
    anchors.fix <- unlist(lapply(road.segment.fix, function(x) {
      x[x %in% cholera::fatalities.address$anchor]
    }))
    manual.compute <- data.frame(addr = anchors.fix, seg = names(anchors.fix),
      row.names = NULL)
  } else stop()

  classfication.err <- geo.addr$id %in% manual.compute$addr

  no.err <- geo.addr$id[!classfication.err]

  orthogonal.projection <- parallel::mclapply(no.err, function(id) {
    case <- geo.addr[geo.addr$id == id, c("x", "y")]

    within.radius <- lapply(geo.rd.segs$id, function(x) {
      seg.data <- geo.rd.segs[geo.rd.segs$id == x, ]
      test1 <- withinRadius(case, seg.data[, c("x1", "y1")], 55)
      test2 <- withinRadius(case, seg.data[, c("x2", "y2")], 55)
      if (any(test1, test2)) unique(seg.data$id)
    })

    within.radius <- unlist(within.radius)

    out <- lapply(within.radius, function(seg.id) {
      sel <- geo.rd.segs$id == seg.id
      segment.data <- geo.rd.segs[sel, seg.endpts]
      road.segment <- data.frame(x = c(segment.data$x1, segment.data$x2),
                                 y = c(segment.data$y1, segment.data$y2))

      ols <- stats::lm(y ~ x, data = road.segment)
      road.intercept <- stats::coef(ols)[1]
      road.slope <- stats::coef(ols)[2]

      if (road.slope == 0 | is.na(road.slope)) {
        if (road.slope == 0) {
          x.proj <- case$x
          y.proj <- road.intercept
        } else if (is.na(road.slope)) {
          x.proj <- unique(road.segment$x)
          y.proj <- case$y
        }
      } else {
       ortho.slope <- -1 / road.slope
       ortho.intercept <- case$y - ortho.slope * case$x
       x.proj <- (ortho.intercept - road.intercept) / (road.slope - ortho.slope)
       y.proj <- road.slope * x.proj + road.intercept
      }

      seg.data <- geo.rd.segs[geo.rd.segs$id == seg.id, seg.endpts]
      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      # segment bisection/intersection test
      distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
               stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

      bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

      if (bisect.test) {
        ortho.dist <- c(stats::dist(rbind(c(case$x, case$y),
          c(x.proj, y.proj))))
        ortho.pts <- data.frame(x.proj, y.proj)
        data.frame(road.segment = seg.id, ortho.pts, ortho.dist)
      } else {
        null.out <- data.frame(matrix(NA, ncol = 4))
        names(null.out) <- c("road.segment", "x.proj", "y.proj", "ortho.dist")
        null.out
      }
    })

    out <- do.call(rbind, out)

    if (all(is.na(out)) == FALSE) {
      sel <- which.min(out$ortho.dist)
      out <- out[sel, ]
    } else {
      # all candidate roads are NA so arbitrarily choose the first obs.
      out <- out[1, ]
    }

    out$case <- id
    row.names(out) <- NULL
    out
  }, mc.cores = cores)

  coordsA <- do.call(rbind, orthogonal.projection)

  orthogonal.projectionB <- lapply(seq_len(nrow(manual.compute)), function(i) {
    addr <- manual.compute[i, "addr"]
    seg <- manual.compute[i, "seg"]

    case <- geo.addr[geo.addr$id == addr, c("x", "y")]
    segment.data <- geo.rd.segs[geo.rd.segs$id == seg, ]

    road.segment <- data.frame(x = c(segment.data$x1, segment.data$x2),
                               y = c(segment.data$y1, segment.data$y2))

    ols <- stats::lm(y ~ x, data = road.segment)
    road.intercept <- stats::coef(ols)[1]
    road.slope <- stats::coef(ols)[2]

    if (road.slope == 0 | is.na(road.slope)) {
      if (road.slope == 0) {
        x.proj <- case$x
        y.proj <- road.intercept
      } else if (is.na(road.slope)) {
        x.proj <- unique(road.segment$x)
        y.proj <- case$y
      }
    } else {
      ortho.slope <- -1 / road.slope
      ortho.intercept <- case$y - ortho.slope * case$x
      x.proj <- (ortho.intercept - road.intercept) / (road.slope - ortho.slope)
      y.proj <- road.slope * x.proj + road.intercept
    }

    seg.data <- geo.rd.segs[geo.rd.segs$id == seg, seg.endpts]
    seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                         y = c(seg.data$y1, seg.data$y2))

    # segment bisection/intersection test
    distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
      stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

    bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

    if (bisect.test) {
      ortho.dist <- c(stats::dist(rbind(c(case$x, case$y),
                                        c(x.proj, y.proj))))
      ortho.pts <- data.frame(x.proj, y.proj)
      data.frame(road.segment = seg, ortho.pts, ortho.dist)
    } else if (addr == 440) {
      # Case 440 check; nominal (non-latlong) does bisect (intersect) #
      # use nearest road segment endpoint #

      # tmp <- rbind(case, seg.df, c(x.proj, y.proj))
      # plot(seg.df, type = "l", xlim = range(tmp$x), ylim = range(tmp$y))
      # points(seg.df, pch = 15)
      # points(case, col = "red")
      # points(x.proj, y.proj, col = "red", pch = 0)
      # segments(case$x, case$y, x.proj, y.proj, col = "red")
      # segments(case$x, case$y, seg.df[2, "x"], seg.df[2, "y"],
      #   col = "dodgerblue")

      nominal.dist <- c(stats::dist(rbind(case, seg.df[2, ])))
      data.frame(road.segment = seg, x.proj = seg.df[2, "x"],
        y.proj = seg.df[2, "y"], ortho.dist = nominal.dist)
    } else {
      null.out <- data.frame(matrix(NA, ncol = 4))
      names(null.out) <- c("road.segment", "x.proj", "y.proj", "ortho.dist")
      null.out
    }
  })

  coordsB <- data.frame(do.call(rbind, orthogonal.projectionB),
    case = manual.compute$addr, row.names = NULL)

  ## Two manual address fixes ##

  # case 286 @ "160-3" edge case
  addr <- 286
  rd.seg  <- "160-3"
  case <- geo.addr[geo.addr$id == addr, ]
  x.proj <- geo.rd.segs[geo.rd.segs$id == rd.seg, "x2"]
  y.proj <- geo.rd.segs[geo.rd.segs$id == rd.seg, "y2"]
  ortho.dist <- stats::dist(rbind(case[, c("x", "y")], c(x.proj, y.proj)))
  vars <- c("x.proj", "y.proj", "ortho.dist")
  coordsA[coordsA$case == addr, vars] <- c(x.proj, y.proj, ortho.dist)
  coordsA[coordsA$case == addr, "road.segment"] <- rd.seg

  # case 440 @ "259-1" edge case
  addr <- 440
  rd.seg  <- "259-1"
  case <- geo.addr[geo.addr$id == addr, ]
  x.proj <- geo.rd.segs[geo.rd.segs$id == rd.seg, "x2"]
  y.proj <- geo.rd.segs[geo.rd.segs$id == rd.seg, "y2"]
  ortho.dist <- stats::dist(rbind(case[, c("x", "y")], c(x.proj, y.proj)))
  vars <- c("x.proj", "y.proj", "ortho.dist")
  coordsB[coordsB$case == addr, vars] <- c(x.proj, y.proj, ortho.dist)
  coordsB[coordsB$case == addr, "road.segment"] <- rd.seg

  coords <- rbind(coordsA, coordsB)
  coords <- coords[order(coords$case), ]

  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  topleft <- data.frame(lon = min(cholera::roads$lon),
                        lat = max(cholera::roads$lat))
  bottomright <- data.frame(lon = max(cholera::roads$lon),
                            lat = min(cholera::roads$lat))

  est.lonlat <- meterLatLong(coords, origin, topleft, bottomright)
  est.lonlat[order(est.lonlat$case), ]
}

# latlong.ortho.addr <- cholera:::latlongOrthoAddress(multi.core = TRUE)
# usethis::use_data(latlong.ortho.addr)
# usethis::use_data(latlong.ortho.addr, overwrite = TRUE)
