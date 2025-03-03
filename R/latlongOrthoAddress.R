#' Compute latitude and longitude for orthogonal case projection (address).
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param radius Numeric. Radius for \code{withinRadius()} to find road segment endpoints.
#' @return An R data frame.
#' @noRd

latlongOrthoAddress <- function(multi.core = FALSE, radius = 60) {
  cores <- multiCore(multi.core)

  geo.addr <- geoCartesian(dat = cholera::fatalities.address,
    case.address = TRUE)

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  geo.rd <- data.frame(street = rd$street, geoCartesian(rd))

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

  # classification errors due to bar orientation (Dodson and Tobler - nominal)
  road.segment.fix <- caseRoadClassificationFix()

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
      test1 <- withinRadius(case, seg.data[, c("x1", "y1")], radius)
      test2 <- withinRadius(case, seg.data[, c("x2", "y2")], radius)
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

  ## One manual address fixes for coordsA (edge case) ##

  addr <- 286
  rd.seg  <- "160-3"
  case <- geo.addr[geo.addr$id == addr, ]
  x.proj <- geo.rd.segs[geo.rd.segs$id == rd.seg, "x2"]
  y.proj <- geo.rd.segs[geo.rd.segs$id == rd.seg, "y2"]
  ortho.dist <- stats::dist(rbind(case[, c("x", "y")], c(x.proj, y.proj)))
  vars <- c("x.proj", "y.proj", "ortho.dist")
  coordsA[coordsA$case == addr, vars] <- c(x.proj, y.proj, ortho.dist)
  coordsA[coordsA$case == addr, "road.segment"] <- rd.seg

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
    } else {
      # pick nearest segment endpoint

      dist.to.endpts <- vapply(seq_len(nrow(seg.df)), function(i) {
        stats::dist(rbind(case, seg.df[i, ]))
      }, numeric(1L))

      sel <- which.min(dist.to.endpts)
      out <- data.frame(seg, seg.df[sel, ], dist.to.endpts[sel])
      names(out) <- c("road.segment", "x.proj", "y.proj", "ortho.dist")
      out
    }
  })

  coordsB <- data.frame(do.call(rbind, orthogonal.projectionB),
    case = manual.compute$addr, row.names = NULL)

  coords <- rbind(coordsA, coordsB)
  coords <- coords[order(coords$case), ]
  est.lonlat <- meterLatLong(coords)
  est.lonlat <- est.lonlat[order(est.lonlat$case), ]

  # Portland Mews (case 286) and Portland Street (case 369 @ St James Workhouse)
  # floating point estimation rounding - 286

  geo.seg <- roadSegments(TRUE)
  nom.seg <- roadSegments(FALSE)

  #

  case.georef <- est.lonlat[est.lonlat$case == 286, ]
  seg.georef <- geo.seg[geo.seg$id == "160-3", ]

  ep1 <- seg.georef[, c("lon1", "lat1")]
  ep2 <- seg.georef[, c("lon2", "lat2")]

  ds <- vapply(list(ep1, ep2), function(x) {
    geosphere::distGeo(case.georef[, c("lon", "lat")], x)
  }, numeric(1L))

  sel <- which.min(ds)

  if (sel == 1L) {
    est.lonlat[est.lonlat$case == 286, c("lon", "lat")] <- ep1
  } else if (sel == 2L) {
    est.lonlat[est.lonlat$case == 286, c("lon", "lat")] <- ep2
  } else stop("err!")

  # identical(est.lonlat[est.lonlat$case == 286, ]$lat, seg.georef$lat2)
  # identical(est.lonlat[est.lonlat$case == 286, ]$lon, seg.georef$lon2)

  #

  case.georef <- est.lonlat[est.lonlat$case == 369, ]
  seg.georef <- geo.seg[geo.seg$id == "194-1", ]

  ep1 <- seg.georef[, c("lon1", "lat1")]
  ep2 <- seg.georef[, c("lon2", "lat2")]

  ds <- vapply(list(ep1, ep2), function(x) {
    geosphere::distGeo(case.georef[, c("lon", "lat")], x)
  }, numeric(1L))

  sel <- which.min(ds)

  if (sel == 1L) {
    est.lonlat[est.lonlat$case == 369, c("lon", "lat")] <- ep1
  } else if (sel == 2L) {
    est.lonlat[est.lonlat$case == 369, c("lon", "lat")] <- ep2
  } else stop("err!")

  # identical(est.lonlat[est.lonlat$case == 369, ]$lon, seg.georef$lon1)
  # identical(est.lonlat[est.lonlat$case == 369, ]$lat, seg.georef$lat1)

  est.lonlat
}

# latlong.ortho.addr <- cholera:::latlongOrthoAddress(multi.core = TRUE)
# usethis::use_data(latlong.ortho.addr)
# usethis::use_data(latlong.ortho.addr, overwrite = TRUE)
