#' Compute landmark othogonal projection longitude and latitude (address).
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores (rounds with \code{as.integer()}). See \code{vignette("Parallelization")} for details.
#' @importFrom geosphere distGeo
#' @noRd

latlongOrthoLandmarks <- function(multi.core = FALSE) {
  cores <- multiCore(multi.core)
  lndmks <- cholera::landmarks

  # reset (delete) x.proj-y.proj for recomputation
  lndmks <- lndmks[, !names(lndmks) %in% c("lon.proj", "lat.proj")]

  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))

  # roads #

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
  seg.endpts <- names(geo.rd.segs)[!names(geo.rd.segs) %in% c("street", "id")]

  geo.lndmks <- do.call(rbind, lapply(lndmks$case, function(x) {
    tmp <- lndmks[lndmks$case == x, c("lon", "lat")]
    x.proj <- c(tmp$lon, origin$lat)
    y.proj <- c(origin$lon, tmp$lat)
    m.lon <- geosphere::distGeo(y.proj, tmp)
    m.lat <- geosphere::distGeo(x.proj, tmp)
    data.frame(id = x, x = m.lon, y = m.lat)
  }))

  # orthogonal projection road addresses #
  # Lion Brewery exception: entrance likely on Broad Street.

  orthogonal.projection <- parallel::mclapply(geo.lndmks$id, function(id) {
    if (id != 20004) {
      case <- geo.lndmks[geo.lndmks$id == id, c("x", "y")]

      within.radius <- lapply(geo.rd.segs$id, function(x) {
        seg.data <- geo.rd.segs[geo.rd.segs$id == x, ]
        # Check tolerance
        test1 <- withinRadius(case, seg.data[, c("x1", "y1")], 75)
        test2 <- withinRadius(case, seg.data[, c("x2", "y2")], 75)
        if (any(test1, test2)) unique(seg.data$id)
      })

      within.radius <- unlist(within.radius)

      ortho.proj.test <- lapply(within.radius, function(seg.id) {
        sel <- geo.rd.segs$id == seg.id
        segment.data <- geo.rd.segs[sel, seg.endpts]
        road.segment <- data.frame(x = c(segment.data$x1, segment.data$x2),
                                   y = c(segment.data$y1, segment.data$y2))

        # tmp <- rbind(road.segment, case)
        # plot(road.segment, xlim = range(tmp$x), ylim = range(tmp$y), asp = 1)
        # segments(segment.data$x1, segment.data$y1, segment.data$x2, segment.data$y2)
        # points(case, pch = 2, col = "red")
        # title(main = paste0("p", p))

        ols <- stats::lm(y ~ x, data = road.segment)
        road.intercept <- stats::coef(ols)[1]
        road.slope <- stats::coef(ols)[2]
        ortho.slope <- -1 / road.slope
        ortho.intercept <- case$y - ortho.slope * case$x
        x.proj <- (ortho.intercept - road.intercept) /
                  (road.slope - ortho.slope)
        y.proj <- road.slope * x.proj + road.intercept

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

      out <- do.call(rbind, ortho.proj.test)

      if (all(is.na(out)) == FALSE) {
        sel <- which.min(out$ortho.dist)
        out <- out[sel, ]
      } else {
        # all candidate roads are NA so arbitrarily choose the first obs.
        out <- out[1, ]
      }

      out$id <- id
      row.names(out) <- NULL
      out
    } else {
      broad.st <- geo.rd.segs[geo.rd.segs$id == "187-1", ]
      broad <- rbind(stats::setNames(broad.st[, c("x1", "y1")], c("x", "y")),
                     stats::setNames(broad.st[, c("x2", "y2")], c("x", "y")))

      h <- c(stats::dist(broad))

      ols <- stats::lm(y ~ x, data = broad)
      road.intercept <- stats::coef(ols)[1]
      road.slope <- stats::coef(ols)[2]
      theta <- ifelse(is.na(road.slope), pi / 2, atan(road.slope))

      delta.x <- (h / 2) * cos(theta)
      delta.y <- (h / 2) * sin(theta)

      left <- broad[which.min(broad$x), ]
      x.new <- left$x + delta.x
      y.new <- left$y + delta.y

      data.frame(road.segment = "187-1", x.proj = x.new, y.proj = y.new,
                 ortho.dist = 0, id = id)
    }

  }, mc.cores = cores)

  coords <- do.call(rbind, orthogonal.projection)
  est.lonlat <- meterLatLong(coords)
  est.lonlat[order(est.lonlat$id), ]
}
