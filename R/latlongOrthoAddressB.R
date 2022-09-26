#' Compute latitude and longitude for orthogonal case projection (address).
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @export

latlongOrthoAddressB <- function(multi.core = TRUE) {
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

  # geo.addr$id[which(vapply(orthogonal.projection, length, numeric(1L)) != 5)]

  orthogonal.projection <- parallel::mclapply(geo.addr$id, function(id) {
    case <- geo.addr[geo.addr$id == id, c("x", "y")]

    within.radius <- lapply(geo.rd.segs$id, function(x) {
      seg.data <- geo.rd.segs[geo.rd.segs$id == x, ]
      test1 <- cholera::withinRadius(case, seg.data[, c("x1", "y1")], 55)
      test2 <- cholera::withinRadius(case, seg.data[, c("x2", "y2")], 55)
      if (any(test1, test2)) unique(seg.data$id)
    })

    within.radius <- unlist(within.radius)

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
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

    out <- do.call(rbind, ortho.proj.test)

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

  coords <- do.call(rbind, orthogonal.projection)

  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  topleft <- data.frame(lon = min(cholera::roads$lon),
                        lat = max(cholera::roads$lat))
  bottomright <- data.frame(lon = max(cholera::roads$lon),
                            lat = min(cholera::roads$lat))

  est.lonlat <- meterLatLong(coords, origin, topleft, bottomright)
  est.lonlat[order(est.lonlat$case), ]
}
