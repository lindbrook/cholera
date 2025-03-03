#' Compute pump coordinates.
#'
#' Returns either the set of x-y coordinates for the pumps themselves or for their orthogonally projected "addresses" on the network of roads.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores (rounds with \code{as.integer()}). See \code{vignette("Parallelization")} for details.
#' @noRd

latlongOrthoPump <- function(vestry = FALSE, multi.core = FALSE) {
  cores <- multiCore(multi.core)

  if (vestry) {
    pmp <- cholera::pumps.vestry
  } else {
    pmp <- cholera::pumps
  }

  geo.pmp <- geoCartesian(pmp)

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

  # test <- rbind(case, stats::setNames(geo.rd.segs[geo.rd.segs$id == "19-1", c("x1", "y1")], c("x", "y")))

  orthogonal.projection <- parallel::mclapply(geo.pmp$id, function(p) {
    case <- geo.pmp[geo.pmp$id == p, c("x", "y")]

    within.radius <- lapply(geo.rd.segs$id, function(x) {
      seg.data <- geo.rd.segs[geo.rd.segs$id == x, ]

      # dist(rbind(case, stats::setNames(seg.data[, c("x1", "y1")], c("x", "y"))))
      # dist(rbind(case, stats::setNames(seg.data[, c("x2", "y2")], c("x", "y"))))

      test1 <- withinRadius(case, seg.data[, c("x1", "y1")], 35)
      test2 <- withinRadius(case, seg.data[, c("x2", "y2")], 35)
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
      x.proj <- (ortho.intercept - road.intercept) / (road.slope - ortho.slope)
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

    out$id <- p
    row.names(out) <- NULL
    out
  }, mc.cores = cores)

  coords <- do.call(rbind, orthogonal.projection)
  est.lonlat <- meterLatLong(coords)
  est.lonlat[order(est.lonlat$id), ]
}

# latlong.ortho.pump <- cholera:::latlongOrthoPump(vestry = FALSE, multi.core = TRUE)
# latlong.ortho.pump.vestry <- cholera:::latlongOrthoPump(vestry = TRUE, multi.core = TRUE)

# usethis::use_data(latlong.ortho.pump, overwrite = TRUE)
# usethis::use_data(latlong.ortho.pump.vestry, overwrite = TRUE)
