#' Compute latlong pump coordinates.
#'
#' Computes the "addresses" or latlong coordinates of orthogonal projections onto the network of roads.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @noRd

latlongOrthoPump <- function(vestry = FALSE) {
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

  orthogonal.projection <- lapply(geo.pmp$id, function(p) {
    case <- geo.pmp[geo.pmp$id == p, c("x", "y")]
    pump.st <- pmp[pmp$id == p, "street"]
    
    sel <- cholera::road.segments$name == pump.st
    pump.segs <- cholera::road.segments[sel, "id"]

    within.radius <- lapply(pump.segs, function(s) {
      seg.data <- geo.rd.segs[geo.rd.segs$id == s, ]
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
      # plot(road.segment, xlim = range(tmp$x), ylim = range(tmp$y), asp = 1, 
      #   pch = NA)
      # points(case, pch = 2, col = "red")
      # # points(x.proj, y.proj, pch = 4, col = "red")

      # if (bisect.test) {
      #   arrows(case$x, case$y, x.proj, y.proj, col = "red", length = 1/10)
      # } else {
      #   arrows(case$x, case$y, x.proj, y.proj, col = "gray", length = 1/10)
      # }

      # # abline(a = ortho.intercept, b = ortho.slope, col = "red", lty = "dotted)
      # abline(ols, lty = "dotted", col = "gray")
      # segments(segment.data$x1, segment.data$y1, segment.data$x2, segment.data$y2)
      # title(main = paste0(seg.id, " -- ", paste0("p", p)))
      # # title(sub = paste0("p", p))
      # title(sub = bisect.test)

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
        ortho.dist <- c(stats::dist(rbind(case, c(x.proj, y.proj))))
        coords <- data.frame(x.proj, y.proj)
        data.frame(road.segment = seg.id, coords, d = ortho.dist,
          type = "ortho")
      } else {
        # nearest road segment endpoint
        d1 <- stats::dist(rbind(seg.df[1, ], case)) 
        d2 <- stats::dist(rbind(seg.df[2, ], case))
        prox.dist <- min(d1, d2)
        coords <- seg.df[which.min(c(d1, d2)), ]
        data.frame(road.segment = seg.id, x.proj = coords$x, y.proj = coords$y,
          d = prox.dist, type = "prox")
      }
    })

    out <- do.call(rbind, ortho.proj.test)
    out <- out[which.min(out$d), ]
    out$id <- p
    row.names(out) <- NULL
    out
  })

  coords <- do.call(rbind, orthogonal.projection)
  est.lonlat <- meterLatLong(coords)
  est.lonlat[order(est.lonlat$id), ]
}

# latlong.ortho.pump <- cholera:::latlongOrthoPump(vestry = FALSE)
# latlong.ortho.pump.vestry <- cholera:::latlongOrthoPump(vestry = TRUE)

# usethis::use_data(latlong.ortho.pump, overwrite = TRUE)
# usethis::use_data(latlong.ortho.pump.vestry, overwrite = TRUE)
