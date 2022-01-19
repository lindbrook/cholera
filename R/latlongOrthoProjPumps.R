#' Compute orthogonal projection of pumps (prototype).
#'
#' Projection from fatality address to nearest road segment.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @param radius Numeric.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

latlongOrthoProjPumps <- function(path, vestry = FALSE, radius = 0.001,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)
  vars <- c("lon", "lat")

  if (vestry) pmp <- cholera::latlong.pumps.vestry
  else pmp <- cholera::latlong.pumps
  
  rd.segs <- cholera::latlong.road.segments

  soln <- parallel::mclapply(pmp$id, function(p) {
    case.data <- pmp[pmp$id == p, vars]

    within.radius <- lapply(rd.segs$id, function(id) {
      seg.data <- rd.segs[rd.segs$id == id, ]
      test1 <- cholera::withinRadius(case.data, seg.data[, paste0(vars, 1)],
        radius)
      test2 <- cholera::withinRadius(case.data, seg.data[, paste0(vars, 2)],
        radius)
      if (any(test1, test2)) unique(seg.data$id)
    })

    within.radius <- unlist(within.radius)

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      sel <- rd.segs$id == seg.id
      segment.data <- rd.segs[sel, c(paste0(vars, 1), paste0(vars, 2))]
      road.segment <- data.frame(x = c(segment.data$lon1, segment.data$lon2),
                                 y = c(segment.data$lat1, segment.data$lat2))

      ols <- stats::lm(y ~ x, data = road.segment)
      road.intercept <- stats::coef(ols)[1]
      road.slope <- stats::coef(ols)[2]
      ortho.slope <- -1 / road.slope
      ortho.intercept <- case.data$lat - ortho.slope * case.data$lon

      lon.proj <- (ortho.intercept - road.intercept) /
                   (road.slope - ortho.slope)
      lat.proj <- road.slope * lon.proj + road.intercept
      seg.df <- data.frame(x = c(segment.data$lon1, segment.data$lon2),
                           y = c(segment.data$lat1, segment.data$lat2))

      dist <- stats::dist(rbind(seg.df[1, ], c(lon.proj, lat.proj))) +
              stats::dist(rbind(seg.df[2, ], c(lon.proj, lat.proj)))

      bisect.segment <- signif(stats::dist(seg.df)) == signif(dist)
      bisect.test <- ifelse(is.na(bisect.segment), FALSE, bisect.segment)

      if (bisect.test) {
        dat <- rbind(c(case.data$lon, case.data$lat), c(lon.proj, lat.proj))
        ortho.dist <- c(stats::dist(dat))
        ortho.pts <- data.frame(lon.proj, lat.proj)
        data.frame(road.segment = seg.id, ortho.pts, ortho.dist, pump = p)
      } else {
        null.out <- data.frame(matrix(NA, ncol = 5))
        names(null.out) <- c("road.segment", "lon.proj", "lat.proj",
          "ortho.dist", "pump")
        null.out
      }
    })

    out <- do.call(rbind, ortho.proj.test)
    out[which.min(out$ortho.dist), ]
  }, mc.cores = cores)

  soln <- do.call(rbind, soln)
  row.names(soln) <- NULL
  latlong.ortho <- soln

  if (vestry) {
    xy.ortho <- cholera::ortho.proj.pump.vestry
  } else {
    xy.ortho <- cholera::ortho.proj.pump
  }

  chk <- latlong.ortho$road.segment != xy.ortho$road.segment

  if (any(chk)) {
    a <- latlong.ortho$road.segment[which(chk)]
    b <- xy.ortho$road.segment[which(chk)]

    theta <- vapply(seq_along(a), function(i) {
      segmentTheta(a[i], b[i])
    }, numeric(1L))

    if (any(theta < 120)) {
      pump.keep.segment <- latlong.ortho$pump[chk][theta < 120]

      # Pump 14 exception
      if (pump.keep.segment == 14) {
        sel <- xy.ortho$pump.id == pump.keep.segment
        segment.fix <- xy.ortho[sel, "road.segment"]
        sel <- rd.segs$id == segment.fix
        seg.data <- rd.segs[sel, c(paste0(vars, 1), paste0(vars, 2))]
        seg.select <- which.min(seg.data[, c("lon1", "lon2")])

        p.data <- rbind(pmp[pmp$id == pump.keep.segment, vars],
          stats::setNames(seg.data[, paste0(vars, seg.select)], vars))

        lon_lat <- unlist(seg.data[, paste0(vars, seg.select)])

        fix <- data.frame(road.segment = segment.fix,
                          lon.proj = lon_lat[1],
                          lat.proj = lon_lat[2],
                          ortho.dist = c(stats::dist(p.data)),
                          pump = pump.keep.segment,
                          row.names = NULL)

        soln[soln$pump == pump.keep.segment, ] <- fix
      }
    }
  }
  soln
}
