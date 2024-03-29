#' Compute latitude and longitude for simulated regular cases.
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param radius Numeric. Radius for \code{withinRadius()} to find road segment endpoints.
#' @param recompute Logical. Recompute regular cases.
#' @noRd

latlongSimulateFatalities <- function(multi.core = TRUE, radius = 75,
  recompute = FALSE) {

  cores <- multiCore(multi.core)

  if (recompute) {
    regular.cases <- latlongRegularCartesianCases()
  } else {
    regular.cases <- cholera::latlong.regular.cases
  }

  rd <- cholera::roads[!cholera::roads$street %in% cholera::border, ]
  cart.rd <- data.frame(street = rd$street, geoCartesian(rd))

  cart.rd.segs <- lapply(unique(cart.rd$street), function(st) {
    dat <- cart.rd[cart.rd$street == st, ]
    names(dat)[names(dat) %in% c("x", "y")] <- c("x1", "y1")
    seg.data <- dat[-1, c("x1", "y1")]
    names(seg.data) <- c("x2", "y2")
    dat <- cbind(dat[-nrow(dat), ], seg.data)
    dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
    dat
  })

  cart.rd.segs <- do.call(rbind, cart.rd.segs)
  seg.endpts <- c("x1", "y1", "x2", "y2")

  idx <- seq_len(nrow(regular.cases))

  orthogonal.projection <- parallel::mclapply(idx, function(i) {
    case <- regular.cases[i, ]

    within.radius <- lapply(cart.rd.segs$id, function(x) {
      dat <- cart.rd.segs[cart.rd.segs$id == x, ]
      test1 <- withinRadius(case, dat[, c("x1", "y1")], radius)
      test2 <- withinRadius(case, dat[, c("x2", "y2")], radius)
      if (any(test1, test2)) unique(dat$id)
    })

    within.radius <- unlist(within.radius)

    out <- lapply(within.radius, function(seg.id) {
      segment.data <- cart.rd.segs[cart.rd.segs$id == seg.id, seg.endpts]
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

      seg.data <- cart.rd.segs[cart.rd.segs$id == seg.id, seg.endpts]
      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      # segment bisection/intersection test #
      distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
               stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

      bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

      if (bisect.test) {
        ortho.dist <- c(stats::dist(rbind(c(case$x, case$y),
          c(x.proj, y.proj))))
        ortho.pts <- data.frame(x.proj, y.proj)
        data.frame(road.segment = seg.id, ortho.pts, dist = ortho.dist,
          bisect = bisect.test)
      } else {
        # select nearest segment endpoint
        dist.to.endpts <- vapply(seq_len(nrow(road.segment)), function(i) {
          stats::dist(rbind(case, road.segment[i, ]))
        }, numeric(1L))

        sel <- which.min(dist.to.endpts)
        data.frame(road.segment = seg.id,
                   x.proj = road.segment[sel, "x"],
                   y.proj = road.segment[sel, "y"],
                   dist = dist.to.endpts[sel],
                   bisect = bisect.test)
      }
    })

    out <- do.call(rbind, out)

    if (any(out$bisect)) {
      out <- out[out$bisect == TRUE, ]
    } else {
      out <- out[out$bisect == FALSE, ]
    }

    if (nrow(out) > 1) out <- out[which.min(out$dist), ]
    out$type <- ifelse(out$bisect, "ortho", "eucl")
    out$bisect <- NULL
    tmp.nms <- names(out)
    out$case <- i
    row.names(out) <- NULL
    out[, c("case", tmp.nms)]
  }, mc.cores = cores)

  coords <- do.call(rbind, orthogonal.projection)

  # translate from geo-cartesian to latlong #
  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  topleft <- data.frame(lon = min(cholera::roads$lon),
                        lat = max(cholera::roads$lat))
  bottomright <- data.frame(lon = max(cholera::roads$lon),
                            lat = min(cholera::roads$lat))

  proj <- parallel::mclapply(seq_len(nrow(coords)), function(i) {
    meterLatLong(coords[i, ], origin, topleft, bottomright)
  }, mc.cores = cores)

  reg <- parallel::mclapply(seq_len(nrow(regular.cases)), function(i) {
    meterLatLong(regular.cases[i, ], origin, topleft, bottomright)
  }, mc.cores = cores)


  list(latlong.sim.ortho.proj = do.call(rbind, proj),
       latlong.regular.cases = do.call(rbind, reg))
}

# > system.time(latlong.reg.sim <- cholera:::latlongSimulateFatalities())
#     user   system  elapsed
# 8793.615   29.496 2279.553

# latlong.reg.sim <- cholera:::latlongSimulateFatalities()
# usethis::use_data(latlong.sim.ortho.proj)
# usethis::use_data(latlong.sim.ortho.proj, overwrite = TRUE)
# usethis::use_data(latlong.regular.cases)
# usethis::use_data(latlong.regular.cases, overwrite = TRUE)

#' Compute Cartesian latitude and longitude for simulated regular cases.
#'
#' @param simulated.obs Numeric. Number of simulated cases.
#' @importFrom geosphere distGeo
#' @importFrom sp Polygon
#' @importFrom sp spsample
#' @noRd

latlongRegularCartesianCases <- function(simulated.obs = 20000L) {
  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))

  frame.id <- cholera::roads[cholera::roads$name == "Map Frame", "id"]

  coords <- lapply(frame.id, function(x) {
    tmp <- cholera::roads[cholera::roads$id %in% x, c("lon", "lat")]
    x.proj <- c(tmp$lon, origin$lat)
    y.proj <- c(origin$lon, tmp$lat)
    m.lon <- geosphere::distGeo(y.proj, tmp)
    m.lat <- geosphere::distGeo(x.proj, tmp)
    data.frame(id = x, x = m.lon, y = m.lat)
  })

  coords <- do.call(rbind, coords)

  coords.centered <- data.frame(x = coords$x - mean(coords$x),
                                y = coords$y - mean(coords$y))

  idx <- order(apply(coords.centered, 1, pracma::cart2pol)[1, ])
  map.frame <- coords[idx, ]

  vars <- c("x", "y")

  sp.frame <- sp::spsample(sp::Polygon(map.frame[, vars]), n = simulated.obs,
    type = "regular")
  stats::setNames(data.frame(sp.frame@coords), vars)
}


