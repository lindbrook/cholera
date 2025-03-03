#' Compute latitude and longitude for simulated regular cases.
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param radius Numeric. Radius for \code{withinRadius()} to find road segment endpoints.
#' @param simulated.obs Numeric. Number of simulated cases.
#' @note radius = 75 - appox. 1 hr; radius = 100 - appox. 1.5 hr
#' @noRd

latlongSimulateFatalities <- function(multi.core = FALSE, radius = 75,
  simulated.obs = 20000L) {

  cores <- multiCore(multi.core)
  reg.cases <- latlongRegularCartesianCases(simulated.obs)

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

  idx <- seq_len(nrow(reg.cases))

  orthogonal.projection <- parallel::mclapply(idx, function(i) {
    case <- reg.cases[i, ]

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

      # segment bisection/intersection test #
      distB <- stats::dist(rbind(road.segment[1, ], c(x.proj, y.proj))) +
               stats::dist(rbind(road.segment[2, ], c(x.proj, y.proj)))

      ortho.dist <- c(stats::dist(rbind(c(case$x, case$y), c(x.proj, y.proj))))

      bisect.test <- signif(stats::dist(road.segment)) == signif(distB)

      if (bisect.test) {
        ortho.pts <- data.frame(x.proj, y.proj)
        data.frame(road.segment = seg.id, ortho.pts, dist = ortho.dist,
          type = "ortho")
      } else {
        dist.to.endpts <- vapply(seq_len(nrow(road.segment)), function(i) {
          stats::dist(rbind(case[, c("x", "y")], road.segment[i, ]))
        }, numeric(1L))

        sel <- which.min(dist.to.endpts)

        data.frame(road.segment = seg.id, x.proj = road.segment[sel, "x"],
          y.proj = road.segment[sel, "y"], dist = dist.to.endpts[sel],
          type = "eucl")
      }
    })

    out <- do.call(rbind, out)

    if (nrow(out) > 1) out <- out[which.min(out$dist), ]
    tmp.nms <- names(out)
    out$case <- i
    row.names(out) <- NULL
    out[, c("case", tmp.nms)]
  }, mc.cores = cores)

  coords <- do.call(rbind, orthogonal.projection)

  # translate from geo-cartesian to latlong #

  proj <- parallel::mclapply(seq_len(nrow(coords)), function(i) {
    meterLatLong(coords[i, ])
  }, mc.cores = cores)

  reg <- parallel::mclapply(seq_len(nrow(reg.cases)), function(i) {
    meterLatLong(reg.cases[i, ])
  }, mc.cores = cores)

  list(reg = do.call(rbind, reg), sim = do.call(rbind, proj))
}

# > system.time(latlong.reg.sim <- cholera:::latlongSimulateFatalities())
#      user    system   elapsed
# 12083.022    59.472  3421.048

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

latlongRegularCartesianCases <- function(simulated.obs) {
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


