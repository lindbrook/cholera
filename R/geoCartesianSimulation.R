#' Compute latitude and longitude for simulated regular cases from geodesic cartesian space (prototype).
#'
#' @param simulated.obs Numeric. Number of sample cases.
#' @param delta Numeric. Increment between simulated values.
#' @importFrom pracma cart2pol
#' @importFrom sp Polygon
#' @importFrom sp spsample
#' @noRd

geoCartesianSimulation <- function(simulated.obs = 20000L, delta = 0.000025) {
  frame <- cholera::frame.data

  origin <- data.frame(lon = min(frame$lon), lat = min(frame$lat))
  topleft <- data.frame(lon = min(frame$lon), lat = max(frame$lat))
  bottomright <- data.frame(lon = max(frame$lon), lat = min(frame$lat))

  # # rbind(origin, topleft, bottomright) |> plot(asp = 1.6)
  # horiz.d <- geosphere::distGeo(origin, topleft)
  # vert.d <- geosphere::distGeo(origin, bottomright)

  # origin.xy <- data.frame(x = min(frame$x), y = min(frame$y))
  # topleft.xy <- data.frame(x = min(frame$x), y = max(frame$y))
  # bottomright.xy <- data.frame(x = max(frame$x), y = min(frame$y))

  # # rbind(origin.xy, topleft.xy, bottomright.xy) |> plot(asp = 1)
  # horiz.d.xy <- stats::dist(rbind(origin.xy, topleft.xy))
  # vert.d.xy <- stats::dist(rbind(origin.xy, bottomright.xy))

  geo.coords <- do.call(rbind, lapply(frame$id, function(id) {
    tmp <- frame[frame$id == id, c("lon", "lat")]
    x.proj <- c(tmp$lon, origin$lat)
    y.proj <- c(origin$lon, tmp$lat)
    m.lon <- geosphere::distGeo(y.proj, tmp)
    m.lat <- geosphere::distGeo(x.proj, tmp)
    data.frame(id = id, x = m.lon, y = m.lat)
  }))

  geo.coords.centered <- data.frame(x = geo.coords$x - mean(geo.coords$x),
                                    y = geo.coords$y - mean(geo.coords$y))

  idx <- order(apply(geo.coords.centered, 1, pracma::cart2pol)[1, ])
  geo.coords <- geo.coords[idx, ]

  vars <- c("x", "y")

  sp.frame <- sp::spsample(sp::Polygon(geo.coords[, vars]), n = simulated.obs,
    type = "regular")
  reg.cases <- stats::setNames(data.frame(sp.frame@coords), vars)

  est.lat <- meterLatitude(reg.cases, origin, topleft)
  lon <- seq(origin$lon, bottomright$lon, delta)

  meters.east <- lapply(est.lat$lat, function(y) {
    y.axis.origin <- cbind(origin$lon, y)
    vapply(lon, function(x) {
      geosphere::distGeo(y.axis.origin, cbind(x, y))
    }, numeric(1L))
  })

  loess.lon <- lapply(meters.east, function(m) {
    dat <- data.frame(lon = lon, m)
    stats::loess(lon ~ m, data = dat,
      control = stats::loess.control(surface = "direct"))
  })

  y.unique <- sort(unique(reg.cases$y))

  out <- lapply(seq_along(y.unique), function(i) {
    if (nrow(reg.cases) > 1) {
      dat <- reg.cases[reg.cases$y == y.unique[i], ]
    } else {
      dat <- reg.cases
    }
    loess.fit <- loess.lon[[i]]
    dat$lon <- vapply(dat$x, function(x) {
      stats::predict(loess.fit, newdata = data.frame(m = x))
    }, numeric(1L))
    dat$lat <- est.lat[est.lat$m == y.unique[i], "lat"]
    dat
  })

  out <- do.call(rbind, out)
}
