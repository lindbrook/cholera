#' Plot Euclidean path to nearest pump (prototype).
#'
#' @param case Numeric.
#' @param destination Numeric. Pump ID.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param distance.unit Character. Unit of distance: "meter" or "yard".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param case.location Character. For \code{observed = FALSE}: "address" or "nominal". "nominal" is the x-y coordinates of \code{regular.cases}.
#' @export

latlongEuclideanPath <- function(case = 1, destination = NULL, vestry = FALSE,
  distance.unit = "meter", time.unit = "second", walking.speed = 5,
  case.location = "nominal", multi.core = TRUE) {

  meter.to.yard <- 1.09361
  cores <- multiCore(multi.core)
  vars <- c("lon", "lat")

  if (!case %in% cholera::fatalities$case) {
    stop("Valid cases range from 1 to 578.", call. = FALSE)
  } else {
    case.data <- cholera::fatalities[cholera::fatalities$case == case, ]
  }

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  if (!is.null(destination)) {
    if (!destination %in% pump.data$id) {
      stop(paste0("Valid pumps range from 1 to ", nrow(pump.data), "."),
        call. = FALSE)
    }
  }

  if (is.null(destination)) {
    d <- geosphere::distGeo(case.data[, vars], pump.data[, vars])
    nr.pump <- pump.data[which.min(d), ]
    euclidean.d <- min(d)
  } else {
    euclidean.d <- geosphere::distGeo(case.data[, vars],
      pump.data[pump.data$id == destination, vars])
    nr.pump <- pump.data[pump.data$id == destination, ]
  }

  eucl.data <- data.frame(case = case, pump = nr.pump$id,
    distance = euclidean.d)

  if (distance.unit == "meter") {
    eucl.data$distance <- eucl.data$distance
  } else if (distance.unit == "yard") {
    eucl.data$distance <- eucl.data$distance * meter.to.yard
  }

  eucl.data$time <- distanceTime(eucl.data$distance,
    distance.unit = distance.unit, time.unit = time.unit,
    walking.speed = walking.speed)

  out <- list(case = case.data[, vars],
              pump = nr.pump[, vars],
              data = eucl.data,
              distance.unit = distance.unit,
              time.unit = time.unit,
              vestry = vestry,
              walking.speed = walking.speed)

  class(out) <- "latlong_euclidean_path"
  out
}

#' Plot the path of the Euclidean distance between cases and/or pumps.
#'
#' @param x An object of class "latlong_euclidean_path" created by latlongEuclideanPath().
#' @param zoom Logical.
#' @param mileposts Logical. Plot mile/time posts.
#' @param milepost.unit Character. "distance" or "time".
#' @param milepost.interval Numeric. Mile post interval unit of distance (yard or meter) or unit of time (seconds).
#' @param ... Additional plotting parameters.
#' @export

plot.latlong_euclidean_path <- function(x, zoom = TRUE, mileposts = TRUE,
  milepost.unit = "distance", milepost.interval = NULL, ...) {

  vars <- c("lon", "lat")
  colors <- snowColors(x$vestry)

  if (zoom) {
    dat <- rbind(x$case, x$pump)
    padding <- 0.00026
    xlim <- c(min(dat$lon) - padding, max(dat$lon) + padding)
    ylim <- c(min(dat$lat) - padding, max(dat$lat) + padding)

    plot(dat[, vars], pch = NA, asp = 1.6, xlim = xlim, ylim = ylim)
    points(cholera::fatalities[, vars], pch = 16, col = "lightgray", cex = 0.5)
    if (x$vestry) pmp <- cholera::pumps.vestry
    else pmp <- cholera::pumps
    points(pmp[, vars], pch = 2, col = colors)
    text(pmp[, vars], labels = paste0("p", pmp$id), pos = 1)
    addRoads(latlong = TRUE, col = "lightgray")
    addFrame(latlong = TRUE)
  } else {
    snowMap(latlong = TRUE, vestry = x$vestry)
  }

  d.info <- paste(round(x$data$distance, 1), x$distance.unit)
  t.info <- paste(round(x$data$time), paste0(x$time.unit, "s"), "@",
    x$walking.speed, "km/hr")

  points(x$case, col = "red")
  text(x$case, col = "red", labels = x$data$case, pos = 1)
  p.col <- colors[paste0("p", x$data$pump)]
  arrows(x$case$lon, x$case$lat, x$pump$lon, x$pump$lat, col = p.col,
         length = 0.0875, lwd = 3)

  if (mileposts) {
    if (is.null(milepost.interval)) {
      if (milepost.unit == "distance") {
        milepost.interval <- 50
      } else if (milepost.unit == "time") {
        milepost.interval <- 60
      }
    }

    if (milepost.unit == "distance") {
      if (x$distance.unit == "meter") {
        post.info <- paste("posts at", milepost.interval, "m intervals")
      } else if (x$distance.unit == "yard") {
        post.info <- paste("posts at", milepost.interval, "yd intervals")
      }
    } else if (milepost.unit == "time") {
      post.info <- paste("posts at", milepost.interval, "sec intervals")
    } else {
      stop('"milepost.unit" muster either be "distance" or "time".',
        call. = FALSE)
    }

    origin <- data.frame(lon = min(cholera::roads$lon),
                         lat = min(cholera::roads$lat))

    x.pump.proj <- c(x$pump$lon, origin$lat)
    y.pump.proj <- c(origin$lon, x$pump$lat)

    x.case.proj <- c(x$case$lon, origin$lat)
    y.case.proj <- c(origin$lon, x$case$lat)

    # distance to origin (meters)
    x.pump.meters <- geosphere::distGeo(x.pump.proj, origin)
    y.pump.meters <- geosphere::distGeo(y.pump.proj, origin)
    x.case.meters <- geosphere::distGeo(x.case.proj, origin)
    y.case.meters <- geosphere::distGeo(y.case.proj, origin)

    cartesian <- data.frame(x = c(x.pump.meters, x.case.meters),
                            y = c(y.pump.meters, y.case.meters),
                            row.names = c("pump", "case"))

    ols <- stats::lm(y ~ x, data = cartesian)
    theta <- atan(stats::coef(ols)["x"])

    mileposts <- seq(milepost.interval, x$data$distance, milepost.interval)
    milepost.ratios <- mileposts / x$data$distance

    # compute milepost meter coordinates
    milepost.coords <- lapply(milepost.ratios, function(r) {
      milepostCoordinates(r, cartesian, theta)
    })

    milepost.coords <- do.call(rbind, milepost.coords)

    # compute milepost latlong coordinates
    topleft <- data.frame(lon = min(cholera::roads$lon),
                          lat = max(cholera::roads$lat))
    bottomright <- data.frame(lon = max(cholera::roads$lon),
                              lat = min(cholera::roads$lat))
    milepost.coords <- meterLatLong(milepost.coords, origin, topleft, bottomright)

    # mileposts as arrows
    arrow.data <- rbind(milepost.coords[, vars], x$case)
    idx <- seq_len(nrow(arrow.data[-nrow(arrow.data), ]))

    invisible(lapply(idx, function(i) {
      arrows(arrow.data[i, "lon"], arrow.data[i, "lat"],
             arrow.data[i + 1, "lon"], arrow.data[i + 1, "lat"],
             code = 1, col = p.col, length = 0.0875, lwd = 3)
    }))

    title(main = paste("Case", x$data$case, "to Pump", x$data$pump),
          sub = paste(d.info, t.info, post.info, sep = "; "))
  } else {
    title(main = paste("Case", x$data$case, "to Pump", x$data$pump),
          sub = paste(d.info, t.info, sep = "; "))
  }
}

milepostCoordinates <- function(r, dat, theta) {
  h <- r * stats::dist(dat)
  delta <- dat["pump", ] - dat["case", ] # relative postion of pump to case

  # Pseudo-Quadrant I
  if (all(delta > 0)) {
    post.x <- dat["pump", "x"] - abs(h * cos(theta))
    post.y <- dat["pump", "y"] - abs(h * sin(theta))

  # Pseudo-Quadrant II
  } else if (delta[1] < 0 & delta[2] > 0) {
    post.x <- dat["pump", "x"] + abs(h * cos(theta))
    post.y <- dat["pump", "y"] - abs(h * sin(theta))

  # Pseudo-Quadrant III
  } else if (all(delta < 0)) {
    post.x <- dat["pump", "x"] + abs(h * cos(theta))
    post.y <- dat["pump", "y"] + abs(h * sin(theta))

  # Pseudo-Quadrant IV ## ##
  } else if (delta["x"] > 0 & delta["y"] < 0) {
    post.x <- dat["pump", "x"] - abs(h * cos(theta))
    post.y <- dat["pump", "y"] + abs(h * sin(theta))

  # I:IV
  } else if (delta[1] > 0 & delta[2] == 0) {
    post.x <- dat["pump", "x"] - abs(h * cos(theta))
    post.y <- dat["pump", "y"]

  # I:II
  } else if (delta[1] == 0 & delta[2] > 0) {
    post.x <- dat["pump", "x"]
    post.y <- dat["pump", "y"] - abs(h * sin(theta))

  # II:III
  } else if (delta[1] < 0 & delta[2] == 0) {
    post.x <- dat["pump", "x"] + abs(h * cos(theta))
    post.y <- dat["pump", "y"]

  # III:IV
  } else if (delta[1] == 0 & delta[2] < 0) {
    post.x <- dat["pump", "x"]
    post.y <- dat["pump", "y"] + abs(h * sin(theta))
  }

  data.frame(x = c(post.x), y = c(post.y))
}
