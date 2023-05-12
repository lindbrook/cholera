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
    pump.name = nr.pump$street, distance = euclidean.d)

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
#' @param ... Additional plotting parameters.
#' @export

plot.latlong_euclidean_path <- function(x, zoom = TRUE, ...) {
  vars <- c("lon", "lat")
  colors <- snowColors(x$vestry)

  if (zoom) {
    dat <- rbind(x$case, x$pump)
    padding <- 0.00026
    xlim <- c(min(dat$lon) - padding, max(dat$lon) + padding)
    ylim <- c(min(dat$lat) - padding, max(dat$lat) + padding)
    plot(dat[, vars], pch = NA, asp = 1.6, xlim = xlim, ylim = ylim)
    addRoads(latlong = TRUE, col = "lightgray")
    addFrame(latlong = TRUE)
  } else {
    snowMap(latlong = TRUE, vestry = x$vestry)
  }

  d.subtitle <- paste(round(x$data$distance, 1), x$distance.unit)
  t.subtitle <- paste(round(x$data$time), paste0(x$time.unit, "s"), "@",
    x$walking.speed, "km/hr")

  p.col <- colors[paste0("p", x$data$pump)]
  points(x$case, col = "red")
  points(x$pump, pch = 2, col = p.col)
  text(x$case, col = "red", labels = x$data$case, pos = 1)
  text(x$pump, labels = paste0("p", x$data$pump), pos = 1)
  arrows(x$case$lon, x$case$lat, x$pump$lon, x$pump$lat, col = p.col,
         length = 0.0875, lwd = 3)
  title(main = paste("Case", x$data$case, "to Pump", x$data$pump),
        sub = paste(d.subtitle, d.subtitle, sep = "; "))
}
