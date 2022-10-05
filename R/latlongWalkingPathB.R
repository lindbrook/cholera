#' Plot walking path to nearest pump (prototype).
#'
#' @param case Numeric.
#' @param zoom Logical or Numeric. A numeric value >= 0 that controls the degree of zoom.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @export

latlongWalkingPathB <- function(case = 1, zoom = TRUE, vestry = FALSE) {
  if (!case %in% cholera::fatalities.address$anchor) {
    stop("Invalid case. See cholera::fatalities.address.", call. = FALSE)
  } else {
    case.id <- which(cholera::fatalities.address$anchor == case)
  }

  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  frame <- cholera::roads[cholera::roads$name == "Map Frame", ]
  fatality <- cholera::fatalities

  if (vestry) pump <- cholera::pumps.vestry
  else pump <- cholera::pumps

  nearest.pump <- latlongNearestPumpB(vestry = vestry)

  p <- names(nearest.pump$path[[case.id]][[1]])
  destination.pump <- names(nearest.pump$path[[case.id]])

  nodes <- do.call(rbind, strsplit(p, "-"))
  dat <- data.frame(x = -as.numeric(nodes[, 2]), y = as.numeric(nodes[, 3]))

  ds <- vapply(seq_len(nrow(dat[-1, ])), function(i) {
    geosphere::distGeo(dat[i, ], dat[i + 1, ])
  }, numeric(1L))

  path.length <- sum(ds)

  dat <- data.frame(id = seq_along(dat$x), dat)
  rev.data <- dat[order(dat$id, decreasing = TRUE), ]

  vars <- c("x", "y")
  seg.vars <- c(paste0(vars, 1), paste0(vars, 2))

  seg.data <- do.call(rbind, lapply(seq_len(nrow(rev.data) - 1), function(i) {
    endpts <- cbind(rev.data[i, vars], rev.data[i + 1, vars])
    names(endpts) <- seg.vars
    data.frame(id = i, endpts)
  }))

  seg.data$d <- rev(ds)
  seg.data$cumulative.d <- cumsum(seg.data$d)

  floor.id <- floor(seg.data$cumulative.d / 50)

  post.count <- unique(floor.id[floor.id != 0])

  milepost.seg.id <- vapply(seq_along(post.count), function(post) {
    which(floor.id == post)[1]
  }, integer(1L))

  milepost.value <- seq_along(milepost.seg.id) * 50

  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  topleft <- data.frame(lon = min(cholera::roads$lon),
                        lat = max(cholera::roads$lat))
  bottomright <- data.frame(lon = max(cholera::roads$lon),
                            lat = min(cholera::roads$lat))

  arrow.data <- do.call(rbind, lapply(seq_along(milepost.seg.id), function(i) {
    milepost <- milepost.value[i]
    tmp <- seg.data[seg.data$id == milepost.seg.id[i], ]
    endpt1 <- stats::setNames(tmp[, grep("1", names(tmp))], c("lon", "lat"))
    endpt2 <- stats::setNames(tmp[, grep("2", names(tmp))], c("lon", "lat"))
    latlong.tmp <- rbind(endpt1, endpt2)

    meter.coords <- lapply(seq_along(latlong.tmp$lon), function(i) {
      tmp <- latlong.tmp[i, c("lon", "lat")]
      x.proj <- c(tmp$lon, origin$lat)
      y.proj <- c(origin$lon, tmp$lat)
      m.lon <- geosphere::distGeo(y.proj, tmp)
      m.lat <- geosphere::distGeo(x.proj, tmp)
      data.frame(x = m.lon, y = m.lat)
    })

    meter.coords <- do.call(rbind, meter.coords)

    ols <- stats::lm(y ~ x, data = meter.coords )
    seg.slope <- stats::coef(ols)[2]
    theta <- atan(seg.slope)
    h <- tmp$cumulative.d - milepost
    arrow.point <- quandrantCoordinatesB(meter.coords, h, theta)

    data.frame(x1 = meter.coords[2, "x"], y1 = meter.coords[2, "y"],
      x2 = arrow.point$x, y2 = arrow.point$y)
  }))

  tail.data <- stats::setNames(arrow.data[, c("x1", "y1")], c("x", "y"))
  head.data <- stats::setNames(arrow.data[, c("x2", "y2")], c("x", "y"))
  tail.data <- meterLatLong(tail.data, origin, topleft, bottomright)
  head.data <- meterLatLong(head.data, origin, topleft, bottomright)
  tail.data <- tail.data[order(row.names(tail.data)), ]
  head.data <- head.data[order(row.names(head.data)), ]

  if (is.logical(zoom)) {
    if (zoom) {
      padding <- 0.00026
      xlim <- c(min(dat$x) - padding, max(dat$x) + padding)
      ylim <- c(min(dat$y) - padding, max(dat$y) + padding)
    } else {
      map.data <- rbind(frame, rd)
      xlim <- range(map.data$lon)
      ylim <- range(map.data$lat)
    }
  } else if (is.numeric(zoom)) {
    if (zoom >= 0) {
      xlim <- c(min(dat$x) - zoom, max(dat$x) + zoom)
      ylim <- c(min(dat$y) - zoom, max(dat$y) + zoom)
    } else stop("If numeric, zoom must be >= 0.")
  } else stop("zoom must either be logical or numeric.")

  vars <- c("lon", "lat")

  plot(rd[, vars], pch = NA, asp = 1.6, xlim = xlim, ylim = ylim)
  roads.list <- split(rd[, vars], rd$street)
  frame.list <- split(frame[, vars], frame$street)
  invisible(lapply(roads.list, lines, col = "gray"))
  invisible(lapply(frame.list, lines))
  points(fatality[, vars], col = "gray", pch = 16, cex = 0.5)
  points(fatality[fatality$case == case, vars], col = "red", pch = 1)
  points(pump[, vars], col = "blue", pch = 24)
  text(pump[, vars], col = "blue", pos = 1, labels = pump$id)
  points(dat[1, c("x", "y")], col = "dodgerblue", pch = 0)
  points(dat[nrow(dat), c("x", "y")], col = "dodgerblue", pch = 0)
  drawPathC(dat, "dodgerblue")
  title(main = paste("Case", case, "to Pump", destination.pump),
        sub = paste(round(path.length, 1), "m"))
  arrows(tail.data$lon, tail.data$lat,
         head.data$lon, head.data$lat,
         length = 0.0875, col = "dodgerblue", lwd = 3)
  arrows(seg.data[1, "x2"], seg.data[1, "y2"],
         seg.data[1, "x1"], seg.data[1, "y1"],
         length = 0.0875, col = "dodgerblue", lwd = 3)
}

drawPathC <- function(x, case.color) {
  path.data <- x
  n1 <- path.data[1:(nrow(path.data) - 1), ]
  n2 <- path.data[2:nrow(path.data), ]
  segments(n1$x, n1$y, n2$x, n2$y, lwd = 3,
    col = grDevices::adjustcolor(case.color, alpha.f = 1/2))
}

quandrantCoordinatesB <- function(dat, h, theta) {
  delta <- dat[2, ] - dat[1, ]

  # Quadrant I
  if (all(delta > 0)) {
    post.x <- dat[2, "x"] - abs(h * cos(theta))
    post.y <- dat[2, "y"] - abs(h * sin(theta))

  # Quadrant II
  } else if (delta[1] < 0 & delta[2] > 0) {
    post.x <- dat[2, "x"] + abs(h * cos(theta))
    post.y <- dat[2, "y"] - abs(h * sin(theta))

  # Quadrant III
  } else if (all(delta < 0)) {
    post.x <- dat[2, "x"] + abs(h * cos(theta))
    post.y <- dat[2, "y"] + abs(h * sin(theta))

  # Quadrant IV
  } else if (delta[1] > 0 & delta[2] < 0) {
    post.x <- dat[2, "x"] - abs(h * cos(theta))
    post.y <- dat[2, "y"] + abs(h * sin(theta))

  # I:IV
  } else if (delta[1] > 0 & delta[2] == 0) {
    post.x <- dat[2, "x"] - abs(h * cos(theta))
    post.y <- dat[2, "y"]

  # I:II
  } else if (delta[1] == 0 & delta[2] > 0) {
    post.x <- dat[2, "x"]
    post.y <- dat[2, "y"] - abs(h * sin(theta))

  # II:III
  } else if (delta[1] < 0 & delta[2] == 0) {
    post.x <- dat[2, "x"] + abs(h * cos(theta))
    post.y <- dat[2, "y"]

  # III:IV
  } else if (delta[1] == 0 & delta[2] < 0) {
    post.x <- dat[2, "x"]
    post.y <- dat[2, "y"] + abs(h * sin(theta))
  }

  data.frame(x = post.x, y = post.y)
}
