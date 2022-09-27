#' Plot walking path to nearest pump (prototype).
#'
#' @param case Numeric.
#' @param zoom Logical or Numeric. A numeric value >= 0 that controls the degree of zoom.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @export

latlongWalkingPathB <- function(case = 1, zoom = TRUE, vestry = FALSE) {
  vars <- c("lon", "lat")

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

  path.length <- sum(vapply(seq_len(nrow(dat[-1, ])), function(i) {
    geosphere::distGeo(dat[i, ], dat[i + 1, ])
  }, numeric(1L)))

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
  drawPathB(dat, "dodgerblue", compute.coords = FALSE)
  title(main = paste("Case", case, "to Pump", destination.pump),
        sub = paste(round(path.length, 1), "m"))
}

drawPathB <- function(x, case.color, compute.coords = TRUE) {
  if (compute.coords) {
    path.data <- numericNodeCoordinates(x)
  } else {
    path.data <- x
  }
  n1 <- path.data[1:(nrow(path.data) - 1), ]
  n2 <- path.data[2:nrow(path.data), ]
  segments(n1$x, n1$y, n2$x, n2$y, lwd = 3,
    col = grDevices::adjustcolor(case.color, alpha.f = 1/2))
}
