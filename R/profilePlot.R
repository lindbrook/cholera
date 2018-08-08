#' Profile Plot.
#'
#' @param pump Numeric. Selected pump focal point.
#' @param theta Numeric. Angle of perspective axis in degrees.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Windows, only "multi.core = FALSE" is available.
#' @param type Character. "base" or "ggplot2".
#' @import ggplot2
#' @export

profilePlot <- function(pump = 7, theta = 0,
  multi.core = FALSE, type = "base") {

  if (type %in% c("base", "ggplot2") == FALSE) {
    stop('type must either be "base" or "ggplot2"')
  }

  a <- profilePerspective("inside", theta = theta, multi.core = multi.core)
  b <- profilePerspective("outside", theta = theta, multi.core = multi.core)

  if (type == "base") {
    par(mfrow = c(3, 1))
    x.rng <- range(a$axis, b$axis)
    y.rng <- range(a$count, b$count)
    plot(a$axis, a$count, type = "h", xlim = x.rng, ylim = y.rng, col = "red")
    title(main = paste("Axis angle =", theta))
    plot(b$axis, b$count, type = "h", xlim = x.rng, ylim = y.rng, col = "blue")
    plot(b$axis, b$count, type = "h", xlim = x.rng, ylim = y.rng,
      col = grDevices::adjustcolor("blue", alpha.f = 1/2))
    points(a$axis, a$count, type = "h",
      col = grDevices::adjustcolor("red", alpha.f = 1/2))
    par(mfrow = c(1, 1))

  } else if (type == "ggplot2") {
    profileA <- data.frame(axis = a$axis, count = a$count)
    profileB <- data.frame(axis = b$axis, count = b$count)
    profileA$Location <- "Inside"
    profileB$Location <- "Outside"
    profileA$facet <- "Inside"
    profileB$facet <- "Outside"
    profileAB <- rbind(profileA, profileB)
    profileAB$facet <- "In & Out"
    profile.data <- rbind(profileA, profileB, profileAB)
    facet <- c("Inside", "Outside", "In & Out")
    profile.data$facet <- factor(profile.data$facet, levels = facet)
    p <- ggplot(data = profile.data, aes(x = axis, xend = axis, y = 0,
                yend = count)) +
      geom_segment(aes(color = Location)) +
      scale_colour_manual(values = c("red", "blue")) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      facet_wrap(~ facet, nrow = 2) +
      ggtitle(paste("Axis angle =", theta))
    p
  }
}

axisSlope <- function(theta) tan(pi * theta / 180)
axisIntercept <- function(m, x, y) {
  int <- y - m * x
  ifelse(m * x == 0, y, int)
}

orthogonalSlope <- function(b) -1 / b
orthogonalIntercept <- function(b, ortho.slope, y) y - ortho.slope * b

#' Intercept and slope of selected axis.
#'
#' @param pump Numeric. Numeric ID of pump (focal point).
#' @param theta Numeric. Angle of axis in degrees.
#' @param vestry Logical. TRUE uses the 14 pumps from the map in the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @noRd

ols <- function(pump = 7, theta = 0, vestry = FALSE) {
  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }
  center <- p.data[p.data$id == pump, ]
  b <- axisSlope(theta)
  data.frame(intercept = axisIntercept(b, center$x, center$y), slope = b)
}

#' Coordinate of projection onto axis
#'
#' @param case Numeric. Numeric ID of case.
#' @param theta Numeric. Angle of axis in degrees.
#' @param vestry Logical. TRUE uses the 14 pumps from the map in the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param observed Logical.
#' @noRd

orthogonalCoordinates <- function(case, pump = 7, theta = 0, vestry = FALSE,
  observed = TRUE) {

  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }

  axis.focus <- p.data[p.data$id == pump, ]

  if (observed) {
    obs <- cholera::fatalities.address[cholera::fatalities.address$anchor.case
      == case, c("x", "y")]
  } else {
    obs <- cholera::regular.cases[case, ]
  }

  axis.data <- ols(pump, theta, vestry)

  if (theta == 0) {
    x.proj <- obs$x
    y.proj <- axis.focus$y
  } else if (theta == 90) {
    x.proj <- axis.focus$x
    y.proj <- obs$y
  } else {
    ortho.slope <- orthogonalSlope(axis.data$slope)
    ortho.intercept <- orthogonalIntercept(obs$x, ortho.slope, obs$y)

    x.proj <- (ortho.intercept - axis.data$intercept) /
              (axis.data$slope - ortho.slope)

    y.proj <- axis.data$slope * x.proj + axis.data$intercept
  }

  data.frame(x = x.proj, y = y.proj, row.names = NULL)
}

utils::globalVariables(c("count", "Location"))

#' Rescale data along axis.
#'
#' @param output Character."inside" or "outside".
#' @param pump Numeric. Selected pump focal point.
#' @param theta Numeric. Angle of perspective axis in degrees.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Windows, only "multi.core = FALSE" is available.
#' @export

profilePerspective <- function(output = "inside", pump = 7, theta = 0,
  multi.core = FALSE) {

  walk <- nearestPump(multi.core = multi.core)
  neighborhood.select <- walk[walk$pump == pump, ]
  neighborhood.others <- walk[walk$pump != pump, ]

  if (output == "inside") {
    cases <- neighborhood.select$case
  } else if (output == "outside") {
    cases <- neighborhood.others$case
  } else {
    stop('output must either be "inside" or "outside".')
  }

  vars <- c("ortho.x", "ortho.y")

  coords <- lapply(cases, function(x) orthogonalCoordinates(x, theta = theta))
  coords <- stats::setNames(do.call(rbind, coords), vars)

  dat <- cholera::fatalities.address[cholera::fatalities.address$anchor.case
    %in% cases, ]
  dat <- cbind(dat, coords)

  pump.data <- cholera::pumps[cholera::pumps$id == pump, c("x", "y")]
  pump.data$ortho.x <- pump.data$x
  pump.data$ortho.y <- pump.data$y
  pump.data$anchor.case <- 0
  pump.data$case.count <- 0
  pump.data <- pump.data[, names(dat)]

  dat <- rbind(pump.data, dat)
  dat <- dat[order(dat$ortho.x, dat$ortho.y), ]
  idx <- seq_len(nrow(dat))

  d <- vapply(idx[-length(idx)], function(i) {
    c(stats::dist(rbind(dat[i, vars], dat[i + 1, vars])))
  }, numeric(1L))

  d <- c(0, d)

  out <- data.frame(axis = cumsum(d) - cumsum(d)[which(dat$anchor.case == 0)],
                    count = dat$case.count)

  class(out) <- "profile"
  out
}

#' Plot method for profilePerspective().
#'
#' @param x An object of class "profile" created by profilePerspective().
#' @param ... Additional plotting parameters.
#' @export

plot.profile <- function(x, ...) {
  if (class(x) != "profile") stop('"x"\'s class needs to be "profile".')
  plot(x$axis, x$count, type = "h")
  abline(v = 0, col = "red", lty = "dotted")
}
