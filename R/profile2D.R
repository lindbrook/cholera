#' 2D Profile .
#'
#' @param angle Numeric. Angle of perspective axis in degrees.
#' @param pump Numeric. Select pump as focal point.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param type Character. Type of graphic: "base" or "ggplot2".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @import ggplot2
#' @export
#' @examples
#' \donttest{
#'
#' profile2D(angle = 30)
#' profile2D(angle = 30, type = "ggplot2")
#' }

profile2D <- function(angle = 0, pump = 7, vestry = FALSE, type = "base",
  multi.core = FALSE) {

  if (angle < 0 | angle > 360) stop("Use 0 >= angle <= 360.")

  if (vestry) {
    pump.id <- cholera::pumps.vestry$id
  } else {
    pump.id <- cholera::pumps$id
  }

  if (is.null(pump) == FALSE) {
    if (any(abs(pump) %in% pump.id == FALSE)) {
      stop('For vestry = ', vestry, ', 1 >= |pump| <= ', max(pump.id))
    }
  }

  cores <- multiCore(multi.core)

  if (length(pump) != 1) {
    stop('Select one pump.')
  }

  inside <- profilePerspective("inside", pump = pump, angle = angle,
    vestry = vestry, multi.core = cores)
  outside <- profilePerspective("outside", pump = pump, angle = angle,
    vestry = vestry, multi.core = cores)

  if (type == "base") {
    par(mfrow = c(3, 1))
    x.rng <- range(inside$axis, outside$axis)
    y.rng <- range(inside$count, outside$count)
    plot(inside$axis, inside$count, type = "h", xlim = x.rng, ylim = y.rng,
      col = "red")
    title(main = paste("Axis angle =", angle))
    plot(outside$axis, outside$count, type = "h", xlim = x.rng, ylim = y.rng,
      col = "blue")
    plot(outside$axis, outside$count, type = "h", xlim = x.rng, ylim = y.rng,
      col = grDevices::adjustcolor("blue", alpha.f = 1/2))
    points(inside$axis, inside$count, type = "h",
      col = grDevices::adjustcolor("red", alpha.f = 1/2))
    par(mfrow = c(1, 1))

  } else if (type == "ggplot2") {
    profileA <- data.frame(axis = inside$axis, count = inside$count)
    profileB <- data.frame(axis = outside$axis, count = outside$count)
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
                yend = profile.data$count)) +
      geom_segment(aes(color = profile.data$Location)) +
      scale_colour_manual(values = c("red", "blue"),
                          guide = guide_legend(title = "Location")) +
      facet_wrap(~ facet, nrow = 3) +
      ggtitle(paste("Axis angle =", angle)) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
    p

  } else {
    if (type %in% c("base", "ggplot2") == FALSE) {
      stop('type must be "base" or "ggplot2".')
    }
  }
}

axisSlope <- function(angle) {
  theta090 <- tan(pi * 90 / 180)
  theta180 <- tan(pi * 180 / 180)
  theta270 <- tan(pi * 270 / 180)
  theta <- tan(pi * signif(angle) / 180)
  if (theta == theta090 | theta == theta270) Inf
  else if (theta == theta180) 0
  else tan(pi * signif(angle) / 180)
}

axisIntercept <- function(m, x, y) {
  if (m == 0) y
  else if (is.infinite(m)) NA
  else y - m * x
}

orthogonalSlope <- function(b) -1 / b

orthogonalIntercept <- function(b, ortho.slope, y) {
  if (is.infinite(ortho.slope)) NA
  else y - ortho.slope * b
}

#' Intercept and slope of selected axis.
#'
#' @param pump Numeric. Numeric ID of pump (focal point).
#' @param angle Numeric. Angle of axis in degrees.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @noRd

profileOLS <- function(pump = 7, angle = 0, vestry = FALSE) {
  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }
  center <- p.data[p.data$id == pump, ]
  b <- axisSlope(angle)
  data.frame(intercept = axisIntercept(b, center$x, center$y), slope = b)
}

#' Coordinate of projection onto axis
#'
#' @param case Numeric. Numeric ID of case.
#' @param angle Numeric. Angle of axis in degrees.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param observed Logical.
#' @noRd

orthogonalCoordinates <- function(case, pump = 7, angle = 0, vestry = FALSE,
  observed = TRUE) {

  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }

  axis.focus <- p.data[p.data$id == pump, ]

  if (observed) {
    obs <- cholera::fatalities.address[cholera::fatalities.address$anchor ==
      case, c("x", "y")]
  } else {
    obs <- cholera::regular.cases[case, ]
  }

  axis.data <- profileOLS(pump, angle, vestry)

  if (angle == 0 | angle == 180) {
    x.proj <- obs$x
    y.proj <- axis.focus$y
  } else if (angle == 90 | angle == 270) {
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

#' Rescale data along axis.
#'
#' @param output Character."inside" or "outside".
#' @param pump Numeric. Selected pump focal point.
#' @param angle Numeric. Angle of perspective axis in degrees.
#' @param vestry Logical. \code{TRUE} for the 14 pumps from Vestry Report. \code{FALSE} for the original 13 pumps.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @noRd

profilePerspective <- function(output = "inside", pump = 7, angle = 0,
  vestry = FALSE, multi.core = FALSE) {

  walk <- nearestPump(vestry = vestry, multi.core = multi.core)
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

  coords <- lapply(cases, function(x) orthogonalCoordinates(x, angle = angle))
  coords <- stats::setNames(do.call(rbind, coords), vars)

  dat <- cholera::fatalities.address[cholera::fatalities.address$anchor %in% 
    cases, ]
  dat <- cbind(dat, coords)

  pump.data <- cholera::pumps[cholera::pumps$id == pump, c("x", "y")]
  pump.data$ortho.x <- pump.data$x
  pump.data$ortho.y <- pump.data$y
  pump.data$anchor <- 0
  pump.data$case.count <- 0
  pump.data <- pump.data[, names(dat)]

  dat <- rbind(pump.data, dat)
  dat <- dat[order(dat$ortho.x, dat$ortho.y), ]
  idx <- seq_len(nrow(dat))

  d <- vapply(idx[-length(idx)], function(i) {
    c(stats::dist(rbind(dat[i, vars], dat[i + 1, vars])))
  }, numeric(1L))

  d <- c(0, d)

  out <- data.frame(axis = cumsum(d) - cumsum(d)[which(dat$anchor == 0)],
                    count = dat$case.count)

  class(out) <- "profile_perspective"
  out
}

#' Plot method for profilePerspective().
#'
#' @param x An object of class "profile" created by \code{profilePerspective()}.
#' @param ... Additional plotting parameters.
#' @export

plot.profile_perspective <- function(x, ...) {
  if (class(x) != "profile_perspective") {
    stop('"x"\'s class needs to be "profile_perspective".')
  }

  plot(x$axis, x$count, type = "h")
  abline(v = 0, col = "red", lty = "dotted")
}
