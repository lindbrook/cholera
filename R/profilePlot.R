#' Profile Plot.
#'
#' @param pump Numeric. Selected pump focal point.
#' @param angle Numeric. Angle of perspective axis in degrees.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @param type Character. "base", "ggplot2", or "threejs".
#' @param drop Logical. Drop negative selection.
#' @import ggplot2
#' @export

profilePlot <- function(pump = 7, angle = 0, vestry = FALSE, multi.core = FALSE,
  type = "threejs", drop = TRUE) {

  if (type %in% c("base", "ggplot2", "threejs") == FALSE) {
    stop('type must be "base", "ggplot2" or "threejs"')
  }

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

  if (type %in% c("base", "ggplot2")) {
    if (length(pump) != 1) {
      stop('For type = ', type, ', select one pump.')
    }

    a <- profilePerspective("inside", pump = pump, angle = angle,
      vestry = vestry, multi.core = cores)
    b <- profilePerspective("outside", pump = pump, angle = angle,
      vestry = vestry, multi.core = cores)
  }

  if (type == "base") {
    par(mfrow = c(3, 1))
    x.rng <- range(a$axis, b$axis)
    y.rng <- range(a$count, b$count)
    plot(a$axis, a$count, type = "h", xlim = x.rng, ylim = y.rng, col = "red")
    title(main = paste("Axis angle =", angle))
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
      facet_wrap(~ facet, nrow = 3) +
      ggtitle(paste("Axis angle =", angle))
    p

  } else if (type == "threejs") {
    x <- cholera::fatalities.address$x
    y <- cholera::fatalities.address$y
    z <- cholera::fatalities.address$case.count
    nearest.pump <- cholera::nearestPump(multi.core = cores)
    snow.colors <- cholera::snowColors()
    address.colors <- snow.colors[paste0("p", nearest.pump$pump)]

    if (is.null(pump) == FALSE) {
      if (all(pump < 0)) {
        neg.selection <- pump.id[pump.id %in% abs(pump) == FALSE]
        alters <- names(address.colors) %in% paste0("p", neg.selection)
        if (drop == FALSE) {
          address.colors[names(address.colors) %in%
            paste0("p", abs(pump))] <- "lightgray"
        } else {
          x <- x[alters]
          y <- y[alters]
          z <- z[alters]
          address.colors <- address.colors[alters]
        }
      } else if (all(pump > 0)) {
        alters <- names(address.colors) %in% paste0("p", pump) == FALSE
        address.colors[alters] <- "lightgray"
      }
    }

    address.colors <- grDevices::adjustcolor(unname(address.colors),
      alpha.f = 2/3)
    threejs::scatterplot3js(x, y, z, cex = 0.5, color = address.colors)
  }
}

axisSlope <- function(angle) tan(pi * angle / 180)

axisIntercept <- function(m, x, y) {
  int <- y - m * x
  ifelse(m * x == 0, y, int)
}

orthogonalSlope <- function(b) -1 / b
orthogonalIntercept <- function(b, ortho.slope, y) y - ortho.slope * b

#' Intercept and slope of selected axis.
#'
#' @param pump Numeric. Numeric ID of pump (focal point).
#' @param angle Numeric. Angle of axis in degrees.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @noRd

ols <- function(pump = 7, angle = 0, vestry = FALSE) {
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
    obs <- cholera::fatalities.address[cholera::fatalities.address$anchor.case
      == case, c("x", "y")]
  } else {
    obs <- cholera::regular.cases[case, ]
  }

  axis.data <- ols(pump, angle, vestry)

  if (angle == 0) {
    x.proj <- obs$x
    y.proj <- axis.focus$y
  } else if (angle == 90) {
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
#' @param angle Numeric. Angle of perspective axis in degrees.
#' @param vestry Logical. \code{TRUE} for the 14 pumps from Vestry Report. \code{FALSE} for the original 13 pumps.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @export

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
