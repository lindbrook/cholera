#' Project observed fatalities onto road network
#'
#' Finds the "addresses" of those cases via orthogonal projection or simple proximity to road graph network. These data are used to generate "observed" pump neighborhoods.
#' @param case.type Character. "address" or "fatality".
#' @param radius Numeric. Radius to check for nearby road segments. Nominal Dodson and Tobler units.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores (rounds with \code{as.integer()}). See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @noRd

orthogonalProjectionFatalities <- function(case.type = "address", radius = 4,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)

  if (case.type == "address") {
    obs.cases <- cholera::fatalities.address
    id <- obs.cases$anchor
  } else if (case.type == "fatality") {
    obs.cases <- cholera::fatalities
    id <- obs.cases$case
  }

  vars <- c("x", "y")

  orthogonal.projection <- parallel::mclapply(id, function(cs) {
    if (case.type == "address") {
      case <- obs.cases[obs.cases$anchor == cs, ]
      case.id <- case$anchor
    } else if (case.type == "fatality") {
      case <- obs.cases[obs.cases$case == cs, ]
      case.id <- case$case
    }

    within.radius <- lapply(cholera::road.segments$id, function(x) {
      dat <- cholera::road.segments[cholera::road.segments$id == x, ]
      d1 <- stats::dist(rbind(case[, vars],
                              stats::setNames(dat[, paste0(vars, 1)], vars)))
      d2 <- stats::dist(rbind(case[, vars],
                              stats::setNames(dat[, paste0(vars, 2)], vars)))
      if (d1 <= radius | d2 <= radius) unique(dat$id)
    })

    within.radius <- unlist(within.radius)

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      ortho.data <- orthogonalProjection(case.id, seg.id, observed = TRUE,
        case.data = case)
      x.proj <- ortho.data$x.proj
      y.proj <- ortho.data$y.proj

      sel <- cholera::road.segments$id == seg.id
      seg.data <- cholera::road.segments[sel, c("x1", "y1", "x2", "y2")]

      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      ## segment bisection/intersection test ##

      distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
               stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

      bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

      if (bisect.test) {
        ortho.dist <- c(stats::dist(rbind(c(case$x, case$y),
          c(x.proj, y.proj))))
        ortho.pts <- data.frame(x.proj, y.proj)
        data.frame(road.segment = seg.id, ortho.pts, ortho.dist)
      } else {
        null.out <- data.frame(matrix(NA, ncol = 4))
        names(null.out) <- c("road.segment", "x.proj", "y.proj", "ortho.dist")
        null.out
      }
    })

    ortho <- do.call(rbind, ortho.proj.test)

    if (all(is.na(ortho$ortho.dist))) {
      ortho.location <- ortho[1, ]
      names(ortho.location)[names(ortho.location) == "ortho.dist"] <- "dist"
      ortho.location$type <- NA
    } else {
      ortho.location <- ortho[which.min(ortho$ortho.dist), ]
      names(ortho.location)[names(ortho.location) == "ortho.dist"] <- "dist"
      ortho.location$type <- "ortho"
    }

    ## nearest road segment endpoint ##
    sel <- cholera::road.segments$id %in% within.radius
    candidates <- cholera::road.segments[sel, ]

    no.bisect1 <- stats::setNames(candidates[, paste0(vars, 1)], vars)
    no.bisect2 <- stats::setNames(candidates[, paste0(vars, 2)], vars)
    no.bisect <- rbind(no.bisect1, no.bisect2)

    endpt.dist <- vapply(seq_len(nrow(no.bisect)), function(i) {
      stats::dist(rbind(case[, vars], no.bisect[i, ]))
    }, numeric(1L))

    nearest.endpt <- which.min(endpt.dist)

    if (nearest.endpt <= nrow(ortho)) {
      c.data <- cbind(candidates[nearest.endpt, c("id", "x1", "y1")],
                      endpt.dist[nearest.endpt])
    } else {
      sel <- nearest.endpt - nrow(ortho)
      c.data <- cbind(candidates[sel, c("id", "x2", "y2")],
                      endpt.dist[nearest.endpt])
    }

    c.data$type <- "eucl"
    prox.location <- stats::setNames(c.data, names(ortho.location))

    nearest <- which.min(c(ortho.location$dist, prox.location$dist))

    if (nearest == 1) {
      out <- ortho.location
    } else if (nearest == 2) {
      out <- prox.location
    }
    out
  }, mc.cores = cores)

  ortho.proj <- data.frame(case = id, do.call(rbind, orthogonal.projection))
  row.names(ortho.proj) <- NULL

  if (any(is.na(ortho.proj))) stop("Can't find a road.", call. = FALSE)

  ortho.proj
}
