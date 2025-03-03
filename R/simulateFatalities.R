#' Project simulated fatalities onto road network
#'
#' Places regularly spaced "simulated" or "expected" cases across the face of the map and then finds the "addresses" of those cases via orthogonal projection or simple proximity to road graph network. These data are used to generate "expected" pump neighborhoods.
#' @param recompute.regular.cases Logical. \code{TRUE} re-computes regular data. \code{FALSE} uses pre-computed data. For replication of data used in the package.
#' @param simulated.obs Numeric. Number of regular cases. For use with \code{recompute.regular.cases = TRUE}.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores (rounds with \code{as.integer()}). See \code{vignette("Parallelization")} for details.
#' @return An R data frame: \code{\link{sim.ortho.proj}}.
#' @note This function is computationally intensive. With "simulated.obs" set to 20,000 (generating 19,993 cases). This function documents the code that generates \code{\link{sim.ortho.proj}} and \code{\link{regular.cases}}. In real world terms, the distance between simulated cases is approximately 6 meters.
#' @importFrom pracma cart2pol
#' @importFrom sp Polygon
#' @importFrom sp spsample
#' @export

simulateFatalities <- function(recompute.regular.cases = FALSE,
  simulated.obs = 20000L, multi.core = FALSE) {

  cores <- multiCore(multi.core)

  if (recompute.regular.cases) {
    reg.cases <- regularCases(simulated.obs = simulated.obs)
  } else {
    reg.cases <- cholera::regular.cases
  }

  idx <- seq_len(nrow(reg.cases))

  orthogonal.projection <- parallel::mclapply(idx, function(i) {
    case <- reg.cases[i, ]

    within.radius <- lapply(cholera::road.segments$id, function(x) {
      dat <- cholera::road.segments[cholera::road.segments$id == x, ]
      test1 <- withinRadius(case, dat[, c("x1", "y1")])
      test2 <- withinRadius(case, dat[, c("x2", "y2")])
      if (any(test1, test2)) unique(dat$id)
    })

    within.radius <- unlist(within.radius)

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      ortho.data <- orthogonalProjection(as.numeric(row.names(case)), seg.id,
        observed = FALSE, case.data = case)
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

    no.bisect1 <- stats::setNames(candidates[, c("x1", "y1")], c("x", "y"))
    no.bisect2 <- stats::setNames(candidates[, c("x2", "y2")], c("x", "y"))
    no.bisect <- rbind(no.bisect1, no.bisect2)

    endpt.dist <- vapply(seq_len(nrow(no.bisect)), function(i) {
      stats::dist(rbind(case, no.bisect[i, ]))
    }, numeric(1L))

    nearest.endpt <- which.min(endpt.dist)

    if (nearest.endpt <= nrow(ortho)) {
      c.data <- cbind(candidates[nearest.endpt, c("id", "x1", "y1")],
        endpt.dist[nearest.endpt])
    } else {
      c.data <- cbind(candidates[nearest.endpt - nrow(ortho),
        c("id", "x2", "y2")], endpt.dist[nearest.endpt])
    }

    c.data$type <- "eucl"
    prox.location <- stats::setNames(c.data, names(ortho.location))

    nearest <- which.min(c(ortho.location$dist, prox.location$dist))

    if (nearest == 1) out <- ortho.location
    else if (nearest == 2) out <- prox.location
    out
  }, mc.cores = cores)

  sim.ortho.proj <- data.frame(case = idx + 10000L,
    do.call(rbind, orthogonal.projection))
  row.names(sim.ortho.proj) <- NULL

  if (any(is.na(sim.ortho.proj))) stop("Can't find a road.", call. = FALSE)

  list(regular.cases = reg.cases, sim.ortho.proj = sim.ortho.proj)
}

# approx. 1/2 hr; odd-OK, even-not?
# sim <- simulateFatalities(recompute.regular.cases = TRUE)
# regular.cases <- sim$regular.cases
# sim.ortho.proj <- sim$sim.ortho.proj
# usethis::use_data(regular.cases, overwrite = TRUE)
# usethis::use_data(sim.ortho.proj, overwrite = TRUE)
