#' Project simulated fatalities onto road network.
#'
#' Places regularly spaced "simulated" or "expected" cases across the face of the map and then finds the "addresses" of those cases via orthogonal projection or simple proximity to road graph network. These data are used to generate "expected" pump neighborhoods.
#' @param recompute.regular.cases Logical. \code{TRUE} re-computes regular data. \code{FALSE} uses pre-computed data. For replication of data used in the package.
#' @param simulated.obs Numeric. Number of regular cases. For use with \code{recompute.regular.cases = TRUE}.
#' @param radius Numeric. Radius to check for nearby road segments. Nominal Dodson and Tobler units.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores (rounds with \code{as.integer()}). See \code{vignette("Parallelization")} for details.
#' @return An R data frame: \code{\link{sim.ortho.proj}}.
#' @note This function is computationally intensive. With "simulated.obs" set to 20,000 (generating 19,993 cases). This function documents the code that generates \code{\link{sim.ortho.proj}} and \code{\link{regular.cases}}. In real world terms, the distance between simulated cases is approximately 6 meters.
#' @noRd

simulateFatalitiesB <- function(recompute.regular.cases = FALSE,
  simulated.obs = 20000L, radius = 3, multi.core = FALSE) {

  cores <- multiCore(multi.core)

  if (recompute.regular.cases) {
    reg.cases <- regularCases(simulated.obs = simulated.obs)
  } else {
    reg.cases <- cholera::regular.cases
  }

  idx <- seq_len(nrow(reg.cases))
  vars <- c("x", "y")
  rd.segs <- cholera::road.segments

  orthogonal.proj <- parallel::mclapply(idx, function(i) {
    case <- reg.cases[i, ]
    ones <- rbind(case[, vars],
                  stats::setNames(rd.segs[, paste0(vars, 1)], vars))
    twos <- rbind(case[, vars],
                  stats::setNames(rd.segs[, paste0(vars, 2)], vars))
    d1 <- as.matrix(stats::dist(ones))[-1, 1]
    d2 <- as.matrix(stats::dist(twos))[-1, 1]
    within.radius <- rd.segs$id[d1 <= radius & d2 <= radius]

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      ortho.data <- orthogonalProjection(case = i, segment.id = seg.id,
        observed = FALSE, case.data = case)
      x.proj <- ortho.data$x.proj
      y.proj <- ortho.data$y.proj

      seg.data <- rd.segs[rd.segs$id == seg.id, c("x1", "y1", "x2", "y2")]
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
        data.frame(road.segment = seg.id, ortho.pts, dist = ortho.dist)
      }
    })

    ortho <- do.call(rbind, ortho.proj.test)

    if (any(!is.na(ortho$dist))) {
      ortho.location <- ortho[which.min(ortho$dist), ]
      ortho.location$type <- "ortho"
    } else {
      ortho.location <- ortho[1, ]
      ortho.location$type <- NA
    }

    ## nearest endpoint of nearest road segment ##

    unbisected.segs <- setdiff(within.radius, ortho$road.segment)
    candidates <- rd.segs[rd.segs$id %in% unbisected.segs, ]

    ones <- stats::setNames(candidates[, paste0(vars, 1)], vars)
    twos <- stats::setNames(candidates[, paste0(vars, 2)], vars)
    ep.dist <- as.matrix(stats::dist(rbind(case[, vars], ones, twos)))[-1, 1]

    nearest <- which.min(ep.dist)

    if (nearest > nrow(candidates)) {
      nearest <- nearest - nrow(candidates)
      var.sel <- paste0(vars, 2)
    } else {
      var.sel <- paste0(vars, 1)
    }

    prox.location <- data.frame(road.segment = candidates[nearest, "id"],
                                x.proj = candidates[nearest, var.sel[1]],
                                y.proj = candidates[nearest, var.sel[2]],
                                dist = ep.dist[nearest],
                                type = "eucl")

    nearest.sel <- which.min(c(ortho.location$dist, prox.location$dist))

    if (nearest.sel == 1) {
      ortho.location
    } else if (nearest.sel == 2) {
      prox.location
    }
  }, mc.cores = cores)

  sim.ortho.proj <- data.frame(case = idx + 2000L, 
    do.call(rbind, orthogonal.proj))
  row.names(sim.ortho.proj) <- NULL

  list(regular.cases = reg.cases, sim.ortho.proj = sim.ortho.proj)
}

# approx. 1/3 hr; 3.1 GHz Dual-Core Intel Core i5
# sim <- simulateFatalities(recompute.regular.cases = TRUE)
# regular.cases <- sim$regular.cases
# sim.ortho.proj <- sim$sim.ortho.proj
# usethis::use_data(regular.cases, overwrite = TRUE)
# usethis::use_data(sim.ortho.proj, overwrite = TRUE)
