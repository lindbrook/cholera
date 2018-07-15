#' Generate simulated fatalities and their orthogonal projection on road network.
#'
#' Places regularly spaced "simulated" or "expected" cases across the face of the map. The function finds the "addresses" of cases on the road network via orthogonal projection (or simple proximity). These data are used to generate "expected" pump neighborhoods. The function relies on sp::spsample() and sp::Polygon().
#' @param compute Logical. TRUE computes data. FALSE uses pre-computed data. For replication of data used in the package, FALSE is the default.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. With Numeric, you specify the number logical cores (rounds with as.integer()). On Windows, only "multi.core = FALSE" is available.
#' @param simulated.obs Numeric. Number of sample cases. Default is 5000.
#' @return An R list with two elements: \code{\link{sim.ortho.proj}} and \code{\link{regular.cases}}
#' @note This function is computationally intensive. With "simulated.obs" set to 20,000 simulated cases (actually generating 19,993 cases), the function takes about 94 minutes to run on a single core of a 2.3 GHz Intel Core i7 with R version 3.5.1 and 22 minutes to run on eight logical (four physical) cores. This function documents the code that generates \code{\link{sim.ortho.proj}} and \code{\link{regular.cases}}. The distance between of these simulated cases is approximately 4.2 meters.
#' @export

simulateFatalities <- function(compute = FALSE, multi.core = FALSE,
  simulated.obs = 20000L) {

  if (compute == FALSE) {
    sim.ortho.proj <- cholera::sim.ortho.proj
    regular.cases <- cholera::regular.cases
    list(sim.ortho.proj = sim.ortho.proj, regular.cases = regular.cases)
  } else {
    cores <- multiCore(multi.core)
    rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]

    map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
    roads.list <- split(rd[, c("x", "y")], rd$street)
    border.list <- split(map.frame[, c("x", "y")], map.frame$street)

    road.segments <- parallel::mclapply(unique(rd$street), function(i) {
      dat <- rd[rd$street == i, ]
      names(dat)[names(dat) %in% c("x", "y")] <- c("x1", "y1")
      seg.data <- dat[-1, c("x1", "y1")]
      names(seg.data) <- c("x2", "y2")
      dat <- cbind(dat[-nrow(dat), ], seg.data)
      dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
      dat
    }, mc.cores = cores)

    road.segments <- do.call(rbind, road.segments)

    ## order vertices for polygon functions ##

    map.frame.centered <- data.frame(x = map.frame$x - mean(map.frame$x),
                                     y = map.frame$y - mean(map.frame$y))

    idx <- order(apply(map.frame.centered, 1, pracma::cart2pol)[1, ])
    map.frame <- map.frame[idx, ]

    ## regularly-spaced simulated cases ##

    sp.frame <- sp::spsample(sp::Polygon(map.frame[, c("x", "y")]),
      n = simulated.obs, type = "regular")
    regular.cases <- data.frame(sp.frame@coords)
    names(regular.cases) <- c("x", "y")
    regular.cases <- split(regular.cases, rownames(regular.cases))

    orthogonal.projection <- parallel::mclapply(regular.cases, function(case) {
      within.radius <- lapply(road.segments$id, function(x) {
        dat <- road.segments[road.segments$id == x, ]
        test1 <- withinRadius(case, dat[, c("x1", "y1")]) # in unstack.R
        test2 <- withinRadius(case, dat[, c("x2", "y2")])
        if (any(test1, test2)) unique(dat$id)
      })

      within.radius <- unlist(within.radius)

      ortho.proj.test <- lapply(within.radius, function(x) {
        seg.data <- road.segments[road.segments$id == x,
          c("x1", "y1", "x2", "y2")]

        seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                             y = c(seg.data$y1, seg.data$y2))

        ols <- stats::lm(y ~ x, data = seg.df)
        segment.slope <- stats::coef(ols)[2]
        segment.intercept <- stats::coef(ols)[1]
        orthogonal.slope <- -1 / segment.slope
        orthogonal.intercept <- case$y - orthogonal.slope * case$x

        x.proj <- (orthogonal.intercept - segment.intercept) /
                  (segment.slope - orthogonal.slope)

        y.proj <- segment.slope * x.proj + segment.intercept

        ## segment bisection/intersection test ##

        distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
          stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

        bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

        if (bisect.test) {
          ortho.dist <- c(stats::dist(rbind(c(case$x, case$y),
            c(x.proj, y.proj))))
          ortho.pts <- data.frame(x.proj, y.proj)
          data.frame(road.segment = x, ortho.pts, ortho.dist,
            stringsAsFactors = FALSE)
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

      # nearest road segment endpoint
      candidates <- road.segments[road.segments$id %in% within.radius, ]
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
      if (nearest == 1) {
        out <- ortho.location
      } else if (nearest == 2) {
        out <- prox.location
      }

      out
    }, mc.cores = cores)

    sim.ortho.proj <- do.call(rbind, orthogonal.projection)
    sim.ortho.proj$case <- as.numeric(names(orthogonal.projection))
    sim.ortho.proj <- sim.ortho.proj[order(sim.ortho.proj$case), ]
    row.names(sim.ortho.proj) <- NULL

    # remove cases that can't find a road
    if (any(is.na(sim.ortho.proj))) {
      sim.ortho.proj <- stats::na.omit(sim.ortho.proj)
    }

    rc <- do.call(rbind, regular.cases)
    rc <- rc[order(as.numeric(row.names(rc))), ]

    list(sim.ortho.proj = sim.ortho.proj, regular.cases = rc)
  }
}
