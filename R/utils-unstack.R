# Unitily functions for unstack()

orthoProj <- function(fatalities, case.id, cores, dev.mode) {
  ortho_proj <- function(case, fatalities) {
    case.data <- fatalities[fatalities$case == case, c("x", "y")]

    within.radius <- lapply(cholera::road.segments$id, function(id) {
      seg.data <- cholera::road.segments[cholera::road.segments$id == id, ]
      test1 <- withinRadius(case.data, seg.data[, c("x1", "y1")])
      test2 <- withinRadius(case.data, seg.data[, c("x2", "y2")])
      if (any(test1, test2)) unique(seg.data$id)
    })

    within.radius <- unlist(within.radius)

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      ortho.data <- orthogonalProjection(case, seg.id)
      x.proj <- ortho.data$x.proj
      y.proj <- ortho.data$y.proj

      seg.data <- cholera::road.segments[cholera::road.segments$id == seg.id,
        c("x1", "y1", "x2", "y2")]

      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      # segment bisection/intersection test
      distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
               stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

      bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

      if (bisect.test) {
        dat <- rbind(c(case.data$x, case.data$y), c(x.proj, y.proj))
        ortho.dist <- c(stats::dist(dat))
        ortho.pts <- data.frame(x.proj, y.proj)
        data.frame(road.segment = seg.id, ortho.pts, ortho.dist,
          stringsAsFactors = FALSE)
      } else {
        null.out <- data.frame(matrix(NA, ncol = 4))
        names(null.out) <- c("road.segment", "x.proj", "y.proj", "ortho.dist")
        null.out
      }
    })

    out <- do.call(rbind, ortho.proj.test)

    if (all(is.na(out)) == FALSE) {
      sel <- which.min(out$ortho.dist)
      out[sel, ]
    } else {
      out[1, ] # all candidate roads are NA; arbitrarily choose first obs.
    }
  }

  # case.id <- fatalities$case

  if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, varlist = "fatalities",
      envir = environment())
    projection <- parallel::parLapply(cl, case.id, function(case) {
      ortho_proj(case, fatalities)
    })
    parallel::stopCluster(cl)
  } else {
    projection <- parallel::mclapply(case.id, function(case) {
      ortho_proj(case, fatalities)
    }, mc.cores = cores)
  }
  projection
}

orthogonalProjectionFatalities <- function(fatality.df, cores, radius = 2) {
  vars <- c("x", "y")
  manual.classification <- caseRoadClassificationFix()

  out <- parallel::mclapply(fatality.df$case, function(case.id) {
    case.data <- fatality.df[fatality.df$case == case.id, vars]
    if (case.id %in% unlist(manual.classification)) {
      sel <- vapply(manual.classification, function(x) {
        case.id %in% x
      }, logical(1L))
      within.radius <- names(manual.classification[sel])
    } else {
      ones <- rbind(case.data[, vars],
        stats::setNames(cholera::road.segments[, paste0(vars, 1)], vars))
      twos <- rbind(case.data[, vars],
        stats::setNames(cholera::road.segments[, paste0(vars, 2)], vars))
      d1 <- as.matrix(stats::dist(ones))[-1, 1]
      d2 <- as.matrix(stats::dist(twos))[-1, 1]

      # choose road segment with _both_ endpoints <= radius
      within.radius <- cholera::road.segments$id[d1 <= radius & d2 <= radius]
    }

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      sel <- cholera::road.segments$id == seg.id
      xs <- unlist(cholera::road.segments[sel, c("x1", "x2")])
      ys <- unlist(cholera::road.segments[sel, c("y1", "y2")])
      seg.df <- data.frame(x = xs, y = ys)

      ols <- stats::lm(y ~ x, data = seg.df)
      road.intercept <- stats::coef(ols)[1]
      road.slope <- stats::coef(ols)[2]
      ortho.slope <- -1 / road.slope
      ortho.intercept <- case.data$y - ortho.slope * case.data$x
      x.proj <- (ortho.intercept - road.intercept) / (road.slope - ortho.slope)
      y.proj <- road.slope * x.proj + road.intercept

      # segment bisection/intersection test
      distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
               stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

      bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

      if (bisect.test) {
        ortho.pts <- data.frame(x.proj, y.proj)
        dat <- rbind(c(case.data$x, case.data$y), c(x.proj, y.proj))
        data.frame(road.segment = seg.id, ortho.pts, dist = c(stats::dist(dat)))
      }
    })

    ortho <- do.call(rbind, ortho.proj.test)

    if (is.null(ortho)) {
      na.vars <-  c("road.segment", "x.proj", "y.proj", "dist", "type")
      na.df <- data.frame(matrix(ncol = length(na.vars)))
      ortho.location <- stats::setNames(na.df, na.vars)
    } else {
      ortho.location <- ortho[which.min(ortho$dist), ]
      ortho.location$type <- "ortho"
    }

    ## nearest endpoint of nearest road segment ##

    unbisected.segs <- setdiff(within.radius, ortho$road.segment)

    if (length(within.radius) <= 1) {
      sel <- cholera::road.segments$id == within.radius
      candidates <- cholera::road.segments[sel, ]

      ones <- stats::setNames(candidates[, paste0(vars, 1)], vars)
      twos <- stats::setNames(candidates[, paste0(vars, 2)], vars)
      ep.data <- rbind(case.data[, vars], ones, twos)
      ep.dist <- as.matrix(stats::dist(ep.data))[-1, 1]

      # nearest endpoint of nearest road segment
      nearest <- which.min(ep.dist)

      if (nearest == 1) {
        var.sel <- paste0(vars, 1)
      } else if (nearest == 2) {
        var.sel <- paste0(vars, 2)
      }

      prox.location <- data.frame(road.segment = candidates$id,
                                  x.proj = candidates[, var.sel[1]],
                                  y.proj = candidates[, var.sel[2]],
                                  dist = ep.dist[nearest],
                                  type = "eucl")

    } else if (length(unbisected.segs) > 1) {
      sel <- cholera::road.segments$id %in% unbisected.segs
      candidates <- cholera::road.segments[sel, ]

      ones <- stats::setNames(candidates[, paste0(vars, 1)], vars)
      twos <- stats::setNames(candidates[, paste0(vars, 2)], vars)
      ep.data <- rbind(case.data[, vars], ones, twos)
      ep.dist <- as.matrix(stats::dist(ep.data))[-1, 1]

      # nearest endpoint of nearest road segment
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
    }

    nearest.sel <- which.min(c(ortho.location$dist, prox.location$dist))

    if (nearest.sel == 1) {
      out <- ortho.location
    } else if (nearest.sel == 2) {
      out <- prox.location
    }

    data.frame(case = case.id, out, row.names = NULL)
  }, mc.cores = cores)
  do.call(rbind, out)
}

multipleAddress <- function(multiple.obs, ortho.proj, fixed.fatalities,
  cutpoint, cores, dev.mode) {

  multiple_address <- function(id, ortho.proj, fixed.fatalities, cutpoint) {
    cases <- ortho.proj[ortho.proj$road.segment == id, "case"]
    ortho <- ortho.proj[ortho.proj$road.segment == id, c("x.proj", "y.proj")]
    orientation <- sign(fixed.fatalities[cases, c("x", "y")] - ortho)

    sideA <- (orientation$x == -1 & orientation$y ==  1) |
             (orientation$x == -1 & orientation$y == -1) |
             (orientation$x ==  0 & orientation$y ==  1) |
             (orientation$x ==  1 & orientation$y ==  0)

    # side of street
    orientation$side <- ifelse(sideA, 1, 0)

    if (length(unique(orientation$side)) == 2) {
      A <- cases[orientation$side == 1]
      B <- cases[orientation$side == 0]
      dataA <- ortho.proj[ortho.proj$case %in% A, c("x.proj", "y.proj")]
      dataB <- ortho.proj[ortho.proj$case %in% B, c("x.proj", "y.proj")]

      if (nrow(dataA) >= 2) {
        clusterA <- stats::hclust(stats::dist(dataA))
        outA <- stats::cutree(clusterA, h = cutpoint)
      } else {
        outA <- 1
        names(outA) <- row.names(dataA)
      }

      if (nrow(dataB) >= 2) {
        clusterB <- stats::hclust(stats::dist(dataB))
        outB <- stats::cutree(clusterB, h = cutpoint)
      } else {
        outB <- 1
        names(outB) <- row.names(dataB)
      }

      outB <- max(outA) + outB
      census <- c(outA, outB)
      out <- data.frame(case = as.numeric(names(census)), group = census)
      row.names(out) <- NULL

    } else if (length(unique(orientation$side)) == 1) {
      dataA <- ortho.proj[ortho.proj$case %in% cases, c("x.proj", "y.proj")]

      if (nrow(dataA) >= 2) {
        clusterA <- stats::hclust(stats::dist(dataA))
        outA <- stats::cutree(clusterA, h = cutpoint)
      } else {
        outA <- 1
      }

      out <- data.frame(case = cases, group = outA)
      row.names(out) <- NULL
    }

    sel <- ortho.proj$road.segment == id
    out <- merge(out, ortho.proj[sel, c("case", "dist")], by = "case")

    out <- out[order(out$group, out$dist), ]
    out$anchor <- ifelse(duplicated(out$group) == FALSE, 1, 0)
    data.frame(id = id, out)
  }

  if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = c("ortho.proj", "fixed.fatalities", "cutpoint"))
    addr <- parallel::parLapply(cl, multiple.obs$id, function(id) {
      multiple_address(id, ortho.proj, fixed.fatalities, cutpoint)
    })
    parallel::stopCluster(cl)
  } else {
    addr <- parallel::mclapply(multiple.obs$id, function(id) {
      multiple_address(id, ortho.proj, fixed.fatalities, cutpoint)
    }, mc.cores = cores)
  }
  addr
}

multipleUnstack <- function(multiple.address, cores, dev.mode) {
  multiple_unstack <- function(x) {
    group.id <- unique(x$group)
    group.data <- lapply(group.id, function(id) {
      tmp <- x[x$group == id, ]
      tmp$case.count <- nrow(tmp)
      tmp$anchor <- tmp[tmp$anchor == 1, "case"]
      tmp$dist <- NULL
      tmp
    })
    do.call(rbind, group.data)
  }

  if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
    cl <- parallel::makeCluster(cores)
    unstacked <- parallel::parLapply(cl, multiple.address, multiple_unstack)
    parallel::stopCluster(cl)
  } else {
    unstacked <- parallel::mclapply(multiple.address, multiple_unstack,
      mc.cores = cores)
  }
  out <- do.call(rbind, unstacked)
  out$multiple.obs.seg <- "Yes"
  out
}
