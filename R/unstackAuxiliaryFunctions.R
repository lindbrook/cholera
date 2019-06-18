#' Auxiliary functions for unstackFatalities().
#' @noRd

unstackAuxiliaryFunctions <- function() NULL

orthoProj <- function(fatalities, case.id, cores, dev.mode) {
  ortho_proj <- function(case, fatalities) {
    case.data <- fatalities[fatalities$case == case, c("x", "y")]

    within.radius <- lapply(cholera::road.segments$id, function(id) {
      seg.data <- cholera::road.segments[cholera::road.segments$id == id, ]
      test1 <- cholera::withinRadius(case.data, seg.data[, c("x1", "y1")])
      test2 <- cholera::withinRadius(case.data, seg.data[, c("x2", "y2")])
      if (any(test1, test2)) unique(seg.data$id)
    })

    within.radius <- unlist(within.radius)

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      ortho.data <- cholera::orthogonalProjection(case, seg.id)
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

orthoProjB <- function(fatalities, road.segment.fix, cores, dev.mode) {
  ortho_projB <- function(i, fatalities, road.segment.fix) {
    case.data <- fatalities[road.segment.fix[[i]], ]
    seg.id <- names(road.segment.fix[i])

    ortho.data <- lapply(case.data$case, function(x) {
      cholera::orthogonalProjection(x, seg.id)
    })

    ortho.data <- do.call(rbind, ortho.data)
    x.proj <- ortho.data$x.proj
    y.proj <- ortho.data$y.proj

    proj.data <- lapply(1:nrow(case.data), function(j) {
      dat <- rbind(case.data[j, c("x", "y")], c(x.proj[j], y.proj[j]))
      cbind(x.proj[j], y.proj[j], c(stats::dist(dat)))
    })

    out <- data.frame(seg.id, do.call(rbind, proj.data), case.data$case,
      stringsAsFactors = FALSE)
    names(out) <- c("road.segment", "x.proj", "y.proj", "ortho.dist", "case")
    out
  }

  fix.id <- seq_along(road.segment.fix)

  if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = c("road.segment.fix", "fatalities"))
    projection <- parallel::parLapply(cl, fix.id, function(i) {
      ortho_projB(i, fatalities, road.segment.fix)
    })
    parallel::stopCluster(cl)
  } else {

    projection <- parallel::mclapply(fix.id, function(i) {
      ortho_projB(i, fatalities, road.segment.fix)
    }, mc.cores = cores)

  }
  projection
}

multipleAddress <- function(multiple.obs, ortho.proj, fatalities, cutpoint,
  cores, dev.mode) {
  multiple_address <- function(i, ortho.proj, fatalities, cutpoint) {
    cases <- ortho.proj[ortho.proj$road.segment == i, "case"]
    ortho <- ortho.proj[ortho.proj$road.segment == i, c("x.proj", "y.proj")]
    orientation <- sign(fatalities[cases, c("x", "y")] - ortho)

    sideA <- (orientation$x == -1 & orientation$y == 1) |
      (orientation$x == -1 & orientation$y == -1) |
      (orientation$x == 0 & orientation$y == 1) |
      (orientation$x == 1 & orientation$y == 0)

    orientation$side <- ifelse(sideA, 1, 0)

    if (length(unique(orientation$side)) == 2) {
      A <- as.numeric(row.names(orientation[orientation$side == 1, ]))
      B <- as.numeric(row.names(orientation[orientation$side == 0, ]))
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

      outB <- max(outA) +  outB
      census <- c(outA, outB)
      out <- data.frame(case = as.numeric(names(census)), group = census,
        stringsAsFactors = FALSE)
      row.names(out) <- NULL
    }

    if (length(unique(orientation$side)) == 1) {
      A <- as.numeric(row.names(orientation))
      dataA <- ortho.proj[ortho.proj$case %in% A, c("x.proj", "y.proj")]

      if (nrow(dataA) >= 2) {
        clusterA <- stats::hclust(stats::dist(dataA))
        outA <- stats::cutree(clusterA, h = cutpoint)
      } else {
        outA <- 1
      }

      out <- data.frame(case = A, group = outA, stringsAsFactors = FALSE)
      row.names(out) <- NULL
    }

    out <- merge(out, ortho.proj[ortho.proj$road.segment == i,
      c("case", "ortho.dist")], by = "case")
    out <- out[order(out$group, out$ortho.dist), ]
    out$anchor <- ifelse(duplicated(out$group) == FALSE, 1, 0)
    data.frame(id = i, out, stringsAsFactors = FALSE)
  }

  if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = c("ortho.proj", "fatalities", "cutpoint"))
    addr <- parallel::parLapply(cl, multiple.obs$id, function(i) {
      multiple_address(i, ortho.proj, fatalities, cutpoint)
    })
    parallel::stopCluster(cl)
  } else {
    addr <- parallel::mclapply(multiple.obs$id, function(i) {
      multiple_address(i, ortho.proj, fatalities, cutpoint)
    }, mc.cores = cores)
  }
  addr
}

multipleUnstack <- function(multiple.address, cores, dev.mode) {
  multiple_unstack <- function(x) {
    group.id <- unique(x$group)
    group.data <- lapply(group.id, function(i) {
      tmp <- x[x$group == i, ]
      tmp$case.count <- nrow(tmp)
      tmp$anchor <- tmp[tmp$anchor == 1, "case"]
      tmp$ortho.dist <- NULL
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
  unstacked
}
