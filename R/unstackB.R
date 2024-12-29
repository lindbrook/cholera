#' Unstack "stacks" in Snow's cholera map.
#'
#' Unstacks fatalities data by 1) assigning the coordinates of the base case to all cases in a stack and 2) setting the base case as an "address" and making the number of fatalities an attribute.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @seealso \code{vignette("unstacking.fatalities")}
#' @return An R list that includes \code{anchor.case}, \code{fatalities.anchor}, \code{fatalities.unstacked} and \code{ortho.proj}.
#' @note This function is computationally intensive. This function documents the code that generates \code{\link{anchor.case}}, \code{\link{fatalities.anchor}}, \code{\link{fatalities.unstacked}} and \code{\link{ortho.proj}}.
#' @noRd

unstackFatalitiesB <- function(multi.core = TRUE, dev.mode = FALSE) {
  cores <- multiCore(multi.core)
  fixed.fatalities <- fixFatalities()
  ortho.proj <- orthogonalProjectionFatalitiesB(fixed.fatalities, cores,
    dev.mode)

  ## Single and Multiple ##

  road.incidence <- table(ortho.proj$road.segment)
  road.incidence <- data.frame(id = names(road.incidence),
    count = c(road.incidence))

  single.obs <- road.incidence[road.incidence$count == 1, ]
  single.address <- lapply(single.obs$id, function(id) {
    sel <- ortho.proj$road.segment == id
    data.frame(id = id, case = ortho.proj[sel, "case"])
  })

  cutpoint <- 0.05
  multiple.obs <- road.incidence[road.incidence$count > 1, ]
  multiple.address <- multipleAddressB(multiple.obs, ortho.proj,
    fixed.fatalities, cutpoint, cores, dev.mode)

  multiple.unstacked <- multipleUnstackB(multiple.address, cores, dev.mode)

  single.unstacked <- do.call(rbind, single.address)
  single.unstacked[ c("group", "anchor",  "case.count")] <- 1
  single.unstacked$anchor <- single.unstacked$case
  single.unstacked$multiple.obs.seg <- "No"

  unstacked <- rbind(multiple.unstacked, single.unstacked)
  unstacked <- merge(unstacked, fixed.fatalities, by.x = "anchor",
    by.y = "case")

  fatalities.unstacked <- unstacked[, c("case", "x", "y")]
  idx <- order(fatalities.unstacked$case)
  fatalities.unstacked <- fatalities.unstacked[idx, ]
  row.names(fatalities.unstacked) <- NULL

  vars <- c("case", "x", "y", "case.count")
  fatalities.anchor <- unstacked[unstacked$anchor == unstacked$case, vars]
  names(fatalities.anchor)[1] <- "anchor"
  row.names(fatalities.anchor) <- NULL

  anchor.case <- unstacked[, c("anchor", "case")]

  list(anchor.case = anchor.case,
       fatalities = fixed.fatalities,
       fatalities.unstacked = fatalities.unstacked,
       fatalities.anchor = fatalities.anchor,
       ortho.proj = ortho.proj)
}

orthogonalProjectionFatalitiesB <- function(fatality.df, cores, dev.mode,
  radius = 2) {

  vars <- c("x", "y")
  manual.classification <- caseRoadClassificationFix() # TODO add workhouse

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
      within.radius <- cholera::road.segments$id[d1 <= radius & d2 <= radius]
    }

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      sel <- cholera::road.segments$id == seg.id
      seg.data <- cholera::road.segments[sel, c("x1", "y1", "x2", "y2")]

      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

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
        dat <- rbind(c(case.data$x, case.data$y), c(x.proj, y.proj))
        ortho.dist <- c(stats::dist(dat))
        ortho.pts <- data.frame(x.proj, y.proj)
        data.frame(road.segment = seg.id, ortho.pts, ortho.dist)
      }
    })

    ortho <- do.call(rbind, ortho.proj.test)

    if (is.null(ortho)) {
      na.vars <-  c("road.segment", "x.proj", "y.proj", "dist", "type")
      na.df <- data.frame(matrix(ncol = length(na.vars)))
      ortho.location <- stats::setNames(na.df, na.vars)
    } else {
      ortho.location <- ortho[which.min(ortho$ortho.dist), ]
      names(ortho.location)[names(ortho.location) == "ortho.dist"] <- "dist"
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

multipleAddressB <- function(multiple.obs, ortho.proj, fixed.fatalities,
  cutpoint, cores, dev.mode) {

  multiple_address <- function(id, ortho.proj, fixed.fatalities, cutpoint) {
    cases <- ortho.proj[ortho.proj$road.segment == id, "case"]
    ortho <- ortho.proj[ortho.proj$road.segment == id, c("x.proj", "y.proj")]
    orientation <- sign(fixed.fatalities[cases, c("x", "y")] - ortho)

    sideA <- (orientation$x == -1 & orientation$y ==  1) |
             (orientation$x == -1 & orientation$y == -1) |
             (orientation$x ==  0 & orientation$y ==  1) |
             (orientation$x ==  1 & orientation$y ==  0)

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

multipleUnstackB <- function(multiple.address, cores, dev.mode) {
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
