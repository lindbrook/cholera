#' Compute the midpoint of "extended" road segments.
#'
#' For segments with endpoints with different nearest pumps using Dodson and Tobler's "native" data.
#' @param diff_pump.endpts data.frame. Data for road segments with endpoints that have different nearest pumps.
#' @param endpt.data data.frame. Road segment endpoint data about their nearest pump.
#' @param same_pump.cases list. Cases classified by pump for road segments with endpoints with same nearest pump.
#' @param cores Numeric or Integer. Number of cores to use for parallel computation.
#' @note An "extended" road segment extends that line segment by the distance to the nearest pump. The midpoint is 1/2 the length of the extended segment.
#' @noRd

midpointNative <- function(diff_pump.endpts, endpt.data, same_pump.cases,
  cores) {

  vars <- c("x", "y")

  midpoint <- lapply(diff_pump.endpts$id, function(seg) {
    sel <- diff_pump.endpts$id == seg
    seg.df <- data.frame(x = unlist(diff_pump.endpts[sel, c("x1", "x2")]),
                         y = unlist(diff_pump.endpts[sel, c("y1", "y2")]),
                         row.names = NULL)

    ols <- stats::lm(y ~ x, data = seg.df)
    segment.slope <- stats::coef(ols)[2]
    theta <- atan(segment.slope)

    delta.x1 <- diff_pump.endpts[sel, "d1"] * cos(theta)
    delta.y1 <- diff_pump.endpts[sel, "d1"] * sin(theta)
    delta.x2 <- diff_pump.endpts[sel, "d2"] * cos(theta)
    delta.y2 <- diff_pump.endpts[sel, "d2"] * sin(theta)

    # pseudo-origin (west or left endpoint)
    ego <- which.min(seg.df$x)

    if (ego == 1) {
      alter <- 2
      xs <- c(seg.df[1, "x"] - delta.x1, seg.df[2, "x"] + delta.x2)
      ys <- c(seg.df[1, "y"] - delta.y1, seg.df[2, "y"] + delta.y2)
    } else if (ego == 2) {
      alter <- 1
      xs <- c(seg.df[2, "x"] + delta.x2, seg.df[1, "x"] - delta.x1)
      ys <- c(seg.df[2, "y"] + delta.y2, seg.df[1, "y"] - delta.y1)
    }

    extended.seg <- data.frame(x = xs, y = ys)

    h <- stats::dist(extended.seg) / 2
    delta.x <- unname(h * cos(theta))
    delta.y <- unname(h * sin(theta))

    data.frame(x = c(extended.seg[ego, "x"] + delta.x),
               y = c(extended.seg[ego, "y"] + delta.y))
  })

  midpoint <- do.call(rbind, midpoint)
  midpoint <- data.frame(id = diff_pump.endpts$id, midpoint, row.names = NULL)

  diff_pump.cases <- parallel::mclapply(midpoint$id, function(seg) {
    sel <- cholera::sim.ortho.proj$road.segment == seg
    seg.data <- cholera::sim.ortho.proj[sel, ]

    mid.pt <- midpoint[midpoint$id == seg, vars]
    ep.data <- endpt.data[endpt.data$id == seg, ]

    case.data <- stats::setNames(seg.data[, c("x.proj", "y.proj")], vars)

    tmp <- lapply(seq_along(case.data$x), function(i) case.data[i, ] - mid.pt)
    mid.pt.delta <- sign(do.call(rbind, tmp))

    one <- sign(ep.data[, c("x1", "y1")] - mid.pt)
    two <- sign(ep.data[, c("x2", "y2")] - mid.pt)

    pmp <- vapply(seq_along(mid.pt.delta$x), function(i) {
      if (all(mid.pt.delta[i, ] == one)) {
        ep.data[, paste0("pump", 1)]
      } else if (all(mid.pt.delta[i, ] == two)) {
        ep.data[, paste0("pump", 2)]
      }
    }, numeric(1L))

    data.frame(case = seg.data$case, pump = pmp)
  }, mc.cores = cores)

  diff_pump.cases <- do.call(rbind, diff_pump.cases)
  diff_pump.cases <- split(diff_pump.cases$case, diff_pump.cases$pump)

  exp.pump.case <- lapply(names(same_pump.cases), function(nm) {
    c(same_pump.cases[[nm]], diff_pump.cases[[nm]])
  })

  names(exp.pump.case) <- names(same_pump.cases)

  diff_pump.road_segs <- lapply(midpoint$id, function(seg) {
    mid.pt <- midpoint[midpoint$id == seg, vars]
    ep.data <- endpt.data[endpt.data$id == seg, ]
    tmp <- rbind(ep.data, ep.data)
    tmp[1, c("x2", "y2")] <- mid.pt
    tmp[2, c("x1", "y1")] <- mid.pt
    tmp[, c("n1", "n2")] <- NULL
    tmp$pump <- c(tmp[1, "pump1"], tmp[2, "pump2"])
    tmp[, c("pump1", "pump2")] <- NULL

    tmp$d <- vapply(seq_along(tmp$street), function(i) {
      stats::dist(rbind(stats::setNames(tmp[i, paste0(vars, 1)], vars),
                        stats::setNames(tmp[i, paste0(vars, 2)], vars)))
    }, numeric(1L))

    tmp[, c("d1", "d2")] <- NULL
    tmp$id <- paste0(tmp$street, "-", c("A", "Z"))
    tmp
  })

  diff_pump.road_segs <- do.call(rbind, diff_pump.road_segs)
  diff_pump.road_segs <- split(diff_pump.road_segs, diff_pump.road_segs$pump)
  list(exp.pump.case = exp.pump.case,
       diff_pump.road_segs = diff_pump.road_segs)
}

