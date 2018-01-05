#' Compute walking path pump neighborhoods.
#'
#' Compute and group walking paths by neighborhood using John Snow's 1854 London cholera data.
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Note that you can't just select the pump on Adam and Eve Court (#2): it is a technical isolate.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path weighted by road length. FALSE computes shortest path in terms of the number of nodes.
#' @param observed Logical. Observed or expected walking path pump neighborhoods.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. Currently, only "multi.core = FALSE" is available on Windows.
#' @return An R list with 7 objects:
#' \itemize{
#'   \item{\code{paths}: list of paths to nearest or selected pump(s).}
#'   \item{\code{cases}: list of cases by pump.}
#'   \item{\code{vestry}: "vestry" from neighborhoodWalking().}
#'   \item{\code{observed}: "observed" from neighborhoodWalking().}
#'   \item{\code{pump.select}: "pump.select" from neighborhoodWalking().}
#'   \item{\code{cores}: number of cores to use for parallel implementation.}
#'   \item{\code{metric}: incremental metric used to find cut point on split road segments.}
#' }
#' @section Notes: This function is computationally intensive: the default configuration takes about 5 seconds; expected configurations (observed == FALSE) take about 30 seconds.
#' @export
#' @examples
#' # neighborhoodWalking()
#' # neighborhoodWalking(pump.select = -6)

neighborhoodWalking <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, observed = TRUE, multi.core = FALSE) {

  if (is.null(pump.select) == FALSE) {
    if (length(pump.select) == 1) {
      if (pump.select == 2) {
        msg1 <- "You can't just select the pump on Adam and Eve Court (#2).\n"
        msg2 <- " It's an isolate, unreachable for observed fatalities."
        stop(paste(msg1, msg2))
      }
    }
  }

  if (is.logical(multi.core)) {
    if (multi.core == TRUE) {
      cores <- parallel::detectCores()
    } else {
      if (is.numeric(multi.core)) {
        if (is.integer(multi.core)) {
          cores <- multi.core
        } else {
          cores <- as.integer(multi.core)
        }
      } else {
        cores <- 1L
      }
    }
  } else if (is.numeric(multi.core)) {
    if (is.integer(multi.core)) {
      cores <- multi.core
    } else {
      cores <- as.integer(multi.core)
    }
  }

  args <- list(pump.select = pump.select,
               vestry = vestry,
               weighted = weighted,
               observed = observed,
               cores = cores)

  nearest.path <- do.call("nearestPath", args)
  nearest.pump <- do.call("nearestPump", args)
  pumpID <- sort(unique(nearest.pump$pump))

  neighborhood.cases <- lapply(pumpID, function(p) {
    which(nearest.pump$pump == p)
  })

  names(neighborhood.cases) <- pumpID

  neighborhood.paths <- lapply(pumpID, function(p) {
    sel <- neighborhood.cases[[paste(p)]]
    nearest.path[sel]
  })

  names(neighborhood.paths) <- pumpID

  out <- list(paths = neighborhood.paths,
              cases = neighborhood.cases,
              vestry = vestry,
              observed = observed,
              pump.select = pump.select,
              cores = cores,
              metric = 1 / cholera::unitMeter(1, "meter"))

  class(out) <- "walking"
  out
}

#' Print method for neighborhoodWalking().
#'
#' Return count of paths (anchor cases) by pump neighborhood.
#' @param x An object of class "walking" created by neighborhoodWalking().
#' @param ... Additional parameters.
#' @return An R vector.
#' @export
#' @examples
#' # neighborhoodWalking()
#' # print(neighborhoodWalking())

print.walking <- function(x, ...) {
  if (class(x) != "walking") {
    stop('"x"\'s class needs to be "walking".')
  }
   out <- vapply(x$paths, length, numeric(1L))
   print(out)
}

#' Plot method for neighborhoodWalking().
#'
#' @param x An object of class "walking" created by neighborhoodWalking().
#' @param area Logical. TRUE returns expected area plot. FALSE returns expected walking paths. Works only with neighborhoodWalking(observed = FALSE).
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @examples
#' # plot(neighborhoodWalking())
#' # plot(neighborhoodWalking(observed = FALSE))
#' # plot(neighborhoodWalking(observed = FALSE), area = TRUE)

plot.walking <- function(x, area = FALSE, ...) {
  if (class(x) != "walking") {
    stop('"x"\'s class needs to be "walking".')
  }

  if (x$observed & area) {
    stop('"area = TRUE" valid only when neighborhoodWalking(observed = FALSE).')
  }

  if (x$vestry) {
    dat <- neighborhoodData(vestry = TRUE)
  } else {
    dat <- neighborhoodData()
  }

  edges <- dat$edges

  edgeAudit <- function(x) {
    vapply(seq_along(x[-1]), function(i) {
      ab <- edges$node1 %in% x[i] &
            edges$node2 %in% x[i + 1]
      ba <- edges$node2 %in% x[i] &
            edges$node1 %in% x[i + 1]
      which(ab | ba)
    }, numeric(1L))
  }

  n.paths <- lapply(x$paths, function(neighborhood) {
    dat <- lapply(neighborhood, edgeAudit)
  })

  edge.data <- lapply(n.paths, function(x) unique(unlist(x)))

  if (x$observed == FALSE) {
    # edge.data only capture segments where paths cross both endpoints.
    # To capture the remaining segments, find the missing whole segments and
    # the missing split segments (sub-segments which lead to different pumps.)
    p.data <- dat$nodes.pump

    if (is.null(x$pump.select)) {
      p.node <- p.data$node
      p.name <- p.data$pump
    } else {
      if (all(x$pump.select > 0)) {
        p.data <- p.data[p.data$pump %in% x$pump.select, ]
      } else if (all(x$pump.select < 0)) {
        p.data <- p.data[p.data$pump %in% abs(x$pump.select) == FALSE, ]
      }
      p.node <- p.data$node
      p.name <- p.data$pump
    }

    edgesID <- seq_len(nrow(edges))
    isolates <- which(edges$name %in%
      c("Adam and Eve Court", "Falconberg Court", "Falconberg Mews"))

    drawn.segments <- sort(unname(unlist(edge.data)))
    missing.segments <- setdiff(edgesID[-isolates], drawn.segments)
    missing.segments <- unique(edges[missing.segments, "id"])

    # Exclude Portland Mews last segment (zero length).
    missing.segments <- missing.segments[missing.segments != "160-3"]

    nearest.pump <- parallel::mclapply(missing.segments, function(s) {
      seg.data <- cholera::road.segments[cholera::road.segments$id == s,
        c("x1", "y1", "x2", "y2")]

      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      ols <- stats::lm(y ~ x, data = seg.df)
      segment.slope <- stats::coef(ols)[2]

      theta <- atan(segment.slope)
      hypotenuse <- c(stats::dist(seg.df))
      hypotenuse.breaks <- seq(0, hypotenuse, x$metric)[-1]

      distances <- lapply(hypotenuse.breaks, function(h) {
        delta.x <- h * cos(theta)
        delta.y <- h * sin(theta)

        EW <- which.min(seg.data[, c("x1", "x2")])

        if (EW == 1) {
          test.x <- seg.data$x1 + delta.x
          test.y <- seg.data$y1 + delta.y
        } else {
          test.x <- seg.data$x2 + delta.x
          test.y <- seg.data$y2 + delta.y
        }

        case.node <- paste0(test.x, "-", test.y)
        seg.edge <- edges[edges$id == s, ]
        seg.edge <- seg.edge[c(1, nrow(seg.edge)), ]
        seg.edge[1, c("x2", "y2")] <- c(test.x, test.y)
        seg.edge[2, c("x1", "y1")] <- c(test.x, test.y)
        seg.edge[1, "node2"] <- case.node
        seg.edge[2, "node1"] <- case.node
        seg.edge[2, "id2"] <- paste0(s, "b")
        seg.edge$d <- sqrt((seg.edge$x1 - seg.edge$x2)^2 +
                           (seg.edge$y1 - seg.edge$y2)^2)

        edges2 <- rbind(seg.edge, edges[edges$id != s, ])
        edge.list <- edges2[, c("node1", "node2")]
        g2 <- igraph::graph_from_data_frame(edge.list, directed = FALSE)
        stats::setNames(c(igraph::distances(g2, case.node, p.node,
          weights = edges2$d)), p.name)
      })

      p <- vapply(distances, function(x) {
        as.numeric(names(which.min((x))))
      }, numeric(1L))

      data.frame(pump = p, cutpoint = hypotenuse.breaks)
    }, mc.cores = x$cores)

    rle.audit <- lapply(nearest.pump, function(x) rle(x$pump))
    rle.ct <- vapply(rle.audit, function(x) length(x$values), numeric(1L))

    whole.missing.segments <- missing.segments[rle.ct == 1]

    whole.missing.pumps <- vapply(rle.audit[rle.ct == 1], function(x) {
      x$values
    }, numeric(1L))

    split.missing.segs <- missing.segments[rle.ct != 1]

    split.missing.id <- vapply(rle.audit[rle.ct != 1], function(x) {
      x$lengths[1]
    }, numeric(1L))

    split.missing.data <- lapply(seq_along(split.missing.id), function(i) {
      dat <- nearest.pump[rle.ct != 1][[i]]
      dat[c(split.missing.id[i], split.missing.id[i] + 1), ]
    })

    split.missing.segments <- lapply(seq_along(split.missing.segs),
      function(i) {

      seg.data <- cholera::road.segments[cholera::road.segments$id ==
        split.missing.segs[i], ]

      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      ols <- stats::lm(y ~ x, data = seg.df)
      segment.slope <- stats::coef(ols)[2]
      theta <- atan(segment.slope)

      split.data <- split.missing.data[[i]]

      h <- split.data$cutpoint
      delta.x <- h * cos(theta)
      delta.y <- h * sin(theta)

      EW <- which.min(seg.data[, c("x1", "x2")])

      if (EW == 1) {
        seg1 <- data.frame(seg.data[, c("x1", "y1")],
                           x2 = seg.data$x1 + delta.x[1],
                           y2 = seg.data$y1 + delta.y[1],
                           row.names = NULL)

        seg2 <- data.frame(x1 = seg.data$x1 + delta.x[2],
                           y1 = seg.data$y1 + delta.y[2],
                           seg.data[, c("x2", "y2")],
                           row.names = NULL)
     } else if (EW == 2) {
       seg1 <- data.frame(seg.data[, c("x2", "y2")],
                          x1 = seg.data$x2 + delta.x[1],
                          y1 = seg.data$y2 + delta.y[1],
                          row.names = NULL)

       seg2 <- data.frame(x2 = seg.data$x2 + delta.x[2],
                          y2 = seg.data$y2 + delta.y[2],
                          seg.data[, c("x1", "y1")],
                          row.names = NULL)
      }

      data.frame(rbind(seg1, seg2), pump = split.data$pump)
    })

    if (area == TRUE) {

      ## drawn segments ##

      sel <- is.na(cholera::sim.ortho.proj$road.segment) == FALSE
      sim.proj <- cholera::sim.ortho.proj[sel, ]

      edge.data.id <- lapply(edge.data, function(i) {
        unique(edges[i, "id"]  )
      })

      case.neighborhood <- lapply(unique(sim.proj$road.segment), function(rd) {
        vapply(edge.data.id, function(x) rd %in% x, logical(1L))
      })

      select <- vapply(case.neighborhood, any, logical(1L))

      p1 <- vapply(case.neighborhood[select], function(x) {
        as.numeric(names(which(x)))
      }, numeric(1L))

      segments1 <- data.frame(id = unique(sim.proj$road.segment)[select],
                              pump = p1,
                              stringsAsFactors = FALSE)

      sim.proj$pump <- NA

      for (id in segments1$id) {
        sim.proj[sim.proj$road.segment %in% id, "pump"] <-
          segments1[segments1$id == id, "pump"]
      }

      ## whole missing segments ##

      whole.missing.id <- split(whole.missing.segments, whole.missing.pumps)

      case.neighborhood2 <- lapply(unique(sim.proj$road.segment), function(rd) {
        vapply(whole.missing.id, function(x) rd %in% x, logical(1L))
      })

      select2 <- vapply(case.neighborhood2, any, logical(1L))

      p2 <- vapply(case.neighborhood2[select2], function(x) {
        as.numeric(names(which(x)))
      }, numeric(1L))

      segments2 <- data.frame(id = unique(sim.proj$road.segment)[select2],
                              pump = p2,
                              stringsAsFactors = FALSE)

      for (id in segments2$id) {
        sim.proj[sim.proj$road.segment %in% id, "pump"] <-
          segments2[segments2$id == id, "pump"]
      }

      ## split missing segments ##

      sim.proj3 <- sim.proj[is.na(sim.proj$pump), ]

      falconberg <- cholera::road.segments[cholera::road.segments$name %in%
        c("Falconberg Court", "Falconberg Mews"), "id"]

      sim.proj3 <- sim.proj3[sim.proj3$road.segment %in% falconberg == FALSE, ]

      split.missing <- lapply(seq_along(split.missing.segs), function(i) {
        seg.data <- cholera::road.segments[cholera::road.segments$id ==
          split.missing.segs[i], ]

        seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                             y = c(seg.data$y1, seg.data$y2))

        ols <- stats::lm(y ~ x, data = seg.df)
        segment.slope <- stats::coef(ols)[2]
        theta <- atan(segment.slope)

        split.data <- split.missing.data[[i]]

        h <- split.data$cutpoint
        delta.x <- h * cos(theta)
        delta.y <- h * sin(theta)

        EW <- which.min(seg.data[, c("x1", "x2")])

        if (EW == 1) {
          seg1 <- data.frame(seg.data[, c("x1", "y1")],
                             x2 = seg.data$x1 + delta.x[1],
                             y2 = seg.data$y1 + delta.y[1],
                             row.names = NULL)

          seg2 <- data.frame(x1 = seg.data$x1 + delta.x[2],
                             y1 = seg.data$y1 + delta.y[2],
                             seg.data[, c("x2", "y2")],
                             row.names = NULL)
       } else if (EW == 2) {
         seg1 <- data.frame(seg.data[, c("x2", "y2")],
                            x1 = seg.data$x2 + delta.x[1],
                            y1 = seg.data$y2 + delta.y[1],
                            row.names = NULL)

         seg2 <- data.frame(x2 = seg.data$x2 + delta.x[2],
                            y2 = seg.data$y2 + delta.y[2],
                            seg.data[, c("x1", "y1")],
                            row.names = NULL)
        }

        out <- data.frame(id = split.missing.segs[i],
                          rbind(seg1, seg2),
                          pump = split.data$pump,
                          stringsAsFactors = FALSE)

        out$id2 <- paste0(out$id, letters[25:26])
        out
      })

      split.missing <- do.call(rbind, split.missing)

      classify <- function(case, segment = segment.data$id2[1]) {
        obs <- case.data[case.data$case == case, c("x.proj", "y.proj")]
        seg.data <- segment.data[segment.data$id2 == segment,
          c("x1", "y1", "x2", "y2")]

        seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                             y = c(seg.data$y1, seg.data$y2))

        distB <- stats::dist(rbind(seg.df[1, ], c(obs$x.proj, obs$y.proj))) +
                 stats::dist(rbind(seg.df[2, ], c(obs$x.proj, obs$y.proj)))

        signif(stats::dist(seg.df)) == signif(distB)
      }

      s3 <- unique(sim.proj3$road.segment)
      s3 <- s3[s3 %in% "44-1" == FALSE]

      for (s in s3) {
        case.data <- sim.proj3[sim.proj3$road.segment == s, ]
        segment.data <- split.missing[split.missing$id == s, ]

        classify.first <- vapply(case.data$case, function(x) {
          classify(x)
        }, logical(1L))

        classify.last <- vapply(case.data$case, function(x) {
          classify(x, segment.data$id2[2])
        }, logical(1L))

        case.data[classify.first, "pump"] <- segment.data[1, "pump"]
        case.data[classify.last, "pump"] <- segment.data[2, "pump"]
        sim.proj[sim.proj$case %in% case.data$case, "pump"] <- case.data$pump
      }

      sim.proj <- sim.proj[sim.proj$road.segment %in% falconberg == FALSE, ]
      sim.proj[sim.proj$road.segment == "44-1", "pump"] <- 2
      sim.proj <- sim.proj[!is.na(sim.proj$pump), ]

      color <- cholera::snowColors()
      sim.proj$color <- paste0("p", sim.proj$pump)

      for (nm in names(color)) {
        sim.proj[sim.proj$color == nm, "color"] <- color[names(color) == nm]
      }
    }
  }

  # Plot #

  n.sel <- as.numeric(names(x$paths))
  snow.colors <- cholera::snowColors()[n.sel]

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  road.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  plot(cholera::fatalities[, c("x", "y")], xlim = x.range, ylim = y.range,
    pch = NA, asp = 1)

  if (area) {
    sel <- as.numeric(row.names(cholera::regular.cases)) %in% sim.proj$case

    points(cholera::regular.cases[sel, ], col = sim.proj$color, pch = 15,
      cex = 1.25)
    invisible(lapply(road.list, lines))
    invisible(lapply(border.list, lines))

    if (is.null(x$pump.select)) {
      if (x$vestry) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 24, bg = "white",
          col = cholera::snowColors(vestry = TRUE))
        text(cholera::pumps.vestry[, c("x", "y")], pos = 1, cex = 0.9,
          col = "white", labels = paste0("p", cholera::pumps.vestry$id))
      } else {
        points(cholera::pumps[, c("x", "y")], pch = 24, bg = "white",
          col = cholera::snowColors())
        text(cholera::pumps[, c("x", "y")], pos = 1, cex = 0.9,
          col = "white", labels = paste0("p", cholera::pumps$id))
      }
    } else {
      if (x$vestry) {
        points(cholera::pumps.vestry[n.sel, c("x", "y")], pch = 24,
          bg = "white", col = snow.colors)
        text(cholera::pumps.vestry[n.sel, c("x", "y")], pos = 1, cex = 0.9,
          col = "white", labels = paste0("p", cholera::pumps.vestry$id[n.sel]))
      } else {
        points(cholera::pumps[n.sel, c("x", "y")], pch = 24, bg = "white",
          col = snow.colors)
        text(cholera::pumps[n.sel, c("x", "y")], pos = 1, cex = 0.9,
          col = "white", labels = paste0("p", cholera::pumps$id[n.sel]))
      }
    }
  } else {
    invisible(lapply(road.list, lines, col = "gray"))
    invisible(lapply(border.list, lines))

    invisible(lapply(seq_along(edge.data), function(i) {
      n.edges <- edges[edge.data[[i]], ]
      segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 2,
       col = snow.colors[i])
    }))

    if (x$observed == FALSE) {
      invisible(lapply(seq_along(whole.missing.segments), function(i) {
        dat <- cholera::road.segments[cholera::road.segments$id ==
          whole.missing.segments[i], ]
        color <- snow.colors[names(snow.colors) %in%
          paste0("p", whole.missing.pumps[i])]
        segments(dat$x1, dat$y1, dat$x2, dat$y2, lwd = 2, col = color)
      }))

      invisible(lapply(split.missing.segments, function(dat) {
        colors <- vapply(dat$pump, function(x) {
          snow.colors[names(snow.colors) == paste0("p", x)]
        }, character(1L))

        segments(dat$x1[1], dat$y1[1], dat$x2[1], dat$y2[1], lwd = 2,
           col = colors[1])
        segments(dat$x1[2], dat$y1[2], dat$x2[2], dat$y2[2], lwd = 2,
          col = colors[2])
      }))
    }

    if (x$case.set == "snow") {
      portland.mews <- which(edges$id == "160-4")
      ship.yard <- which(edges$id %in% c("163-1", "163-2"))
      tylers.court <- which(edges$id == "221-1")
      maidenheard.court <- which(edges$id == "244-1")
      cock.court <- which(edges$id == "225-1")
      hopkins.street <- which(edges$id %in% c("245-2", "265-1", "265-2"))
      unknownB <- which(edges$id == "263-1")
      duck.ham <- which(edges$id %in% paste0(198, "-", 2:4))

      dufours.place <- which(edges$id2 == "217-2c")
      silver.street <- which(edges$id2 == "275-1a")
      pulteney.court1 <- which(edges$id2 == "242-1h")
      new.husband.street <- which(edges$id2 == "259-1d")
      st.anns.place <- which(edges$id2 == "138-1a")
      hopkins.street.sub <- which(edges$id2 == "245-1c")
      kemps.court <- which(edges$id2 == "196-1d")

      whole.segs <-  c(portland.mews, ship.yard, tylers.court,
                       maidenheard.court, cock.court, hopkins.street, unknownB,
                       duck.ham)

      sub.segs <- c(dufours.place, silver.street, pulteney.court1,
                    new.husband.street, st.anns.place, hopkins.street.sub,
                    kemps.court)

      invisible(lapply(c(whole.segs, sub.segs), function(x) {
        n.edges <- edges[x, ]
        segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = 2,
                 col = snow.colors)

      }))
    }

    if (x$case.set == "observed") {
      invisible(lapply(seq_along(n.sel), function(i) {
        points(cholera::fatalities.address[x$cases[[i]], c("x", "y")],
               pch = 20, cex = 0.75, col = snow.colors[i])
      }))
    }

    if (is.null(x$pump.select)) {
      if (x$vestry) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 24,
          col = cholera::snowColors(vestry = TRUE))
        text(cholera::pumps.vestry[, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", cholera::pumps.vestry$id))
      } else {
        points(cholera::pumps[, c("x", "y")], pch = 24,
          col = cholera::snowColors())
        text(cholera::pumps[, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", cholera::pumps$id))
      }
    } else {
      if (x$vestry) {
        points(cholera::pumps.vestry[n.sel, c("x", "y")], pch = 24,
          col = snow.colors)
        text(cholera::pumps.vestry[n.sel, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", cholera::pumps.vestry$id[n.sel]))
      } else {
        points(cholera::pumps[n.sel, c("x", "y")], pch = 24,
          col = snow.colors)
        text(cholera::pumps[n.sel, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", cholera::pumps$id[n.sel]))
      }
    }
  }

  title(main = "Pump Neighborhoods: Walking")
}

#' Compute walking path from anchor cases to nearest pump (or from among selected pumps).
#'
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @param observed Logical. Observed or expected walking path pump neighborhoods.
#' @param cores Numeric. The number logical cores (truncates with as.integer()). Default is 1, which is the only possible value for Windows.
#' @export
#' @return A R list of vectors of nodes.

nearestPath <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE,
  observed = TRUE, cores = 1L) {

  dat <- neighborhoodData(vestry)
  path.data <- pathData(dat, weighted, observed, cores)
  distances <- path.data$distances
  paths <- path.data$paths
  nodes.pump <- dat$nodes.pump

  if (is.null(pump.select)) {
    nearest <- vapply(distances, function(x) names(which.min(x)), character(1L))
  } else {
    if (all(pump.select > 0)) {
      nearest <- vapply(distances, function(x) {
        candidates <- x[names(x) %in% pump.select]
        sel <- which.min(candidates)
        names(sel)
      }, character(1L))
    } else if (all(pump.select < 0)) {
      nearest <- vapply(distances, function(x) {
        candidates <- x[names(x) %in% abs(pump.select) == FALSE]
        sel <- which.min(candidates)
        names(sel)
      }, character(1L))
    }
  }

  lapply(seq_along(paths), function(i) {
    out <- names(paths[[i]][[nearest[i]]])
  })
}

#' Compute walking distance from anchor cases to nearest pump (or from among selected pumps).
#'
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @param observed Logical. Observed or expected walking path pump neighborhoods.
#' @param cores  Numeric. The number logical cores (truncates with as.integer()). Default is 1, which is the only possible value for Windows.
#' @export
#' @return An R data frame.

nearestPump <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE,
  observed = TRUE, cores = 1L) {

  dat <- neighborhoodData(vestry)
  path.data <- pathData(dat, weighted, observed, cores)
  distances <- path.data$distances
  nodes.pump <- dat$nodes.pump

  if (is.null(pump.select)) {
    dat <- lapply(distances, function(x) {
      data.frame(pump = as.numeric(names(which.min(x))),
                 distance = x[which.min(x)])
    })

  } else {
    if (all(pump.select > 0)) {
      dat <- lapply(distances, function(x) {
        candidates <- x[names(x) %in% pump.select]
        dat <- candidates[which.min(candidates)]
        data.frame(pump = as.numeric(names(dat)),
                   distance = dat)
      })
    } else if (all(pump.select < 0)) {
      dat <- lapply(distances, function(x) {
        candidates <- x[names(x) %in% abs(pump.select) == FALSE]
        dat <- candidates[which.min(candidates)]
        data.frame(pump = as.numeric(names(dat)),
                   distance = dat)
      })
    }
  }

  if (observed) {
    out <- data.frame(anchor = cholera::fatalities.address$anchor.case,
      do.call(rbind, dat), row.names = NULL)
  } else {
    out <- data.frame(anchor = seq_along(dat), do.call(rbind, dat),
      row.names = NULL)
  }

  out$pump.name <- NA

  if (vestry) {
    for (p in unique(out$pump)) {
      out[out$pump == p, "pump.name"] <-
        cholera::pumps.vestry[cholera::pumps.vestry$id == p, "street"]
    }
  } else {
    for (p in unique(out$pump)) {
      out[out$pump == p, "pump.name"] <-
        cholera::pumps[cholera::pumps$id == p, "street"]
    }
  }

  out[, c("anchor", "pump", "pump.name", "distance")]
}

neighborhoodData <- function(vestry = FALSE) {
  if (vestry) {
    node.data <- cholera::nodeData(vestry = TRUE)
  } else {
    node.data <- cholera::nodeData()
  }

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g
  nodes.pump <- nodes[nodes$pump != 0, ]
  nodes.pump <- nodes.pump[order(nodes.pump$pump), c("pump", "node")]
  nodes.pump <- nodes.pump[nodes.pump$pump != 2, ] # P2 is a technical isolate
  list(g = g, nodes = nodes, edges = edges, nodes.pump = nodes.pump)
}

pathData <- function(dat, weighted, observed, cores) {
  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges
  nodes.pump <- dat$nodes.pump

  if (observed) {
    anchor <- cholera::fatalities.address$anchor.case

    paths <- parallel::mclapply(anchor, function(x) {
      case.node <- nodes[nodes$anchor == x, "node"]
      if (weighted) {
        stats::setNames(igraph::shortest_paths(g, case.node, nodes.pump$node,
          weights = edges$d)$vpath, nodes.pump$pump)
      } else {
        stats::setNames(igraph::shortest_paths(g, case.node,
          nodes.pump$node)$vpath, nodes.pump$pump)
      }
    }, mc.cores = cores)

    distances <- parallel::mclapply(anchor, function(x) {
      case.node <- nodes[nodes$anchor == x, "node"]
      if (weighted) {
        stats::setNames(c(igraph::distances(g, case.node, nodes.pump$node,
          weights = edges$d)), nodes.pump$pump)
      } else {
        stats::setNames(c(igraph::distances(g, case.node, nodes.pump$node)),
          nodes.pump$pump)
      }
    }, mc.cores = cores)

    list(distances = distances, paths = paths)
  } else {
    road.nodes <- nodes[nodes$anchor == 0 & nodes$pump == 0, ]

    AE <- cholera::road.segments[cholera::road.segments$name ==
      "Adam and Eve Court", ]
    FC <- cholera::road.segments[cholera::road.segments$name ==
      "Falconberg Court", ]
    FM <- cholera::road.segments[cholera::road.segments$name ==
      "Falconberg Mews", ]

    ep1 <- which(road.nodes$x.proj == AE$x1 & road.nodes$y.proj == AE$y1)
    ep2 <- which(road.nodes$x.proj == AE$x2 & road.nodes$y.proj == AE$y2)
    ep3 <- which(road.nodes$x.proj == FC$x1 & road.nodes$y.proj == FC$y1)
    ep4 <- which(road.nodes$x.proj == FC$x2 & road.nodes$y.proj == FC$y2)

    ep5 <- vapply(seq_len(nrow(FM)), function(i) {
      which(road.nodes$x.proj == FM$x1[i] & road.nodes$y.proj == FM$y1[i])
    }, numeric(1L))

    ep6 <- vapply(seq_len(nrow(FM)), function(i) {
      which(road.nodes$x.proj == FM$x2[i] & road.nodes$y.proj == FM$y2[i])
    }, numeric(1L))

    exclude <- unique(c(ep1, ep2, ep3, ep4, ep5, ep6))
    road.nodes <- road.nodes[-exclude, "node"]

    distances <- parallel::mclapply(road.nodes, function(x) {
      if (weighted) {
        stats::setNames(c(igraph::distances(g, x, nodes.pump$node,
          weights = edges$d)), nodes.pump$pump)
      } else {
        stats::setNames(c(igraph::distances(g, x, nodes.pump$node)),
          nodes.pump$pump)
      }
    }, mc.cores = cores)

    paths <- parallel::mclapply(seq_along(road.nodes), function(i) {
      if (weighted) {
        stats::setNames(igraph::shortest_paths(g, road.nodes[i],
          nodes.pump$node, weights = edges$d)$vpath, nodes.pump$pump)
      } else {
        stats::setNames(igraph::shortest_paths(g, road.nodes[i],
          nodes.pump$node)$vpath, nodes.pump$pump)
      }
    }, mc.cores = cores)

    list(distances = distances, paths = paths)
  }
}
