#' Compute walking path neighborhoods.
#'
#' Data for walking neighborhoods for John Snow's 1854 London cholera data.
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param statistic Character. "address" computes the number of addresses in each selected pump neighborhood. "fatality" computes the number of fatalities in pump neighborhoods.
#' @param weighted Logical. TRUE uses distance weighted by edge length (i.e., road length). FALSE uses unweighted distance.
#' @param snow Logical. TRUE computes Snow's Broad Street pump neighborhood. Note: this sets "pump.select" to 7 and "vestry" to NULL.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. With Numeric, you specify the number logical cores (rounds with as.integer()). On Windows, only "multi.core = FALSE" is available.
#' @return An R list with 12 objects:
#' \itemize{
#'   \item{\code{distances}: walking distances to selected pumps.}
#'   \item{\code{pump}: vector of selected pumps.}
#'   \item{\code{nearest.pump}: an observed fatality's nearest pump.}
#'   \item{\code{pump.seg}: "trimmed" neighborhood road segments based on observed fatalities.}
#'   \item{\code{pump.case}: observed fatality IDs by pump neighborhood.}
#'   \item{\code{sim.pump.seg}:  "trimmed" neighborhood road segments based on simulated fatalities.}
#'   \item{\code{sim.pump.case}: simulated fatality ID by pump neighborhood.}
#'   \item{\code{observed}: observed neighborhood fatality counts.}
#'   \item{\code{expected}: expected neighborhood fatality counts, based on road length.}
#'   \item{\code{pump.select}: "pump.select" from neighborhoodWalking().}
#'   \item{\code{statistic}: "statistic" from neighborhoodWalking().}
#'   \item{\code{vestry}: "vestry" from neighborhoodWalking().}
#' }
#' @section Notes: This function is computationally intensive (the default configuration takes about 1-2 minutes to run on a single core). However, seven configurations will return pre-computed results. The first three use the 13 pumps in the original map: 1) the default set of arguments, which uses all pumps; 2) the default set excluding the pump on Little Marlborough Street (pump 6), and 3) the default set with just the Little Marlborough Street and the Broad Street pumps (6 and 7). The next three use the same set of arguments as above but uses the 14 pumps in the second version from the Vestry report. This includes a repositioned Broad Street pump. The seventh and final is Snow's Broad Street pump neighborhood, which represents his graphical annotation of the version of the map that appeared in the Vestry report.
#' @seealso \code{\link{plot.walking}}, \code{\link{summary.walking}}, \code{vignette("pump.neighborhoods")}
#' @export
#' @examples
#' neighborhoodWalking()
#' neighborhoodWalking(pump.select = -6)

neighborhoodWalking <- function(pump.select = NULL, vestry = FALSE,
  statistic = "address", weighted = TRUE, snow = FALSE, multi.core = FALSE) {

  if (all(statistic %in% c("address", "fatality")) == FALSE) {
    stop('"statistic" must either be "address" or "fatality".')
  }

  if (is.null(pump.select) == FALSE) {
    if (length(pump.select) == 1) {
      if (pump.select == 2) {
        msg1 <- "You can't just select the pump on Adam and Eve Court (#2).\n"
        msg2 <- " It's an isolate, unreachable for observed fatalities."
        stop(paste(msg1, msg2))
      }
    }
  }

  test1 <- is.null(pump.select) &
           vestry == FALSE &
           statistic == "address" &
           weighted &
           snow == FALSE &
           multi.core == FALSE
  test2 <- length(pump.select) == 1 & -6 %in% pump.select &
           vestry == FALSE &
           statistic == "address" &
           weighted &
           snow == FALSE &
           multi.core == FALSE
  test3 <- length(pump.select) == 2 & all(6:7 %in% pump.select) &
           vestry == FALSE &
           statistic == "address" &
           weighted &
           snow == FALSE &
           multi.core == FALSE
  test4 <- is.null(pump.select) &
           vestry &
           statistic == "address" &
           weighted &
           snow == FALSE &
           multi.core == FALSE
  test5 <- length(pump.select) == 1 & -6 %in% pump.select &
           vestry &
           statistic == "address" &
           weighted &
           snow == FALSE &
           multi.core == FALSE
  test6 <- length(pump.select) == 2 & all(6:7 %in% pump.select) &
           vestry &
           statistic == "address" &
           weighted &
           snow == FALSE &
           multi.core == FALSE
  test7 <- is.null(pump.select) &
           vestry == FALSE &
           statistic == "address" &
           weighted &
           snow == TRUE &
           multi.core == FALSE

  if (test1) {
    output <- sysdata[["address"]]
  } else if (test2) {
    output <- sysdata[["address.not6"]]
  } else if (test3) {
    output <- sysdata[["address67"]]
  } else if (test4) {
    output <- sysdata[["vestry"]]
  } else if (test5) {
    output <- sysdata[["vestry67"]]
  } else if (test6) {
    output <- sysdata[["vestry.not6"]]
  } else if (test7) {
    output <- sysdata[["snow"]]
  } else {
    if (vestry) {
      if (is.null(pump.select) == FALSE) {
        if (any(abs(pump.select) %in% 1:14 == FALSE)) {
          stop('With "vestry = TRUE", 1 >= |"pump.select"| <= 14.')
        }
      }
    } else {
      if (is.null(pump.select) == FALSE ) {
        if (any(abs(pump.select) %in% 1:13 == FALSE)) {
          stop('With "vestry = FALSE", 1 >= |"pump.select"| <= 13.')
        }
      }
    }

    if (snow) {
      pump.select <- 7
      vestry <- FALSE
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

    # pumps #

    if (vestry) {
      pump.coordinates <- pumpCoordinates(vestry = TRUE)
    } else {
      pump.coordinates <- pumpCoordinates()
    }

    if (is.null(pump.select)) {
      if (vestry) {
        pump.road.segments <- pumpIntegrator(cholera::ortho.proj.pump.vestry)
      } else {
        pump.road.segments <- pumpIntegrator()
      }
    } else {
      if (vestry) {
        pump.road.segments <-
          pumpIntegrator(cholera::ortho.proj.pump.vestry[pump.select, ])
      } else {
        pump.road.segments <-
          pumpIntegrator(cholera::ortho.proj.pump[pump.select, ])
      }
    }

    if (is.null(pump.select)) {
      select.pumps <- pump.coordinates
    } else {
      select.pumps <- pump.coordinates[pump.select]
    }

    pump.names <- names(select.pumps)

    # cases #

    if (snow) {
      if (statistic == "address") {
        sel <- cholera::snow.neighborhood[cholera::snow.neighborhood %in%
          cholera::fatalities.address$anchor.case]
        ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
      } else if (statistic == "fatality") {
        sel <- cholera::snow.neighborhood
        ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
      }
    } else {
      if (statistic == "address") {
        sel <- cholera::fatalities.address$anchor.case
        ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
      } else if (statistic == "fatality") {
        sel <- cholera::fatalities.unstacked$case
        ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
      }
    }

    neighborhoods <- trimPaths(expected = FALSE, pump.road.segments,
      select.pumps, pump.names, ortho, vestry, weighted, cores)

    sim.neighborhoods <- trimPaths(expected = TRUE, pump.road.segments,
      select.pumps, pump.names, ortho, vestry, weighted, cores)

    sim.road.length <- roadLength(sim.neighborhoods$trimmed.segments)

    expected <- sum(neighborhoods$observed$count) * sim.road.length /
      sum(sim.road.length)

    expected <- data.frame(pump = names(expected), count = expected,
      stringsAsFactors = FALSE)
    rownames(expected) <- NULL

    output <- list(
      distances = neighborhoods$nearest.pump.data,
      pump = pump.names,
      nearest.pump = neighborhoods$nearest.pump,
      pump.seg = neighborhoods$trimmed.segments,
      pump.case = neighborhoods$pump.cases,
      sim.pump.seg = sim.neighborhoods$trimmed.segments,
      sim.pump.case = sim.neighborhoods$pump.cases,
      observed = neighborhoods$observed,
      expected = expected,
      pump.select = pump.select,
      statistic = statistic,
      vestry = vestry)

    class(output) <- "walking"
  }
  output
}

#' Plot observed and simulated walking neighborhoods.
#'
#' Neighborhoods are based on the shortest paths between a fatality's address and its nearest pump.
#'
#' @param x An object of class "walking" created by neighborhoodWalking().
#' @param streets Logical. TRUE plots neighborhoods by street. FALSE plots orthogonal neighborhoods (area).
#' @param observed Logical. TRUE uses observed cases. FALSE uses "regular" simulated cases.
#' @param ... Additional plotting parameters.
#' @return A base R graphics plot.
#' @export
#' @examples
#' plot(neighborhoodWalking())

plot.walking <- function(x, streets = TRUE, observed = TRUE, ...) {
  if (class(x) != "walking") {
    stop('Input object\'s class needs to be "walking".')
  }

  if (is.null(x$pump.select)) {
    if (x$vestry) {
      pump.select <- 1:14
    } else {
      pump.select <- 1:13
    }
  } else {
    if (all(x$pump.select < 0)) {
      if (x$vestry) {
        pump.select <- (1:14)[x$pump.select]
      } else {
        pump.select <- (1:13)[x$pump.select]
      }
    } else {
      pump.select <- x$pump.select
    }
  }

  if (x$vestry == TRUE) {
    snow.colors <- cholera::snowColors(vestry = TRUE)
  } else {
    snow.colors <- cholera::snowColors()
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  observed.pump <- vapply(x$pump.case, function(p) length(p) != 0, logical(1L))
  observed.pump <- names(observed.pump[observed.pump])

  plot(cholera::fatalities[, c("x", "y")], xlim = x.range, ylim =  y.range,
    pch = NA, asp = 1)

  if (streets) {
    invisible(lapply(roads.list, lines, col = "gray"))
    invisible(lapply(border.list, lines))

    if (observed) {
      if (length(x$pump) == length(observed.pump)) {
        invisible(lapply(observed.pump, function(nm) {
          plotSegment(x$pump.seg[[nm]], snow.colors[nm])

          if (is.null(x$statistic) | x$statistic == "address") {
            sel <- cholera::fatalities.address$anchor.case %in%
              x$pump.case[[nm]]
            points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
              cex = 0.75, col = snow.colors[nm])
          } else if (x$statistic == "fatality") {
            sel <- x$pump.case[[nm]]
            points(cholera::fatalities[sel, c("x", "y")], pch = 20, cex = 0.75,
              col = snow.colors[nm])
          }
        }))

        if (is.null(x$pump.select)) {
          if (x$vestry) {
            points(cholera::pumps.vestry[, c("x", "y")], pch = 24,
              col = snow.colors)
            text(cholera::pumps.vestry[, c("x", "y")], cex = 0.9, pos = 1,
              label = x$pump)
          } else {
            points(cholera::pumps[, c("x", "y")], pch = 24, col = snow.colors)
            text(cholera::pumps[, c("x", "y")], cex = 0.9, pos = 1,
              label = x$pump)
          }
          title(main = "Observed Walking Paths")
        } else {
          if (x$vestry) {
            points(cholera::pumps.vestry[pump.select, c("x", "y")], pch = 24,
              col = snow.colors[pump.select])
            text(cholera::pumps.vestry[pump.select, c("x", "y")], cex = 0.9,
              pos = 1, label = x$pump)
          } else {
            points(cholera::pumps[pump.select, c("x", "y")], pch = 24,
              col = snow.colors[pump.select])
            text(cholera::pumps[pump.select, c("x", "y")], cex = 0.9, pos = 1,
              label = x$pump)
          }

          if (length(x$pump) > 1) {
            title(main = paste0("Observed Walking Paths", "\n", "Pumps ",
              paste(sort(x$pump.select), collapse = ", ")))
          } else {
            title(main = paste0("Observed Walking Paths", "\n", "Pump ",
              paste(sort(x$pump.select), collapse = ", ")))
          }
        }
      } else {
        invisible(lapply(observed.pump, function(nm) {
          plotSegment(x$pump.seg[[nm]], snow.colors[nm])

          if (is.null(x$statistic) | x$statistic == "address") {
            sel <- cholera::fatalities.address$anchor.case %in%
              x$pump.case[[nm]]
            points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
              cex = 0.75, col = snow.colors[nm])
          } else if (x$statistic == "fatality") {
            sel <- x$pump.case[[nm]]
            points(cholera::fatalities[sel, c("x", "y")], pch = 20, cex = 0.75,
              col = snow.colors[nm])
          }
        }))

        if (is.null(x$pump.select)) {
          if (x$vestry) {
            points(cholera::pumps.vestry[, c("x", "y")], pch = 24,
              col = snow.colors)
            text(cholera::pumps.vestry[, c("x", "y")], cex = 0.9, pos = 1,
              label = x$pump)
          } else {
            points(cholera::pumps[, c("x", "y")], pch = 24, col = snow.colors)
            text(cholera::pumps[, c("x", "y")], cex = 0.9, pos = 1,
              label = x$pump)
          }
          title(main = "Observed Walking Paths")
        } else {
          if (x$vestry) {
            points(cholera::pumps.vestry[pump.select, c("x", "y")], pch = 24,
              col = snow.colors[pump.select])
            text(cholera::pumps.vestry[pump.select, c("x", "y")], cex = 0.9,
              pos = 1, label = x$pump)
          } else {
            points(cholera::pumps[pump.select, c("x", "y")], pch = 24,
              col = snow.colors[pump.select])
            text(cholera::pumps[pump.select, c("x", "y")], cex = 0.9, pos = 1,
              label = x$pump)
          }

          if (length(x$pump) > 1) {
            title(main = paste0("Observed Walking Paths", "\n", "Pumps ",
              paste(sort(x$pump.select), collapse = ", ")))
          } else {
            title(main = paste0("Observed Walking Paths", "\n", "Pump ",
              paste(sort(x$pump.select), collapse = ", ")))
          }
        }
      }
    } else {
      invisible(lapply(x$pump, function(nm) {
        plotSegment(x$sim.pump.seg[[nm]], snow.colors[nm])
      }))

      if (is.null(x$pump.select)) {
        if (x$vestry) {
          points(cholera::pumps.vestry[, c("x", "y")], pch = 24,
            col = snow.colors)
          text(cholera::pumps.vestry[, c("x", "y")], cex = 0.9, pos = 1,
            label = x$pump)
        } else {
          points(cholera::pumps[, c("x", "y")], pch = 24, col = snow.colors)
          text(cholera::pumps[, c("x", "y")], cex = 0.9, pos = 1,
            label = x$pump)
        }
        title(main = "Expected Walking Paths")
      } else {
        if (x$vestry) {
          points(cholera::pumps.vestry[pump.select, c("x", "y")], pch = 24,
            col = snow.colors[pump.select])
          text(cholera::pumps.vestry[pump.select, c("x", "y")], cex = 0.9,
            pos = 1, label = x$pump)
        } else {
          points(cholera::pumps[pump.select, c("x", "y")], pch = 24,
            col = snow.colors[pump.select])
          text(cholera::pumps[pump.select, c("x", "y")], cex = 0.9, pos = 1,
            label = x$pump)
        }

        if (length(x$pump) > 1) {
          title(main = paste0("Expected Walking Paths", "\n", "Pumps ",
            paste(sort(x$pump.select), collapse = ", ")))
        } else {
          title(main = paste0("Expected Walking Paths", "\n", "Pump ",
            paste(sort(x$pump.select), collapse = ", ")))
        }
      }
    }
  } else {
    if (observed) {
      invisible(lapply(roads.list, lines, col = "gray"))
      invisible(lapply(border.list, lines))

      if (x$vestry) {
        invisible(lapply(seq_along(cholera::pumps.vestry$id), function(i) {
          points(cholera::regular.cases[x$sim.pump.case[[i]], ],
            col = scales::alpha(snow.colors[i], 0.33), pch = 15)
        }))
      } else {
        invisible(lapply(seq_along(cholera::pumps$id), function(i) {
          points(cholera::regular.cases[x$sim.pump.case[[i]], ],
            col = scales::alpha(snow.colors[i], 0.33), pch = 15)
        }))
      }

      invisible(lapply(observed.pump, function(nm) {
        plotSegment(x$pump.seg[[nm]], snow.colors[nm])
        sel <- cholera::fatalities.address$anchor.case %in% x$pump.case[[nm]]
        points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
          cex = 0.75, col = snow.colors[nm])
      }))

      if (is.null(x$pump.select)) {
        if (x$vestry) {
          points(cholera::pumps.vestry[, c("x", "y")], pch = 24,
            col = snow.colors)
          text(cholera::pumps.vestry[, c("x", "y")], cex = 0.9, pos = 1,
            label = x$pump)
        } else {
          points(cholera::pumps[, c("x", "y")], pch = 24, col = snow.colors)
          text(cholera::pumps[, c("x", "y")], cex = 0.9, pos = 1,
            label = x$pump)
        }
        title(main = "Observed Walking Neighborhoods and Paths")
      } else {
        if (x$vestry) {
          points(cholera::pumps.vestry[pump.select, c("x", "y")], pch = 24,
            col = snow.colors[pump.select])
          text(cholera::pumps.vestry[pump.select, c("x", "y")], cex = 0.9,
            pos = 1, label = x$pump)
        } else {
          points(cholera::pumps[pump.select, c("x", "y")], pch = 24,
            col = snow.colors[pump.select])
          text(cholera::pumps[pump.select, c("x", "y")], cex = 0.9, pos = 1,
            label = x$pump)
        }

        if (length(x$pump) > 1) {
          title(main = paste0("Observed Walking Neighborhoods and Paths", "\n",
            "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
        } else {
          title(main = paste0("Observed Walking Neighborhoods and Paths", "\n",
            "Pump ", paste(sort(x$pump.select), collapse = ", ")))
        }
      }
    } else {
      if (x$vestry) {
        invisible(lapply(seq_along(cholera::pumps.vestry$id), function(i) {
          points(cholera::regular.cases[x$sim.pump.case[[i]], ],
            col = snow.colors[i], pch = 15)
        }))
      } else {
        invisible(lapply(seq_along(cholera::pumps$id), function(i) {
          points(cholera::regular.cases[x$sim.pump.case[[i]], ],
            col = snow.colors[i], pch = 15)
        }))
      }

      invisible(lapply(roads.list, lines))
      invisible(lapply(border.list, lines))

      if (is.null(x$pump.select)) {
        if (x$vestry) {
          points(cholera::pumps.vestry[, c("x", "y")], pch = 24, bg = "white")
          text(cholera::pumps.vestry[, c("x", "y")], cex = 0.9, pos = 1,
            col = "white", label = x$pump)
        } else {
          points(cholera::pumps[, c("x", "y")], pch = 24, bg = "white")
          text(cholera::pumps[, c("x", "y")], cex = 0.9, pos = 1, col = "white",
            label = x$pump)
        }
        title(main = "Expected Walking Neighborhoods")
      } else {
        if (x$vestry) {
          points(cholera::pumps.vestry[pump.select, c("x", "y")], pch = 24,
            bg = "white")
          text(cholera::pumps.vestry[pump.select, c("x", "y")], cex = 0.9,
            pos = 1, col = "white", label = x$pump)
        } else {
          points(cholera::pumps[pump.select, c("x", "y")], bg = "white",
            pch = 24)
          text(cholera::pumps[pump.select, c("x", "y")], cex = 0.9, pos = 1,
            col = "white", label = x$pump)
        }

        if (length(x$pump) > 1) {
          title(main = paste0("Observed Walking Neighborhoods", "\n",
            "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
        } else {
          title(main = paste0("Observed Walking Neighborhoods", "\n", "Pump ",
            paste(sort(x$pump.select), collapse = ", ")))
        }
      }
    }
  }
}

#' Compute summary statistics for walking path neighborhoods.
#'
#' @param object An object of class "walking" created by neighborhoodWalking().
#' @param ... Additional arguments.
#' @return A data frame with observed and expected counts, observed percentage, and the Pearson residual, (observed - expected) / sqrt(expected).
#' @export
#' @examples
#' summary(neighborhoodWalking())

summary.walking <- function(object, ...) {
  if (class(object) != "walking") {
    stop('Input object\'s class needs to be "walking".')
  }

  obs <- object$observed
  exp <- object$expected
  output <- merge(obs, exp, by = "pump", all.y = TRUE)
  names(output)[-1] <- c("Count", "Expected")
  output[is.na(output)] <- 0
  output$Percent <- round(100 * output$Count / sum(output$Count), 2)
  output <- output[, c("pump", "Count", "Percent", "Expected")]
  output$Pearson <- (output$Count - output$Expected) / sqrt(output$Expected)
  output <- output[order(pumpNumber(output$pump)), ]
  rownames(output) <- NULL
  output
}

pumpCoordinates <- function(vestry = FALSE) {
  if (vestry) {
    coordinates <- paste0(cholera::ortho.proj.pump.vestry$x.proj, "-",
                          cholera::ortho.proj.pump.vestry$y.proj)
    names(coordinates) <- paste0("p", seq_along(coordinates))
  } else {
    coordinates <- paste0(cholera::ortho.proj.pump$x.proj, "-",
                          cholera::ortho.proj.pump$y.proj)
    names(coordinates) <- paste0("p", seq_along(coordinates))
  }
  coordinates
}

pumpIntegrator <- function(pump.data = cholera::ortho.proj.pump,
  road.segments = cholera::road.segments) {

  pump.segments <- pump.data$road.segment
  mat <- matrix(0, ncol = ncol(road.segments), nrow = 2 * length(pump.segments))
  road.pump.data <- data.frame(mat)
  start.pt <- seq(1, nrow(road.pump.data), 2)
  end.pt <- seq(2, nrow(road.pump.data), 2)

  for (i in seq_along(pump.segments)) {
    road.data <- road.segments[road.segments$id == pump.segments[i], ]
    pump.coords <- pump.data[pump.data$road.segment == pump.segments[i],
                             c("x.proj", "y.proj")]
    temp <- road.data[, names(road.data) %in% c("x1", "y1") == FALSE]
    temp <- cbind(temp[, c("street", "id", "name")],
                  pump.coords,
                  temp[, c("x2", "y2")])
    names(temp)[names(temp) %in% c("x.proj", "y.proj")] <- c("x1", "y1")
    road.data[, c("x2", "y2")] <- pump.coords
    temp <- rbind(road.data, temp)
    temp$id <- paste0(road.data$id, letters[seq_len(nrow(temp))])
    road.pump.data[start.pt[i]:end.pt[i], ] <- temp
  }

  names(road.pump.data) <- names(road.segments)
  road.segments <- road.segments[road.segments$id %in% pump.segments == FALSE, ]
  out <- rbind(road.segments, road.pump.data)
  out <- out[order(out$street, out$id), ]
  out$node1 <- paste0(out$x1, "-", out$y1)
  out$node2 <- paste0(out$x2, "-", out$y2)
  out$d <- sqrt((out$x1 - out$x2)^2 + (out$y1 - out$y2)^2)
  out
}

caseIntegrator <- function(case, pump.road.segments) {
  seg <- unlist(strsplit(pump.road.segments$id, "a"))
  seg <- unlist(strsplit(seg, "b"))
  temp <- pump.road.segments[which(case$road.segment == seg), ]
  case.coord <- case[, c("x.proj", "y.proj")]

  # if case is on street with a well, nrow(temp) > 1
  # id != 1 : 9  12  18 119 138 191 283 317 320
  distance <- vapply(seq_len(nrow(temp)), function(i) {
    sqrt(sum((case.coord - temp[i, c("x1", "y1")])^2)) +
    sqrt(sum((case.coord - temp[i, c("x2", "y2")])^2))
  }, numeric(1L))

  temp <- temp[which(signif(temp$d) == signif(distance)), ]
  appended <- rbind(temp, temp)
  appended[1, c("x2", "y2")] <- case.coord
  appended[2, c("x1", "y1")] <- case.coord

  if (grepl("a", temp$id) | grepl("b", temp$id)) {
    appended$id <- paste0(appended$id, seq_len(nrow(appended)))
  } else {
    appended$id <- paste0(appended$id, letters[seq_len(nrow(appended))])
  }

  appended$node1 <- paste0(appended$x1, "-", appended$y1)
  appended$node2 <- paste0(appended$x2, "-", appended$y2)
  appended$d <- sqrt((appended$x1 - appended$x2)^2 +
                     (appended$y1 - appended$y2)^2)
  road.segments2 <- pump.road.segments
  rbind(road.segments2[road.segments2$id != case$road.segment, ], appended)
}

intermediateSegments <- function(x) {
  path.roads <- lapply(x, function(y) {
    p <- names(unlist(y))
    intermediate.nodes <- p[-c(1, length(p))]
    node.data <- do.call(rbind, strsplit(intermediate.nodes, "-"))
    node.data <- data.frame(x = as.numeric(node.data[, 1]),
                            y = as.numeric(node.data[, 2]))

    lapply(seq_along(node.data$x[-1]), function(i) {
      t1 <- cholera::road.segments[which(cholera::road.segments[, "x1"] %in%
        node.data[i, "x"]), "id"]
      t2 <- cholera::road.segments[which(cholera::road.segments[, "x2"] %in%
        node.data[i, "x"]), "id"]
      t3 <- cholera::road.segments[which(cholera::road.segments[, "x1"] %in%
        node.data[i + 1, "x"]), "id"]
      t4 <- cholera::road.segments[which(cholera::road.segments[, "x2"] %in%
        node.data[i + 1, "x"]), "id"]
      tab <- table(c(t1, t2, t3, t4))
      names(tab[tab > 1])
    })
  })

  cholera::road.segments[cholera::road.segments$id %in%
    unique(unlist(path.roads)), ]
}

caseSegments <- function(x, neighborhood.paths, pump.cases, vestry,
  obs = TRUE) {

  n.paths <- neighborhood.paths[[x]]
  n.cases <- pump.cases[[x]]

  if (obs) {
    obs.seg <- unique(cholera::ortho.proj[cholera::ortho.proj$case %in%
      n.cases, "road.segment"])
  } else {
    obs.seg <- unique(cholera::sim.ortho.proj[cholera::sim.ortho.proj$case %in%
      n.cases, "road.segment"])
  }

  if (vestry) {
    pump.seg <- cholera::ortho.proj.pump.vestry[
      cholera::ortho.proj.pump.vestry$pump.id == pumpNumber(x), "road.segment"]
  } else {
    pump.seg <- cholera::ortho.proj.pump[cholera::ortho.proj.pump$pump.id ==
      pumpNumber(x), "road.segment"]
  }

  new.segs <- lapply(obs.seg, function(seg) {
    st <- cholera::road.segments[cholera::road.segments$id == seg, ]
    st$trimmed <- FALSE

    if (obs) {
      sel <- cholera::ortho.proj$road.segment %in% st$id &
        cholera::ortho.proj$case %in% n.cases
      st.cases <- cholera::ortho.proj[sel, ]
    } else {
      sel <- cholera::sim.ortho.proj$road.segment %in% st$id &
        cholera::sim.ortho.proj$case %in% n.cases
      st.cases <- cholera::sim.ortho.proj[sel, ]
    }

    entry.node <- lapply(st.cases$case, function(x) {
      p <- names(unlist(n.paths[[which(x == n.cases)]]))
      as.numeric(unlist(strsplit(p[2], "-")))
    })

    entry.data <- data.frame(do.call(rbind, entry.node))
    names(entry.data) <- c("x", "y")

    entry1 <- entry.data$x == st$x1 & entry.data$y == st$y1
    entry2 <- entry.data$x == st$x2 & entry.data$y == st$y2

    if (all(entry1)) {
      idx <- which(entry1)
      d <- vapply(idx, function(i) {
        a <- st.cases[i, c("x.proj", "y.proj")]
        names(a) <- c("x", "y")
        b <- data.frame(x = st$x2, y = st$y2)
        stats::dist(rbind(a, b))
      }, numeric(1L))

      st[, c("x2", "y2")] <- st.cases[idx[which.min(d)], c("x.proj", "y.proj")]
      st$trimmed <- TRUE

    } else if (all(entry2)) {
      idx <- which(entry2)
      d <- vapply(idx, function(i) {
        a <- st.cases[i, c("x.proj", "y.proj")]
        names(a) <- c("x", "y")
        b <- data.frame(x = st$x1, y = st$y1)
        stats::dist(rbind(a, b))
      }, numeric(1L))

      st[, c("x1", "y1")] <- st.cases[idx[which.min(d)], c("x.proj", "y.proj")]
      st$trimmed <- TRUE

    } else if (any(entry1) & any(entry2)) {
      st

    } else if (length(entry1) == 1 & length(entry2) == 1) {
      if (entry1 == FALSE & entry2 == FALSE) {
        st <- cbind(st[, c("street", "id", "name")],
          st.cases[, c("x.proj", "y.proj")], entry.data)
        names(st)[4:7] <- c("x1", "y1", "x2", "y2")
        st$trimmed <- TRUE
      }
    }
    st
  })

  do.call(rbind, new.segs)
}

pumpSegments <- function(x, neighborhood.paths, vestry) {
  n.paths <- neighborhood.paths[[x]]

  last.segment <- lapply(n.paths, function(p) {
    p.coords <- names(unlist(p))
    last.nodes <- p.coords[(length(p.coords) - 1):length(p.coords)]
    node.data <- do.call(rbind, strsplit(last.nodes, "-"))
    data.frame(x = as.numeric(node.data[, 1]), y = as.numeric(node.data[, 2]))
  })

  if (vestry) {
    pump.segment.audit <- lapply(last.segment, function(dat) {
      c(which(signif(dat[2, "x"]) ==
          signif(cholera::ortho.proj.pump.vestry$x.proj)),
        which(signif(dat[2, "y"]) ==
          signif(cholera::ortho.proj.pump.vestry$y.proj)))
    })
  } else {
    pump.segment.audit <- lapply(last.segment, function(dat) {
      c(which(signif(dat[2, "x"]) == signif(cholera::ortho.proj.pump$x.proj)),
        which(signif(dat[2, "y"]) == signif(cholera::ortho.proj.pump$y.proj)))
    })
  }

  neighborhood.pump <- unique(unlist(pump.segment.audit))

  if (length(neighborhood.pump) == 1) {
    if (vestry) {
      v.ortho <- cholera::ortho.proj.pump.vestry
      seg <- v.ortho[v.ortho$pump.id == neighborhood.pump, "road.segment"]
    } else {
      seg <- cholera::ortho.proj.pump[cholera::ortho.proj.pump$pump.id ==
        neighborhood.pump, "road.segment"]
    }
    st <- cholera::road.segments[cholera::road.segments$id == seg, ]
  }

  segment.audit <- lapply(last.segment, function(dat) {
    A <- signif(dat[1, "x"]) == signif(st$x1) &
      signif(dat[1, "y"]) == signif(st$y1)
    B <- signif(dat[1, "x"]) == signif(st$x2) &
      signif(dat[1, "y"]) == signif(st$y2)
    data.frame(A, B)
  })

  segment.audit <- do.call(rbind, segment.audit)

  if (any(segment.audit$A) && any(segment.audit$B)) {
    st$trimmed <- FALSE
  } else if (all(segment.audit$A)) {
    st[, c("x2", "y2")] <- last.segment[[1]][2, ]
    st$trimmed <- TRUE
  } else if (all(segment.audit$B)) {
    st[, c("x1", "y1")] <- last.segment[[1]][2, ]
    st$trimmed <- TRUE
  } else if (all(segment.audit$A) == FALSE & all(segment.audit$A) == FALSE) {
    st <- cbind(st[, c("street", "id", "name")], last.segment[[1]][1, ],
                last.segment[[1]][2, ])
    names(st)[4:7] <- c("x1", "y1", "x2", "y2")
    st$trimmed <- TRUE
  }
  st
}

trimPaths <- function(expected = FALSE, pump.road.segments,
  select.pumps, pump.names, ortho, vestry, weighted, cores) {

  if (expected) {
    edge.case <- cholera::sim.ortho.proj[is.na(cholera::sim.ortho.proj$x.proj),
      "case"]
    falconberg.id <- pump.road.segments[pump.road.segments$name ==
      "Falconberg Court" | pump.road.segments$name == "Falconberg Mews", "id"]
    falconberg <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$road.segment
      %in% falconberg.id, "case"]

    if (all(pump.names %in% "p2" == FALSE)) {
      # if pump 2, on Adam and Eve Court, is not included, siumuated cases
      # located on that street cannot reach any other pump.
      adam.eve <- c(4536, 4537, 4538, 4539, 4608, 4609, 4610, 4611, 4612, 4681,
        4682, 4683, 4684, 4753, 4754, 4755, 4756, 4757, 4827, 4828, 4829, 4830,
        4900, 4901, 4902, 4955, 4956)
      ortho <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case
        %in% c(edge.case, falconberg, adam.eve) == FALSE, ]
    } else {
      ortho <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case
        %in% c(edge.case, falconberg) == FALSE, ]
    }
  }

  case <- split(ortho, ortho$case)

  case.pump.road.segments <- parallel::mclapply(case, function(x)
    caseIntegrator(x, pump.road.segments), mc.cores = cores)

  g <- parallel::mclapply(case.pump.road.segments, function(x) {
    edge.list <- x[, c("node1", "node2")]
    igraph::graph_from_data_frame(edge.list, directed = FALSE)
  }, mc.cores = cores)

  case.node <- vapply(names(case), function(nm) {
    case.coord <- paste0(ortho[ortho$case == as.numeric(nm), "x.proj"], "-",
      ortho[ortho$case == as.numeric(nm), "y.proj"])
    which(igraph::V(g[[nm]])$name == case.coord)
  }, numeric(1L))

  pump.nodes <- parallel::mclapply(g, function(graph) {
    vapply(select.pumps, function(pump) {
      which(igraph::V(graph)$name == pump)
    }, numeric(1L))
  }, mc.cores = cores)

  nearest.pump.data <- parallel::mclapply(names(case), function(nm) {
    if (weighted) {
      wts <- case.pump.road.segments[[nm]]$d
      d <- unname(igraph::distances(g[[nm]], case.node[[nm]], pump.nodes[[nm]],
        weights = wts))
    } else {
      d <- unname(igraph::distances(g[[nm]], case.node[[nm]], pump.nodes[[nm]]))
    }
  }, mc.cores = cores)

  nearest.pump.data <- do.call(rbind, nearest.pump.data)
  idx <- apply(nearest.pump.data, 1, which.min)
  nearest.pump <- names(select.pumps)[idx]
  names(nearest.pump) <- names(g)

  obs.pumps <- table(nearest.pump)
  pump.nm <- names(obs.pumps)
  observed <- data.frame(pump = pump.nm, count = unname(c(obs.pumps)),
    stringsAsFactors = FALSE)

  paths <- parallel::mclapply(names(g), function(nm) {
    sel <- names(pump.nodes[[nm]]) == nearest.pump[nm]
    p.node <- pump.nodes[[nm]][sel]
    igraph::shortest_paths(g[[nm]], case.node[nm], p.node,
      weights = case.pump.road.segments[[nm]]$d)$vpath
  }, mc.cores = cores)

  neighborhood.paths <- split(paths, nearest.pump)

  intermediate.segments <- parallel::mclapply(neighborhood.paths,
    intermediateSegments, mc.cores = cores)

  intermediate.segments <- lapply(intermediate.segments, function(x) {
    if (nrow(x) == 0) {
     dat <- NULL
    } else {
     dat <- x
     dat$trimmed <- FALSE
     dat
    }
  })

  if (vestry) {
    pump.cases <- lapply(cholera::pumps.vestry$id, function(i) {
      ortho[which(nearest.pump == paste0("p", i)), "case"]
    })
    names(pump.cases) <- paste0("p", 1:14)
  } else {
    pump.cases <- lapply(cholera::pumps$id, function(i) {
      ortho[which(nearest.pump == paste0("p", i)), "case"]
    })
    names(pump.cases) <- paste0("p", 1:13)
  }

  if (expected) {
    # w/ expected or "simulated" data, set obs = FALSE
    case.segments <- parallel::mclapply(pump.nm, caseSegments,
      neighborhood.paths, pump.cases, vestry, obs = FALSE, mc.cores = cores)
  } else {
    case.segments <- parallel::mclapply(pump.nm, caseSegments,
      neighborhood.paths, pump.cases, vestry, mc.cores = cores)
  }

  names(case.segments) <- pump.nm

  pump.segments <- parallel::mclapply(pump.nm, pumpSegments,
    neighborhood.paths, vestry, mc.cores = cores)
  names(pump.segments) <- pump.nm

  neighborhood.segments <- lapply(pump.nm, function(nm) {
    case.seg <- case.segments[[nm]]
    intermediate.seg <- intermediate.segments[[nm]]
    pump.seg <- pump.segments[[nm]]

    # prioritize "intermediate" segments over "case" segments
    case.seg <- case.seg[case.seg$id %in% intermediate.seg$id == FALSE, ]

    # case on pump segment test
    if (any(case.seg$id %in% pump.seg$id)) {
      id <- intersect(case.seg$id, pump.seg$id)
      actual.seg <- cholera::road.segments[cholera::road.segments$id == id, ]

      # identify and select *.seg that includes an endpoint of actual.seg
      p.test <- pump.seg[, c("x1", "y1")] == actual.seg[, c("x1", "y1")] |
                pump.seg[, c("x1", "y1")] == actual.seg[, c("x2", "y2")] |
                pump.seg[, c("x2", "y2")] == actual.seg[, c("x1", "y1")] |
                pump.seg[, c("x2", "y2")] == actual.seg[, c("x2", "y2")]

      c.test <- case.seg[case.seg$id == id, c("x1", "y1")] ==
                         actual.seg[, c("x1", "y1")] |
                case.seg[case.seg$id == id, c("x1", "y1")] ==
                         actual.seg[, c("x2", "y2")] |
                case.seg[case.seg$id == id, c("x2", "y2")] ==
                         actual.seg[, c("x1", "y1")] |
                case.seg[case.seg$id == id, c("x2", "y2")] ==
                         actual.seg[, c("x2", "y2")]

      if (all(p.test)) {
        case.seg <- case.seg[-which(case.seg$id == id), ]
      } else if (all(c.test)) {
        pump.seg <- pump.seg[-which(pump.seg$id == id), ]
      }
    }

    # unique handles identical case and pump segments
    unique(rbind(case.seg, intermediate.seg, pump.seg))
  })

  names(neighborhood.segments) <- pump.nm
  list(trimmed.segments = neighborhood.segments, pump.cases = pump.cases,
    observed = observed, nearest.pump.data = nearest.pump.data,
    nearest.pump = nearest.pump)
}

roadLength <- function(dat) {
  if (is.data.frame(dat)) {
    sum(sqrt((dat$x1 - dat$x2)^2 + (dat$y1 - dat$y2)^2))
  } else {
    vapply(dat, function(dat) {
      sum(sqrt((dat$x1 - dat$x2)^2 + (dat$y1 - dat$y2)^2))
    }, numeric(1L))
  }
}

plotSegment <- function(neighborhood, color) {
  invisible(lapply(neighborhood$id, function(x) {
    lines(neighborhood[neighborhood$id == x, c("x1", "x2")],
          neighborhood[neighborhood$id == x, c("y1", "y2")],
          col = color, lwd = 2)
  }))
}

pumpNumber <- function (x) as.numeric(substr(x, 2, nchar(x)))
