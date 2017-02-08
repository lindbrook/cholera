#' Walking path neighborhoods.
#'
#' Data for walking neighbohoods for John Snow's 1854 London cholera data.
#' @param selection Numeric. Default is NULL: all pumps are used. Ortherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param statistic Character. "address" computes the number of addresses in each selected pump neighborhood. "fatality" computes the number of fatalities in pump neighborhoods.
#' @param weighted Logical. TRUE uses distance weighted by edge length (i.e., road length). FALSE uses unweighted distance.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. With Numeric, you specify the number logical cores (rounds with as.integer()). On Windows, only "multi.core = FALSE" is available.
#' @return A list of data and parameters of computed walking path neighborhoods.
#' @section Notes: This function is computationally intensive (the default configuration takes about 1-2 minutes to run on a single core). However, two configurations will return pre-computed results: the default set of arguments, and the default set with pump 6 (Little Marlborough Street) excluded: neighborhoodWalking(selection = -6).
#' @export
#' @examples
#' neighborhoodWalking()
#' neighborhoodWalking(selection = -6)

neighborhoodWalking <- function(selection = NULL, vestry = FALSE,
  statistic = "address", weighted = TRUE, multi.core = FALSE) {

  # address <- cholera::neighborhoodWalking(multi.core = TRUE)
  # address.not6 <- cholera::neighborhoodWalking(-6, multi.core = TRUE)
  # address67 <- cholera::neighborhoodWalking(6:7, multi.core = TRUE))
  # walking.data <- list(address = address, address.not6 = address.not6,
  #   address67 = address67)
  # devtools::use_data(walking.data, internal = TRUE, overwrite = TRUE)

  test1 <- (is.null(selection) & vestry == FALSE & statistic == "address" &
    weighted)
  test2 <- (length(selection) == 1 & -6 %in% selection & vestry == FALSE &
    statistic == "address" & weighted)
  test3 <- (length(selection) == 2 & all(6:7 %in% selection) & vestry == FALSE &
    statistic == "address" & weighted)

  if (test1) {
    output <- walking.data[["address"]]
  } else if (test2) {
    output <- walking.data[["address.not6"]]
  } else if (test3) {
    output <- walking.data[["address67"]]
  } else {
    if (vestry) {
      if (is.null(selection) == FALSE) {
        if (any(abs(selection) %in% 1:14 == FALSE)) {
          stop('With "vestry = TRUE", 1 >= |"selection"| <= 14.')
        }
      }
    } else {
      if (is.null(selection) == FALSE ) {
        if (any(abs(selection) %in% 1:13 == FALSE)) {
          stop('With "vestry = FALSE", 1 >= |"selection"| <= 13.')
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

    # pumps #

    if (is.null(selection)) {
      if (vestry) {
        pump.road.segments <- pumpIntegrator(cholera::ortho.proj.pump.vestry)
        pump.coordinates <- pumpCoordinates(vestry = TRUE)
        snow.colors <- cholera::snowColors(vestry = TRUE)
      } else {
        pump.road.segments <- pumpIntegrator()
        pump.coordinates <- pumpCoordinates()
        snow.colors <- cholera::snowColors()
      }
      select.pumps <- pump.coordinates
      pump.names <- names(pump.coordinates)
    } else {
      if (vestry) {
        pump.road.segments <-
          pumpIntegrator(cholera::ortho.proj.pump.vestry[selection, ])
        pump.coordinates <- pumpCoordinates(vestry = TRUE)[selection]
        snow.colors <- cholera::snowColors(vestry = TRUE)[selection]
      } else {
        pump.road.segments <-
          pumpIntegrator(cholera::ortho.proj.pump[selection, ])
        pump.coordinates <- pumpCoordinates()[selection]
        snow.colors <- cholera::snowColors()[selection]
      }
      select.pumps <- pump.coordinates
      pump.names <- names(pump.coordinates)
    }

    # cases #

    if (statistic == "address") {
      sel <- cholera::fatalities.address$anchor.case
      ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
    }

    if (statistic == "fatality") {
      sel <- cholera::fatalities.unstacked$case
      ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
    }

    case <- split(ortho, ortho$case)

    case.pump.road.segments <- parallel::mclapply(case, function(x)
      caseIntegrator(x, pump.road.segments), mc.cores = cores)

    g <- parallel::mclapply(case.pump.road.segments, function(x) {
      edge.list <- x[, c("node1", "node2")]
      igraph::graph_from_data_frame(edge.list, directed = FALSE)
    }, mc.cores = cores)

    case.node <- vapply(seq_along(case), function(i) {
      case.coord <- paste0(ortho[i, "x.proj"], "-", ortho[i, "y.proj"])
      which(igraph::V(g[[i]])$name == case.coord)
    }, numeric(1L))

    pump.nodes <- parallel::mclapply(g, function(graph) {
      vapply(select.pumps, function(pump) {
        which(igraph::V(graph)$name == pump)
      }, numeric(1L))
    }, mc.cores = cores)

    nearest.pump.data <- parallel::mclapply(seq_along(case), function(i) {
      if (weighted) {
        wts <- case.pump.road.segments[[i]]$d
        d <- unname(igraph::distances(g[[i]], case.node[[i]], pump.nodes[[i]],
          weights = wts))
      } else {
        d <- unname(igraph::distances(g[[i]], case.node[[i]], pump.nodes[[i]]))
      }
    }, mc.cores = cores)

    nearest.pump.data <- do.call(rbind, nearest.pump.data)
    idx <- apply(nearest.pump.data, 1, which.min)
    nearest.pump <- names(select.pumps)[idx]

    obs.pumps <- table(nearest.pump)
    pump.id <- as.numeric(substr(names(obs.pumps), 2, length(names(obs.pumps))))
    observed <- data.frame(pump.id = pump.id, count = unname(c(obs.pumps)))

    paths <- parallel::mclapply(seq_along(g), function(i) {
      sel <- names(pump.nodes[[i]]) == nearest.pump[i]
      p.node <- pump.nodes[[i]][sel]
      igraph::shortest_paths(g[[i]], case.node[i], p.node,
        weights = case.pump.road.segments[[i]]$d)$vpath
    }, mc.cores = cores)

    neighborhood.paths <- split(paths, nearest.pump)

    idx <- order(as.numeric(substr(names(neighborhood.paths), 2,
      length(names(neighborhood.paths)))))

    neighborhood.paths <- neighborhood.paths[idx]

    intermediate.segments <- lapply(neighborhood.paths,
      intermediateSegments)

    intermediate.segments <- lapply(intermediate.segments, function(x) {
      if (is.null(x)) {
        dat <- NULL
      } else {
        dat <- cholera::road.segments[cholera::road.segments$id %in% x, ]
        dat$trimmed <- FALSE
      }
      dat
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

    neighborhood.id <- sort(pump.id)

    case.segments <- lapply(neighborhood.id, caseSegments, neighborhood.paths,
      pump.cases, intermediate.segments, vestry)

    names(case.segments) <- paste0("p", neighborhood.id)

    pump.segments <- lapply(names(neighborhood.paths), pumpSegments,
      neighborhood.paths, vestry)

    names(pump.segments) <- paste0("p", neighborhood.id)

    neighborhood.segments <- lapply(seq_along(neighborhood.id), function(i) {
      rbind(case.segments[[i]], intermediate.segments[[i]], pump.segments[[i]])
    })

    names(neighborhood.segments) <- paste0("p", neighborhood.id)

    neighborhoods <- list(trimmed.segments = neighborhood.segments,
                          pump.cases = pump.cases)

    sim.neighborhoods <- trimExpPaths(pump.road.segments, select.pumps,
      pump.names, vestry, weighted, cores)

    sim.road.length <- roadLength(sim.neighborhoods$trimmed.segments)

    expected <- sum(observed$count) * sim.road.length / sum(sim.road.length)
    pid <- as.numeric(substr(names(expected), 2, length(names(expected))))
    expected <- data.frame(pump.id = pid, count = expected)

    output <- list(
      distances = nearest.pump.data,
      pump = pump.names,
      nearest.pump = nearest.pump,
      pump.seg = neighborhoods$trimmed.segments,
      pump.case = neighborhoods$pump.cases,
      sim.pump.seg = sim.neighborhoods$trimmed.segments,
      sim.pump.case = sim.neighborhoods$pump.cases,
      observed = observed,
      expected = expected,
      snow.colors = stats::setNames(snow.colors, pump.names),
      selection = selection,
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
#' @param x An objectect of class "walking" created by neighborhoodWalking().
#' @param streets Logical. TRUE plots neighborhoods by street. FALSE plots orthogonal neighborhoods (area).
#' @param observed Logical. TRUE uses observed cases. FALSE uses "regular" simulated cases.
#' @param ... Additional plotting parameters.
#' @return A base R graphics plot.
#' @export
#' @examples
#' dat <- neighborhoodWalking()
#' plot(dat)

plot.walking <- function(x, streets = TRUE, observed = TRUE, ...) {
  if (class(x) != "walking") {
    stop('Input object\'s class needs to be "walking".')
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  obs.pump <- vapply(x$pump.case, function(p) length(p) != 0, logical(1L))
  obs.pump <- which(obs.pump)

  plot(cholera::fatalities[, c("x", "y")], xlim = x.range, ylim =  y.range,
    pch = NA, asp = 1)

  if (streets) {
    invisible(lapply(roads.list, lines, col = "gray"))
    invisible(lapply(border.list, lines))

    if (observed) {
      if (length(x$pump) == length(obs.pump)) {
        invisible(lapply(seq_along(obs.pump), function(i) {
          plotSegment(x$pump.seg[[i]], x$snow.colors[i])

          sel <- cholera::fatalities.address$anchor.case %in%
            x$pump.case[[names(obs.pump)[i]]]

          points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
            cex = 0.75, col = x$snow.colors[i])
        }))

        if (is.null(x$selection)) {
          points(cholera::pumps[, c("x", "y")], pch = 24, col = x$snow.colors)
          text(cholera::pumps[, c("x", "y")], cex = 0.9, pos = 1,
            label = x$pump)
        } else {
          points(cholera::pumps[x$selection, c("x", "y")], pch = 24,
            col = x$snow.colors)
          text(cholera::pumps[x$selection, c("x", "y")], cex = 0.9, pos = 1,
            label = x$pump)
        }

      } else {
        invisible(lapply(seq_along(obs.pump), function(i) {
          plotSegment(x$pump.seg[[names(obs.pump)[i]]],
                      x$snow.colors[names(obs.pump)[i]])

          sel <- cholera::fatalities.address$anchor.case %in%
            x$pump.case[[names(obs.pump)[i]]]

          points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
            cex = 0.75, col = x$snow.colors[names(obs.pump)[i]])
        }))

        if (is.null(x$selection)) {
          points(cholera::pumps[, c("x", "y")], pch = 24, col = x$snow.colors)
          text(cholera::pumps[, c("x", "y")], cex = 0.9, pos = 1,
            label = x$pump)
        } else if (all(x$selection < 0)) {
          points(cholera::pumps[obs.pump, c("x", "y")], pch = 24,
            col = x$snow.colors)
          text(cholera::pumps[obs.pump, c("x", "y")], cex = 0.9, pos = 1,
            label = names(obs.pump))
        } else {
          points(cholera::pumps[x$selection, c("x", "y")], pch = 24,
            col = x$snow.colors)
          text(cholera::pumps[x$selection, c("x", "y")], cex = 0.9, pos = 1,
            label = x$pump)
        }
      }

      title(main = "Observed Walking Paths")

    } else {
      invisible(lapply(seq_along(x$pump), function(i) {
        plotSegment(x$sim.pump.seg[[i]], x$snow.colors[i])
      }))

      if (is.null(x$selection)) {
        points(cholera::pumps[, c("x", "y")], pch = 24, col = x$snow.colors)
        text(cholera::pumps[, c("x", "y")], cex = 0.9, pos = 1, label = x$pump)
      } else {
        points(cholera::pumps[x$selection, c("x", "y")], pch = 24,
          col = x$snow.colors)
        text(cholera::pumps[x$selection, c("x", "y")], cex = 0.9, pos = 1,
          label = x$pump)
      }
      title(main = "Expected Walking Paths")
    }
  } else {
    if (observed) {
      invisible(lapply(roads.list, lines, col = "gray"))
      invisible(lapply(border.list, lines))

      invisible(lapply(seq_along(cholera::pumps$id), function(i) {
        points(cholera::regular.cases[x$sim.pump.case[[i]], ],
          col = scales::alpha(cholera::snowColors()[i], 0.33), pch = 15)
        points(cholera::pumps[i, c("x", "y")], pch = 24,
          bg = cholera::snowColors()[i])
      }))

      invisible(lapply(seq_along(obs.pump), function(i) {
        plotSegment(x$pump.seg[[names(obs.pump)[i]]],
                    x$snow.colors[names(obs.pump)[i]])

        sel <- cholera::fatalities.address$anchor.case %in%
          x$pump.case[[names(obs.pump)[i]]]

        points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
          cex = 0.75, col = x$snow.colors[names(obs.pump)[i]])
      }))


      if (is.null(x$selection)) {
        points(cholera::pumps[, c("x", "y")], pch = 24, col = x$snow.colors)
        text(cholera::pumps[, c("x", "y")], cex = 0.9, pos = 1, label = x$pump)
      } else {
        points(cholera::pumps[x$selection, c("x", "y")], pch = 24,
          col = x$snow.colors)
        text(cholera::pumps[x$selection, c("x", "y")], cex = 0.9, pos = 1,
          label = x$pump)
      }

      title(main = "Observed Walking Neighborhoods and Paths")
    } else {
      invisible(lapply(cholera::pumps$id, function(i) {
        points(cholera::regular.cases[x$sim.pump.case[[i]], ],
          col = cholera::snowColors()[i], pch = 15)
      }))

      invisible(lapply(roads.list, lines))
      invisible(lapply(border.list, lines))

      if (is.null(x$selection)) {
        points(cholera::pumps[, c("x", "y")], pch = 24, bg = "white") #, col = x$snow.colors)
        text(cholera::pumps[, c("x", "y")], cex = 0.9, pos = 1, col = "white",
          label = x$pump)
      } else {
        points(cholera::pumps[x$selection, c("x", "y")], bg = "white", pch = 24)
          # col = x$snow.colors)
        text(cholera::pumps[x$selection, c("x", "y")], cex = 0.9, pos = 1,
          col = "white", label = x$pump)
      }
      title(main = "Expected Walking Neighborhoods")
    }
  }
}

#' Summary statistics for walking path neighborhoods.
#'
#' @param object An object of class "walking" created by neighborhoodWalking().
#' @param ... Additional arguments.
#' @return A data frame with observed and expected counts, observed percentage, and the Pearson residual, (observed - expected) / sqrt(expected).
#' @export
#' @examples
#' dat <- neighborhoodWalking()
#' summary(dat)

summary.walking <- function(object, ...) {
  if (class(object) != "walking") {
    stop('Input objectect\'s class needs to be "walking".')
  }

  obs <- object$observed
  exp <- object$expected
  output <- merge(obs, exp, by = "pump.id", all.y = TRUE)
  names(output)[-1] <- c("Observed", "Expected")
  output[is.na(output)] <- 0
  output$Pearson <- (output$Observed - output$Expected) / sqrt(output$Expected)
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

    # directional test of whether both endpoints are included
    testAB <- lapply(seq_along(node.data$x[-1]), function(i) {
      A <- node.data[i, "x"] == cholera::road.segments$x1 &
        node.data[i, "y"] == cholera::road.segments$y1
      B <- node.data[i + 1, "x"] == cholera::road.segments$x2 &
        node.data[i + 1, "y"] == cholera::road.segments$y2
      A & B
    })

    testBA <- lapply(seq_along(node.data$x[-1]), function(i) {
      B <- node.data[i, "x"] == cholera::road.segments$x2 &
        node.data[i, "y"] == cholera::road.segments$y2
      A <- node.data[i + 1, "x"] == cholera::road.segments$x1 &
        node.data[i + 1, "y"] == cholera::road.segments$y1
      B & A
    })

    AB <- vapply(testAB, any, logical(1L))
    BA <- vapply(testBA, any, logical(1L))

    if (any(AB)) {
      dataAB <- cholera::road.segments[vapply(testAB[AB], which, integer(1L)), ]
    }
    if (any(BA)) {
      dataBA <- cholera::road.segments[vapply(testBA[BA], which, integer(1L)), ]
    }
    if (exists("dataAB") & !exists("dataBA")) {
      road.data <- dataAB
    }
    if (!exists("dataAB") & exists("dataBA")) {
      road.data <- dataBA
    }
    if (exists("dataAB") & exists("dataBA")) {
      road.data <- rbind(dataAB, dataBA)
    }
    if (exists("road.data")) road.data$id else NULL
  })

  unique(unlist(path.roads))
}

caseSegments <- function(x, neighborhood.paths, pump.cases,
  intermediate.segments, vestry, obs = TRUE) {

  n.name <- paste0("p", x)
  n.paths <- neighborhood.paths[[n.name]]
  n.cases <- pump.cases[[n.name]]

  if (obs) {
    obs.seg <- unique(cholera::ortho.proj[cholera::ortho.proj$case %in%
      n.cases, "road.segment"])
  } else {
    obs.seg <- unique(cholera::ortho.proj.sp[cholera::ortho.proj.sp$case %in%
      n.cases, "road.segment"])
  }

  if (is.null(intermediate.segments[[n.name]]) == FALSE) {
    obs.seg <- obs.seg[obs.seg %in% intermediate.segments[[n.name]]$id == FALSE]
  }

  if (vestry) {
    pump.seg <- cholera::ortho.proj.pump.vestry[
      cholera::ortho.proj.pump.vestry$pump.id == x, "road.segment" ]
  } else {
    pump.seg <- cholera::ortho.proj.pump[cholera::ortho.proj.pump$pump.id == x,
      "road.segment" ]
  }

  obs.seg <- obs.seg[obs.seg %in% pump.seg == FALSE]

  new.segs <- lapply(obs.seg, function(seg) {
    st <- cholera::road.segments[cholera::road.segments$id == seg, ]
    st$trimmed <- FALSE

    if (obs) {
      sel <- cholera::ortho.proj$road.segment %in% st$id &
        cholera::ortho.proj$case %in% n.cases
      st.cases <- cholera::ortho.proj[sel, ]

    } else {
      sel <- cholera::ortho.proj.sp$road.segment %in% st$id &
        cholera::ortho.proj.sp$case %in% n.cases
      st.cases <- cholera::ortho.proj.sp[sel, ]
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
      d <- vapply(seq_along(st.cases$case), function(i) {
        a <- st.cases[i, c("x.proj", "y.proj")]
        names(a) <- c("x", "y")
        b <- data.frame(x = st$x2, y = st$y2)
        stats::dist(rbind(a, b))
      }, numeric(1L))
      st[, c("x2", "y2")] <- st.cases[which.min(d), c("x.proj", "y.proj")]
      st$trimmed <- TRUE
    }

    if (all(entry2)) {
      d <- vapply(seq_along(st.cases$case), function(i) {
        a <- st.cases[i, c("x.proj", "y.proj")]
        names(a) <- c("x", "y")
        b <- data.frame(x = st$x1, y = st$y1)
        stats::dist(rbind(a, b))
      }, numeric(1L))
      st[, c("x1", "y1")] <- st.cases[which.min(d), c("x.proj", "y.proj")]
      st$trimmed <- TRUE
    }

    if (all(entry1 + entry2 == 1)) {
      st
    }

    st
  })

  do.call(rbind, new.segs)
}

pumpSegments <- function(x, neighborhood.paths, vestry) {
  # n.name <- paste0("p", x)
  n.paths <- neighborhood.paths[[x]]
  # n.cases <- pump.cases[[n.name]]

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
  # else {
  #   stop("Error")
  # }

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

trimExpPaths <- function(pump.road.segments, select.pumps, pump.names, vestry,
  weighted, cores) {

  edge.case <- cholera::ortho.proj.sp[is.na(cholera::ortho.proj.sp$x.proj),
    "case"]
  falconberg.id <- pump.road.segments[pump.road.segments$name ==
    "Falconberg Court" | pump.road.segments$name == "Falconberg Mews", "id"]
  falconberg <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$road.segment
    %in% falconberg.id, "case"]

  if (all(pump.names %in% "p2" == FALSE)) {
    # if pump 2, on Adam and Eve Court, is not included, siumuated cases located
    # on that street cannot reach any other pump.

    adam.eve <- c(4536, 4537, 4538, 4539, 4608, 4609, 4610, 4611, 4612, 4681,
      4682, 4683, 4684, 4753, 4754, 4755, 4756, 4757, 4827, 4828, 4829, 4830,
      4900, 4901, 4902, 4955, 4956)

    ortho <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$case
      %in% c(edge.case, falconberg, adam.eve) == FALSE, ]

  } else {
    ortho <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$case
      %in% c(edge.case, falconberg) == FALSE, ]
  }

  case <- split(ortho, ortho$case)

  case.pump.road.segments <- parallel::mclapply(case, function(x)
    caseIntegrator(x, pump.road.segments), mc.cores = cores)

  g <- parallel::mclapply(case.pump.road.segments, function(x) {
    edge.list <- x[, c("node1", "node2")]
    igraph::graph_from_data_frame(edge.list, directed = FALSE)
  }, mc.cores = cores)

  case.node <- vapply(seq_along(case), function(i) {
    case.coord <- paste0(ortho[i, "x.proj"], "-", ortho[i, "y.proj"])
    which(igraph::V(g[[i]])$name == case.coord)
  }, numeric(1L))

  pump.nodes <- parallel::mclapply(g, function(graph) {
    vapply(select.pumps, function(pump) {
      which(igraph::V(graph)$name == pump)
    }, numeric(1L))
  }, mc.cores = cores)

  nearest.pump.data <- parallel::mclapply(seq_along(case), function(i) {
    if (weighted) {
      d <- unname(igraph::distances(g[[i]], case.node[[i]], pump.nodes[[i]],
        weights = case.pump.road.segments[[i]]$d))
    } else {
      d <- unname(igraph::distances(g[[i]], case.node[[i]], pump.nodes[[i]]))
    }
  }, mc.cores = cores)

  nearest.pump.data <- do.call(rbind, nearest.pump.data)
  idx <- apply(nearest.pump.data, 1, which.min)
  nearest.pump <- names(select.pumps)[idx]

  obs.pumps <- table(nearest.pump)
  pump.id <- as.numeric(substr(names(obs.pumps), 2, length(names(obs.pumps))))
  observed <- data.frame(pump.id = pump.id, count = unname(c(obs.pumps)))

  paths <- parallel::mclapply(seq_along(g), function(i) {
    sel <- names(pump.nodes[[i]]) == nearest.pump[i]
    p.node <- pump.nodes[[i]][sel]
    igraph::shortest_paths(g[[i]], case.node[i], p.node,
      weights = case.pump.road.segments[[i]]$d)$vpath
  }, mc.cores = cores)

  neighborhood.paths <- split(paths, nearest.pump)

  idx <- order(as.numeric(substr(names(neighborhood.paths), 2,
    length(names(neighborhood.paths)))))

  neighborhood.paths <- neighborhood.paths[idx]

  intermediate.segments <- lapply(neighborhood.paths,
    intermediateSegments)

  intermediate.segments <- lapply(intermediate.segments, function(x) {
    if (is.null(x)) {
      dat <- NULL
    } else {
      dat <- cholera::road.segments[cholera::road.segments$id %in% x, ]
      dat$trimmed <- FALSE
    }
    dat
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

  neighborhood.id <- sort(pump.id)

  case.segments <- lapply(neighborhood.id, caseSegments, neighborhood.paths,
    pump.cases, intermediate.segments, vestry, obs = FALSE)

  names(case.segments) <- paste0("p", neighborhood.id)

  pump.segments <- lapply(names(neighborhood.paths), pumpSegments,
    neighborhood.paths, vestry)

  names(pump.segments) <- paste0("p", neighborhood.id)

  neighborhood.segments <- lapply(seq_along(neighborhood.id), function(i) {
    rbind(case.segments[[i]], intermediate.segments[[i]], pump.segments[[i]])
  })

  names(neighborhood.segments) <- paste0("p", neighborhood.id)

  list(trimmed.segments = neighborhood.segments, pump.cases = pump.cases)
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
