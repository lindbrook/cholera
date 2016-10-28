#' Plots expected walking neighborhoods.
#'
#' Neighborhoods are based on the shortest paths between a fatality's address and its nearest pump.
#'
#' Currently computationally intensive (On an Intel Core i7, streets = TRUE appox. 90 seocnds; streets = FALSE appox. 75 seconds). For better performance, run in parallel with multiple cores in the terminal or in batch mode (appox. 45 and 30 seconds). Note that this is currently unavailable on Windows. See \code{vignette}("walking.neighborhoods") for details.
#' @param selection Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: from 1 to 13 for \code{pumps}; from 1 to 14 for \code{pumps.vestry}
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param streets Logical. TRUE plots neighborhoods by street. FALSE plots orthogonal neighborhoods (area).
#' @param weighted Logical. TRUE uses shortest path weighted by road distance.
#' @param add.obs Logical. Include observed addresses.
#' @param add.landmarks Logical. Include landmarks.
#' @param arrow Logical. Use arrows.
#' @param alpha.level, Numeric. Set alpha level of paths.
#' @param obs.lwd Numeric. Set line width for observed paths.
#' @param sim.lwd Numeric. Set line width for expected paths
#' @param multi.core Logical. TRUE uses parallel::mclapply and parallel::detectCores(). Note that this option is not currently available on Windows. See parallel::mclapply() for details. FALSE uses 1 logical core and can be run in GUI and on Windows.
#' @return A base R graphics plot.
#' @seealso \code{addLandmarks()}
#'
#' @export
#' @examples
#' # neighborhoodExpected()
#' # neighborhoodExpected(streets = FALSE)

neighborhoodExpected <- function(selection = NULL, vestry = FALSE,
  streets = TRUE, weighted = TRUE, add.obs = FALSE, add.landmarks = TRUE,
  arrow = FALSE, alpha.level = NULL, obs.lwd = 2, sim.lwd = 2,
  multi.core = FALSE) {

  if (vestry) {
    if (is.null(selection) == FALSE) {
      if (any(abs(selection) %in% 1:14 == FALSE)) {
        stop('With "vestry = TRUE", "selection" must be between 1 and 14.')
      }
    }
  } else {
    if (is.null(selection) == FALSE ) {
      if (any(abs(selection) %in% 1:13 == FALSE)) {
        stop('With "vestry = FALSE", "selection" must be between 1 and 13.')
      }
    }
  }

  if (multi.core) cores <- parallel::detectCores() else cores <- 1L

  roadsB <- cholera::roads[cholera::roads$street %in%
                           cholera::border == FALSE, ]

  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(roadsB[, c("x", "y")], roadsB$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  if (vestry) {
    colors <- snowColors(vestry = TRUE)
    pump.road.segments <- pumpIntegrator(cholera::ortho.proj.pump.vestry)
    pump.coordinates <- pumpCoordinates(vestry = TRUE)
  } else {
    colors <- snowColors()
    pump.road.segments <- pumpIntegrator()
    pump.coordinates <- pumpCoordinates()
  }

  if (is.null(selection)) {
    select.pumps <- pump.coordinates
    pump.names <- names(pump.coordinates)
  } else {
    colors <- colors[selection]
    select.pumps <- pump.coordinates[selection]
    pump.names <- names(pump.coordinates[selection])
  }

  ## --------------- Case Data --------------- ##

  # Isolates are technically not part of road network

  no.pump <- cholera::ortho.proj.sp[is.na(cholera::ortho.proj.sp$x.proj),
    "case"]

  falconberg.id <- pump.road.segments[pump.road.segments$name ==
    "Falconberg Court" | pump.road.segments$name == "Falconberg Mews", "id"]

  falconberg <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$road.segment %in%
    falconberg.id, "case"]

  # Expected cases

  selected.case <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$case %in%
    c(no.pump, falconberg) == FALSE, ]

  selected.case <- split(selected.case, selected.case$case)

  # Integrates case into road netowrk

  case.pump.road.segments <- parallel::mclapply(selected.case, function(x)
    caseIntegrator(x, pump.road.segments), mc.cores = cores)

  edge.list <- case.pump.road.segments

  g <- lapply(edge.list, function(x) {
    edges <- x[, c("node1", "node2")]
    igraph::graph_from_data_frame(edges, directed = FALSE)
  })

  nearest.pump.data <- parallel::mclapply(seq_along(g), function(x) {
    case <- selected.case[[x]]
    case.graph <- g[[x]]
    case.coord <- paste0(case$x.proj, "-", case$y.proj)
    case.node <- which(igraph::V(case.graph)$name == case.coord)

    if (is.null(selection)) {
      pump.nodes <- vapply(pump.coordinates, function(p) {
        which(igraph::V(case.graph)$name == p)
      }, numeric(1L))
    } else {
      pump.nodes <- vapply(pump.coordinates[selection], function(p) {
        which(igraph::V(case.graph)$name == p)
      }, numeric(1L))
    }

    if (weighted) {
      wts <- case.pump.road.segments[[x]]$d
      d <- unname(igraph::distances(case.graph, case.node, pump.nodes,
        weights = wts))
    } else {
      d <- unname(igraph::distances(case.graph, case.node, pump.nodes))
    }

    d
  }, mc.cores = cores)

  nearest.pump.data <- data.frame(do.call(rbind, nearest.pump.data))
  names(nearest.pump.data) <- pump.names

  ortho.cases <- do.call(rbind, selected.case)

  id <- apply(nearest.pump.data, 1, which.min)

  nearest.pump <- data.frame(case = ortho.cases$case,
                             wtd.pump = names(nearest.pump.data)[id])

  nearest.pump$wtd.pump <- as.numeric(substr(nearest.pump$wtd.pump, 2,
    length(nearest.pump$wtd.pump)))

  # ## ------------------- Graphs ------------------- ##

  exp.address <- data.frame(ortho.cases[, c("case", "x.proj", "y.proj")],
    wtd.pump = nearest.pump$wtd.pump)

  exp.address$col <- NA

  if (!vestry) {
    if (is.null(selection)) {
      for (i in 1:13) {
        exp.address[exp.address$wtd.pump == i, "col"] <- colors[i]
      }
    } else {
      obs.pumps <- sort(unique(exp.address$wtd.pump))
      for (i in seq_along(obs.pumps)) {
        exp.address[exp.address$wtd.pump == obs.pumps[i], "col"] <- colors[i]
      }
    }
  } else {
     if (is.null(selection)) {
       for (i in 1:13) {
         exp.address[exp.address$wtd.pump == i, "col"] <- colors[i]
       }
     } else {
       obs.pumps <- sort(unique(exp.address$wtd.pump))
       for (i in seq_along(obs.pumps)) {
         exp.address[exp.address$wtd.pump == obs.pumps[i], "col"] <- colors[i]
       }
     }
  }

  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  plot(cholera::fatalities[, c("x", "y")], xlim = range(cholera::roads$x),
    ylim = range(cholera::roads$y), pch = NA, asp = 1)

  if (streets) {
    invisible(lapply(roads.list, lines, col = "gray"))
    invisible(lapply(border.list, lines))

    invisible(lapply(exp.address$case, function(x) {
      id <- which(exp.address$case == x)
      case <- selected.case[[id]]
      case.graph <- g[[id]]
      case.coord <- paste0(case$x.proj, "-", case$y.proj)
      case.node <- which(igraph::V(case.graph)$name == case.coord)
      sel <- nearest.pump[nearest.pump$case == x, "wtd.pump"]
      pump.node <- which(igraph::V(case.graph)$name == pump.coordinates[sel])
      wts <- case.pump.road.segments[[id]]$d
      case.path <- unlist(igraph::shortest_paths(case.graph, case.node,
        pump.node, weights = wts)$vpath)
      case.path <- names(case.path)
      dat <- numericNodeCoordinates(case.path)
      n1 <- dat[1:(nrow(dat) - 1), ]
      n2 <- dat[2:nrow(dat), ]

      if (is.null(alpha.level)) {
        seg.col <- exp.address[exp.address$case == x , "col"]
      } else {
        seg.col <- scales::alpha(exp.address[exp.address$case == x , "col"],
          alpha.level)
      }

      if (arrow) {
        arrows(n1$x, n1$y, n2$x, n2$y, col = seg.col, lwd = sim.lwd,
          length = 0.075)
      } else {
        segments(n1$x, n1$y, n2$x, n2$y, col = seg.col, lwd = sim.lwd)
      }
    }))

    if (is.null(selection)) {
      if (!vestry) {
        text(cholera::pumps[, c("x", "y")], label = cholera::pumps$id, cex = 1,
          pos = 1)
        points(cholera::pumps[, c("x", "y")], pch = 2, col = colors)
      } else {
        text(cholera::pumps.vestry[, c("x", "y")], cex = 1, pos = 1,
          label = cholera::pumps.vestry$id)
        points(cholera::pumps.vestry[, c("x", "y")], pch = 2, col = colors)
      }
    } else {
      if (!vestry) {
        text(cholera::pumps[selection, c("x", "y")], cex = 1, pos = 1,
          label = cholera::pumps$id[selection])
        points(cholera::pumps[selection, c("x", "y")], pch = 2, col = colors)
      } else {
        text(cholera::pumps.vestry[selection, c("x", "y")], cex = 1, pos = 1,
          label = cholera::pumps.vestry$id[selection])
        points(cholera::pumps.vestry[selection, c("x", "y")], pch = 2,
          col = colors)
      }
    }
    title(main = "Expected Paths")

  } else {
    points(cholera::regular.cases[exp.address$case, ], col = exp.address$col,
      pch = 15)
    invisible(lapply(roads.list, lines, col = "black"))
    invisible(lapply(border.list, lines))

    if (is.null(selection)) {
      if (!vestry) {
        text(cholera::pumps[, c("x", "y")], label = cholera::pumps$id, cex = 1,
          col = "white", pos = 1)
        points(cholera::pumps[, c("x", "y")], pch = 24, bg = "white")
      } else {
        text(cholera::pumps.vestry[, c("x", "y")], col = "white", pos = 1,
          label = cholera::pumps.vestry$id, cex = 1)
        points(cholera::pumps.vestry[, c("x", "y")], pch = 24, bg = "white")
      }
    } else {
      if (!vestry) {
        text(cholera::pumps[selection, c("x", "y")], cex = 1, col = "white",
          pos = 1, label = cholera::pumps$id[selection])
        points(cholera::pumps[selection, c("x", "y")], pch = 24, bg = "white")
      } else {
        text(cholera::pumps.vestry[selection, c("x", "y")], cex = 1,
          col = "white", pos = 1, label = cholera::pumps.vestry$id[selection])
        points(cholera::pumps.vestry[selection, c("x", "y")], pch = 24,
          bg = "white")
      }
    }
    title(main = "Expected Path Neighborhoods")
  }

  if (add.obs) {
    obs.address <- data.frame(case = cholera::fatalities.address$anchor.case,
      cholera::fatalities.address[, c("x", "y")])
    points(obs.address[, c("x", "y")], pch = 21, bg = "white", cex = 0.75)
  }

  if (add.landmarks) cholera::addLandmarks()
}

#' Summary statistics for expected walking neighborhoods.
#'
#' Neighborhoods are based on the shortest paths between a fatality's address and its nearest pump.
#'
#' Currently computationally intensive (On an Intel Core i7, streets = TRUE appox. 90 seocnds; streets = FALSE appox. 75 seconds). For better performance, run in parallel with multiple cores in the terminal or in batch mode (appox. 45 and 30 seconds). Note that this is currently unavailable on Windows. See \code{vignette}("walking.neighborhoods") for details.
#' @param selection Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: from 1 to 13 for \code{pumps}; from 1 to 14 for \code{pumps.vestry}
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param weighted Logical. TRUE uses shortest path weighted by road distance.
#' @param multi.core Logical. TRUE uses parallel::mclapply and parallel::detectCores(). Note that this option is not currently available on Windows. See parallel::mclapply() for details. FALSE uses 1 logical core and can be run in GUI and on Windows.
#' @return A data frame.
#' @export
#' @examples
#' # neighborhoodExpectedCensus()
#' # neighborhoodExpectedCensus(streets = FALSE)

neighborhoodExpectedCensus <- function(selection = NULL, vestry = FALSE,
  weighted = TRUE, multi.core = FALSE) {

  if (vestry) {
    if (is.null(selection) == FALSE) {
      if (any(abs(selection) %in% 1:14 == FALSE)) {
        stop('With "vestry = TRUE", "selection" must be between 1 and 14.')
      }
    }
  } else {
    if (is.null(selection) == FALSE ) {
      if (any(abs(selection) %in% 1:13 == FALSE)) {
        stop('With "vestry = FALSE", "selection" must be between 1 and 13.')
      }
    }
  }

  if (multi.core) cores <- parallel::detectCores() else cores <- 1L

  if (vestry) {
    pump.road.segments <- pumpIntegrator(cholera::ortho.proj.pump.vestry)
    pump.coordinates <- pumpCoordinates(vestry = TRUE)
  } else {
    pump.road.segments <- pumpIntegrator()
    pump.coordinates <- pumpCoordinates()
  }

  if (is.null(selection)) {
    select.pumps <- pump.coordinates
    pump.names <- names(pump.coordinates)
  } else {
    select.pumps <- pump.coordinates[selection]
    pump.names <- names(pump.coordinates[selection])
  }

  ## --------------- Case Data --------------- ##

  # Isolates are technically not part of road network

  no.pump <- cholera::ortho.proj.sp[is.na(cholera::ortho.proj.sp$x.proj),
    "case"]

  falconberg.id <- pump.road.segments[pump.road.segments$name ==
    "Falconberg Court" | pump.road.segments$name == "Falconberg Mews", "id"]

  falconberg <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$road.segment %in%
    falconberg.id, "case"]

  # Expected cases

  selected.case <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$case %in%
    c(no.pump, falconberg) == FALSE, ]

  selected.case <- split(selected.case, selected.case$case)

  # Integrates case into road netowrk

  case.pump.road.segments <- parallel::mclapply(selected.case, function(x)
    caseIntegrator(x, pump.road.segments), mc.cores = cores)

  edge.list <- case.pump.road.segments

  g <- lapply(edge.list, function(x) {
    edges <- x[, c("node1", "node2")]
    igraph::graph_from_data_frame(edges, directed = FALSE)
  })

  # wtd.dist.sp <- parallel::mclapply(seq_along(case.network.sp), function(x) {

  nearest.pump.data <- parallel::mclapply(seq_along(g), function(x) {
    case <- selected.case[[x]]
    case.graph <- g[[x]]
    case.coord <- paste0(case$x.proj, "-", case$y.proj)
    case.node <- which(igraph::V(case.graph)$name == case.coord)

    if (is.null(selection)) {
      pump.nodes <- vapply(pump.coordinates, function(p) {
        which(igraph::V(case.graph)$name == p)
      }, numeric(1L))
    } else {
      pump.nodes <- vapply(pump.coordinates[selection], function(p) {
        which(igraph::V(case.graph)$name == p)
      }, numeric(1L))
    }

    if (weighted) {
      wts <- case.pump.road.segments[[x]]$d
      d <- unname(igraph::distances(case.graph, case.node, pump.nodes,
        weights = wts))
    } else {
      d <- unname(igraph::distances(case.graph, case.node, pump.nodes))
    }

    d
  }, mc.cores = cores)

  nearest.pump.data <- data.frame(do.call(rbind, nearest.pump.data))
  names(nearest.pump.data) <- pump.names

  ortho.cases <- do.call(rbind, selected.case)

  id <- apply(nearest.pump.data, 1, which.min)

  nearest.pump <- data.frame(case = ortho.cases$case,
                             wtd.pump = names(nearest.pump.data)[id])

  nearest.pump$wtd.pump <- as.numeric(substr(nearest.pump$wtd.pump, 2,
    length(nearest.pump$wtd.pump)))

  census <- unclass(table(nearest.pump$wtd.pump))
  
  data.frame(pump.id = as.numeric(names(census)),
             Count = census,
             Percent = round(100 * census / sum(census), 2))
}

snowColors <- function(vestry = FALSE) {
  colors.pair <- RColorBrewer::brewer.pal(10, "Paired")
  colors.dark <- RColorBrewer::brewer.pal(8, "Dark2")
  if (!vestry) {
    c("dodgerblue", "gray", colors.dark[1:4], colors.pair[2], colors.dark[5:8],
      "red", colors.pair[1])
  } else {
    c("dodgerblue", "gray", colors.dark[1:4], colors.pair[2], colors.dark[5:8],
      "red", colors.pair[1], "darkorange")
  }
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

numericNodeCoordinates <- function(x) {
  nodes <- do.call(rbind, (strsplit(x, "-")))
  data.frame(x = as.numeric(nodes[, 1]), y = as.numeric(nodes[, 2]))
}

roadSegments <- function() {
  roadsB <- cholera::roads[cholera::roads$street %in%
    cholera::border == FALSE, ]

  out <- lapply(unique(roadsB$street), function(i) {
    dat <- roadsB[roadsB$street == i, ]
    names(dat)[names(dat) %in% c("x", "y")] <- c("x1", "y1")
    seg.data <- dat[-1, c("x1", "y1")]
    names(seg.data) <- c("x2", "y2")
    dat <- cbind(dat[-nrow(dat), ], seg.data)
    dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
    dat
  })

  do.call(rbind, out)
}

pumpIntegrator <- function(pump.data = cholera::ortho.proj.pump,
  road.segments = roadSegments()) {

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
    temp <- cbind(temp[, c("street", "n")], pump.coords,
      temp[, c("id", "name", "x2", "y2")])

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

caseIntegrator <- function(x, dat) {
  seg <- unlist(strsplit(dat$id, "a"))
  seg <- unlist(strsplit(seg, "b"))
  temp <- dat[which(x$road.segment == seg), ]
  case.coord <- x[, c("x.proj", "y.proj")]

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
  road.segments2 <- dat
  rbind(road.segments2[road.segments2$id != x$road.segment, ], appended)
}
