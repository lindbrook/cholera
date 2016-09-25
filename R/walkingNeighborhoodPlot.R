#' Plots walking neighborhoods.
#'
#' Plots walking neighborhoods based on the weighted shortest paths between a fatalities and the nearest pump. Beta version. 
#'
#' Currently computationally intensive. For now, it is better to run in batch mode in parallel with multiple cores. See \code{vignette}("walking.neighborhoods") for details
#' @param selection Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param streets TRUE plots neighborhoods by street. FALSE plots orthogonal neighborhoods.
#' @param weighted Logical. Shortest path weighted by road distance.
#' @param add.obs Logical. Include observed addresses.
#' @param add.landmarks Logical. Include landmarks.
#' @param cores Numeric or "all". Number of cores to use; default is 1. If you select more than 1 core or use "all", which uses parallel::detectCores(), you should run this function in batch, not interactive mode (i.e., not in GUI).
#' @return A base R graphics plot.
#' @seealso \code{addLandmarks()}
#'
#' @export

walkingNeighborhoodPlot <- function(selection = NULL, vestry = FALSE,
  streets = TRUE, weighted = TRUE, add.obs = FALSE, add.landmarks = TRUE,
  cores = 1L) {

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

  if (is.numeric(cores) == FALSE & cores != "all") {
    stop('The only non-numeric choice for "cores" is "all".')
  }

  roadsB <- cholera::roads[cholera::roads$street %in%
                           cholera::border == FALSE, ]

  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(roadsB[, c("x", "y")], roadsB$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  road.segments <- lapply(unique(roadsB$street), function(i) {
    dat <- roadsB[roadsB$street == i, ]
    names(dat)[names(dat) %in% c("x", "y")] <- c("x1", "y1")
    seg.data <- dat[-1, c("x1", "y1")]
    names(seg.data) <- c("x2", "y2")
    dat <- cbind(dat[-nrow(dat), ], seg.data)
    dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
    dat
  })

  road.segments <- do.call(rbind, road.segments)

  if (vestry) {
    colors <- snowColors(vestry = TRUE)
    road.segments <- integratePumpNodes(road.segments, vestry = TRUE)
    pump.coordinates <- paste0(cholera::ortho.proj.pump.vestry$x.proj, "-",
                               cholera::ortho.proj.pump.vestry$y.proj)
    names(pump.coordinates) <- paste0("p", seq_along(pump.coordinates))
  } else {
    colors <- snowColors()
    road.segments <- integratePumpNodes(road.segments)
    pump.coordinates <- paste0(cholera::ortho.proj.pump$x.proj, "-",
                               cholera::ortho.proj.pump$y.proj)
    names(pump.coordinates) <- paste0("p", seq_along(pump.coordinates))
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

  falconberg.id <- road.segments[road.segments$name == "Falconberg Court" |
                                 road.segments$name == "Falconberg Mews", "id"]

  falconberg <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$road.segment %in%
    falconberg.id, "case"]

  # Expected cases

  expected.cases <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$case %in%
    c(no.pump, falconberg) == FALSE, "case"]

  selected.case.sp <- lapply(expected.cases, caseSelector, obs = FALSE)

  case.road.segments.sp <- parallel::mclapply(expected.cases, function(x) {
    seg <- unlist(strsplit(road.segments$id, "a"))
    seg <- unlist(strsplit(seg, "b"))
    case <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$case == x, ]

    temp <- road.segments[which(case$road.segment == seg), ]
    case.coord <- case[, c("x.proj", "y.proj")]

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

    road.segments2 <- road.segments
    sel <- road.segments2$id != case$road.segment
    rbind(road.segments2[sel, ], appended)
  }, mc.cores = cores)

  case.network.sp <- lapply(case.road.segments.sp, function(x) {
    edge.list <- x[, c("node1", "node2")]
    igraph::graph_from_data_frame(edge.list, directed = FALSE)
  })

  wtd.dist.sp <- parallel::mclapply(expected.cases, function(x) {
    id <- which(x == expected.cases)
    g <- case.network.sp[[id]]
    case <- selected.case.sp[[id]]
    case.coord <- paste0(case$x.proj, "-", case$y.proj)
    case.node <- which(igraph::V(g)$name == case.coord)

    pump.coordinates <- paste0(cholera::ortho.proj.pump$x.proj, "-",
      cholera::ortho.proj.pump$y.proj)
    names(pump.coordinates) <- paste0("p", seq_along(pump.coordinates))

    if (is.null(selection)) {
      select.pumps <- pump.coordinates
      pump.names <- names(pump.coordinates)
    } else {
      select.pumps <- pump.coordinates[selection]
      pump.names <- names(pump.coordinates)[selection]
    }

    pump.nodes <- vapply(seq_along(select.pumps), function(i) {
      which(igraph::V(g)$name == select.pumps[i])
    }, numeric(1L))

    wts <- case.road.segments.sp[[id]]$d
    d <- unname(igraph::distances(g, case.node, pump.nodes, weights = wts))
  }, mc.cores = cores)

  wtd.dist.sp <- data.frame(do.call(rbind, wtd.dist.sp))
  names(wtd.dist.sp) <- pump.names


  nearest.pump.sp <- names(wtd.dist.sp)[apply(wtd.dist.sp, 1, which.min)]

  nearest.pump.sp <- data.frame(case = expected.cases,
    wtd.pump = nearest.pump.sp)

  nearest.pump.sp$wtd.pump <- as.numeric(substr(nearest.pump.sp$wtd.pump, 2,
    length(nearest.pump.sp$wtd.pump)))

  # ## ------------------- Graphs ------------------- ##

  sel <- cholera::ortho.proj.sp$case %in% expected.cases

  exp.address <- data.frame(cholera::ortho.proj.sp[sel,
    c("case", "x.proj", "y.proj")], wtd.pump = nearest.pump.sp$wtd.pump)

  exp.address$col <- NA

  for (i in 1:13) {
    exp.address[exp.address$wtd.pump == i, "col"] <- colors[i]
  }

  x.range <- range(cholera::roads$x)
  y.range <- range(cholera::roads$y)

  plot(cholera::fatalities[, c("x", "y")], xlim = range(cholera::roads$x),
    ylim = range(cholera::roads$y), pch = NA, asp = 1)

  if (streets) {
    invisible(lapply(roads.list, lines, col = "gray"))
    invisible(lapply(border.list, lines))
    text(cholera::pumps[, c("x", "y")], label = cholera::pumps$id, cex = 1,
      pos = 1)
    points(cholera::pumps[, c("x", "y")], pch = 2, col = colors)
    invisible(lapply(expected.cases, drawPath, obs = FALSE))
    title(main = "Expected Paths")
  } else {
    points(cholera::regular.cases[expected.cases, ], col = exp.address$col,
      pch = 15)
    invisible(lapply(roads.list, lines, col = "black"))
    invisible(lapply(border.list, lines))

    if (is.null(selection)) {
      text(cholera::pumps[, c("x", "y")], label = cholera::pumps$id, cex = 1,
        col = "white", pos = 1)
      points(cholera::pumps[, c("x", "y")], pch = 24, bg = "white")
    } else {
      text(cholera::pumps[selection, c("x", "y")], cex = 1, col = "white",
        pos = 1, label = cholera::pumps$id[selection])
      points(cholera::pumps[selection, c("x", "y")], pch = 24, bg = "white")
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

caseSelector <- function(x, obs = TRUE) {
  if (obs) {
    case <- cholera::ortho.proj[cholera::ortho.proj$case == x, ]
  } else {
    case <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$case == x, ]
  }
  case
}

numericNodeCoordinates <- function(x) {
  nodes <- do.call(rbind, (strsplit(x, "-")))
  data.frame(x = as.numeric(nodes[, 1]), y = as.numeric(nodes[, 2]))
}

drawPath <- function(x, vestry = FALSE, obs.lwd = 2, sim.lwd = 2, 
  arrows = FALSE, alpha = NULL, case.network.sp, case.road.segments.sp, 
  selected.case.sp, expected.cases, exp.address, nearest.pump.sp) {

  if (vestry == FALSE) {
    pump.coordinates <- paste0(cholera::ortho.proj.pump$x.proj, "-",
      cholera::ortho.proj.pump$y.proj)
    names(pump.coordinates) <- paste0("p", seq_along(pump.coordinates))
  } else {
    pump.coordinates <- paste0(cholera::ortho.proj.pump.vestry$x.proj, "-",
      cholera::ortho.proj.pump.vestry$y.proj)
    names(pump.coordinates) <- paste0("p", seq_along(pump.coordinates))
  }

  id <- which(x == expected.cases)
  g <- case.network.sp[[id]]
  case <- selected.case.sp[[id]]
  case.coord <- paste0(case$x.proj, "-", case$y.proj)
  case.node <- which(igraph::V(g)$name == case.coord)

  sel <- nearest.pump.sp[nearest.pump.sp$case == x, "wtd.pump"]
  pump.node <- which(igraph::V(g)$name == pump.coordinates[sel])

  wts <- case.road.segments.sp[[id]]$d
  case.path <- unlist(igraph::shortest_paths(g, case.node, pump.node,
    weights = wts)$vpath)
  case.path <- names(case.path)

  dat <- numericNodeCoordinates(case.path)
  n1 <- dat[1:(nrow(dat) - 1), ]
  n2 <- dat[2:nrow(dat), ]

  if (is.null(alpha)) {
    seg.col <- exp.address[exp.address$case == x , "col"]
  } else {
    seg.col <- scales::alpha(exp.address[exp.address$case == x , "col"],
      alpha)
  }

  if (arrows) {
    arrows(n1$x, n1$y, n2$x, n2$y, col = seg.col, lwd = sim.lwd,
      length = 0.075)
  } else {
    segments(n1$x, n1$y, n2$x, n2$y, col = seg.col, lwd = sim.lwd)
  }
}

integratePumpNodes <- function(road.segments, vestry = FALSE) {
  if (vestry == FALSE) {
    pump.data <- cholera::ortho.proj.pump
  } else {
    pump.data <- cholera::ortho.proj.pump.vestry
  }

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
    temp <- cbind(temp[, c("street", "n")],
                  pump.coords,
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
  out$d <- sqrt((out$x1 - out$x2)^2 +
                          (out$y1 - out$y2)^2)
  out
}
