#' Plots observed walking neighborhoods.
#'
#' Neighborhoods are based on the shortest paths between a fatality's address and its nearest pump.
#'
#' @param selection Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE uses shortest path weighted by road distance.
#' @param statistic Character. NULL plots address points. "addresses" plots the total count of addresses at pumps' locations. "fatalities" plots the total count of fatalities at pumps' locations.
#' @param add.landmarks Logical. Include landmarks.
#' @return A base R graphics plot.
#' @seealso \code{addLandmarks()}
#'
#' \code{vignette}("walking.neighborhoods")
#' @export
#' @examples
#' # observedNeighborhood()
#' # observedNeighborhood(selection = 6:7)

observedNeighborhood <- function(selection = NULL, vestry = FALSE,
  weighted = TRUE, statistic = NULL, add.landmarks = TRUE) {

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

  if (is.null(statistic) == FALSE) {
    if (all(statistic %in% c("addresses", "fatalities")) == FALSE) {
      text.a <- 'If specified, "statistic" must either be "addresses"'
      text.b <- 'or "fatalities".'
      stop(paste(text.a, text.b))
    }
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

  # integrate pumps into road network

  if (vestry) {
    colors <- snowColors(vestry = TRUE)
    pump.data <- cholera::ortho.proj.pump.vestry
    pump.segments <- pump.data$road.segment

    mat <- matrix(0, ncol = ncol(road.segments ),
      nrow = 2 * length(pump.segments))
    road.pump.data <- data.frame(mat)
    start.pt <- seq(1, nrow(road.pump.data), 2)
    end.pt <- seq(2, nrow(road.pump.data), 2)

    for (i in seq_along(pump.segments)) {
      road.data <- road.segments [road.segments $id == pump.segments[i], ]
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

    road.segments <- road.segments[road.segments$id %in%
      pump.segments == FALSE, ]
    out <- rbind(road.segments, road.pump.data)
    out <- out[order(out$street, out$id), ]
    out$node1 <- paste0(out$x1, "-", out$y1)
    out$node2 <- paste0(out$x2, "-", out$y2)
    out$d <- sqrt((out$x1 - out$x2)^2 + (out$y1 - out$y2)^2)
    road.segments <- out

    pump.coordinates <- paste0(cholera::ortho.proj.pump.vestry$x.proj, "-",
                               cholera::ortho.proj.pump.vestry$y.proj)
    names(pump.coordinates) <- paste0("p", seq_along(pump.coordinates))
  } else {
    colors <- snowColors()
    pump.data <- cholera::ortho.proj.pump
    pump.segments <- pump.data$road.segment

    mat <- matrix(0, ncol = ncol(road.segments ),
      nrow = 2 * length(pump.segments))
    road.pump.data <- data.frame(mat)
    start.pt <- seq(1, nrow(road.pump.data), 2)
    end.pt <- seq(2, nrow(road.pump.data), 2)

    for (i in seq_along(pump.segments)) {
      road.data <- road.segments [road.segments $id == pump.segments[i], ]
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

    road.segments <- road.segments[road.segments$id %in%
      pump.segments == FALSE, ]
    out <- rbind(road.segments, road.pump.data)
    out <- out[order(out$street, out$id), ]
    out$node1 <- paste0(out$x1, "-", out$y1)
    out$node2 <- paste0(out$x2, "-", out$y2)
    out$d <- sqrt((out$x1 - out$x2)^2 + (out$y1 - out$y2)^2)
    road.segments <- out

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

  if (is.null(statistic)) {
    sel <- cholera::fatalities.address$anchor.case
    ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
    case <- split(ortho, ortho$case)
  } else {
    if (statistic == "addresses") {
      sel <- cholera::fatalities.address$anchor.case
      ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
      case <- split(ortho, ortho$case)
    }

    if (statistic == "fatalities") {
      sel <- cholera::fatalities.unstacked$case
      ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
      case <- split(ortho, ortho$case)
    }
  }

  # integrate case into road network

  case.road.segments <- lapply(case, function(x) {
    seg <- unlist(strsplit(road.segments$id, "a"))
    seg <- unlist(strsplit(seg, "b"))
    temp <- road.segments[which(x$road.segment == seg), ]
    case.coord <- x[, c("x.proj", "y.proj")]

    # case is on street with a well so nrow(temp) > 1
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

    road.segments2 <- road.segments
    rbind(road.segments2[road.segments2$id != x$road.segment, ], appended)
  })

  g <- lapply(case.road.segments, function(x) {
    edge.list <- x[, c("node1", "node2")]
    igraph::graph_from_data_frame(edge.list, directed = FALSE)
  })

  case.node <- vapply(seq_along(case), function(i) {
    case.coord <- paste0(ortho[i, "x.proj"], "-", ortho[i, "y.proj"])
    which(igraph::V(g[[i]])$name == case.coord)
  }, numeric(1L))

  pump.nodes <- lapply(g, function(graph) {
    vapply(select.pumps, function(pump) {
      which(igraph::V(graph)$name == pump)
    }, numeric(1L))
  })

  nearest.pump.data <- lapply(seq_along(case), function(i) {
    if (weighted) {
      wts <- case.road.segments[[i]]$d
      d <- unname(igraph::distances(g[[i]], case.node[[i]], pump.nodes[[i]],
        weights = wts))
    } else {
      d <- unname(igraph::distances(g[[i]], case.node[[i]], pump.nodes[[i]]))
    }

    d <- data.frame(d)
    names(d) <- pump.names
    pump.coordinates[names(pump.coordinates) == names(which.min(d))]
  })

  pump.id <- names(unlist(nearest.pump.data))

  nearest.pump.node <- vapply(seq_along(pump.id), function(i) {
    pump.nodes[[i]][pump.id[i]]
  }, numeric(1L))

  pump.census <- as.numeric(substr(pump.id, 2, nchar(pump.id)))
  nearest.pump <- unname(select.pumps[pump.census])

  wtd.paths <- lapply(seq_along(g), function(i) {
    igraph::shortest_paths(g[[i]], case.node[i], nearest.pump.node[i],
      weights = case.road.segments[[i]]$d)$vpath
  })

  paths <- lapply(seq_along(g), function(i) {
    igraph::shortest_paths(g[[i]], case.node[i], nearest.pump.node[i])$vpath
  })

  census <- table(pump.census)

  if (weighted) {
    path.data <- lapply(wtd.paths, function(x) {
      nodes <- do.call(rbind, strsplit(names(unlist(x)), "-"))
      data.frame(x = as.numeric(nodes[, 1]), y = as.numeric(nodes[, 2]))
    })
  } else {
    path.data <- lapply(paths, function(x) {
      nodes <- do.call(rbind, strsplit(names(unlist(x)), "-"))
      data.frame(x = as.numeric(nodes[, 1]), y = as.numeric(nodes[, 2]))
    })
  }

  plot(cholera::fatalities[, c("x", "y")], xlim = range(cholera::roads$x),
    ylim = range(cholera::roads$y), pch = NA, asp = 1)

  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(border.list, lines))

  if (is.null(statistic)) {
    points(cholera::fatalities.address[, c("x", "y")], pch = 20, cex = 0.75,
      col = colors[pump.census])
  }

  if (is.null(selection)) {
    if (vestry) {
      if (is.null(statistic)) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 2, col = colors)
        text(cholera::pumps.vestry[, c("x", "y")],
          label = cholera::pumps.vestry$id, pos = 1)
      } else {
        obs <- as.numeric(names(census))
        text(cholera::pumps.vestry[obs, c("x", "y")], cex = 1.25,
          label = census, col = colors[obs], font = 2)
      }
    } else {
      if (is.null(statistic)) {
        points(cholera::pumps[, c("x", "y")], pch = 2, col = colors)
        text(cholera::pumps[, c("x", "y")], label = cholera::pumps$id, pos = 1)
      } else {
        obs <- as.numeric(names(census))
        text(cholera::pumps.vestry[obs, c("x", "y")], cex = 1.25,
          label = census, col = colors[obs], font = 2)
      }
    }
  } else {
    if (vestry) {
      if (is.null(statistic)) {
        sel.pumps <- as.numeric(substr(pump.names, 2, nchar(pump.names)))
        points(cholera::pumps.vestry[sel.pumps, c("x", "y")], pch = 2,
          col = colors[sel.pumps])
        text(cholera::pumps.vestry[sel.pumps, c("x", "y")],
          label = cholera::pumps$id[sel.pumps], pos = 1)
      } else {
        obs <- as.numeric(names(census))
        text(cholera::pumps.vestry[obs, c("x", "y")], cex = 1.25,
          label = census, col = colors[obs], font = 2)
      }
    } else {
      if (is.null(statistic)) {
        sel.pumps <- as.numeric(substr(pump.names, 2, nchar(pump.names)))
        points(cholera::pumps[sel.pumps, c("x", "y")], pch = 2,
          col = colors[sel.pumps])
        text(cholera::pumps[sel.pumps, c("x", "y")],
          label = cholera::pumps.vestry$id[sel.pumps], pos = 1)
      } else {
        obs <- as.numeric(names(census))
        text(cholera::pumps.vestry[obs, c("x", "y")], cex = 1.25,
          label = census, col = colors[obs], font = 2)
      }
    }
  }

  if (is.null(statistic)) {
    invisible(lapply(seq_along(path.data), function(i) {
      dat <- path.data[[i]]
      n1 <- dat[1:(nrow(dat) - 1), ]
      n2 <- dat[2:nrow(dat), ]
      segments(n1$x, n1$y, n2$x, n2$y, col = colors[pump.census][i], lwd = 2)
    }))
  } else {
    invisible(lapply(seq_along(path.data), function(i) {
      dat <- path.data[[i]]
      n1 <- dat[1:(nrow(dat) - 1), ]
      n2 <- dat[2:nrow(dat), ]
      segments(n1$x, n1$y, n2$x, n2$y, col = colors[pump.census][i])
    }))
  }

  title(main = "Observed Walking Path Neighborhoods")

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
