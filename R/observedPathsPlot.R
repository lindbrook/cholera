#' Plot observed shortest walking path.
#'
#' Plots the shortest walking path between observed addressses and their nearest pump.
#' @param weighted Logical. Shortest path weighted by road distance.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param selection Numeric. Default is NULL and all pumps are used. Ortherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @return A base R graphics plot.
#' @seealso
#' \code{fatalities.address}
#' @import graphics
#' @export
#' @examples
#' # observedPathsPlot()
#' # observedPathsPlot(selection = -7) # exclude pump 7
#' # observedPathsPlot(selection = 6)  # only consider pump 6

observedPathsPlot <- function(weighted = TRUE, vestry = FALSE,
  selection = NULL) {

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

  if (!vestry) {
    colors <- snowColors()
    pump.coordinates <- paste0(cholera::ortho.proj.pump$x.proj, "-",
                               cholera::ortho.proj.pump$y.proj)
    names(pump.coordinates) <- paste0("p", seq_along(pump.coordinates))

    pump.data <- cholera::ortho.proj.pump
    pump.segments <- pump.data$road.segment

    mat <- matrix(0, ncol = ncol(road.segments), nrow = 2 *
      length(pump.segments))
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

    road.segments <- road.segments[road.segments$id %in%
      pump.segments == FALSE, ]
    road.segments <- rbind(road.segments, road.pump.data)
    order.id <- order(road.segments$street, road.segments$id)
    road.segments <- road.segments[order.id, ]
    road.segments$node1 <- paste0(road.segments$x1, "-", road.segments$y1)
    road.segments$node2 <- paste0(road.segments$x2, "-", road.segments$y2)
    road.segments$d <- sqrt((road.segments$x1 - road.segments$x2)^2 +
                            (road.segments$y1 - road.segments$y2)^2)
  } else {
    colors <- snowColors(vestry = TRUE)
    pump.coordinates <- paste0(cholera::ortho.proj.pump.vestry$x.proj, "-",
                               cholera::ortho.proj.pump.vestry$y.proj)
    names(pump.coordinates) <- paste0("p", seq_along(pump.coordinates))

    pump.data <- cholera::ortho.proj.pump.vestry
    pump.segments <- pump.data$road.segment

    mat <- matrix(0, ncol = ncol(road.segments), nrow = 2 *
      length(pump.segments))
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

    road.segments <- road.segments[road.segments$id %in%
      pump.segments == FALSE, ]
    road.segments <- rbind(road.segments, road.pump.data)
    order.id <- order(road.segments$street, road.segments$id)
    road.segments <- road.segments[order.id, ]
    road.segments$node1 <- paste0(road.segments$x1, "-", road.segments$y1)
    road.segments$node2 <- paste0(road.segments$x2, "-", road.segments$y2)
    road.segments$d <- sqrt((road.segments$x1 - road.segments$x2)^2 +
                            (road.segments$y1 - road.segments$y2)^2)
  }

  if (is.null(selection)) {
    select.pumps <- pump.coordinates
    pump.names <- names(pump.coordinates)
  } else {
    select.pumps <- pump.coordinates[selection]
    pump.names <- names(pump.coordinates[selection])
  }

  ## --------------- Case Data --------------- ##

  x.rng <- range(cholera::roads$x)
  y.rng <- range(cholera::roads$y)

  plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
    pch = NA, asp = 1)
  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(border.list, lines))

  if (is.null(selection)) {
    if (vestry) {
      points(cholera::pumps.vestry[, c("x", "y")], pch = 2, cex = 1,
        col = colors)
      text(cholera::pumps.vestry[, c("x", "y")],
        label = cholera::pumps.vestry$id, pos = 1)
    } else {
      points(cholera::pumps[, c("x", "y")], pch = 2, cex = 1, col = colors)
      text(cholera::pumps[, c("x", "y")], label = cholera::pumps$id, pos = 1)
    }
  } else {
    if (vestry) {
      sel.pumps <- as.numeric(substr(pump.names, 2, nchar(pump.names)))
      points(cholera::pumps.vestry[sel.pumps, c("x", "y")], pch = 2, cex = 1,
        col = colors[sel.pumps])
      text(cholera::pumps.vestry[sel.pumps, c("x", "y")],
        label = cholera::pumps$id[sel.pumps], pos = 1)
    } else {
      sel.pumps <- as.numeric(substr(pump.names, 2, nchar(pump.names)))
      points(cholera::pumps[sel.pumps, c("x", "y")], pch = 2, cex = 1,
        col = colors[sel.pumps])
      text(cholera::pumps[sel.pumps, c("x", "y")],
        label = cholera::pumps.vestry$id[sel.pumps], pos = 1)
    }
  }

  addr <- cholera::fatalities.address$anchor.case

  for (i in seq_along(addr)) {
    case <- caseSelector(addr[i])
    seg <- unlist(strsplit(road.segments$id, "a"))
    seg <- unlist(strsplit(seg, "b"))
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
    case.road.segments <- rbind(road.segments2[sel, ], appended)

    g <- caseNetwork(case.road.segments)

    case.coord <- paste0(case$x.proj, "-", case$y.proj)
    case.node <- which(igraph::V(g)$name == case.coord)

    pump.nodes <- vapply(seq_along(select.pumps), function(i) {
      which(igraph::V(g)$name == select.pumps[i])
    }, numeric(1L))

    if (weighted) {
      wts <- case.road.segments$d
      d <- unname(igraph::distances(g, case.node, pump.nodes, weights = wts))
    } else {
      d <- unname(igraph::distances(g, case.node, pump.nodes))
    }

    d <- data.frame(d)
    names(d) <- pump.names

    nearest.pump <- names(which.min(d))
    nearest.pump.node <- pump.coordinates[names(pump.coordinates) ==
      nearest.pump]

    if (weighted) {
      case.path <- unlist(igraph::shortest_paths(g, case.node,
        nearest.pump.node, weights = wts)$vpath)
      case.path <- names(case.path)
    } else {
      case.path <- unlist(igraph::shortest_paths(g, case.node,
        nearest.pump.node)$vpath)
      case.path <- names(case.path)
    }

    dat <- numericNodeCoordinates(case.path)
    n1 <- dat[1:(nrow(dat) - 1), ]
    n2 <- dat[2:nrow(dat), ]
    nearest.pump.id <- as.numeric(substr(nearest.pump, 2, nchar(nearest.pump)))
    case.color <- colors[nearest.pump.id]
    points(cholera::fatalities.address[ i,
      c("x", "y")], pch = 20, col = colors[nearest.pump.id], cex = 0.75)
    segments(n1$x, n1$y, n2$x, n2$y, col = colors[nearest.pump.id], lwd = 2)
  }

  title(main = "Observed Addresses")
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

caseNetwork <- function(x) {
  edge.list <- x[, c("node1", "node2")]
  igraph::graph_from_data_frame(edge.list, directed = FALSE)
}

numericNodeCoordinates <- function(x) {
  nodes <- do.call(rbind, (strsplit(x, "-")))
  data.frame(x = as.numeric(nodes[, 1]), y = as.numeric(nodes[, 2]))
}
