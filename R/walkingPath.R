#' Plot the walking path from a case to the nearest selected pump(s).
#'
#' Plot shortest walking path from an observed or "simulated" case to the nearest selected pump(s).
#' @param x Numeric or Integer. Observed cases must be a whole number between 1 and 578. With three exceptions, "Simulated" cases must be a whole number between 1 and 4993: 1) one case, 3334, does not have a valid orthogonal projector to any street; 2) the 20 cases that project onto Falconberg Court and Falconberg Mews (4427, 4428, 4499, 4500, 4501, 4570, 4571, 4572, 4573, 4574, 4643, 4644, 4645, 4646, 4647, 4716, 4717, 4718, 4719, 4720) form an isolate that are "technically" disconnected from the road network and cannot reach any pump; 3) because Adam and Eve Court is also disconnected from the larger road network, the 27 cases that project onto that road can only reach pump 2, which lies on that road. This means that all other cases cannot reach pump 2.
#' @param observed Logical. TRUE for observed cases; FALSE for "regular" simulated cases.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param weighted Logical. Shortest path weighted by road distance.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param pump.select Numeric. Default is NULL and all pumps are considered. Otherwise, selection is done by a vector of numeric IDs: 1 to 13 for \code{link{pumps}}; 1 to 14 for \code{\link{pumps.vestry}}.
#' @param unit Character. Default is NULL, which returns the graph's unit scale. "yard" returns the approximate distance in yards. "meter" returns the approximate distance in meters. Either implies "weighted" is TRUE.
#' @return A base R graphics plot.
#' @seealso \code{\link{fatalities}}, \code{\link{simulateFatalities}}, \code{vignette("pump.neighborhoods")}
#' @import graphics
#' @export
#' @examples
#' walkingPath(1)
#' walkingPath(1, observed = FALSE)
#' walkingPath(1, pump.select = -7) # exclude pump 7 from consideration.
#' walkingPath(1, pump.select = 6)  # path from case 1 to pump 6.

walkingPath <- function(x, observed = TRUE, zoom = FALSE, radius = 0.5,
  weighted = TRUE, vestry = FALSE, pump.select = NULL, unit = NULL) {

  if (observed) {
    if (x %in% 1:578 == FALSE) {
     stop('With observed cases, x must be between 1 and 578.')
    }
  } else {
    edge.case <- 3334
    falconberg <- c(4427, 4428, 4499, 4500, 4501, 4570, 4571, 4572, 4573, 4574,
      4643, 4644, 4645, 4646, 4647, 4716, 4717, 4718, 4719, 4720)
    excluded <- c(edge.case, falconberg)

    if (x %in% (1:4993)[-excluded] == FALSE) {
      msg1 <- "With simulated cases,"
      msg2 <- "x must be a valid number between 1 and 4993."
      stop(paste(msg1, msg2))
    }
  }

  if (vestry) {
    if (is.null(pump.select) == FALSE) {
      if (any(abs(pump.select) %in% 1:14 == FALSE)) {
        stop('With "vestry = TRUE", "pump.select" must be between 1 and 14.')
      }
    }
  } else {
    if (is.null(pump.select) == FALSE ) {
      if (any(abs(pump.select) %in% 1:13 == FALSE)) {
        stop('With "vestry = FALSE", "pump.select" must be between 1 and 13.')
      }
    }
  }

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  road.segments <- lapply(unique(rd$street), function(i) {
    dat <- rd[rd$street == i, ]
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
    pump.data <- pumpDataSelect(vestry = TRUE)
    road.segments <- addSegmentLength(road.segments, pump.data)
    pump.coordinates <- extractPumpCoordinates(pump.data)
  } else {
    colors <- snowColors()
    pump.data <- pumpDataSelect()
    road.segments <- addSegmentLength(road.segments, pump.data)
    pump.coordinates <- extractPumpCoordinates(pump.data)
  }

  if (is.null(pump.select)) {
    select.pumps <- pump.coordinates
    pump.names <- names(pump.coordinates)
  } else {
    select.pumps <- pump.coordinates[pump.select]
    pump.names <- names(pump.coordinates[pump.select])
  }

  ## --------------- Case Data --------------- ##

  if (observed == TRUE) {
    case <- caseSelector(x)
  } else {
    case <- caseSelector(x, FALSE)
  }

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

  wts <- case.road.segments$d
  wtd.d <- unname(igraph::distances(g, case.node, pump.nodes, weights = wts))
  wtd.d <- data.frame(wtd.d)
  names(wtd.d) <- pump.names

  unwtd.d <- unname(igraph::distances(g, case.node, pump.nodes))
  unwtd.d <- data.frame(unwtd.d)
  names(unwtd.d) <- pump.names

  wtd.test <- wtd.d == min(wtd.d)
  unwtd.test <- unwtd.d == min(unwtd.d)

  if (weighted | is.null(unit) == FALSE) {
    if (sum(wtd.test) > 1) {
      # randomly select among ties; note when x of sample(x, size) equals 1
      nearest.pump <- names(wtd.d[sample(which(wtd.test), 1)])
    } else {
      nearest.pump <- names(which.min(wtd.d))
    }
  } else {
    if (sum(unwtd.test) > 1) {
      nearest.pump <- names(unwtd.d[sample(which(unwtd.test), 1)])
    } else {
      nearest.pump <- names(which.min(unwtd.d))
    }
  }

  nearest.pump.node <- pump.coordinates[names(pump.coordinates) == nearest.pump]

  if (weighted | is.null(unit) == FALSE) {
    case.path <- names(unlist(igraph::shortest_paths(g, case.node,
      nearest.pump.node, weights = wts)$vpath))
  } else {
    case.path <- names(unlist(igraph::shortest_paths(g, case.node,
      nearest.pump.node)$vpath))
  }

  dat <- numericNodeCoordinates(case.path)
  n1 <- dat[1:(nrow(dat) - 1), ]
  n2 <- dat[2:nrow(dat), ]

  if (zoom) {
    x.rng <- c(min(dat$x) - radius, max(dat$x) + radius)
    y.rng <- c(min(dat$y) - radius, max(dat$y) + radius)
  } else {
    x.rng <- range(cholera::roads$x)
    y.rng <- range(cholera::roads$y)
  }

  nearest.pump.id <- as.numeric(substr(nearest.pump, 2, nchar(nearest.pump)))
  case.color <- colors[nearest.pump.id]

  if (observed == TRUE) {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      xlab = "x", ylab = "y", pch = 15, cex = 0.5, col = "lightgray", asp = 1)
    invisible(lapply(roads.list, lines, col = "lightgray"))
    invisible(lapply(border.list, lines))
    title(main = paste("Observed Case #", case$case))

    if (is.null(pump.select)) {
      if (vestry) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 24, cex = 1,
          col = colors)
        text(cholera::pumps.vestry[, c("x", "y")], label = pump.names, pos = 1)
      } else {
        points(cholera::pumps[, c("x", "y")], pch = 24, cex = 1, col = colors)
        text(cholera::pumps[, c("x", "y")], label = pump.names, pos = 1)
      }
    } else {
      if (vestry) {
        sel.pumps <- as.numeric(substr(pump.names, 2, nchar(pump.names)))
        points(cholera::pumps.vestry[sel.pumps, c("x", "y")], pch = 24, cex = 1,
          col = colors[sel.pumps])
        text(cholera::pumps.vestry[sel.pumps, c("x", "y")], label = pump.names,
          pos = 1)
      } else {
        sel.pumps <- as.numeric(substr(pump.names, 2, nchar(pump.names)))
        points(cholera::pumps[sel.pumps, c("x", "y")], pch = 24, cex = 1,
          col = colors[sel.pumps])
        text(cholera::pumps[sel.pumps, c("x", "y")], label = pump.names,
          pos = 1)
      }
    }

    if (zoom) {
      points(cholera::fatalities[cholera::fatalities$case == case$case,
        c("x", "y")],col = "red")
      text(cholera::fatalities[cholera::fatalities$case == case$case,
        c("x", "y")], labels = case$case, pos = 1, col = "red")
      points(dat[1, c("x", "y")], col = case.color, pch = 0)
      points(dat[nrow(dat), c("x", "y")], col = case.color, pch = 0)
    } else {
      points(cholera::fatalities[cholera::fatalities$case == case$case,
        c("x", "y")], col = "red")
      points(dat[1, c("x", "y")], col = case.color, pch = 0)
      points(dat[nrow(dat), c("x", "y")], col = case.color, pch = 0)
    }

  } else {
    plot(cholera::sim.ortho.proj[, c("x.proj", "y.proj")], xlim = x.rng,
      ylim = y.rng, xlab = "x", ylab = "y", pch = NA, asp = 1)
    invisible(lapply(roads.list, lines, col = "lightgray"))
    invisible(lapply(border.list, lines))
    title(main = paste('"Simulated" Case #', case$case))

    if (is.null(pump.select)) {
      if (vestry) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 24, cex = 1,
          col = colors)
        text(cholera::pumps.vestry[, c("x", "y")], label = pump.names, pos = 1)
      } else {
        points(cholera::pumps[, c("x", "y")], pch = 24, cex = 1, col = colors)
        text(cholera::pumps[, c("x", "y")], label = pump.names, pos = 1)
      }
    } else {
      if (vestry) {
        sel.pumps <- as.numeric(substr(pump.names, 2, nchar(pump.names)))
        points(cholera::pumps.vestry[sel.pumps, c("x", "y")], pch = 24, cex = 1,
          col = colors[sel.pumps])
        text(cholera::pumps.vestry[sel.pumps, c("x", "y")], label = pump.names,
          pos = 1)
      } else {
        sel.pumps <- as.numeric(substr(pump.names, 2, nchar(pump.names)))
        points(cholera::pumps[sel.pumps, c("x", "y")], pch = 24, cex = 1,
          col = colors[sel.pumps])
        text(cholera::pumps[sel.pumps, c("x", "y")], label = pump.names,
          pos = 1)
      }
    }

    if (zoom) {
      points(cholera::regular.cases[case$case, c("x", "y")], col = "red")
      text(cholera::regular.cases[case$case, c("x", "y")], labels = case$case,
        pos = 1, col = "red")
      points(dat[1, c("x", "y")], col = case.color, pch = 0)
      points(dat[nrow(dat), c("x", "y")], col = case.color, pch = 0)
    } else {
      points(cholera::regular.cases[case$case, c("x", "y")],
        col = "red")
      points(dat[1, c("x", "y")], col = case.color, pch = 0)
      points(dat[nrow(dat), c("x", "y")], col = case.color, pch = 0)
    }
  }

  if (is.null(unit)) {
    if (weighted) {
      title(sub = paste("Distance =", round(min(wtd.d), 1), "units"))
    } else {
      title(sub = paste("Distance =", round(min(unwtd.d), 1), "nodes"))
    }
  } else {
    if (unit == "yard") {
      title(sub = paste("Distance =", round(min(wtd.d) * 177 / 3, 1), "yards"))
    }
    if (unit == "meter") {
      title(sub = paste("Distance =", round(min(wtd.d) * 54, 1), "meters"))
    }
  }

  if (zoom) {
    arrows(n1$x, n1$y, n2$x, n2$y, col = case.color, lwd = 2, length = 0.1)
  } else {
    sel <- seq_len(nrow(dat))
    med <- round(stats::median(sel))
    selA <- sel[-med]
    selB <- sel[med]
    segments(n1$x[selA], n1$y[selA], n2$x[selA], n2$y[selA],
      col = colors[nearest.pump.id], lwd = 2)
    arrows(n1$x[selB], n1$y[selB], n2$x[selB], n2$y[selB],
      col = case.color, lwd = 2, length = 0.075)
  }
}

caseSelector <- function(x, observed = TRUE) {
  if (observed) {
    case <- cholera::ortho.proj[cholera::ortho.proj$case == x, ]
  } else {
    case <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case == x, ]
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

addSegmentLength <- function(road.segments, pump.data) {
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

  out <- road.segments[road.segments$id %in% pump.segments == FALSE, ]
  out <- rbind(out, road.pump.data)
  order.id <- order(out$street, out$id)
  out <- out[order.id, ]
  out$node1 <- paste0(out$x1, "-", out$y1)
  out$node2 <- paste0(out$x2, "-", out$y2)
  out$d <- sqrt((out$x1 - out$x2)^2 + (out$y1 - out$y2)^2)
  out
}

pumpDataSelect <- function(vestry = FALSE) {
  if (vestry) {
    cholera::ortho.proj.pump.vestry
  } else {
    cholera::ortho.proj.pump
  }
}

extractPumpCoordinates <- function(pump.data) {
  pump.coordinates <- paste0(pump.data$x.proj, "-", pump.data$y.proj)
  stats::setNames(pump.coordinates, paste0("p", seq_along(pump.coordinates)))
}
