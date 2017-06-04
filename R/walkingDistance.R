#' Compute the walking distance between a case and the nearest (selected) pump.
#'
#' Compute the shortest walking distance between an observed or "simulated" case and the nearest (selected) pump.
#' @param x Numeric or Integer. Observed cases must be a whole number between 1 and 578. With three exceptions, "Simulated" cases must be a whole number between 1 and 4993: 1) one case, 3334, does not have a valid orthogonal projector to any street; 2) the 20 cases that project onto Falconberg Court and Falconberg Mews (4427, 4428, 4499, 4500, 4501, 4570, 4571, 4572, 4573, 4574, 4643, 4644, 4645, 4646, 4647, 4716, 4717, 4718, 4719, 4720) form an isolate that are "technically" disconnected from the road network and cannot reach any pump; 3) because Adam and Eve Court is also disconnected from the larger road network, the 27 cases that project onto that road can only reach pump 2, which lies on that road. This means that all other cases cannot reach pump 2.
#' @param observed Logical. TRUE for observed cases; FALSE for "regular" simulated cases.
#' @param weighted Logical. Shortest path weighted by road distance.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param pump.select Numeric. Default is NULL and all pumps are considered. Otherwise, selection is done by a vector of numeric IDs: 1 to 13 for \code{link{pumps}}; 1 to 14 for \code{\link{pumps.vestry}}.
#' @param unit Character. Default is NULL, which returns the graph's unit scale. "yard" returns the approximate distance in yards. "meter" returns the approximate distance in meters. Either implies "weighted" is TRUE.
#' @return A base R data frame.
#' @seealso \code{\link{fatalities}}, \code{\link{simulateFatalities}}, \code{vignette("pump.neighborhoods")}
#' @import graphics
#' @export
#' @examples
#' walkingDistance(1)
#' walkingDistance(1, observed = FALSE)
#' walkingDistance(1, pump.select = -7) # exclude pump 7 from consideration.
#' walkingDistance(1, pump.select = 6)  # path from case 1 to pump 6.

walkingDistance <- function(x, observed = TRUE, weighted = TRUE,
  vestry = FALSE, pump.select = NULL, unit = NULL) {

  if (observed) {
    if (x %in% 1:578 == FALSE) {
     stop('With observed cases, x must be between 1 and 578.')
    }
  } else {
    # isolates
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
    pump.data <- pumpDataSelect(vestry = TRUE)
    road.segments <- addSegmentLength(road.segments, pump.data)
    pump.coordinates <- extractPumpCoordinates(pump.data)
  } else {
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

  if (weighted) {
    wts <- case.road.segments$d
    d <- c(igraph::distances(g, case.node, pump.nodes, weights = wts))
    names(d) <- pump.names
  } else {
    d <- c(igraph::distances(g, case.node, pump.nodes))
    names(d) <- pump.names
  }

  id <- which.min(d)

  if (is.null(unit)) {
    out <- data.frame(case = x, pump = pump.names[id], distance = d[id])
    rownames(out) <- NULL
    out
  } else if (unit == "yard") {
    out <- data.frame(case = x, pump = pump.names[id],
      distance = round(d[id] * 177 / 3, 1))
    rownames(out) <- NULL
    out
  } else if (unit == "meter") {
    out <- data.frame(case = x, pump = pump.names[id],
      distance = round(d[id] * 54, 1))
    rownames(out) <- NULL
    out
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
