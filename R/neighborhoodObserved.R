#' Summary statistics for observed walking neighborhoods.
#'
#' Neighborhoods are based on the shortest paths between a fatality's address and its nearest pump.
#'
#' @param pump.select Numeric. Default is NULL; all pumps are used. Otherwise, pump.select is a vector of numeric IDs that ranges from 1 to 13 for \code{pumps} or 1 to 14 for \code{pumps.vestry}. This determines which pumps are used to compute neighborhoods.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE uses shortest path weighted by road distance.
#' @param statistic Character. "address" uses the count of address at pumps' locations. "fatality" plots the total count of fatality at pumps' locations.
#' @return A data frame.
#' @seealso  \code{vignette}("walking.neighborhoods")
#' @export
#' @examples
#' # neighborhoodObservedCensus()
#' # neighborhoodObservedCensus(6:7)

neighborhoodObservedCensus <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, statistic = "address") {

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

  if (all(statistic %in% c("address", "fatality")) == FALSE) {
    text.a <- 'If specified, "statistic" must either be "address"'
    text.b <- 'or "fatality".'
    stop(paste(text.a, text.b))
  }

  # integrate pumps into road network

  if (vestry) {
    pump.road.segments <- pumpIntegrator(cholera::ortho.proj.pump.vestry)
    pump.coordinates <- pumpCoordinates(vestry = TRUE)

  } else {
    pump.road.segments <- pumpIntegrator()
    pump.coordinates <- pumpCoordinates()
  }

  if (is.null(pump.select)) {
    select.pumps <- pump.coordinates
    pump.names <- names(pump.coordinates)
  } else {
    select.pumps <- pump.coordinates[pump.select]
    pump.names <- names(pump.coordinates[pump.select])
  }

  if (statistic == "address") {
    sel <- cholera::fatalities.address$anchor.case
    ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
    case <- split(ortho, ortho$case)
  } else if (statistic == "fatality") {
    sel <- cholera::fatalities.unstacked$case
    ortho <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
    case <- split(ortho, ortho$case)
  }

  # integrate case into road network

  case.pump.road.segments <- lapply(case, function(x)
    caseIntegrator(x, pump.road.segments))

  edge.list <- case.pump.road.segments

  g <- lapply(edge.list, function(x) {
    edges <- x[, c("node1", "node2")]
    igraph::graph_from_data_frame(edges, directed = FALSE)
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
      wts <- case.pump.road.segments[[i]]$d
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

  census <- unclass(table(pump.census))

  data.frame(pump.id = as.numeric(names(census)),
             Count = census,
             Precent = round(100 * census / sum(census), 2))
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
    temp <- cbind(temp[, c("street", "id", "name")], pump.coords,
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

pathData <- function(x) {
  nodes <- do.call(rbind, strsplit(names(unlist(x)), "-"))
  data.frame(x = as.numeric(nodes[, 1]), y = as.numeric(nodes[, 2]))
}
