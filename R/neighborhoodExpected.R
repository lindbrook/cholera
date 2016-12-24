#' Summary statistics for expected walking neighborhoods.
#'
#' Neighborhoods are based on the shortest paths between a fatality's address and its nearest pump.
#'
#' Currently computationally intensive (On an Intel Core i7, streets = TRUE appox. 90 seocnds; streets = FALSE appox. 75 seconds). For better performance, run in parallel with multiple cores in the terminal or in batch mode (appox. 45 and 30 seconds). Note that this is currently unavailable on Windows. See \code{vignette}("walking.neighborhoods") for details.
#' @param pump.select Numeric. Default is NULL; all pumps are used. Otherwise, pump.select by a vector of numeric IDs: from 1 to 13 for \code{pumps}; from 1 to 14 for \code{pumps.vestry}
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param weighted Logical. TRUE uses shortest path weighted by road distance.
#' @param multi.core Logical. TRUE uses parallel::mclapply and parallel::detectCores(). Note that this should NOT be used in the GUI and is not currently available on Windows. See parallel::mclapply() for details. FALSE uses 1 logical core and can be run in GUI and on Windows.
#' @return A data frame.
#' @export
#' @examples
#' # neighborhoodExpectedCensus()
#' # neighborhoodExpectedCensus(streets = FALSE)

neighborhoodExpectedCensus <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, multi.core = FALSE) {

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

  if (multi.core) cores <- parallel::detectCores() else cores <- 1L

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

    if (is.null(pump.select)) {
      pump.nodes <- vapply(pump.coordinates, function(p) {
        which(igraph::V(case.graph)$name == p)
      }, numeric(1L))
    } else {
      pump.nodes <- vapply(pump.coordinates[pump.select], function(p) {
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

adamEveCourt <- function() {
  x <- cholera::roads[cholera::roads$name == "Adam and Eve Court", "street"]
  dat <- do.call(rbind, strsplit(cholera::ortho.proj.sp$road.segment, "-"))[, 1]
  dat <- as.numeric(dat)
  id <- which(dat == unique(x))
  cholera::ortho.proj.sp[id, "case"]
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
