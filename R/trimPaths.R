#' Trim computed walking paths
#'
#' To reduce duplication when plotting, this function computes the unique road segments of all walking paths. Note that only "anchor cases" are used.
#'
#' @param pump.select Default is NULL: all pumps are used. Ortherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param obs Logical. TRUE uses paths of observed cases. FALSE uses paths of simulated cases.
#' @param multi.core Logical. TRUE uses parallel::detectCores(). FALSE uses one, single core.
#' @param save.file Logical. TRUE save output to working directory.
#' @return A list of data frames.
#' @export
#' @examples
#' # trimPaths()
#' # trimPaths(6:7)
#' # trimPaths(-6)
#' # trimPaths(vestry = TRUE)

trimPaths <- function(pump.select = NULL, vestry = FALSE, obs = TRUE,
  multi.core = FALSE, save.file = FALSE) {

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
    pump.coordinates <- pump.coordinates[pump.select]
  }

  if (multi.core) {
    cores <- parallel::detectCores()
  } else {
    cores <- 1L
  }

  if (obs) {
    sel <- cholera::fatalities.address$anchor.case
    sel.cases.data <- cholera::ortho.proj[cholera::ortho.proj$case %in% sel, ]
    sel.cases <- split(sel.cases.data, sel.cases.data$case)
  } else {
    edge.case <- cholera::ortho.proj.sp[is.na(cholera::ortho.proj.sp$x.proj),
      "case"]
    falconberg.id <- pump.road.segments[pump.road.segments$name ==
      "Falconberg Court" | pump.road.segments$name == "Falconberg Mews", "id"]
    falconberg <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$road.segment
      %in% falconberg.id, "case"]
    sel.cases.data <- cholera::ortho.proj.sp[cholera::ortho.proj.sp$case
      %in% c(edge.case, falconberg) == FALSE, ]
    sel.cases <- split(sel.cases.data, sel.cases.data$case)
  }

  case.pump.road.segments <- parallel::mclapply(sel.cases, function(x) {
    caseIntegrator(x, pump.road.segments)
  }, mc.cores = cores)

  g <- parallel::mclapply(case.pump.road.segments, function(x) {
    edge.list <- x[, c("node1", "node2")]
    igraph::graph_from_data_frame(edge.list, directed = FALSE)
  }, mc.cores = cores)

  nearest.pump.data <- parallel::mclapply(seq_along(g), function(x) {
    case <- sel.cases[[x]]
    case.graph <- g[[x]]
    case.coord <- paste0(case$x.proj, "-", case$y.proj)
    case.node <- which(igraph::V(case.graph)$name == case.coord)

    pump.nodes <- vapply(pump.coordinates, function(p) {
      which(igraph::V(case.graph)$name == p)
    }, numeric(1L))

    wts <- case.pump.road.segments[[x]]$d
    unname(igraph::distances(case.graph, case.node, pump.nodes, weights = wts))
  }, mc.cores = cores)

  nearest.pump.data <- data.frame(do.call(rbind, nearest.pump.data))
  names(nearest.pump.data) <- pump.names

  nearest.pump <- vapply(seq_along(g), function(i) {
    dat <- nearest.pump.data[i, ]
    if (all(is.infinite(unlist(dat))) == FALSE) {
      p.name <- names(which.min(dat))
      as.integer(substr(p.name, 2, nchar(p.name)))
    } else {
      NA
    }
  }, integer(1L))

  case.node <- vapply(seq_along(g), function(i) {
    case <- sel.cases[[i]]
    case.coord <- paste0(case$x.proj, "-", case$y.proj)
    which(igraph::V(g[[i]])$name == case.coord)
  }, numeric(1L))

  # pump nodes in a case's graph
  case.pump.nodes <- parallel::mclapply(g, function(graph) {
    vapply(select.pumps, function(pump) {
      which(igraph::V(graph)$name == pump)
    }, numeric(1L))
  }, mc.cores = cores)

  paths <- parallel::mclapply(seq_along(g), function(i) {
    sel <- names(case.pump.nodes[[i]]) == paste0("p", nearest.pump[i])
    pump.node <- case.pump.nodes[[i]][sel]
    igraph::shortest_paths(g[[i]], case.node[i], pump.node,
      weights = case.pump.road.segments[[i]]$d)$vpath
  }, mc.cores = cores)

  neighborhood.paths <- split(paths, nearest.pump)

  # list of vectors of cases for each pump neighborhood
  pump.cases <- lapply(cholera::pumps$id, function(i) {
    sel.cases.data[which(nearest.pump == i), "case"]
  })

  intermediate.segments <- parallel::mclapply(neighborhood.paths,
    intermediateSegments, mc.cores = cores)

  neighborhood <- which(vapply(pump.cases, function(x) length(x) != 0,
    logical(1L)))

  case.segments <- parallel::mclapply(seq_along(neighborhood.paths),
    caseSegments, intermediate.segments, neighborhood, neighborhood.paths,
    paths, pump.cases, sel.cases.data, mc.cores = cores)

  pump.segments <- lapply(seq_along(neighborhood.paths), pumpSegments,
    neighborhood, neighborhood.paths, pump.cases)

  intermediate.segments <- lapply(intermediate.segments, function(x) {
    if (is.null(x)) {
      dat <- NULL
    } else {
      dat <- cholera::road.segments[cholera::road.segments$id %in% x, ]
      dat$trimmed <- FALSE
    }
    dat
  })

  neighborhood.segments <- lapply(seq_along(neighborhood), function(i) {
    rbind(case.segments[[i]], intermediate.segments[[i]], pump.segments[[i]])
  })

  names(neighborhood.segments) <- neighborhood

  neighborhood.segments

  if(save.file) {
    if (obs) {
      save(neighborhood.segments, file = "neighborhood.segments.RData")
      save(pump.cases, file = "pump.cases.RData")
    } else {
      neighborhood.segments.sp <- neighborhood.segments
      pump.cases.sp <- pump.cases
      save(neighborhood.segments.sp, file = "neighborhood.segments.sp.RData")
      save(pump.cases.sp, file = "pump.cases.sp.RData")
    }
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

caseSegments <- function(x, intermediate.segments, neighborhood,
  neighborhood.paths, paths, pump.cases, sel.cases.data, obs = TRUE) {

  n.paths <- neighborhood.paths[[x]]
  n.cases <- pump.cases[[neighborhood[x]]]

  if (obs) {
    obs.seg <- unique(cholera::ortho.proj[cholera::ortho.proj$case %in%
      n.cases, "road.segment"])
  } else {
    obs.seg <- unique(cholera::ortho.proj.sp[cholera::ortho.proj.sp$case %in%
      n.cases, "road.segment"])
  }

  obs.seg <- obs.seg[obs.seg %in% intermediate.segments[[x]] == FALSE]
  pump.seg <- cholera::ortho.proj.pump[cholera::ortho.proj.pump$pump.id ==
    neighborhood[x], "road.segment" ]

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
      p <- names(unlist(paths[[which(sel.cases.data$case == x)]]))
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
    } else if (all(entry2)) {
      d <- vapply(seq_along(st.cases$case), function(i) {
        a <- st.cases[i, c("x.proj", "y.proj")]
        names(a) <- c("x", "y")
        b <- data.frame(x = st$x1, y = st$y1)
        stats::dist(rbind(a, b))
      }, numeric(1L))
      st[, c("x1", "y1")] <- st.cases[which.min(d), c("x.proj", "y.proj")]
      st$trimmed <- TRUE
    } else if (all(entry1 + entry2 == 1)) {
      st
    }
    st
  })

  do.call(rbind, new.segs)
}

pumpSegments <- function(x, neighborhood, neighborhood.paths, pump.cases,
  obs = TRUE) {

  n.paths <- neighborhood.paths[[x]]
  n.cases <- pump.cases[[neighborhood[x]]]

  last.segment <- lapply(n.paths, function(path) {
    p.coords <- names(unlist(path))
    last.nodes <- p.coords[(length(p.coords) - 1):length(p.coords)]
    node.data <- do.call(rbind, strsplit(last.nodes, "-"))
    data.frame(x = as.numeric(node.data[, 1]), y = as.numeric(node.data[, 2]))
  })

  pump.segment.audit <- lapply(last.segment, function(dat) {
    c(which(signif(dat[2, "x"]) == signif(cholera::ortho.proj.pump$x.proj)),
      which(signif(dat[2, "y"]) == signif(cholera::ortho.proj.pump$y.proj)))
  })

  neighborhood.pump <- unique(unlist(pump.segment.audit))

  if (length(neighborhood.pump) == 1) {
    seg <- cholera::ortho.proj.pump[cholera::ortho.proj.pump$pump.id ==
                                      neighborhood.pump, "road.segment"]
    st <- cholera::road.segments[cholera::road.segments$id == seg, ]
  } else {
    stop("Error")
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
