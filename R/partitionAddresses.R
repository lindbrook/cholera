#' Partiion overlapping fatality addresses.
#'
#' Based on set of subgraphs defined by selected inter-point distance.
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An R list.
#' @noRd

partitionAddresses <- function(inter.point.dist = 0.15) {
  g <- thresholdAddressGraph(inter.point.dist = inter.point.dist)
  subgraphs <- igraph::decompose(g)
  names(subgraphs) <- seq_along(subgraphs)
  census <- igraph::groups(igraph::components(g))
  census.ct <- vapply(census, length, integer(1L))

  ## dyads ##

  dyads <- do.call(rbind, lapply(census[census.ct == 2], as.numeric))
  dyads <- data.frame(group = as.numeric(row.names(dyads)), v1 = dyads[, 1],
    v2 = dyads[, 2], row.names = NULL)

  ## triads ##

  triads <- openTriadAddresses(subgraphs, census, census.ct)

  ## tetrads ##

  string.four <- c("5", "10", "12", "20")
  tetrad.string <- partitionEvenString(subgraphs[string.four])
  tetrad.string <- do.call(rbind, tetrad.string)
  row.names(tetrad.string) <- NULL

  # star graph: 1 central, 3 peripheral vertices
  star.four <- c("25", "45")
  tetrad.stars <- starGraph(subgraphs, star.four)

  # one complete graph all four vertices connect
  complete.four <- t(names(igraph::V(subgraphs[["19"]])))
  complete.four <- stats::setNames(data.frame(complete.four), paste0("v", 1:4))

  ## pentads: five-in-a-row string ##

  string.five <- subgraphs[names(census[census.ct == 5])]
  pentads <- partitionOddString(string.five)

  ## sextad: 6-in-a-row strings ##

  string.six <- subgraphs[names(census[census.ct == 6])]
  sextads <- partitionEvenString(string.six)
  sextads <- do.call(rbind, sextads)
  row.names(sextads) <- NULL

  ## Group 1: 7 vertices (triadic) ##

  septad <- list(v1 = c("507", "553", "348"),
                 v2 = c("38", "1"),
                 v3 = c("552", "393"))

  ## Group 15: 10 vertices ##

  decad <- list(v1 = c("224", "481", "213", "68"),
                v2 = c("49", "325", "428", "296"),
                v3 = c("449", "45"))

  symmetric <- rbind(dyads[, -1], triads, tetrad.string, tetrad.stars, pentads,
    sextads)

  dat <- cbind(symmetric[1:35, ],
    stats::setNames(symmetric[36:70, ], c("v3", "v4")))

  dat <- rbind(dat, complete.four)

  lst <- c(dat)

  lst$v1 <- c(lst$v1, decad$v1)
  lst$v2 <- c(lst$v2, decad$v2)
  lst$v3 <- c(lst$v3, decad$v3)

  lst$v4 <- c(lst$v4, septad$v1)
  lst$v3 <- c(lst$v3, septad$v2)
  lst$v1 <- c(lst$v1, septad$v3)

  lst$v2 <- c(lst$v2, symmetric[71, "v1"])
  lst$v3 <- c(lst$v3, symmetric[71, "v2"])

  # add addresses with distances above inter-point threshold
  lst$v5 <- paste(setdiff(cholera::fatalities.address$anchor, unlist(lst)))
  lst
}

#' Network graph of addresses with selected inter-point distance.
#'
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An 'igraph' object.
#' @noRd

thresholdAddressGraph <- function(inter.point.dist = 0.15) {
  idx <- index0(cholera::fatalities.address$anchor)
  d <- stats::dist(cholera::fatalities.address[, c("x", "y")])
  addr.dist <- data.frame(idx, d = c(d))
  overlap <- addr.dist[addr.dist$d <= inter.point.dist, ]
  edge.list <- overlap[, c("v1", "v2")]
  igraph::graph_from_data_frame(edge.list, directed = FALSE)
}

#' Rotate, stack and partition open triads.
#'
#' @param subgraphs Object. 'igraph' list of graphs.
#' @param census Object. List of graph vertices.
#' @param census.ct Object. Count of graph vertices.
#' @return An R data frame.
#' @noRd

openTriadAddresses <- function(subgraphs, census, census.ct) {
  dat <- subgraphs[names(census[census.ct == 3])]

  triads <- lapply(names(dat), function(nm) {
    v <- as.numeric(igraph::as_edgelist(subgraphs[[nm]]))
    v.table <- table(v)
    pivot <- as.numeric(names(v.table[which.max(v.table)]))
    others <- setdiff(as.numeric(names(v.table)), pivot)
    list(pivot = pivot, others = others)
  })

  even <- triads[seq_along(triads) %% 2 == 0]
  odd <- triads[seq_along(triads) %% 2 == 1]

  v1 <- c(unlist(lapply(even, function(x) x$others)),
          unlist(lapply(odd, function(x) x$pivot)))
  v2 <- c(unlist(lapply(odd, function(x) x$others)),
          unlist(lapply(even, function(x) x$pivot)))
  data.frame(v1, v2)
}

#' Decompose and partition star graphs.
#'
#' Used for graphs with 1 core and 3 peripheral (4) vertices.
#' @param subgraphs Object. 'igraph' list of graphs.
#' @param group Character. Vector of group IDs.
#' @return An R data frame.
#' @noRd

starGraph <- function(subgraphs, group = c("25", "45")) {
  dat <- subgraphs[group]
  vertices <- lapply(dat, function(x) {
    v <- as.numeric(igraph::as_edgelist(x))
    v.table <- table(v)
    core <- as.numeric(names(v.table[which.max(v.table)]))
    periphery <- setdiff(as.numeric(names(v.table)), core)
    list(core = core, periphery = periphery)
  })
  v1 <- c(vertices[[1]]$periphery, vertices[[2]]$core)
  v2 <- c(vertices[[1]]$core, vertices[[2]]$periphery)
  data.frame(v1, v2)
}

#' Rotate, stack and partition even n-tuple strings (n >= 4).
#'
#' Separate even and odd vertices to avoid point overlap.
#' @param g Object. 'igraph' list of graphs.
#' @return An R data frame.
#' @noRd

partitionEvenString <- function(g) {
  edge.list.ordered <- lapply(g, function(x) {
    edg.lst <- igraph::as_edgelist(x)
    edg.lst <- data.frame(v1 = as.numeric(edg.lst[, 1]),
                          v2 = as.numeric(edg.lst[, 2]))

    v.table <- table(unlist(edg.lst))
    endpt <- as.numeric(names(v.table[v.table == 1]))[1]

    row.id <- vector("integer", nrow(edg.lst))
    link.id <- vector("integer", nrow(edg.lst))

    for (i in seq_along(row.id)) {
      if (i == 1) {
        endpt.id <- vapply(seq_along(edg.lst$v1), function(i) {
          any(endpt %in% edg.lst[i, ])
        }, logical(1L))

        sel <- which(endpt.id)
        row.id[1] <- sel
        alpha <- edg.lst[sel, ]
        link.id[1] <- alpha[alpha != endpt]

      } else {
        id <- row.id[row.id != 0]
        id <- id[length(id)]
        link <- link.id[link.id != 0]
        link <- link[length(link)]

        candidate <- which(vapply(seq_along(edg.lst$v1), function(i) {
          any(link %in% edg.lst[i, ])
        }, logical(1L)))

        new.id <- setdiff(candidate, id)
        row.id[i] <- new.id
        tmp <- edg.lst[new.id, ]
        link.id[i] <- tmp[tmp != link]
      }
    }
    list(edge.list = edg.lst[row.id, ], vertices = c(endpt, link.id))
  })

  names(edge.list.ordered) <- names(g)

  lapply(edge.list.ordered, function(x) {
    dat <- x$vertices
    data.frame(v1 = dat[seq_along(dat) %% 2 != 0],
               v2 = dat[seq_along(dat) %% 2 == 0])
  })
}

#' Rotate, stack and partition odd n-tuple strings (n >= 5).
#'
#' @param subgraphs Object. 'igraph' list of graphs.
#' @param group Character. Vector of group IDs.
#' @return An R data frame.
#' @noRd

partitionOddString <- function(subgraphs, group = c("6", "7")) {
  dat <- subgraphs[group]

  vertices <- lapply(dat, function(x) {
    edg.lst <- igraph::as_edgelist(x)
    edg.lst <- data.frame(v1 = as.numeric(edg.lst[, 1]),
                          v2 = as.numeric(edg.lst[, 2]))
    v.table <- table(unlist(edg.lst))
    endpt <- as.numeric(names(v.table[v.table == 1]))[1]
    row.id <- vector("integer", nrow(edg.lst))
    link.id <- vector("integer", nrow(edg.lst))
    for (i in seq_along(row.id)) {
      if (i == 1) {
        endpt.id <- vapply(seq_along(edg.lst$v1), function(i) {
          any(endpt %in% edg.lst[i, ])
        }, logical(1L))
        sel <- which(endpt.id)
        row.id[1] <- sel
        alpha <- edg.lst[sel, ]
        link.id[1] <- alpha[alpha != endpt]
      } else {
        id <- row.id[row.id != 0]
        id <- id[length(id)]
        link <- link.id[link.id != 0]
        link <- link[length(link)]
        candidate <- which(vapply(seq_along(edg.lst$v1), function(i) {
          any(link %in% edg.lst[i, ])
        }, logical(1L)))
        new.id <- setdiff(candidate, id)
        row.id[i] <- new.id
        tmp <- edg.lst[new.id, ]
        link.id[i] <- tmp[tmp != link]
      }
    }
    c(endpt, link.id)
  })

  even <- lapply(vertices, function(x) x[seq_along(x) %% 2 == 0])
  odd  <- lapply(vertices, function(x) x[seq_along(x) %% 2 == 1])
  v1 <- c(odd[[1]], even[[2]])
  v2 <- c(even[[1]], odd[[2]])
  data.frame(v1, v2)
}

#' Create PDFs of fatality address partitions (prototype).
#'
#' For georeferencing in QGIS.
#' @param path Character. e.g., "~/Documents/Data/".
#' @param pch Numeric or Character.
#' @noRd

partitionAddressesPDF <- function(path, pch = 46) {
  pts <- partitionAddresses()
  rng <- mapRange()
  pre <- "address."
  post <- ".pdf"
  invisible(lapply(names(pts), function(nm) {
    file.nm <- paste0(path, pre, nm, post)
    sel <- cholera::fatalities.address$anchor %in% pts[[nm]]
    dat <- cholera::fatalities.address[sel, c("x", "y")]
    grDevices::pdf(file = file.nm)
    plot(dat, pch = pch, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
      xlim = rng$x, ylim = rng$y, bty = "n", asp = 1)
    grDevices::dev.off()
  }))
}
