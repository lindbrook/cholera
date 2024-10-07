#' Partiion overlapping road line segment endpoints.
#'
#' Based on set of subgraphs defined by selected inter-point distance.
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An R list.
#' @noRd

partitionRoads <- function(inter.point.dist = 0.15) {
  g <- thresholdRoadGraph(inter.point.dist = inter.point.dist)
  subgraphs <- igraph::decompose(g)
  names(subgraphs) <- seq_along(subgraphs)
  census <- igraph::groups(igraph::components(g))
  census.ct <- vapply(census, length, integer(1L))

  ## dyads ##

  dyads <- do.call(rbind, lapply(census[census.ct == 2], as.numeric))
  dyads <- data.frame(group = as.numeric(row.names(dyads)), v1 = dyads[, 1],
    v2 = dyads[, 2], row.names = NULL)

  ## triads ##

  open.triads <- openTriadRoads(subgraphs, census, census.ct)
  closed.triad <- as.numeric(census$`11`)

  ## tetrads ##

  string.four <- c("42", "46", "50")
  tetrad.string <- partitionEvenString(subgraphs[string.four])
  tetrad.string <- do.call(rbind, tetrad.string)
  row.names(tetrad.string) <- NULL

  triad.tail <- data.frame(v1 = 885, v2 = 886, v3 = 887, v4 = 884)

  ## assembly ##

  symmetric <- rbind(dyads[, -1], open.triads, tetrad.string)

  dat <- cbind(symmetric[1:32, ],
    stats::setNames(symmetric[33:64, ], c("v3", "v4")))

  dat <- rbind(dat, triad.tail)
  lst <- c(dat)

  lst$v1 <- c(lst$v1, closed.triad[1])
  lst$v2 <- c(lst$v2, closed.triad[2])
  lst$v3 <- c(lst$v3, closed.triad[3])

  lst$v1 <- c(lst$v1, symmetric[nrow(symmetric), "v1"])
  lst$v2 <- c(lst$v2, symmetric[nrow(symmetric), "v2"])

  tmp <- cholera::roads[cholera::roads$name != "Map Frame", ]
  tmp <- tmp[!duplicated(tmp[, c("x", "y")]), ]
  vertices.above.threshold <- setdiff(tmp$id, unlist(lst))
  lst$v5 <- vertices.above.threshold
  
  lst
}

#' Network graph of addresses with selected inter-point distance.
#'
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An 'igraph' object.
#' @noRd

thresholdRoadGraph <- function(inter.point.dist = 0.15) {
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rd <- rd[!duplicated(rd[, c("x", "y")]), ]
  idx <- index0(rd$id)
  d <- stats::dist(rd[, c("x", "y")])
  rd.dist <- data.frame(idx, d = c(d))
  overlap <- rd.dist[rd.dist$d <= inter.point.dist, ]
  edge.list <- overlap[, c("v1", "v2")]
  igraph::graph_from_data_frame(edge.list, directed = FALSE)
}

#' Rotate, stack and partition open road point triads.
#'
#' @param subgraphs Object. 'igraph' list of graphs.
#' @param census Object. List of graph vertices.
#' @param census.ct Object. Count of graph vertices.
#' @return An R data frame.
#' @noRd

openTriadRoads <- function(subgraphs, census, census.ct) {
  dat <- subgraphs[names(census[census.ct == 3])]
  dat <- dat[names(dat) != "11"]

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

#' Create PDFs of road endpoints partition (prototype).
#'
#' For georeferencing in QGIS.
#' @param path Character. e.g., "~/Documents/Data/".
#' @param pch Numeric or Character.
#' @noRd

partitionRoadsPDF <- function(path, pch = 46) {
  pts <- partitionRoads()
  rng <- mapRange()
  pre <- "roads."
  post <- ".pdf"

  invisible(lapply(names(pts), function(nm) {
    file.nm <- paste0(path, pre, nm, post)
    dat <- cholera::roads[cholera::roads$id %in% pts[[nm]], c("x", "y")]
    grDevices::pdf(file = file.nm)
    plot(dat, pch = pch, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
      xlim = rng$x, ylim = rng$y, bty = "n", asp = 1)
    grDevices::dev.off()
  }))
}
