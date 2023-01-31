#' Partiion overlapping orthogonal fatality addresses.
#'
#' Based on set of subgraphs defined by selected inter-point distance.
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An R list.
#' @noRd

partitionOrthoAddresses <- function(inter.point.dist = 0.15) {
  g <- thresholdOrthoAddressGraph(inter.point.dist = inter.point.dist)
  subgraphs <- igraph::decompose(g)
  names(subgraphs) <- seq_along(subgraphs)
  census <- igraph::groups(igraph::components(g))
  census.ct <- vapply(census, length, integer(1L))

  ## Motivation ##
  # To avoid point overlap, flatten graph network into vectors (columns) of
  # non-adjacent points.

  ## dyads ##

  dyads <- do.call(rbind, lapply(census[census.ct == 2], as.numeric))
  dyads <- data.frame(group = as.numeric(row.names(dyads)), v1 = dyads[, 1],
    v2 = dyads[, 2], row.names = NULL)

  ## triads ##

  threes <- names(census[census.ct == 3])
  # plotNtuple(threes, subgraphs)

  triad.edge.ct <- ntupleOpenClosed(threes, subgraphs)
  open.triads <- threes[triad.edge.ct != 3]
  closed.triads <- threes[triad.edge.ct == 3]

  stack.open.triads <- stackOpenTriads(open.triads, subgraphs)
  stack.closed.triads <- stackClosedTriads(closed.triads, subgraphs)

  ## tetrads ##

  fours <- names(census[census.ct == 4])
  # plotNtuple(fours, subgraphs)

  tetrad.edge.ct <- ntupleOpenClosed(fours, subgraphs)
  open.tetrads <- fours[tetrad.edge.ct != 6]
  closed.tetrads <- fours[tetrad.edge.ct == 6]

  four.closed <- t(as.numeric(names(igraph::V(subgraphs[[closed.tetrads]]))))
  four.closed <- stats::setNames(data.frame(four.closed), paste0("v", 1:4))

  four.string <- fours[tetrad.edge.ct == 3]
  # plotNtuple(four.string, subgraphs)
  stack.four.string <- partitionEvenString(subgraphs[four.string])
  stack.four.string <- do.call(rbind, stack.four.string)
  row.names(stack.four.string) <- NULL

  four.triangle.tail <- fours[tetrad.edge.ct == 4]
  # plotNtuple(four.triangle.tail, subgraphs)
  partitioned.four.triangle.tail <- fourTriangleTail(four.triangle.tail,
    subgraphs)

  four.double.triangle <- fours[tetrad.edge.ct == 5]
  # plotNtuple(four.double.triangle[1], subgraphs)
  partitioned.four.double.triangle <- fourDoubleTriangle(four.double.triangle,
    subgraphs)

  ## pentads ##

  fives <- names(census[census.ct == 5])
  # plotNtuple(fives, subgraphs)

  pentad.edge.ct  <- ntupleOpenClosed(fives, subgraphs)
  five.triangle.tail <- fives[pentad.edge.ct == 5]
  five.double.triangle.tail <- fives[pentad.edge.ct == 6]

  partitioned.five.triangle.tail <- fiveTriangleTail(five.triangle.tail,
    subgraphs)

  partitioned.five.double.triangle.tail <-
    fiveDoubleTriangleTail(five.double.triangle.tail, subgraphs)

  ## sextads ##

  sixes <- names(census[census.ct == 6])
  # plotNtuple(sixes, subgraphs)
  # ntupleOpenClosed(sixes, subgraphs)

  partitioned.six.double.triangle.one.tail <-
    sixDoubleTriangleOneTail("12", subgraphs)

  partitioned.six.double.triangle.two.tail <-
    sixDoubleTriangleTwoTail("26", subgraphs)

  ## septad ##

  sevens <- names(census[census.ct == 7])
  # plotNtuple(sevens, subgraphs)

  partitioned.seven <- list(v1 = c(393, 552),
                            v2 = c(507, 553),
                            v3 = c(38, 1),
                            v4 = 348)

  # all(unlist(partitioned.seven) %in%
  #   as.numeric(names(igraph::V(subgraphs[["1"]]))))

  ## octad ##

  eights <- names(census[census.ct == 8])
  # plotNtuple(eights, subgraphs)

  partitioned.eight <- data.frame(v1 = c(40, 165),
                                  v2 = c(5, 94),
                                  v3 = c(315, 28),
                                  v4 = c(512, 210))

  # all(unlist(partitioned.eight) %in%
  #   as.numeric(names(igraph::V(subgraphs[["3"]]))))

  ## decad ##

  tens <- names(census[census.ct == 10])
  # plotNtuple(tens, subgraphs)

  partitioned.ten.linked.double.triangles <-
    list(v1 = c(26, 311, 223),
         v2 = c(246, 204, 254),
         v3 = c(29, 266),
         v4 = c(475, 236))

  # all(unlist(partitioned.ten.linked.double.triangles) %in%
  #   as.numeric(names(igraph::V(subgraphs[["8"]]))))

  partitioned.ten.adjacent.double.triangles <-
    list(v1 = c(45, 449, 49),
         v2 = c(296, 325, 224),
         v3 = c(68, 213),
         v4 = c(428, 481))

  # all(unlist(partitioned.ten.adjacent.double.triangles) %in%
  #   as.numeric(names(igraph::V(subgraphs[["17"]]))))


  ## 19-tuple ##

  t19 <- names(census[census.ct == 19])
  # plotNtuple(t19, subgraphs)

  partitioned.nineteen <- list(v1 = c(321, 32, 188, 120),
                               v2 = c(25, 577, 289, 359, 137),
                               v3 = c(239, 21, 231, 180, 418),
                               v4 = c(572, 242, 386, 141, 102))

  # all(unlist(partitioned.nineteen) %in%
  #   as.numeric(names(igraph::V(subgraphs[["7"]]))))

  ## Assemble output ##

  two.cols <- rbind(dyads[, -1], stack.open.triads, stack.four.string)

  two2four.cols <- cbind(two.cols[1:(nrow(two.cols) / 2), ],
                         two.cols[(1 + nrow(two.cols) / 2):nrow(two.cols), ])

  names(two2four.cols) <- paste0("v", 1:4)

  #

  tmp5 <- partitioned.five.double.triangle.tail
  names(tmp5) <- paste0("v", c(4, 1:3))

  tmp7 <- partitioned.seven

  five_seven <- data.frame(v1 = c(tmp5$v1, tmp7$v1),
                           v2 = c(tmp5$v2, tmp7$v2),
                           v3 = c(tmp5$v3, tmp7$v3),
                           v4 = c(tmp5$v4, tmp7$v4))

  #

  tmp6 <- list(`12` = partitioned.six.double.triangle.one.tail,
               `26` = partitioned.six.double.triangle.two.tail)
  tmp10 <- list(`8` = partitioned.ten.linked.double.triangles,
                `17` = partitioned.ten.adjacent.double.triangles)

  six_ten <- lapply(seq_along(tmp6), function(i) {
    ptA <- tmp6[[i]]
    ptB <- stats::setNames(tmp10[[i]], paste0("v", c(3:4, 1:2)))
    data.frame(v1 = c(ptA$v1, ptB$v1),
               v2 = c(ptA$v2, ptB$v2),
               v3 = c(ptA$v3, ptB$v3),
               v4 = c(ptA$v4, ptB$v4))
  })

  six_ten <- do.call(rbind, six_ten)

  #

  tmp5 <-  partitioned.five.triangle.tail[[1]]
  tmp19 <- partitioned.nineteen
  five_nineteen <- data.frame(v1 = c(tmp5$v1, tmp19$v1),
                              v2 = c(tmp5$v2, tmp19$v2),
                              v3 = c(tmp5$v3, tmp19$v3),
                              v4 = c(tmp5$v4, tmp19$v4))

  four.cols.df <- rbind(partitioned.four.triangle.tail,
                        partitioned.four.double.triangle,
                        partitioned.eight,
                        five_seven,
                        six_ten,
                        five_nineteen)

  ptA <- as.list(rbind(two2four.cols, four.cols.df))
  ptB <- partitioned.five.triangle.tail[[2]]
  partitions <- list(v1 = c(ptA$v1, ptB$v1),
                     v2 = c(ptA$v2, ptB$v2),
                     v3 = c(ptA$v3, ptB$v3),
                     v4 = c(ptA$v4, ptB$v4))

  tmp <- setdiff(cholera::fatalities.address$anchor, unlist(partitions))
  # length(tmp) / 4
  # [1] 28
  tmp <- matrix(tmp, ncol = 4)
  above.threshold <- stats::setNames(data.frame(tmp), paste0("v", 1:4))

  list(v1 = c(partitions$v1, above.threshold$v1),
       v2 = c(partitions$v2, above.threshold$v2),
       v3 = c(partitions$v3, above.threshold$v3),
       v4 = c(partitions$v4, above.threshold$v4))
}

plotNtuple <- function(ntuple, subgraphs) {
  invisible(lapply(ntuple, function(x) {
    plot(subgraphs[[x]], vertex.size = 0, main = x)
  }))
}

ntupleOpenClosed <- function(ntuple, subgraphs) {
  vapply(ntuple, function(x) length(igraph::E(subgraphs[[x]])), integer(1L))
}

thresholdOrthoAddressGraph <- function(inter.point.dist = 0.15) {
  idx <- data.frame(t(utils::combn(cholera::fatalities.address$anchor, 2)))
  names(idx) <- c("v1", "v2")
  sel <- cholera::ortho.proj$case %in% cholera::fatalities.address$anchor
  d <- stats::dist(cholera::ortho.proj[sel, c("x.proj", "y.proj")])
  addr.dist <- data.frame(idx, d = c(d))
  overlap <- addr.dist[addr.dist$d <= inter.point.dist, ]
  edge.list <- overlap[, c("v1", "v2")]
  igraph::graph_from_data_frame(edge.list, directed = FALSE)
}

## Partion Functions ##

stackOpenTriads <- function(dat, subgraphs) {
  triads <- lapply(dat, function(nm) {
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

stackClosedTriads <- function(dat, subgraphs) {
  out <- lapply(dat, function(nm) {
    v <- as.numeric(igraph::as_edgelist(subgraphs[[nm]]))
    stats::setNames(data.frame(t(unique(v))), paste0("v", 1:3))
  })
  data.frame(group = as.numeric(dat), do.call(rbind, out))
}

fourTriangleTail <- function(dat, subgraphs) {
  out <- lapply(dat, function(nm) {
     tmp <- subgraphs[[nm]]
     e.lst <- igraph::as_edgelist(tmp)
     e.tbl <- table(c(e.lst))
     center <- as.numeric(names(e.tbl[e.tbl == 3]))
     periphery <- as.numeric(names(e.tbl[e.tbl == 2]))
     tail <- as.numeric(names(e.tbl[e.tbl == 1]))
     data.frame(v1 = periphery[1], v2 = periphery[2], v3 = center, v4 = tail)
  })
  # stats::setNames(out, dat)
  do.call(rbind, out)
}

fourDoubleTriangle <- function(dat, subgraphs) {
  out <- lapply(dat, function(nm) {
    tmp <- subgraphs[[nm]]
    e.lst <- igraph::as_edgelist(tmp)
    e.tbl <- table(c(e.lst))
    center <- as.numeric(names(e.tbl[e.tbl == 3]))
    periphery <- as.numeric(names(e.tbl[e.tbl == 2]))
    data.frame(v1 = center[1], v2 = center[2], v3 = periphery[1],
      v4 = periphery[2])
  })
  # stats::setNames(out, dat)
  do.call(rbind, out)
}

fiveTriangleTail <- function(dat, subgraphs) {
  out <- lapply(dat, function(nm) {
     tmp <- subgraphs[[nm]]
     vertices <- names(igraph::V(tmp))

     e.lst <- igraph::as_edgelist(tmp)
     e.tbl <- table(c(e.lst))
     center <- as.numeric(names(e.tbl[e.tbl == 3]))

     sel <- igraph::count_triangles(tmp) == 1
     triangle <- vertices[sel]

     tail <- as.numeric(setdiff(vertices, triangle))

     list(v1 = c(center, tail[2]),
          v2 = as.numeric(triangle[triangle != center][1]),
          v3 = as.numeric(c(triangle[triangle != center][2])),
          v4 = tail[1])
  })
  stats::setNames(out, dat)
}

fiveDoubleTriangleTail <- function(dat, subgraphs) {
  tmp <- subgraphs[[dat]]
  vertices <- names(igraph::V(tmp))
  e.lst <- igraph::as_edgelist(tmp)

  triangles <- names(igraph::triangles(tmp))
  tail <- setdiff(vertices, triangles)

  if (tail %in% e.lst[, 1]) {
    pivot <- e.lst[e.lst[, 1] == tail, 2]
  } else if (tail %in% e.lst[, 2]) {
    pivot <- e.lst[e.lst[, 2] == tail, 1]
  }

  endpt <- seq_along(triangles)[seq_along(triangles) %% 3 == 0]
  startpt <- c(0, endpt[-length(endpt)]) + 1
  id <- lapply(seq_along(startpt), function(i) seq(startpt[i], endpt[i]))

  triangle.vs <- lapply(id, function(idx) triangles[idx])

  sel <- which(vapply(triangle.vs, function(x) !pivot %in% x, logical(1L)))
  primary.triangle <- unlist(triangle.vs[sel])

  # e.tbl <- table(c(e.lst))
  # peripheral.vertex <- names(which(e.tbl == 2))
  # which(primary.triangle ==  peripheral.vertex)

  list(v1 = as.numeric(c(primary.triangle[1], tail)),
       v2 = as.numeric(primary.triangle[2]),
       v3 = as.numeric(primary.triangle[3]),
       v4 = as.numeric(pivot))
}

sixDoubleTriangleOneTail <- function(dat, subgraphs) {
  tmp <- subgraphs[[dat]]
  vertices <- names(igraph::V(tmp))
  e.lst <- igraph::as_edgelist(tmp)

  triangles <- names(igraph::triangles(tmp))
  tail <- setdiff(vertices, triangles)

  v.degree <- igraph::degree(tmp)
  tail.end <- names(v.degree[v.degree == 1])
  tail.bridge <- setdiff(tail, tail.end)

  pivot.test1 <- e.lst[, 1] %in% triangles & e.lst[, 2] == tail.bridge
  pivot.test2 <- e.lst[, 2] %in% triangles & e.lst[, 2] == tail.bridge

  if (any(pivot.test1)) {
    pivot <- intersect(e.lst[pivot.test1, ], triangles)
  } else if (any(pivot.test2)) {
    pivot <- intersect(e.lst[pivot.test2, ], triangles)
  }

  endpt <- seq_along(triangles)[seq_along(triangles) %% 3 == 0]
  startpt <- c(0, endpt[-length(endpt)]) + 1
  id <- lapply(seq_along(startpt), function(i) seq(startpt[i], endpt[i]))
  triangle.vs <- lapply(id, function(idx) triangles[idx])

  sel <- which(vapply(triangle.vs, function(x) !pivot %in% x, logical(1L)))
  primary.triangle <- unlist(triangle.vs[sel])

  list(v1 = as.numeric(c(primary.triangle[1], tail[1])),
       v2 = as.numeric(c(primary.triangle[2], tail[2])),
       v3 = as.numeric(primary.triangle[3]),
       v4 = as.numeric(pivot))
}

sixDoubleTriangleTwoTail <- function(dat, subgraphs) {
  tmp <- subgraphs[[dat]]
  vertices <- names(igraph::V(tmp))
  e.lst <- igraph::as_edgelist(tmp)

  triangles <- names(igraph::triangles(tmp))
  tail <- setdiff(vertices, triangles)

  pivot.test1 <- e.lst[, 1] %in% triangles & e.lst[, 2] %in% tail
  pivot.test2 <- e.lst[, 2] %in% triangles & e.lst[, 1] %in% tail
  pivot1 <- intersect(e.lst[pivot.test1, ], triangles)
  pivot2 <- intersect(e.lst[pivot.test2, ], triangles)

  tail1 <- e.lst[e.lst[, 1] %in% pivot1 & e.lst[, 2] %in% tail, 2]
  tail2 <- e.lst[e.lst[, 2] %in% pivot2 & e.lst[, 1] %in% tail, 1]

  triangle.vertices <- unique(triangles)
  # which(!triangle.vertices %in% c(pivot1, pivot2))

  list(v1 = as.numeric(c(triangle.vertices[1], tail1)),
       v2 = as.numeric(c(triangle.vertices[3], tail2)),
       v3 = as.numeric(triangle.vertices[2]),
       v4 = as.numeric(triangle.vertices[4]))
}

#' Create PDFs of fatality address orthogonal projection partitions (prototype).
#'
#' For georeferencing in QGIS.
#' @param path Character. e.g., "~/Documents/Data/".
#' @param pch Numeric or Character.
#' @noRd

partitionOrthoAddressesPDF <- function(path, pch = 46) {
  pts <- partitionOrthoAddresses()
  rng <- mapRange()
  pre <- "ortho.address."
  post <- ".pdf"

  invisible(lapply(names(pts), function(nm) {
    file.nm <- paste0(path, pre, nm, post)
    sel <- cholera::ortho.proj$case %in% pts[[nm]]
    dat <- cholera::ortho.proj[sel, c("x.proj", "y.proj")]
    grDevices::pdf(file = file.nm)
    plot(dat, pch = pch, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
      xlim = rng$x, ylim = rng$y, bty = "n", asp = 1)
    grDevices::dev.off()
  }))
}
