#' Partiion overlapping orthogonal fatality addresses.
#'
#' Based on set of subgraphs defined by selected inter-point distance.
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An R list.
#' @export

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
                            v2 = c(507, 553, 348),
                            v3 = c(38, 1))

  # all(unlist(partitioned.seven) %in%
  #   as.numeric(names(igraph::V(subgraphs[["1"]]))))

  ## octad ##

  eights <- names(census[census.ct == 8])
  # plotNtuple(eights, subgraphs)

  partitioned.eight <- list(v1 = c(40, 165, 210),
                            v2 = c(5, 512, 94),
                            v3 = c(315, 28))

  # all(unlist(partitioned.eight) %in%
  #   as.numeric(names(igraph::V(subgraphs[["3"]]))))

  ## decad ##

  tens <- names(census[census.ct == 10])
  # plotNtuple(tens, subgraphs)

  partitioned.ten.linked.double.triangles <-
    list(v1 = c(246, 311, 236, 254),
         v2 = c(26, 475, 204),
         v3 = c(29, 266, 223))

  # all(unlist(partitioned.ten.linked.double.triangles) %in%
  #   as.numeric(names(igraph::V(subgraphs[["8"]]))))

  partitioned.ten.adjacent.double.triangles <-
    list(v1 = c(45, 449, 481, 224),
         v2 = c(296, 428, 325),
         v3 = c(68, 213, 49))

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
     center <- names(e.tbl[e.tbl == 3])
     periphery <- names(e.tbl[e.tbl == 2])
     tail <- names(e.tbl[e.tbl == 1])
     list(v1 = c(periphery[1], tail), v2 = periphery[2], v3 = center)
  })
  stats::setNames(out, dat)
}

fourDoubleTriangle <- function(dat, subgraphs) {
  out <- lapply(dat, function(nm) {
    tmp <- subgraphs[[nm]]
    e.lst <- igraph::as_edgelist(tmp)
    e.tbl <- table(c(e.lst))
    center <- names(e.tbl[e.tbl == 3])
    periphery <- names(e.tbl[e.tbl == 2])
    list(v1 = center[1], v2 = center[2], v3 = periphery)
  })
  stats::setNames(out, dat)
}

fiveTriangleTail <- function(dat, subgraphs) {
  out <- lapply(dat, function(nm) {
     tmp <- subgraphs[[nm]]
     vertices <- names(igraph::V(tmp))

     e.lst <- igraph::as_edgelist(tmp)
     e.tbl <- table(c(e.lst))
     center <- names(e.tbl[e.tbl == 3])

     sel <- igraph::count_triangles(tmp) == 1
     triangle <- vertices[sel]

     tail <- setdiff(vertices, triangle)

     list(v1 = center,
          v2 = c(triangle[triangle != center][1], tail[1]),
          v3 = c(triangle[triangle != center][2], tail[2]))
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

  list(v1 = c(primary.triangle[1], tail),
       v2 = c(primary.triangle[3], pivot),
       v3 = primary.triangle[2])
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

  data.frame(v1 = c(primary.triangle[1], tail[1]),
             v2 = c(primary.triangle[2], tail[2]),
             v3 = c(primary.triangle[3], pivot))
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

  endpt <- seq_along(triangles)[seq_along(triangles) %% 3 == 0]
  startpt <- c(0, endpt[-length(endpt)]) + 1
  id <- lapply(seq_along(startpt), function(i) seq(startpt[i], endpt[i]))
  triangle.vs <- lapply(id, function(idx) triangles[idx])
  triangle.vs <- stats::setNames(triangle.vs, paste(1:2))

  triangle1 <- triangle.vs$`1`
  # triangle2 <- triangle.vs$`2`

  data.frame(v1 = c(setdiff(triangle1, pivot1)[1], tail1),
             v2 = c(setdiff(triangle1, pivot1)[2], tail2),
             v3 = c(pivot1, pivot2))
}
