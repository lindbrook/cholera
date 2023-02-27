#' Partition non-address fatalities (prototype).
#'
#' Based on set of subgraphs defined by selected inter-point distance.
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An R list.
#' @noRd

partitionFatalities <- function(inter.point.dist = 0.15) {
  g <- thresholdFatalitiesGraph(inter.point.dist = inter.point.dist)
  subgraphs <- igraph::decompose(g)
  names(subgraphs) <- seq_along(subgraphs)
  census <- igraph::groups(igraph::components(g))
  census.ct <- vapply(census, length, integer(1L))

  ## dyads ##

  dyads <- do.call(rbind, lapply(census[census.ct == 2], as.numeric))
  stack.dyads <- data.frame(v1 = dyads[, 1], v2 = dyads[, 2])

  ## triads ##

  threes <- names(census[census.ct == 3])
  # plotNtuple(threes, subgraphs) # 15 closed triads

  triad.edge.ct <- ntupleOpenClosed(threes, subgraphs)
  # open.triads <- threes[triad.edge.ct != 3] # No open triads
  closed.triads <- threes[triad.edge.ct == 3]

  stack.closed.triads <- stackClosedTriads(closed.triads, subgraphs)[, -1]

  ## tetrads ##

  fours <- names(census[census.ct == 4])
  # plotNtuple(fours, subgraphs)

  tetrad.edge.ct <- ntupleOpenClosed(fours, subgraphs)
  open.tetrads <- fours[tetrad.edge.ct != 6]
  closed.tetrads <- fours[tetrad.edge.ct == 6]

  four.closed <- do.call(rbind, lapply(closed.tetrads, function(nm) {
    as.numeric(names(igraph::V(subgraphs[[nm]])))
  }))

  stack.closed.tetrad <- stats::setNames(data.frame(four.closed),
    paste0("v", 1:4))

  # plotNtuple(open.tetrads, subgraphs)
  stack.open.tetrad <- fourTriangleTail(open.tetrads, subgraphs)

  ## pentads ##

  fives <- names(census[census.ct == 5])
  # plotNtuple(fives, subgraphs) # 1 bowtie

  stack.open.pentad <- list(v1 = c(160, 126), v2 = c(205, 353), v3 = 229)

  ## sextads ##

  sixes <- names(census[census.ct == 6])
  # plotNtuple(sixes, subgraphs)

  sextad.edge.ct <- ntupleOpenClosed(sixes, subgraphs)

  # sort(igraph::degree(subgraphs$`45`), decreasing = TRUE)
  # 303 494 513 405 544 445
  # 3   3   3   2   2   1

  # sort(igraph::degree(subgraphs$`29`), decreasing = TRUE)
  # 444 497 500 571 145 480
  # 5   5   5   5   4   4

  # igraph::V(subgraphs$`45`)
  # igraph::V(subgraphs$`29`)
  # plot(subgraphs$`45`, vertex.size = 0, layout = igraph::layout_in_circle)
  # plot(subgraphs$`29`, vertex.size = 0, layout = igraph::layout_in_circle)

  # 2 x 3 data frame
  s45 <- data.frame(v1 = c(405, 494), v2 = c(513, 544), v3 = c(303, 445))
  threePlus <- rbind(stack.closed.triads, s45)

  s29 <- list(v1 = c(145, 480), v2 = 444, v3 = 497, v4 = 500, v5 = 571)

  # heptads

  sevens <- names(census[census.ct == 7])
  # plotNtuple(sevens, subgraphs)

  heptads.edge.ct <- ntupleOpenClosed(sevens, subgraphs)

  # sort(igraph::degree(subgraphs$`9`), decreasing = TRUE)
  # 16  66 504 457 556 127 545
  #  6   5   5   4   4   3   3
  # plot(subgraphs$`9`, vertex.size = 0, layout = igraph::layout_in_circle)

  # sort(igraph::degree(subgraphs$`26`), decreasing = TRUE)
  # 124 298 488 531 569 169 399
  #   6   6   6   6   6   5   5
  # plot(subgraphs$`26`, vertex.size = 0, layout = igraph::layout_in_circle)

  s9 <- list(v1 = c(457, 556, 545), v2 = c(66, 504), v3 = 16, v4 = 127)
  s26 <- list(v1 = c(169, 399), v2 = 124, v3 = 298, v4 = 488, v5 = 531,
    v6 = 569)

  # octads

  eights <- names(census[census.ct == 8])
  # plotNtuple(eights, subgraphs)

  # lapply(eights, function(x) {
  #   sort(igraph::degree(subgraphs[[x]]), decreasing = TRUE)
  # })
  # invisible(lapply(eights, function(x) {
  #   plot(subgraphs[[x]], vertex.size = 0, layout = igraph::layout_in_circle)
  # }))

  s11 <- list(v1 = c(77, 136, 560), v2 = c(88, 323), v3 = c(156, 328), v4 = 24)
  s20 <- list(v1 = c( 226, 90, 549), v2 = c(110, 331), v3 = c(412, 417),
    v4 = 442)

  sll.s20 <- data.frame(v1 = c(s11$v1, s20$v4),
                        v2 = c(s11$v2, s20$v3),
                        v3 = c(s11$v3, s20$v2),
                        v4 = c(s11$v4, s20$v1))

  fourPlus <- rbind(stack.closed.tetrad, stack.open.tetrad, sll.s20)

  s42 <- list(v1 = c(248, 308), v2 = c(306, 471), v3 = c(408, 335), v4 = 317,
    v5 = 329)

  # nonads

  nines <- names(census[census.ct == 9])
  # plotNtuple(nines, subgraphs)

  # sort(igraph::degree(subgraphs$`10`), decreasing = TRUE)
  # 566 575  18 131 297 518 142 409 117
  #   7   7   6   6   6   6   5   4   1
  # plot(subgraphs$`10`, vertex.size = 0, layout = igraph::layout_in_circle)

  s10 <- list(v1 = c(566, 117), v2 = c(131, 142), v3 = c(297, 409), v4 = 575,
    v5 = 18, v6 = 518)

  # 22-ad

  twentytwos <- names(census[census.ct == 22])
  # plotNtuple(twentytwos, subgraphs)
  # sort(igraph::degree(subgraphs$`7`), decreasing = TRUE)
  # plot(subgraphs$`7`, vertex.size = 0, layout = igraph::layout_in_circle)
  # plot(subgraphs$`7`, vertex.size = 0, layout = igraph::layout_with_dh)

  s7 <- list(v1 = c(194, 397, 138),
             v2 = c(12, 61, 340),
             v3 = c(91, 190, 508),
             v4 = c(93, 290, 521),
             v5 = c(122, 421),
             v6 = c(310, 174),
             v7 = c(398, 523),
             v8 = c(562, 547),
             v9 = c(574, 59))

  sel <- !cholera::fatalities$case %in% cholera::fatalities.address$anchor
  dat <- cholera::fatalities[sel, ]

  tmp <- c(stack.dyads, stack.closed.triads, stack.closed.tetrad,
           stack.open.tetrad, stack.open.pentad, s45, s29, s9, s26, s11, s20,
           s42, s10, s7)

  out <- s7

  out$v8 <- c(out$v8, stack.dyads$v1)
  out$v9 <- c(out$v9, stack.dyads$v2)

  out$v5 <- c(out$v5, threePlus$v1)
  out$v6 <- c(out$v6, threePlus$v2)
  out$v7 <- c(out$v7, threePlus$v3)

  out$v1 <- c(out$v1, fourPlus$v1)
  out$v2 <- c(out$v2, fourPlus$v2)
  out$v3 <- c(out$v3, fourPlus$v3)
  out$v4 <- c(out$v4, fourPlus$v4)

  # 5s

  out$v1 <- c(out$v1, stack.open.pentad$v1)
  out$v2 <- c(out$v2, stack.open.pentad$v2)
  out$v3 <- c(out$v3, stack.open.pentad$v3)

  #6s

  out$v1 <- c(out$v1, s29$v1)
  out$v2 <- c(out$v2, s29$v2)
  out$v3 <- c(out$v3, s29$v3)
  out$v4 <- c(out$v4, s29$v4)
  out$v5 <- c(out$v5, s29$v5)

  # 7s

  out$v1 <- c(out$v1, s9$v1)
  out$v2 <- c(out$v2, s9$v2)
  out$v3 <- c(out$v3, s9$v3)
  out$v4 <- c(out$v4, s9$v4)

  out$v1 <- c(out$v1, s26$v1)
  out$v2 <- c(out$v2, s26$v2)
  out$v3 <- c(out$v3, s26$v3)
  out$v4 <- c(out$v4, s26$v4)
  out$v5 <- c(out$v5, s26$v5)
  out$v6 <- c(out$v6, s26$v6)

  # 8s

  out$v1 <- c(out$v1, s42$v1)
  out$v2 <- c(out$v2, s42$v2)
  out$v3 <- c(out$v3, s42$v3)
  out$v4 <- c(out$v4, s42$v4)
  out$v5 <- c(out$v5, s42$v5)

  #9s

  out$v1 <- c(out$v1, s10$v1)
  out$v2 <- c(out$v2, s10$v2)
  out$v3 <- c(out$v3, s10$v3)
  out$v4 <- c(out$v4, s10$v4)
  out$v5 <- c(out$v5, s10$v5)
  out$v6 <- c(out$v6, s10$v6)

  leftovers <- fatalitiesGraphData()$leftovers

  delta <- max(vapply(out, length, integer(1L))) -
               vapply(out, length, integer(1L))

  delta[length(delta)] <- abs(sum(delta) - length(leftovers) -
    delta[length(delta)])

  end.pt <- cumsum(delta)
  start.pt <- unname(c(0, end.pt[-length(end.pt)]) + 1)

  delta <- delta[-1]
  end.pt <- end.pt[-1]
  start.pt <- start.pt[-1]

  append.data <- lapply(seq_along(delta), function(i) {
    leftovers[start.pt[i]:end.pt[i]]
  })

  names(append.data) <- names(delta)

  for (nm in names(delta)) {
    out[[nm]] <- c(out[[nm]], append.data[[nm]])
  }

  out
}

#' Create PDFs of non-address fatalities (prototype).
#'
#' For georeferencing in QGIS.
#' @param path Character. e.g., "~/Documents/Data/".
#' @param pch Numeric or Character.
#' @noRd

partitionFatalitiesPDF <- function(path, pch = 46) {
  pts <- partitionFatalities()
  rng <- mapRange()
  pre <- "fatalities."
  post <- ".pdf"
  invisible(lapply(names(pts), function(nm) {
    file.nm <- paste0(path, pre, nm, post)
    sel <- cholera::fatalities$case %in% pts[[nm]]
    dat <- cholera::fatalities[sel, c("x", "y")]
    grDevices::pdf(file = file.nm)
    plot(dat, pch = pch, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
      xlim = rng$x, ylim = rng$y, bty = "n", asp = 1)
    grDevices::dev.off()
  }))
}
