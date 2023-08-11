#' Embed anchors and pumps into road segments (prototype).
#'
#' @param vestry Logical.
#' @param case.set Character. "observed" or "expected".
#' @param embed.addr Logical. Embed the 321 case addresses into graph network.
#' @param embed.landmarks Logical. Embed landmarks into road network.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @noRd

nominalEmbed <- function(vestry = FALSE, case.set = "observed",
  embed.addr = TRUE, embed.landmarks = TRUE, multi.core = TRUE) {

  cores <- multiCore(multi.core)

  if (vestry) {
    ortho.pump <- cholera::ortho.proj.pump.vestry
  } else {
    ortho.pump <- cholera::ortho.proj.pump
  }

  sel <- c("x.proj", "y.proj", "pump.id")
  names(ortho.pump)[names(ortho.pump) %in% sel] <- c("x", "y", "pump")

  road.data <- roadSegments()

  if (embed.addr) {
    if (case.set == "observed") {
      sel <- cholera::ortho.proj$case %in% cholera::fatalities.address$anchor
      ortho.addr <- cholera::ortho.proj[sel, ]
    } else if (case.set == "expected") ortho.addr <- cholera::sim.ortho.proj

    sel <- names(ortho.addr) %in% c("x.proj", "y.proj")
    names(ortho.addr)[sel] <-  c("x", "y")

    obs.segs <- union(ortho.addr$road.segment, ortho.pump$road.segment)
  } else {
    obs.segs <- ortho.pump$road.segment
  }

  if (embed.landmarks) {
    obs.segs <- union(obs.segs, cholera::landmarks$road.segment)
  }

  no_embeds <- road.data[!road.data$id %in% obs.segs, ]

  vars <- c("x", "y")
  vars2 <- c(vars, "case", "pump")

  embeds <- parallel::mclapply(obs.segs, function(s) {
    rd.tmp <- road.data[road.data$id == s, ]
    pump.tmp <- ortho.pump[ortho.pump$road.segment == s, ]
    endpts <- data.frame(x = unlist(rd.tmp[, paste0(vars[1], 1:2)]),
                         y = unlist(rd.tmp[, paste0(vars[2], 1:2)]),
                         case = 0,
                         pump = 0,
                         row.names = NULL)

    if (nrow(pump.tmp) > 0) {
      pump.tmp$case <- 0L
      pump.embed <- pump.tmp[, vars2]
    }

    if (embed.addr) {
      addr.tmp <- ortho.addr[ortho.addr$road.segment == s, ]
      land.tmp <- cholera::landmarks[cholera::landmarks$road.segment == s, ]

      if (nrow(land.tmp) > 0) {
        land.embed <- land.tmp[, c("case", "x.proj", "y.proj")]
        names(land.embed)[names(land.embed) %in% c("x.proj", "y.proj")] <- vars
      }

      if (nrow(addr.tmp) > 0 & (nrow(land.tmp) > 0)) {
        addr.embed <- rbind(addr.tmp[, c(vars, "case")], land.embed)
      } else if (nrow(addr.tmp) == 0 & (nrow(land.tmp) > 0)) {
        addr.embed <- land.embed
      } else { # (case & no landmark) | (no case & no landmark)
        addr.embed <- addr.tmp[, c(vars, "case")]
      }

      if (nrow(addr.embed) > 0 & nrow(pump.tmp) > 0) {
        addr.embed$pump <- 0
        pump.embed$case <- 0
        pump.embed <- pump.embed[, vars2]
        embed.data <- rbind(endpts, addr.embed, pump.embed)
      } else if (nrow(addr.embed) == 0 & nrow(pump.tmp) > 0) {
        pump.embed$case <- 0
        pump.embed <- pump.embed[, vars2]
        embed.data <- rbind(endpts, pump.embed)
      } else if (nrow(addr.embed) > 0 & nrow(pump.tmp) == 0) {
        addr.embed$pump <- 0
        embed.data <- rbind(endpts, addr.embed)
      }
    } else {
      embed.data <- rbind(endpts, pump.embed)
    }

    nodes <- embed.data[order(embed.data$x, embed.data$y), ]

    # cases/landmarks that overlap road segment endpoints (e.g. Golden Sq)
    dup.nodes <- duplicated(signif(nodes$x)) & duplicated(signif(nodes$y))

    if (any(dup.nodes)) {
      idx <- index0(seq_len(nrow(nodes)))

      coord.audit <- vapply(seq_len(nrow(idx)), function(i) {
        node.tmp <- nodes[unlist(idx[i, ]), ]
        identicalCoords(node.tmp[, c("x", "y")])
      }, logical(1L))

      idx2 <- idx[coord.audit, ]

      nodes.ok <- nodes[-unlist(idx2), ]

      nodes.unduplicated <- lapply(seq_len(nrow(idx2)), function(i) {
        node.tmp <- nodes[unlist(idx2[i, ]), ]
        node.tmp[node.tmp$case > 0 | node.tmp$pump > 0, ]
      })

      nodes.unduplicated <- do.call(rbind, nodes.unduplicated)
      nodes <- rbind(nodes.ok, nodes.unduplicated)
      nodes <- nodes[order(nodes$x, nodes$y), ]
    }

    tmp <- nodes[, vars]
    tmp <- cbind(tmp[-nrow(tmp), ], tmp[-1, ])
    coord.nms <- paste0(names(tmp), c(rep(1, 2), rep(2, 2)))
    names(tmp) <- coord.nms
    tmp <- cbind(tmp, rd.tmp[, c("street", "id", "name")])
    edges <- tmp[, c("street", "id", "name", coord.nms)]

    if (case.set == "observed") {
      edges$id2 <- paste0(edges$id, letters[seq_len(nrow(edges))])
    } else if (case.set == "expected") {
      edges$id2 <- paste0(edges$id, "-", seq_len(nrow(edges)))
    }

    list(edges = edges, nodes = nodes)
  }, mc.cores = cores)

  no_embeds$id2 <- paste0(no_embeds$id, "a")

  edges <- do.call(rbind, lapply(embeds, function(x) x$edges))
  edges <- rbind(edges, no_embeds[, names(edges)])
  edges <- edges[order(edges$street), ]
  edges$node1 <- paste0(edges$x1, "_&_", edges$y1)
  edges$node2 <- paste0(edges$x2, "_&_", edges$y2)

  edges$d <- vapply(seq_len(nrow(edges)), function(i) {
    stats::dist(rbind(stats::setNames(edges[i, c("x1", "y1")], vars),
                      stats::setNames(edges[i, c("x2", "y2")], vars)))
  }, numeric(1L))

  edge.list <- edges[, c("node1", "node2")]
  g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

  nodes <- do.call(rbind, lapply(embeds, function(x) x$nodes))
  nodes$node <- paste0(nodes$x, "_&_", nodes$y)

  list(g = g, edges = edges, nodes = nodes)
}

identicalCoords <- function(df) {
  num <- signif(df[, c("x", "y")])
  identical(num$x[1], num$x[2]) & identical(num$y[1], num$y[2])
}
