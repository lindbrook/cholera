#' Embed anchors and pumps into road segments (prototype).
#'
#' @param vestry Logical.
#' @param case.set Character. "observed" or "expected".
#' @param embed.addr Logical. Embed the 321 case addresses into graph network.
#' @param embed.landmarks Logical. Embed landmarks into road network.
#' @param embed.pumps Logical. Embed pumps into road network.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @noRd

nominalEmbed <- function(vestry = FALSE, case.set = "observed",
  embed.addr = TRUE, embed.landmarks = TRUE, embed.pumps = TRUE,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)
  road.data <- roadSegments()
  vars <- c("x", "y")

  if (embed.addr & embed.landmarks & embed.pumps) {
    ortho.addr <- orthoAddr(case.set = case.set)
    ortho.land <- orthoLand()
    ortho.pump <- orthoPumps(vestry = vestry)
    obs.segs <- union(ortho.addr$road.segment, ortho.land$road.segment)
    obs.segs <- union(obs.segs, ortho.pump$road.segment)
  } else if (!embed.addr & embed.landmarks & embed.pumps) {
    ortho.land <- orthoLand()
    ortho.pump <- orthoPumps(vestry = vestry)
    obs.segs <- union(ortho.land$road.segment, ortho.pump$road.segment)
  } else if (embed.addr & !embed.landmarks & embed.pumps) {
    ortho.addr <- orthoAddr(case.set = case.set)
    ortho.pump <- orthoPumps(vestry = vestry)
    obs.segs <- union(ortho.addr$road.segment, ortho.pump$road.segment)
  } else if (!embed.addr & !embed.landmarks & embed.pumps) {
    ortho.pump <- orthoPumps(vestry = vestry)
    obs.segs <- unique(ortho.pump$road.segment)
  } else if (embed.addr & embed.landmarks & !embed.pumps) {
    ortho.addr <- orthoAddr(case.set = case.set)
    ortho.land <- orthoLand()
    obs.segs <- union(ortho.addr$road.segment, ortho.land$road.segment)
  } else if (!embed.addr & embed.landmarks & !embed.pumps) {
    ortho.land <- orthoLand()
    obs.segs <- unique(ortho.land$road.segment)
  } else if (embed.addr & !embed.landmarks & !embed.pumps) {
    ortho.addr <- orthoAddr(case.set = case.set)
    obs.segs <- unique(ortho.addr$road.segment)
  } else if (!embed.addr & !embed.landmarks & !embed.pumps) {
    obs.segs <- NULL
  }

  if (!is.null(obs.segs)) {
    no_embeds <- road.data[!road.data$id %in% obs.segs, ]

    vars2 <- c(vars, "case", "pump")

    null.nms <- c("case", "road.segment", "x", "y")
    null.df <- stats::setNames(data.frame(matrix(nrow = 0, ncol = 4)), null.nms)

    embeds <- parallel::mclapply(obs.segs, function(s) {
      rd.tmp <- road.data[road.data$id == s, ]
      endpts <- data.frame(x = unlist(rd.tmp[, paste0(vars[1], 1:2)]),
                           y = unlist(rd.tmp[, paste0(vars[2], 1:2)]),
                           case = 0,
                           pump = 0,
                           row.names = NULL)

      if (exists("ortho.addr")) {
        addr.tmp <- ortho.addr[ortho.addr$road.segment == s, ]
      } else {
        addr.tmp <- null.df
      }

      if (exists("ortho.land")) {
        land.tmp <- ortho.land[ortho.land$road.segment == s, ]
      } else {
        land.tmp <- null.df
      }

      if (exists("ortho.pump")) {
        pump.tmp <- ortho.pump[ortho.pump$road.segment == s, ]
      } else {
        pump.tmp <- null.df
      }

      if (nrow(addr.tmp) > 0 & nrow(land.tmp) > 0 & nrow(pump.tmp) > 0) {
        addr.tmp <- ortho.addr[ortho.addr$road.segment == s, ]

        land.tmp <- cholera::landmarks[cholera::landmarks$road.segment == s, ]
        land.embed <- land.tmp[, c("case", "x.proj", "y.proj")]
        names(land.embed)[names(land.embed) %in% c("x.proj", "y.proj")] <- vars

        addr.embed <- rbind(addr.tmp[, c(vars, "case")], land.embed)

        pump.tmp <- ortho.pump[ortho.pump$road.segment == s, ]
        pump.tmp$case <- 0L
        pump.embed <- pump.tmp[, vars2]

        addr.embed$pump <- 0
        pump.embed$case <- 0
        pump.embed <- pump.embed[, vars2]
        embed.data <- rbind(endpts, addr.embed, pump.embed)

      } else if (nrow(addr.tmp) == 0 &
                 nrow(land.tmp) > 0 &
                 nrow(pump.tmp) > 0) {

        land.tmp <- cholera::landmarks[cholera::landmarks$road.segment == s, ]
        land.embed <- land.tmp[, c("case", "x.proj", "y.proj")]
        names(land.embed)[names(land.embed) %in% c("x.proj", "y.proj")] <- vars

        addr.embed <- land.embed

        pump.tmp <- ortho.pump[ortho.pump$road.segment == s, ]
        pump.tmp$case <- 0L
        pump.embed <- pump.tmp[, vars2]

        addr.embed$pump <- 0
        pump.embed$case <- 0
        pump.embed <- pump.embed[, vars2]
        embed.data <- rbind(endpts, addr.embed, pump.embed)

      } else if (nrow(addr.tmp) > 0 &
                 nrow(land.tmp) == 0 &
                 nrow(pump.tmp) > 0) {

        addr.tmp <- ortho.addr[ortho.addr$road.segment == s, ]
        addr.embed <- addr.tmp[, c(vars, "case")]

        pump.tmp <- ortho.pump[ortho.pump$road.segment == s, ]
        pump.tmp$case <- 0L
        pump.embed <- pump.tmp[, vars2]

        addr.embed$pump <- 0
        pump.embed$case <- 0
        pump.embed <- pump.embed[, vars2]
        embed.data <- rbind(endpts, addr.embed, pump.embed)

      } else if (nrow(addr.tmp) == 0 &
                 nrow(land.tmp) == 0 &
                 nrow(pump.tmp) > 0) {

        pump.tmp <- ortho.pump[ortho.pump$road.segment == s, ]
        pump.tmp$case <- 0L
        pump.embed <- pump.tmp[, vars2]

        pump.embed$case <- 0
        pump.embed <- pump.embed[, vars2]
        embed.data <- rbind(endpts, pump.embed)

      } else if (nrow(addr.tmp) > 0 &
                 nrow(land.tmp) > 0 &
                 nrow(pump.tmp) == 0) {

        addr.tmp <- ortho.addr[ortho.addr$road.segment == s, ]

        land.tmp <- cholera::landmarks[cholera::landmarks$road.segment == s, ]
        land.embed <- land.tmp[, c("case", "x.proj", "y.proj")]
        names(land.embed)[names(land.embed) %in% c("x.proj", "y.proj")] <- vars

        addr.embed <- rbind(addr.tmp[, c(vars, "case")], land.embed)
        addr.embed$pump <- 0

        embed.data <- rbind(endpts, addr.embed)

      } else if (nrow(addr.tmp) == 0 &
                 nrow(land.tmp) > 0 &
                 nrow(pump.tmp) == 0) {

        land.tmp <- cholera::landmarks[cholera::landmarks$road.segment == s, ]
        land.embed <- land.tmp[, c("case", "x.proj", "y.proj")]
        names(land.embed)[names(land.embed) %in% c("x.proj", "y.proj")] <- vars

        addr.embed <- land.embed
        addr.embed$pump <- 0

        embed.data <- rbind(endpts, addr.embed)

      } else if (nrow(addr.tmp) > 0 &
                 nrow(land.tmp) == 0 &
                 nrow(pump.tmp) == 0) {

        addr.tmp <- ortho.addr[ortho.addr$road.segment == s, ]
        addr.embed <- addr.tmp[, c(vars, "case")]

        addr.embed$pump <- 0

        embed.data <- rbind(endpts, addr.embed)

      } else if (nrow(addr.tmp) == 0 &
                 nrow(land.tmp) == 0 &
                 nrow(pump.tmp) == 0) {

        embed.data <- endpts
      }

      nodes <- embed.data[order(embed.data$x, embed.data$y), ]

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
    n1 <- stats::setNames(no_embeds[, paste0(vars, 1)], vars)
    n2 <- stats::setNames(no_embeds[, paste0(vars, 2)], vars)
    nodes.no_embeds <- rbind(n1, n2)
    nodes.no_embeds$case <- 0
    nodes.no_embeds$pump <- 0
    nodes <- rbind(nodes, nodes.no_embeds)
    nodes$node <- paste0(nodes$x, "_&_", nodes$y)
  
  } else {
    edges <- road.data
    edges$node1 <- paste0(edges$x1, "_&_", edges$y1)
    edges$node2 <- paste0(edges$x2, "_&_", edges$y2)

    edges$d <- vapply(seq_len(nrow(edges)), function(i) {
      stats::dist(rbind(stats::setNames(edges[i, paste0(vars, 1)], vars),
                        stats::setNames(edges[i, paste0(vars, 1)], vars)))
    }, numeric(1L))

    edge.list <- edges[, c("node1", "node2")]
    g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

    node.nms <- c("x", "y", "node")
    n1 <- stats::setNames(edges[, c("x1", "y1", "node1")], node.nms)
    n2 <- stats::setNames(edges[, c("x2", "y2", "node2")], node.nms)
    nodes <- rbind(n1, n2)
    nodes$case <- 0
    nodes$pump <- 0
    nodes <- nodes[, c("x", "y", "case", "pump", "node")]
  }

  if (!is.null(obs.segs)) {
    edges <- edges[order(edges$street, edges$id2), ]
  } else {
    edges <- edges[order(edges$street, edges$id), ]
  }
  
  nodes <- nodes[order(nodes$x, nodes$y), ]
  row.names(edges) <- NULL
  row.names(nodes) <- NULL

  list(g = g, edges = edges, nodes = nodes)
}

orthoAddr <- function(case.set = "observed") {
  if (case.set == "observed") {
    sel <- cholera::ortho.proj$case %in% cholera::fatalities.address$anchor
    out <- cholera::ortho.proj[sel, ]
  } else if (case.set == "expected") {
    out <- cholera::sim.ortho.proj
  }
  sel <- names(out) %in% c("x.proj", "y.proj")
  names(out)[sel] <- c("x", "y")
  out
}

orthoLand <- function() {
  out <- cholera::landmarks[, c("case", "road.segment", "x.proj", "y.proj")]
  names(out)[names(out) %in% c("x.proj", "y.proj")] <- c("x", "y")
  out
}

orthoPumps <- function(vestry = TRUE) {
  if (vestry) {
    out <- cholera::ortho.proj.pump.vestry
  } else {
    out <- cholera::ortho.proj.pump
  }
  sel <- c("x.proj", "y.proj", "pump.id")
  names(out)[names(out) %in% sel] <- c("x", "y", "pump")
  out
}
