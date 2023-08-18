#' Embed anchors and pumps into road segments (latlong prototype).
#'
#' @param vestry Logical.
#' @param case.set Character. "observed" or "expected".
#' @param embed.addr Logical. Embed the 321 case addresses into graph network.
#' @param embed.landmarks Logical. Embed landmarks into road network.
#' @param embed.pumps Logical. Embed pumps into road network.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @noRd

latlongEmbedB <- function(vestry = FALSE, case.set = "observed",
  embed.addr = TRUE, embed.landmarks = TRUE, embed.pumps = TRUE,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)
  road.data <- roadSegments(latlong = TRUE)
  vars <- c("lon", "lat")

  if (embed.addr & embed.landmarks & embed.pumps) {
    ortho.addr <- orthoAddrB(case.set = case.set)
    ortho.land <- orthoLandB()
    ortho.pump <- orthoPumpsB(vestry = vestry)
    obs.segs <- union(ortho.addr$road.segment, ortho.land$road.segment)
    obs.segs <- union(obs.segs, ortho.pump$road.segment)
  } else if (!embed.addr & embed.landmarks & embed.pumps) {
    ortho.land <- orthoLandB()
    ortho.pump <- orthoPumpsB(vestry = vestry)
    obs.segs <- union(ortho.land$road.segment, ortho.pump$road.segment)
  } else if (embed.addr & !embed.landmarks & embed.pumps) {
    ortho.addr <- orthoAddrB(case.set = case.set)
    ortho.pump <- orthoPumpsB(vestry = vestry)
    obs.segs <- union(ortho.addr$road.segment, ortho.pump$road.segment)
  } else if (!embed.addr & !embed.landmarks & embed.pumps) {
    ortho.pump <- orthoPumpsB(vestry = vestry)
    obs.segs <- unique(ortho.pump$road.segment)
  } else if (embed.addr & embed.landmarks & !embed.pumps) {
    ortho.addr <- orthoAddrB(case.set = case.set)
    ortho.land <- orthoLandB()
    obs.segs <- union(ortho.addr$road.segment, ortho.land$road.segment)
  } else if (!embed.addr & embed.landmarks & !embed.pumps) {
    ortho.land <- orthoLandB()
    obs.segs <- unique(ortho.land$road.segment)
  } else if (embed.addr & !embed.landmarks & !embed.pumps) {
    ortho.addr <- orthoAddrB(case.set = case.set)
    obs.segs <- unique(ortho.addr$road.segment)
  } else if (!embed.addr & !embed.landmarks & !embed.pumps) {
    obs.segs <- NULL
  }

  if (!is.null(obs.segs)) {
    no_embeds <- road.data[!road.data$id %in% obs.segs, ]

    vars2 <- c(vars, "case", "pump")

    null.nms <- c("case", "road.segment", vars)
    null.df <- stats::setNames(data.frame(matrix(nrow = 0, ncol = 4)), null.nms)

    embeds <- parallel::mclapply(obs.segs, function(s) {
      rd.tmp <- road.data[road.data$id == s, ]
      endpts <- data.frame(lon = unlist(rd.tmp[, paste0(vars[1], 1:2)]),
                           lat = unlist(rd.tmp[, paste0(vars[2], 1:2)]),
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
        land.embed <- land.tmp[, c("case", "lon", "lat")]

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
        land.embed <- land.tmp[, c("case", "lon", "lat")]

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
        land.embed <- land.tmp[, c("case", "lon", "lat")]

        addr.embed <- rbind(addr.tmp[, c(vars, "case")], land.embed)
        addr.embed$pump <- 0

        embed.data <- rbind(endpts, addr.embed)

      } else if (nrow(addr.tmp) == 0 &
                 nrow(land.tmp) > 0 &
                 nrow(pump.tmp) == 0) {

        land.tmp <- cholera::landmarks[cholera::landmarks$road.segment == s, ]
        land.embed <- land.tmp[, c("case", "lon", "lat")]

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

      nodes <- embed.data[order(embed.data$lon, embed.data$lat), ]

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
    edges$node1 <- paste0(edges$lon1, "_&_", edges$lat1)
    edges$node2 <- paste0(edges$lon2, "_&_", edges$lat2)

    edges$d <- vapply(seq_len(nrow(edges)), function(i) {
      p1 <- edges[i, c("lon1", "lat1")]
      p2 <- edges[i, c("lon2", "lat2")]
      geosphere::distGeo(p1, p2)
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
    nodes$node <- paste0(nodes$lon, "_&_", nodes$lat)

  } else {
    edges <- road.data
    edges$node1 <- paste0(edges$lon1, "_&_", edges$lat1)
    edges$node2 <- paste0(edges$lon2, "_&_", edges$lat2)

    edges$d <- vapply(seq_len(nrow(edges)), function(i) {
      p1 <- edges[i, paste0(vars, 1)]
      p2 <- edges[i, paste0(vars, 2)]
      geosphere::distGeo(p1, p2)
    }, numeric(1L))

    edge.list <- edges[, c("node1", "node2")]
    g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

    node.nms <- c(vars, "node")
    n1 <- stats::setNames(edges[, c("lon1", "lat1", "node1")], node.nms)
    n2 <- stats::setNames(edges[, c("lon2", "lat2", "node2")], node.nms)
    nodes <- rbind(n1, n2)
    nodes$case <- 0
    nodes$pump <- 0
    nodes <- nodes[, c("lon", "lat", "case", "pump", "node")]
  }

  if (!is.null(obs.segs)) {
    edges <- edges[order(edges$street, edges$id2), ]
  } else {
    edges <- edges[order(edges$street, edges$id), ]
  }

  nodes <- nodes[order(nodes$lon, nodes$lat), ]
  # names(nodes)[names(nodes) == "case"] <- "anchor"
  row.names(edges) <- NULL
  row.names(nodes) <- NULL

  list(g = g, edges = edges, nodes = nodes)
}

orthoAddrB <- function(case.set = "observed") {
  if (case.set == "observed") {
    out <- cholera::latlong.ortho.addr
  } else if (case.set == "expected") {
    out <- cholera::latlong.sim.ortho.proj
  }
  out
}

orthoLandB <- function() {
  cholera::landmarks[, c("case", "road.segment", "lon", "lat")]
}

orthoPumpsB <- function(vestry = TRUE) {
  if (vestry) {
    out <- cholera::latlong.ortho.pump.vestry
  } else {
    out <- cholera::latlong.ortho.pump
  }
  names(out)[names(out) == "id"] <- "pump"
  out
}
