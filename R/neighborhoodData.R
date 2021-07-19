#' Compute network graph of roads, cases and pumps.
#'
#' Assembles cases, pumps and road into a network graph.
#' @param vestry Logical. Use Vestry Report pump data.
#' @param case.set Character. "observed" or "expected", or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param embed Logical. Embed cases and pumps into road network.
#' @param embed.landmarks Logical. Embed landmarks into road network.
#' @export
#' @return An R list of nodes, edges and an 'igraph' network graph.

neighborhoodData <- function(vestry = FALSE, case.set = "observed",
  embed = TRUE, embed.landmarks = TRUE) {

  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('case.set must be "observed", "expected" or "snow".')
  }

  if (case.set == "expected") {
    args <- list(embed = embed, embed.landmarks = embed.landmarks,
      vestry = vestry, observed = FALSE)
  } else {
    args <- list(embed = embed, embed.landmarks = embed.landmarks,
      vestry = vestry)
  }

  node.data <- do.call("nodeData", args)
  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g
  nodes.pump <- nodes[nodes$pump != 0, ]
  nodes.pump <- nodes.pump[order(nodes.pump$pump), c("pump", "node")]
  out <- list(g = g, nodes = nodes, edges = edges, nodes.pump = nodes.pump)
  class(out) <- "neighborhood_data"
  out
}

#' Plot method for neighborhoodData().
#'
#' Visualize underlying road network (with or without cases and pumps).
#' @param x An 'igraph' object of class "neighborhood_data" created by \code{neighborhoodData()}.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @examples
#' \donttest{
#' plot(neighborhoodData())
#' plot(neighborhoodData(embed = FALSE))
#' }

plot.neighborhood_data <- function(x, ...) {
  plot(x$g, vertex.label = NA, vertex.size = 2, ...)
}

## auxilliary functions ##

nodeData <- function(embed = TRUE, embed.landmarks = FALSE, vestry = FALSE,
  observed = TRUE) {

  if (embed) {
    if (vestry) {
      ortho.pump <- cholera::ortho.proj.pump.vestry
    } else {
      ortho.pump <- cholera::ortho.proj.pump
    }

    if (observed) {
      sel <- cholera::ortho.proj$case %in% cholera::fatalities.address$anchor
      case.segments <- unique(cholera::ortho.proj[sel, "road.segment"])
    } else {
      sim.proj <- cholera::sim.ortho.proj
      case.segments <- unique(sim.proj$road.segment)
      case.segments <- case.segments[is.na(case.segments) == FALSE]
    }

    if (embed.landmarks) {
      landmark.segments <- cholera::landmarks$road.segment
      site.segments <- unique(c(case.segments, landmark.segments))
    } else {
      site.segments <- case.segments
    }

    site.pump <- intersect(ortho.pump$road.segment, site.segments)
    site.no_pump <- setdiff(site.segments, ortho.pump$road.segment)
    no_site.pump <- setdiff(ortho.pump$road.segment, site.segments)
    edits <- c(site.pump, site.no_pump, no_site.pump)

    if (observed) {
      if (vestry) {
        nodes <- lapply(edits, embedSites, vestry = TRUE)
        edges <- lapply(edits, embedSites, type = "edges", vestry = TRUE)
      } else {
        nodes <- lapply(edits, embedSites)
        edges <- lapply(edits, embedSites, type = "edges")
      }
    } else {
      if (vestry) {
        nodes <- lapply(edits, embedSites, observed = FALSE, vestry = TRUE)
        edges <- lapply(edits, embedSites, type = "edges", observed = FALSE,
          vestry = TRUE)
      } else {
        nodes <- lapply(edits, embedSites, observed = FALSE)
        edges <- lapply(edits, embedSites, type = "edges", observed = FALSE)
      }
    }

    # Edges #

    edges <- do.call(rbind, edges)
    road.segments <- cholera::road.segments
    road.segments <- road.segments[road.segments$id %in% edits == FALSE, ]
    road.segments$node1 <- paste0(road.segments$x1, "-", road.segments$y1)
    road.segments$node2 <- paste0(road.segments$x2, "-", road.segments$y2)

    if (observed) {
      road.segments$id2 <- paste0(road.segments$id, "a")
    } else {
      road.segments$id2 <- paste0(road.segments$id, "-1")
    }

    edges <- rbind(edges, road.segments)
    edges$d <- sqrt((edges$x1 - edges$x2)^2 + (edges$y1 - edges$y2)^2)

    # Nodes #

    nodes <- do.call(rbind, nodes)
    select1 <- road.segments$node1 %in% nodes$node == FALSE
    select2 <- road.segments$node2 %in% nodes$node == FALSE
    endpt.node1 <- road.segments[select1, "node1"]
    endpt.node2 <- road.segments[select2, "node2"]

    null.nodes <- lapply(union(endpt.node1, endpt.node2), function(x) {
      sel <- road.segments$node1 == x | road.segments$node2 == x
      dat <- road.segments[sel, ]

      endpt1 <- dat$node1 == x & dat$node2 != x
      endpt2 <- dat$node1 != x & dat$node2 == x

      if (any(endpt1)) {
        data.sel <- unique(dat[endpt1, c("x1", "y1")])
      } else if (any(endpt2)) {
        data.sel <- unique(dat[endpt2, c("x2", "y2")])
      }

      data.sel <- stats::setNames(data.sel, c("x.proj", "y.proj"))
      data.frame(data.sel, anchor = 0, pump = 0, node = x)
    })

    null.nodes <- do.call(rbind, null.nodes)
    nodes <- rbind(nodes, null.nodes)

    # Network Graph #

    edge.list <- edges[, c("node1", "node2")]
    g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)
    list(nodes = nodes, edges = edges, g = g)

  } else {
    road.segments$node1 <- paste0(road.segments$x1, "-", road.segments$y1)
    road.segments$node2 <- paste0(road.segments$x2, "-", road.segments$y2)

    if (observed) {
      road.segments$id2 <- paste0(road.segments$id, "a")
    } else {
      road.segments$id2 <- paste0(road.segments$id, "-1")
    }

    road.segments$d <- sqrt((road.segments$x1 - road.segments$x2)^2 +
                            (road.segments$y1 - road.segments$y2)^2)

    rs1 <- road.segments[, c("x1", "y1", "node1")]
    rs2 <- road.segments[, c("x2", "y2", "node2")]
    a <- stats::setNames(rs1, c("x.proj", "y.proj", "node"))
    b <- stats::setNames(rs2, c("x.proj", "y.proj", "node"))

    nodes <- unique(rbind(a, b))
    nodes <- data.frame(nodes[, c("x.proj", "y.proj")],
                        anchor = 0,
                        pump = 0,
                        node = nodes$node,
                        stringsAsFactors = FALSE)

    edge.list <- road.segments[, c("node1", "node2")]
    g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)
    list(nodes = nodes, edges = road.segments, g = g)
  }
}

embedSites <- function(id, type = "nodes", observed = TRUE, vestry = FALSE) {
  if (vestry) {
    ortho.pump <- cholera::ortho.proj.pump.vestry
  } else {
    ortho.pump <- cholera::ortho.proj.pump
  }

  road.data <- cholera::road.segments[cholera::road.segments$id == id, ]

  ## embed filters ##

  CaseObs <- id %in% cholera::ortho.proj$road.segment
  Landmark <- id %in% cholera::landmarks$road.segment
  Pump <- id %in% ortho.pump$road.segment

  if (observed == FALSE) {
    sim.proj <- cholera::sim.ortho.proj
    CaseExp <- id %in% sim.proj $road.segment
  }

  if (observed) {
    if (CaseObs) {
      sel <- cholera::ortho.proj$road.segment %in% id
      road.fatalities <- cholera::ortho.proj[sel, ]
      sel <- road.fatalities$case %in% cholera::fatalities.address$anchor
      cases <- road.fatalities[sel, "case"]
      road.address <- road.fatalities[road.fatalities$case %in% cases, ]
      case.data <- data.frame(road.address[, c("x.proj", "y.proj")],
                              anchor = road.address$case,
                              pump = 0)
    }
  } else {
    if (CaseExp) {
      road.fatalities <- sim.proj[sim.proj$road.segment %in% id, ]
      sel <- road.fatalities$case[road.fatalities$case %in% sim.proj$case]
      road.address <- road.fatalities[road.fatalities$case %in% sel, ]
      case.data <- data.frame(road.address[, c("x.proj", "y.proj")],
                              anchor = road.address$case,
                              pump = 0)
    }
  }

  if (Landmark) {
    sel <- cholera::landmarks$road.segment %in% id
    road.landmarks <- cholera::landmarks[sel, ]
    landmark.data <- data.frame(road.landmarks[, c("x.proj", "y.proj")],
                                anchor = road.landmarks$case,
                                pump = 0)
  }

  endptA <- data.frame(x.proj = road.data$x1,
                       y.proj = road.data$y1,
                       anchor = 0,
                       pump = 0)

  endptB <- data.frame(x.proj = road.data$x2,
                       y.proj = road.data$y2,
                       anchor = 0,
                       pump = 0)

  if (observed) {
    if (CaseObs & Landmark) {
      site.seg <- CaseObs & Landmark
      rds <- rbind(case.data, landmark.data)

    } else if (CaseObs & !Landmark) {
      site.seg <- CaseObs
      rds <- case.data

    } else if (!CaseObs & Landmark) {
      site.seg <- Landmark
      rds <- landmark.data

    } else site.seg <- FALSE
  } else {
    if (CaseExp & Landmark) {
      site.seg <- CaseExp & Landmark
      rds <- rbind(case.data, landmark.data)

    } else if (CaseExp & !Landmark) {
      site.seg <- CaseExp
      rds <- case.data

    } else if (!CaseExp & Landmark) {
      site.seg <- Landmark
      rds <- landmark.data

    } else site.seg <- FALSE
  }

  if (Pump) {
     pump.data <- ortho.pump[ortho.pump$road.segment == id, ]
     ps <- data.frame(pump.data[, c("x.proj", "y.proj")],
                      anchor = 0,
                      pump = pump.data$pump.id)
  }

  if (site.seg & Pump) {
    dat <- rbind(rds, ps)
    nodes <- rbind(endptA, dat, endptB)
    nodes <- nodes[order(nodes$x.proj), ]

  } else if (site.seg & !Pump) {
    nodes <- rbind(endptA, rds, endptB)
    nodes <- nodes[order(nodes$x.proj), ]

  } else if (!site.seg & Pump) {
    nodes <- rbind(endptA, ps, endptB)
    nodes <- nodes[order(nodes$x.proj), ]

  } else {
    nodes <- rbind(endptA, endptB)
    nodes <- nodes[order(nodes$x.proj), ]
  }

  row.names(nodes) <- NULL
  nodes$node <- paste0(nodes$x.proj, "-", nodes$y.proj)

  # Edges

  sel <- c("x.proj", "y.proj")
  edges <- cbind(nodes[-nrow(nodes), sel], nodes[-1, sel])
  names(edges) <- c("x1", "y1", "x2", "y2")
  edges$node1 <- paste0(edges$x1, "-", edges$y1)
  edges$node2 <- paste0(edges$x2, "-", edges$y2)

  edges <- cbind(road.data[1, c("street", "id", "name")], edges,
    row.names = NULL)

  if (observed) {
    edges$id2 <- paste0(edges$id, letters[seq_len(nrow(edges))])
  } else {
    edges$id2 <- paste0(edges$id, "-", seq_len(nrow(edges)))
  }

  if (type == "nodes") {
    nodes
  } else if (type == "edges") {
    edges
  }
}
