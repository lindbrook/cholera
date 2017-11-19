#' Embed cases and pumps into road network graph.
#' @param vestry Logical. Use Vestry Report pump data.
#' @export
#' @return An R list of nodes, edges and graph.

nodeData <- function(vestry = FALSE) {
  case.segments <- unique(cholera::ortho.proj[cholera::ortho.proj$case %in%
    cholera::fatalities.address$anchor.case, "road.segment"])

  rd.segs <- cholera::road.segments

  if (vestry) {
    ortho.pump <- cholera::ortho.proj.pump.vestry
  } else {
    ortho.pump <- cholera::ortho.proj.pump
  }

  case.pump <- intersect(ortho.pump$road.segment, case.segments)
  case.no_pump <- setdiff(case.segments, ortho.pump$road.segment)
  no_case.pump <- setdiff(ortho.pump$road.segment, case.segments)

  edits <- c(case.pump, case.no_pump, no_case.pump)

  if (vestry) {
    nodes <- lapply(edits, embedSites, vestry = TRUE)
    edges <- lapply(edits, embedSites, type = "edges",
      vestry = TRUE)
  } else {
    nodes <- lapply(edits, embedSites)
    edges <- lapply(edits, embedSites, type = "edges")
  }

  nodes <- do.call(rbind, nodes)
  edges <- do.call(rbind, edges)

  null.segments <- rd.segs[rd.segs$id %in% edits == FALSE, ]
  null.segments$node1 <- paste0(null.segments$x1, "-", null.segments$y1)
  null.segments$node2 <- paste0(null.segments$x2, "-", null.segments$y2)
  null.segments$id2 <- paste0(null.segments$id, "a")

  no_case.no_pump <- null.segments$id

  road.segmentsB <- rbind(edges, null.segments)
  road.segmentsB$d <- sqrt((road.segmentsB$x1 - road.segmentsB$x2)^2 +
                           (road.segmentsB$y1 - road.segmentsB$y2)^2)

  edge.list <- road.segmentsB[, c("node1", "node2")]
  g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

  list(nodes = nodes, edges = road.segmentsB, g = g)
}

embedSites <- function(id = "242-1", type = "nodes", vestry = FALSE) {
  if (id %in% cholera::road.segments$id == FALSE) {
      stop('Valid "id" are listed in cholera::road.segments$id.')
  }

  road.data <- cholera::road.segments[cholera::road.segments$id == id, ]

  if (road.data$id %in% cholera::ortho.proj$road.segment) {
    road.fatalities <- cholera::ortho.proj[cholera::ortho.proj$road.segment %in%
      road.data$id, ]

    sel <- road.fatalities$case[road.fatalities$case %in%
      cholera::fatalities.address$anchor.case]
    road.address <- road.fatalities[road.fatalities$case %in% sel, ]

    rds <- data.frame(road.address[, c("x.proj", "y.proj")],
                      anchor = road.address$case,
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

  if (vestry) {
    pumps <- cholera::ortho.proj.pump.vestry
  } else {
    pumps <- cholera::ortho.proj.pump
  }

  case.seg <- road.data$id %in% cholera::ortho.proj$road.segment
  pump.seg <- id %in% pumps$road.segment

  if (pump.seg) {
     pump.data <- pumps[pumps$road.segment == id, ]
     ps <- data.frame(pump.data[, c("x.proj", "y.proj")], anchor = 0,
       pump = pump.data$pump.id)
  }

  if (case.seg & pump.seg) {
    dat <- rbind(rds, ps)
    nodes <- rbind(endptA, dat, endptB)
    nodes <- nodes[order(nodes$x.proj), ]
  } else if (case.seg & !pump.seg) {
    nodes <- rbind(endptA, rds, endptB)
    nodes <- nodes[order(nodes$x.proj), ]
  } else if (!case.seg & pump.seg) {
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

  edges$id2 <- paste0(edges$id, letters[seq_len(nrow(edges))])

  if (type == "nodes") {
    nodes
  } else if (type == "edges") {
    edges
  }
}
