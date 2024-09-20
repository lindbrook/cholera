#' Compute network graph of roads, cases and pumps.
#'
#' Assembles cases, pumps and road into a network graph.
#' @param vestry Logical.
#' @param case.set Character. "observed" or "expected".
#' @param embed.addr Logical. Embed case address into graph network.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @importFrom geosphere distGeo
#' @noRd

latlongNeighborhoodData <- function(vestry = FALSE, case.set = "observed",
  embed.addr = TRUE, multi.core = TRUE) {

  cores <- multiCore(multi.core)
  dat <- latlongEmbed(vestry = vestry, case.set = case.set,
    embed.addr = embed.addr, multi.core = cores)

  road.data <- dat$road.data
  road.data$node1 <- paste0(road.data$lon1, "_&_", road.data$lat1)
  road.data$node2 <- paste0(road.data$lon2, "_&_", road.data$lat2)

  edges <- dat$edges
  edges$node1 <- paste0(edges$lon1, "_&_", edges$lat1)
  edges$node2 <- paste0(edges$lon2, "_&_", edges$lat2)
  edge.list <- edges[, c("node1", "node2")]

  g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

  edge.nodes <- attributes(igraph::E(g))$vname
  edge.nodes <- strsplit(edge.nodes, "|", fixed = TRUE)
  edge.dist <- lapply(edge.nodes, function(e) {
    endpts <- lapply(e, function(x) unlist(strsplit(x, "_&_")))
    endpts <- lapply(endpts, as.numeric)
    nms <- paste0(c("lon", "lat"), c(rep(1, 2), rep(2, 2)))
    out <- stats::setNames(data.frame(t(do.call(c, endpts))), nms)
    p1 <- out[, grep(1, names(out))]
    p2 <- out[, grep(2, names(out))]
    out$d <- geosphere::distGeo(p1, p2)
    out
  })
  edge.dist <- do.call(rbind, edge.dist)

  edges$node1 <- paste0(pmin(edges$lon1, edges$lon2), "_&_",
                        pmin(edges$lat1, edges$lat2))
  edges$node2 <- paste0(pmax(edges$lon1, edges$lon2), "_&_",
                        pmax(edges$lat1, edges$lat2))
  edges$node.node <- paste0(edges$node1, "_&_", edges$node2)

  edge.dist$node1 <- paste0(pmin(edge.dist$lon1, edge.dist$lon2), "_&_",
                            pmin(edge.dist$lat1, edge.dist$lat2))
  edge.dist$node2 <- paste0(pmax(edge.dist$lon1, edge.dist$lon2), "_&_",
                            pmax(edge.dist$lat1, edge.dist$lat2))
  edge.dist$node.node <- paste0(edge.dist$node1, "_&_", edge.dist$node2)

  edges <- merge(edges, edge.dist[, c("d", "node.node")], by = "node.node")
  edges$node.node <- NULL

  nodes <- dat$nodes
  nodes.pump <- nodes[nodes$pump != 0, ]
  nodes.pump$node <- paste0(nodes.pump$lon, "_&_", nodes.pump$lat)
  nodes.pump <- nodes.pump[order(nodes.pump$pump), c("pump", "node")]

  out <- list(edge.list = edge.list, g = g, edges = edges, nodes = nodes,
              nodes.pump = nodes.pump, road.data = road.data)
  class(out) <- "latlong_neighborhood_data"
  out
}

#' Plot method for latlongNeighborhoodData().
#'
#' Visualize underlying road network (with or without cases and pumps).
#' @param x An 'igraph' object of class "latlong_neighborhood_data" created by \code{latlongNeighborhoodData()}.
#' @param ... Additional plotting parameters.
#' @return An igraph base graphics plot.
#' @noRd

plot.latlong_neighborhood_data <- function(x, ...) {
  plot(x$g, vertex.label = NA, vertex.size = 2, ...)
}
