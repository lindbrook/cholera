#' Compute network graph of roads, cases and pumps (prototype).
#'
#' Assembles cases, pumps and road into a network graph.
#' @param vestry Logical. Use Vestry Report pump data.
#' @param case.set Character. "observed", "expected", or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param embed.addr Logical. Embed cases into road network.
#' @param embed.landmarks Logical. Embed landmarks into road network.
#' @param embed.pumps Logical. Embed pumps into road network.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @export
#' @return An R list of nodes, edges and an 'igraph' network graph.

neighborhoodData <- function(vestry = FALSE, case.set = "observed",
  embed.addr = TRUE, embed.landmarks = TRUE, embed.pumps = TRUE,
  latlong = FALSE) {

  args <- list(embed.addr = embed.addr, embed.landmarks = embed.landmarks,
    embed.pumps = embed.pumps, vestry = vestry, case.set = case.set,
    latlong = latlong)

  node.data <- do.call("embedNodes", args)
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

plot.neighborhood_data <- function(x, ...) {
  plot(x$g, vertex.label = NA, vertex.size = 2, ...)
}