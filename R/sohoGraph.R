#' Compute network graph of roads and embed cases, landmarks and pumps (prototype).
#'
#' @param vestry Logical. Use Vestry Report pump data.
#' @param case.set Character. "observed", "expected", or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param embed.anchor Logical. Embed all 321 or selected anchor cases into graph network.
#' @param embed.landmarks Logical or Numeric. Embed all or selected landmarks into road network.
#' @param embed.pumps Logical or Numeric. Embed all or selected pumps into road network.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param drop.isolates Logical. Exclude Adam and Eve Court (and Pump #2) and Falconberg Court and Mews.
#' @param ellipsoid Character. "WGS" for WGS-84 or "BNG" for British National Grid (i.e., Airy 1830).
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details. Useful with `case.set = "expected"`.
#' @export
#' @return An R list of nodes, edges and an 'igraph' network graph.

sohoGraph <- function(vestry = FALSE, case.set = "observed",
  embed.anchor = TRUE, embed.landmarks = TRUE, embed.pumps = TRUE,
  latlong = FALSE, drop.isolates = FALSE, ellipsoid = "WGS",
  multi.core = FALSE) {

  if (.Platform$OS.type == "windows") {
    cores <- 1L
  } else {
    cores <- multiCore(multi.core)
  }

  args <- list(embed.anchor = embed.anchor, embed.landmarks = embed.landmarks,
    embed.pumps = embed.pumps, vestry = vestry, case.set = case.set,
    latlong = latlong, drop.isolates = drop.isolates, ellipsoid = ellipsoid,
    cores = cores)

  network <- do.call("embedNodes", args)
  nodes <- network$nodes
  edges <- network$edges
  g <- network$g
  
  if (embed.pumps) {
    nodes.pump <- nodes[nodes$pump != 0, ]
    nodes.pump <- nodes.pump[order(nodes.pump$pump), c("pump", "node")]
    out <- list(g = g, nodes = nodes, edges = edges, nodes.pump = nodes.pump)
  } else {
    out <- list(g = g, nodes = nodes, edges = edges)
  }
  
  class(out) <- "soho_graph"
  out
}

#' Plot method for sohoGraph().
#'
#' Visualize underlying road network (with or without cases and pumps).
#' @param x An 'igraph' object of class "soho_graph" created by \code{sohoGraph()}.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.soho_graph <- function(x, ...) {
  plot(x$g, vertex.label = NA, vertex.size = 2, ...)
}