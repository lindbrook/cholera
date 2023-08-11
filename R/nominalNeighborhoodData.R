#' Compute network graph of roads, cases and pumps (prototype).
#'
#' Assembles cases, pumps and road into a network graph.
#' @param vestry Logical. Use Vestry Report pump data.
#' @param case.set Character. "observed", "expected", or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param embed.addr Logical. Embed cases into road network.
#' @param embed.landmarks Logical. Embed landmarks into road network.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export
#' @return An R list of nodes, edges and an 'igraph' network graph.

nominalNeighborhoodData <- function(vestry = FALSE, case.set = "observed",
  embed.addr = TRUE, embed.landmarks = TRUE, multi.core = TRUE) {

  args <- list(embed.addr = embed.addr, embed.landmarks = embed.landmarks,
    vestry = vestry, case.set = case.set, multi.core = multiCore(multi.core))

  node.data <- do.call("nominalEmbed", args)
  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g
  nodes.pump <- nodes[nodes$pump != 0, ]
  nodes.pump <- nodes.pump[order(nodes.pump$pump), c("pump", "node")]
  out <- list(g = g, nodes = nodes, edges = edges, nodes.pump = nodes.pump)
  class(out) <- "neighborhood_data"
  out
}
