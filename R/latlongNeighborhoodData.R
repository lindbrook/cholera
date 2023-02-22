#' Compute network graph of roads, cases and pumps.
#'
#' Assembles cases, pumps and road into a network graph.
#' @param vestry Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

latlongNeighborhoodData <- function(vestry = FALSE, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  dat <- latlongEmbed(vestry = vestry, multi.core = cores)

  edges <- dat$edges
  edges$node1 <- paste0(edges$lon1, "_&_", edges$lat1)
  edges$node2 <- paste0(edges$lon2, "_&_", edges$lat2)
  edge.list <- edges[, c("node1", "node2")]

  g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

  edges <- attributes(igraph::E(g))$vname
  edges <- strsplit(edges, "|", fixed = TRUE)
  edges <- lapply(edges, function(e) {
    endpts <- lapply(e, function(x) unlist(strsplit(x, "_&_")))
    endpts <- lapply(endpts, as.numeric)
    nms <- paste0(c("lon", "lat"), c(rep(1, 2), rep(2, 2)))
    out <- stats::setNames(data.frame(t(do.call(c, endpts))), nms)
    p1 <- out[, grep(1, names(out))]
    p2 <- out[, grep(2, names(out))]
    out$d <- geosphere::distGeo(p1, p2)
    out
  })
  edges <- do.call(rbind, edges)

  edges$node1 <- paste0(edges$lon1, "_&_", edges$lat1)
  edges$node2 <- paste0(edges$lon2, "_&_", edges$lat2)

  nodes <- dat$nodes
  nodes.pump <- nodes[nodes$pump != 0, ]
  nodes.pump$node <- paste0(nodes.pump$lon, "_&_", nodes.pump$lat)
  nodes.pump <- nodes.pump[order(nodes.pump$pump), c("pump", "node")]

  out <- list(edge.list = edge.list, g = g, edges = edges, nodes = nodes,
              nodes.pump = nodes.pump)
  class(out) <- "latlong_neighborhood_data"
  out
}

#' Plot method for latlongNeighborhoodData().
#'
#' Visualize underlying road network (with or without cases and pumps).
#' @param x An 'igraph' object of class "latlong_neighborhood_data" created by \code{latlongNeighborhoodData()}.
#' @param ... Additional plotting parameters.
#' @return An igraph base graphics plot.
#' @export

plot.latlong_neighborhood_data <- function(x, ...) {
  plot(x$g, vertex.label = NA, vertex.size = 2, ...)
}
