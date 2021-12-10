#' Network graph of addresses with selected inter-point distance.
#'
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An 'igraph' object.
#' @noRd

thresholdRoadGraph <- function(inter.point.dist = 0.15) {
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rd <- rd[!duplicated(rd[, c("x", "y")]), ]
  idx <- stats::setNames(data.frame(t(utils::combn(rd$id, 2))), c("v1", "v2"))
  d <- stats::dist(rd[, c("x", "y")])
  rd.dist <- data.frame(idx, d = c(d))
  overlap <- rd.dist[rd.dist$d <= inter.point.dist, ]
  edge.list <- overlap[, c("v1", "v2")]
  igraph::graph_from_data_frame(edge.list, directed = FALSE)
}
