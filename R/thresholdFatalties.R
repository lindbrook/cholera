#' Network graph of non-address fatalities with selected inter-point distance.
#'
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An 'igraph' object.
#' @noRd

thresholdFatalitiesGraph <- function(inter.point.dist = 0.15) {
  dat <- fatalitiesGraphData(inter.point.dist = inter.point.dist)
  edge.list <- dat$proximate[, c("v1", "v2")]
  igraph::graph_from_data_frame(edge.list, directed = FALSE)
}

# plot(thresholdFatalitiesGraph(), vertex.label = NA, vertex.size = 2)

#' Proximity data for non-address fatalities with selected inter-point distance.
#'
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @return An R list with data frame of proximate vertices and vector of non-proximate vertices.
#' @noRd

fatalitiesGraphData <- function(inter.point.dist = 0.15) {
  sel <- !cholera::fatalities$case %in% cholera::fatalities.address$anchor
  dat <- cholera::fatalities[sel, ]
  idx <- data.frame(t(utils::combn(dat$case, 2)))
  names(idx) <- c("v1", "v2")
  d <- stats::dist(dat[, c("x", "y")])
  fatality.dist <- data.frame(idx, d = c(d))
  proximate <- fatality.dist[fatality.dist$d <= inter.point.dist, ]
  non.proximate <- fatality.dist[fatality.dist$d > inter.point.dist, ]
  proximate.vertices <- unique(unlist(proximate[, c("v1", "v2")]))
  non.proximate.vertices <- unique(unlist(non.proximate[, c("v1", "v2")]))
  leftovers <- setdiff(non.proximate.vertices, proximate.vertices)
  list(proximate = proximate, leftovers = leftovers)
}
