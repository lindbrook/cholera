#' Compute network graph of roads, cases and pumps.
#'
#' Assembles cases, pumps and road into a network graph.
#' @param vestry Logical.
#' @export

latlongNeighborhoodDataB <- function(vestry = FALSE) {
  dat <- latlongEmbedB(vestry = vestry)
  dat$node1 <- paste0(dat$lon1, "-", dat$lat1)
  dat$node2 <- paste0(dat$lon2, "-", dat$lat2)
  edge.list <- dat[, c("node1", "node2")]
  g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

  edges <- attributes(igraph::E(g))$vname
  edges <- strsplit(edges, "|", fixed = TRUE)
  edges <- lapply(edges, function(e) {
    endpts <- lapply(e, function(x) unlist(strsplit(x, "-")))
    endpts <- lapply(endpts, function(x) as.numeric(x[2:3]))
    nms <- paste0(c("lon", "lat"), c(rep(1, 2), rep(2, 2)))
    out <- stats::setNames(data.frame(t(do.call(c, endpts))), nms)
    out[, grep("lon", names(out))] <- -1 * out[, grep("lon", names(out))]
    p1 <- out[, grep(1, names(out))]
    p2 <- out[, grep(2, names(out))]
    out$d <- geosphere::distGeo(p1, p2)
    out
  })
  edges <- do.call(rbind, edges)

  out <- list(edge.list = edge.list, g = g, edges = edges)
  class(out) <- "latlong_neighborhood_dataB"
  out
}

#' Plot method for latlongNeighborhoodDataB().
#'
#' Visualize underlying road network (with or without cases and pumps).
#' @param x An 'igraph' object of class "latlong_neighborhood_data" created by \code{latlongNeighborhoodData()}.
#' @param ... Additional plotting parameters.
#' @return An igraph base graphics plot.
#' @export

plot.latlong_neighborhood_dataB <- function(x, ...) {
  plot(x$g, vertex.label = NA, vertex.size = 2, ...)
}
