#' Compute network graph of roads, cases and pumps.
#'
#' Assembles cases, pumps and road into a network graph.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @export

latlongNeighborhoodData <- function(path, vestry = FALSE) {
  dat <- latlongEmbed(path, vestry = FALSE)
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
    out$d <- sp::spDistsN1(as.matrix(out[, grep(1, names(out))]),
                           as.matrix(out[, grep(2, names(out))]),
                           longlat = TRUE) * 1000L
    out
  })
  edges <- do.call(rbind, edges)

  out <- list(edge.list = edge.list, g = g, edges = edges)
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
