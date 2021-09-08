#' Compute network graph of roads, cases and pumps.
#'
#' Assembles cases, pumps and road into a network graph.
#' @param tif Character. Georeferenced QGIS TIFF file.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @export

latlongNeighborhoodData <- function(path, tif, vestry = FALSE) {
  dat <- latlongEmbed(path, tif, vestry = FALSE)
  dat$node1 <- paste0(dat$long1, "-", dat$lat1)
  dat$node2 <- paste0(dat$long2, "-", dat$lat2)
  dat$d <- sqrt((dat$long1 - dat$long2)^2 +
                    (dat$lat1 - dat$lat2)^2)

  edge.list <- dat[, c("node1", "node2")]
  g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)
  out <- list(edge.list = edge.list, g = g)
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