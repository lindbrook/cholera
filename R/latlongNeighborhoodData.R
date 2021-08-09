#' Compute network graph of roads, cases and pumps.
#'
#' Assembles cases, pumps and road into a network graph.
#' @param path Character. e.g., "~/Documents/Data/".
#' @export

latlongNeighborhoodData <- function(path) {
  dat <- latitudeLongitudeRoads(path)

  rd.segs <- parallel::mclapply(unique(dat$street), function(i) {
      st <- dat[dat$street == i, ]
      names(st)[names(st) %in% c("long", "lat")] <- c("long1", "lat1")
      seg.end <- st[-1, c("long1", "lat1")]
      names(seg.end) <- c("long2", "lat2")
      st <- cbind(st[-nrow(st), c("street", "id", "name")],
                  st[-nrow(st), c("long1", "lat1")],
                  seg.end)
      st$id <- paste0(st$street, "-", seq_len(nrow(st)))
      st
    })

  rd.segs <- do.call(rbind, rd.segs)
  rd.segs$node1 <- paste0(rd.segs$long1, "-", rd.segs$lat1)
  rd.segs$node2 <- paste0(rd.segs$long2, "-", rd.segs$lat2)
  rd.segs$d <- sqrt((rd.segs$long1 - rd.segs$long2)^2 +
                    (rd.segs$lat1 - rd.segs$lat2)^2)

  edge.list <- rd.segs[, c("node1", "node2")]
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
