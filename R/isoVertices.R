#' isochrone and isodistance vertices (prototype)
#'
#' @param pump.select Numeric.
#' @param post Numeric.
#' @param post.type Character. "distance or "time".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @export

isoVertices <- function(pump.select = 7, post = 50, post.type = "distance",
  multi.core = FALSE) {

  cores <- multiCore(multi.core)
  ortho.dist <- signif(c(stats::dist(cholera::regular.cases[1:2, ])))
  diag.dist <- signif(c(stats::dist(cholera::regular.cases[c(1, 37), ])))

  pump.dist <- cholera::sim.walking.distance[[paste(pump.select)]]
  delta <- post # increment
  isobands <- seq(0, 350, delta)

  iso.vertices <- parallel::mclapply(isobands, function(cutpt) {
    sel <- pump.dist$distance > cutpt & pump.dist$distance <= cutpt + delta
    neighborhood.points <- pump.dist[sel, "case"]
    vertices <- peripheryCases(neighborhood.points)

    d <- c(stats::dist(cholera::regular.cases[vertices, ]))
    d <- stats::setNames(as.data.frame(d), "d")
    idx <- index0(vertices)
    distance <- data.frame(v1 = vertices[idx$V1],
                           v2 = vertices[idx$V2],
                           d = signif(d),
                           stringsAsFactors = FALSE)

    adjacency <- distance[distance$d == ortho.dist | distance$d == diag.dist, ]
    adjacency$node <- paste0(adjacency$v1, "-", adjacency$v2)

    edge.list <- adjacency[, c("v1", "v2")]
    g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

    if (length(igraph::articulation_points(g)) == 0) {
      if (igraph::components(g)$no == 1) {
        out <- travelingSalesman(vertices)
      } else {
        members <- igraph::components(g)$membership
        components <- lapply(seq_along(unique(members)), function(i) {
          names(members[members == i])
        })

        subgraphs <- lapply(components, function(x) {
          igraph::induced_subgraph(g, x)
        })

        articulation.test <- vapply(subgraphs, function(x) {
          any(igraph::articulation_points(x))
        }, logical(1L))

        if (any(articulation.test)) {
          stop("articulation points found!")
        } else {
          out <- lapply(components, travelingSalesman)
        }
      }
    } else {
      network.components <- igraph::biconnected.components(g)$components
      out <- lapply(network.components, function(x) travelingSalesman(names(x)))
    }
  }, mc.cores = cores)

  names(iso.vertices) <- isobands
  iso.vertices
}

index0 <- function(x) as.data.frame(t(utils::combn(length(x), 2)))
index1 <- function(x) as.data.frame(t(utils::combn(nrow(x), 2)))
