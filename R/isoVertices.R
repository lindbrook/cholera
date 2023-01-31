#' Isochrone and isodistance vertices (prototype)
#'
#' @param post Numeric.
#' @param post.type Character. "distance or "time".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @noRd

isoVertices <- function(post = 50, post.type = "distance", multi.core = TRUE,
  dev.mode = FALSE) {

  cores <- multiCore(multi.core)
  ortho.dist <- signif(c(stats::dist(cholera::regular.cases[1:2, ])))
  diag.dist <- signif(c(stats::dist(cholera::regular.cases[c(1, 37), ])))
  pump.dist <- cholera::sim.walking.distance
  isobands <- seq(0, 600, post)
  band.id <- seq_along(isobands)[-length(isobands)]

  if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = c("peripheryCases"))
    iso.vertices <- parallel::parLapply(cl, band.id, iso_vertices, isobands,
      pump.dist, ortho.dist, diag.dist)
    parallel::stopCluster(cl)
  } else {
    iso.vertices <- parallel::mclapply(band.id, iso_vertices, mc.cores = cores,
       isobands, pump.dist, ortho.dist, diag.dist)
  }

  names(iso.vertices) <- isobands[-1]
  output <- list(post = post, post.type = post.type, vertices = iso.vertices)
  class(output) <- "iso"
  output
}

iso_vertices <- function(i, isobands, pump.dist, ortho.dist, diag.dist) {
  sel <- pump.dist$distance > isobands[i] &
         pump.dist$distance <= isobands[i + 1]
  neighborhood.points <- pump.dist[sel, "case"]
  vertices <- peripheryCases(neighborhood.points)
  d <- c(stats::dist(cholera::regular.cases[vertices, ]))
  d <- stats::setNames(as.data.frame(d), "d")
  idx <- index0(vertices)
  distance <- data.frame(v1 = vertices[idx$V1], v2 = vertices[idx$V2],
    d = signif(d), stringsAsFactors = FALSE)

  adjacency <- distance[distance$d == ortho.dist | distance$d == diag.dist, ]
  adjacency$node <- paste0(adjacency$v1, "-", adjacency$v2)
  edge.list <- adjacency[, c("v1", "v2")]
  g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

  if (length(igraph::articulation_points(g)) == 0) {
    if (igraph::components(g)$no == 1) travelingSalesman(vertices)
    else {
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
        lapply(components, travelingSalesman)
      }
    }
  } else {
    network.components <- igraph::biconnected.components(g)$components
    lapply(network.components, function(x) travelingSalesman(names(x)))
  }
}

#' Plot method for isoVertices().
#'
#' @param x An object of class "iso" created by \code{isoVertices()}.
#' @param selected.post Character or Numeric. Select milepost polygon. "all" or number.
#' @param palette Character.
#' @param alpha.level Numeric. Alpha level transparency
#' @param ... Additional arguments.
#' @return A vector with observed counts.
#' @noRd

plot.iso <- function(x, selected.post = "all", palette = "plasma",
  alpha.level = 1/2, ...) {

  pump.dist <- cholera::sim.walking.distance
  isobands <- seq(0, 600, x$post)

  if (palette == "plasma") {
    mypalette <- viridisLite::plasma(length(isobands), alpha = alpha.level,
      begin = 0, end = 1, direction = -1)
  }

  if (is.numeric(selected.post)) {
    if (selected.post %in% names(x$vertices) == FALSE) {
      stop('If numeric, selected.post must be ',
           paste(names(x$vertices), collapse = ", "), ".")
    }

    i <- which(names(x$vertices) == selected.post)
    vertices <- x$vertices[[i]]
    color <- mypalette[i]
    if (is.atomic(vertices)) {
      polygon(cholera::regular.cases[vertices, ], col = color)
    } else {
      invisible(lapply(vertices, function(dat) {
        polygon(cholera::regular.cases[dat, ], col = color)
      }))
    }
  } else if (is.character(selected.post)) {
    if (selected.post != "all") {
      stop('If not numeric, only other choice for selected.post is "all".')
    } else {
      invisible(lapply(seq_along(x$vertices), function(i) {
        color <- mypalette[i]
        if (is.list(x$vertices[[i]])) {
          invisible(lapply(x$vertices[[i]], function(vs) {
            polygon(cholera::regular.cases[vs, ], col = color)
          }))
        } else {
          polygon(cholera::regular.cases[x$vertices[[i]], ], col = color)
        }
      }))
    }
  }
}

#' Print method for isoVertices().
#'
#' @param x An object of class "iso" created by \code{isoVertices()}.
#' @param ... Additional arguments.
#' @return A vector with observed counts.
#' @export

print.iso <- function(x, ...) {
  print(vapply(x$vertices, length, numeric(1L)))
}
