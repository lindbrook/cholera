#' Census of overlapping fatalties
#'
#' "Resolution" of georeferenced points with pch = 46 (i.e., pch = ".").
#' @param path Character. e.g., "~/Documents/Data/"
#' @param dist.between.pts Numeric. Ceiling for overlapping points.
#' @param output Character. "dyad" (data frame) or "n-tuple" (PDF)
#' @export

pointOverlap <- function(path, dist.between.pts = 0.15, output = "n-tuple") {
  idx <- data.frame(t(utils::combn(cholera::fatalities.address$anchor, 2)))
  names(idx) <- c("v1", "v2")
  d <- stats::dist(cholera::fatalities.address[, c("x", "y")])
  addr.dist <- data.frame(idx, d = c(d))
  overlap <- addr.dist[addr.dist$d <= dist.between.pts, ]
  edge.list <- overlap[, c("v1", "v2")]
  g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

  subgraphs <- igraph::decompose(g)
  census <- igraph::groups(igraph::components(g))
  census.ct <- vapply(census, length, integer(1L))

  dyads <- census[census.ct == 2]
  ntuple <- census[census.ct > 2]
  ntuple.nm <- unname(vapply(ntuple, function(x) as.numeric(x[1]), numeric(1L)))
  ntuple.id <- as.numeric(names(ntuple))

  if (output == "dyad") {
    dat <- do.call(rbind, lapply(dyads, as.numeric))
    data.frame(group.id = as.numeric(row.names(dat)), case1 = dat[, 1],
      case2 = dat[, 2], row.names = NULL)
  } else if (output == "n-tuple") {
    grDevices::pdf(file = paste0(path, "components.pdf"))
    invisible(lapply(seq_along(ntuple.id), function(i) {
      id <- ntuple.id[i]
      nm <- ntuple.nm[i]
      plot(subgraphs[[id]], vertex.color = "white", vertex.size = 0)
      title(main = paste("Group ", nm))
    }))
    grDevices::dev.off()
  } else {
    stop('output must be "dyad" or "n-tuple"')
  }
}
