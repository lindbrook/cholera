#' Census of overlapping road line segment endpoints.
#'
#' "Resolution" of georeferenced points with pch = 46 (i.e., pch = ".").
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @noRd

pointOverlapRoad <- function(inter.point.dist = 0.15) {
  g <- thresholdRoadGraph(inter.point.dist = inter.point.dist)
  subgraphs <- igraph::decompose(g)
  names(subgraphs) <- seq_along(subgraphs)
  census <- igraph::groups(igraph::components(g))
  census.ct <- vapply(census, length, integer(1L))
  dyad <- census[census.ct == 2]
  ntuple <- census[census.ct > 2]
  dyad <- do.call(rbind, lapply(dyad, as.numeric))
  dyad <- data.frame(group.id = as.numeric(row.names(dyad)), case1 = dyad[, 1],
    case2 = dyad[, 2], row.names = NULL)
  out <- list(dyad = dyad, ntuple = ntuple, subgraphs = subgraphs)
  class(out) <- "pointOverlapRoad"
  out
}

#' Plot method for pointOverlapRoad().
#'
#' @param x Object.
#' @param data Character. "ntuple" or "dyad".
#' @param type Character. "map" or "graph".
#' @param ... Additional plotting parameters.
#' @noRd

plot.pointOverlapRoad <- function(x, data = "ntuple", type = "map", ...) {
  vars <- c("x", "y")

  if (data == "ntuple") {
    ntuple <- x$ntuple
    subgraphs <- x$subgraphs
    ntuple.ct <- vapply(ntuple, length, integer(1L))
    names(ntuple.ct) <- names(ntuple)
    ntuple.ct <- sort(ntuple.ct, decreasing = TRUE)

    if (type == "graph") {
      invisible(lapply(names(ntuple.ct), function(nm) {
        plot(subgraphs[[nm]], vertex.color = "white", vertex.size = 30)
        title(main = paste("Group ", nm))
      }))
    } else if (type == "map") {
      invisible(lapply(names(ntuple.ct), function(nm) {
        rd.nm <- cholera::roads[cholera::roads$id %in% ntuple[[nm]], ]$name

        if (length(unique(rd.nm)) == 1) {
          rd.nm <- unique(rd.nm)
        } else {
          nm.tbl <- table(rd.nm)
          rd.nm <- names(nm.tbl[which.max(nm.tbl)])
        }

        sub.edge.list <- igraph::as_edgelist(subgraphs[[nm]])
        sub.edge.list <- data.frame(v1 = as.numeric(sub.edge.list[, 1]),
                                    v2 = as.numeric(sub.edge.list[, 2]))

        plot(cholera::roads[cholera::roads$id %in% ntuple[[nm]], vars],
          pch = NA, asp = 1.6)
        addRoads()

        invisible(lapply(seq_along(sub.edge.list$v1), function(i) {
          p1 <- sub.edge.list$v1[i]
          p2 <- sub.edge.list$v2[i]
          segments(cholera::roads[cholera::roads$id == p1, ]$x,
                   cholera::roads[cholera::roads$id == p1, ]$y,
                   cholera::roads[cholera::roads$id == p2, ]$x,
                   cholera::roads[cholera::roads$id == p2, ]$y,
                   col = grDevices::adjustcolor("red", alpha.f = 2/3))
        }))

        invisible(lapply(ntuple[[nm]], function(id) {
          text(cholera::roads[cholera::roads$id == id, vars], labels = id)
        }))

        sel <- cholera::roads$id %in% ntuple[[nm]]
        points(cholera::roads[!sel, vars], col = "gray")
        title(main = rd.nm, sub = paste("Group", nm))
      }))
    } else stop('type must be "graph" or "map".', call. = FALSE)

  } else if (data == "dyad") {
    dyad <- x$dyad
    if (type == "graph") {
      stop("Not applicable or illuminating.", call. = FALSE)
    } else if (type == "map") {
      invisible(lapply(seq_along(dyad$group.id), function(i) {
        pts <- unlist(dyad[i, c("case1", "case2")])
        rd.nm <- cholera::roads[cholera::roads$id %in% pts, ]$name

        if (length(unique(rd.nm)) == 1) {
          rd.nm <- unique(rd.nm)
        } else {
          nm.tbl <- table(rd.nm)
          rd.nm <- names(nm.tbl[which.max(nm.tbl)])
        }

        streetNameLocator(rd.nm, add.pump = FALSE, add.subtitle = FALSE,
          cases = NULL, highlight = FALSE, zoom = 0)

        segments(cholera::roads[cholera::roads$id == pts[1], ]$x,
                 cholera::roads[cholera::roads$id == pts[1], ]$y,
                 cholera::roads[cholera::roads$id == pts[2], ]$x,
                 cholera::roads[cholera::roads$id == pts[2], ]$y,
                 col = grDevices::adjustcolor("red", alpha.f = 2/3))

        sel <- cholera::roads$id %in% pts
        points(cholera::roads[sel, vars], col = "red", pch = 16)
        points(cholera::roads[!sel, vars], col = "gray")
        title(sub = paste("Group", dyad[i, "group.id"], ":",
          paste(pts, collapse = ", ")))
      }))
    }
  } else {
    stop('data must be "map" or "graph".', call. = FALSE)
  }
}
