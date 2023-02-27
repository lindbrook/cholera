#' Census of overlapping fatalities.
#'
#' "Resolution" of georeferenced points with pch = 46 (i.e., pch = ".").
#' @param inter.point.dist Numeric. Ceiling for overlapping points.
#' @noRd

pointOverlap <- function(inter.point.dist = 0.15) {
  g <- thresholdAddressGraph(inter.point.dist = inter.point.dist)
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
  class(out) <- "pointOverlap"
  out
}

#' Plot method for pointOverlap().
#'
#' @param x Object.
#' @param data Character. "ntuple" or "dyad".
#' @param type Character. "map" or "graph".
#' @param ... Additional plotting parameters.
#' @noRd

plot.pointOverlap <- function(x, data = "ntuple", type = "map", ...) {
  vars <- c("x", "y")

  if (data == "ntuple") {
    ntuple <- x$ntuple
    subgraphs <- x$subgraphs
    ntuple.ct <- vapply(ntuple, length, integer(1L))
    names(ntuple.ct) <- names(ntuple)
    ntuple.ct <- sort(ntuple.ct, decreasing = TRUE)

    if (type == "graph") {
      invisible(lapply(names(ntuple.ct), function(nm) {
        plot(subgraphs[[nm]], vertex.color = "white", vertex.size = 20)
        title(main = paste("Group ", nm))
      }))
    } else if (type == "map") {
      invisible(lapply(names(ntuple.ct), function(nm) {
        case <- sort(as.numeric(ntuple[[nm]]))
        sel <- cholera::ortho.proj$case %in% case
        seg.ct <- table(cholera::ortho.proj[sel, "road.segment"])

        if (length(seg.ct) == 1) {
          seg <- names(seg.ct)
        } else if (length(seg.ct) > 1) {
          seg <- names(sort(seg.ct, decreasing = TRUE))[1]
        }

        sub.edge.list <- igraph::as_edgelist(subgraphs[[nm]])
        sub.edge.list <- data.frame(v1 = as.numeric(sub.edge.list[, 1]),
                                    v2 = as.numeric(sub.edge.list[, 2]))

        segmentLocator(seg, add.subtitle = FALSE, cases = NULL,
          highlight = FALSE, zoom = 0.4)
        invisible(lapply(seq_along(sub.edge.list$v1), function(i) {
          c1 <- sub.edge.list$v1[i]
          c2 <- sub.edge.list$v2[i]
          segments(cholera::fatalities[cholera::fatalities$case == c1, ]$x,
                   cholera::fatalities[cholera::fatalities$case == c1, ]$y,
                   cholera::fatalities[cholera::fatalities$case == c2, ]$x,
                   cholera::fatalities[cholera::fatalities$case == c2, ]$y,
                   col = grDevices::adjustcolor("red", alpha.f = 2/3))
        }))
        sel <- cholera::fatalities$case %in% case
        text(cholera::fatalities[sel, vars], col = "black", cex = 1,
          labels = case)
        sel <- cholera::fatalities.address$anchor %in% case
        points(cholera::fatalities.address[!sel, vars], col = "gray")
        title(sub = paste("Group", nm))
      }))
    } else stop('type must be "graph" or "map".', call. = FALSE)

  } else if (data == "dyad") {
    dyad <- x$dyad
    if (type == "graph") {
      stop("Not applicable or illuminating.", call. = FALSE)
    } else if (type == "map") {
      lapply(seq_along(dyad$group.id), function(i) {
        case <- unlist(dyad[i, c("case1", "case2")])
        sel <- cholera::ortho.proj$case %in% case
        seg.ct <- table(cholera::ortho.proj[sel, "road.segment"])

        if (length(seg.ct) == 1) {
          seg <- names(seg.ct)
        } else if (length(seg.ct) > 1) {
          seg <- names(sort(seg.ct, decreasing = TRUE))[1]
        }

        segmentLocator(seg, add.subtitle = FALSE, cases = NULL,
          highlight = FALSE, zoom = 0.4)
        segments(cholera::fatalities[cholera::fatalities$case == case[1], ]$x,
                 cholera::fatalities[cholera::fatalities$case == case[1], ]$y,
                 cholera::fatalities[cholera::fatalities$case == case[2], ]$x,
                 cholera::fatalities[cholera::fatalities$case == case[2], ]$y,
                 col = grDevices::adjustcolor("red", alpha.f = 2/3))
        sel <- cholera::fatalities$case %in% case
        text(cholera::fatalities[sel, vars], col = "black", cex = 1,
          labels = case)
        sel <- cholera::fatalities.address$anchor %in% case
        points(cholera::fatalities.address[!sel, vars], col = "gray")
        title(sub = paste("Group", dyad$group.id[i]))
      })
    }
  } else {
    stop('data must be "map" or "graph".', call. = FALSE)
  }
}
