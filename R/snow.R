#' Compute Snow's graphical annotation of Broad Street pump neighborhood.
#'
#' Computational approximation.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @export

neighborhoodSnow <- function(latlong = FALSE) {
  dat <- neighborhoodData(case.set = "observed", vestry = FALSE,
    latlong = FALSE)

  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges

  p.select <- nodes[nodes$pump == 7L, ]

  sel <- cholera::snow.neighborhood %in% cholera::fatalities.address$anchor
  snow.anchors <- cholera::snow.neighborhood[sel]

  path.id2 <- lapply(snow.anchors, function(cs) {
    p <- igraph::shortest_paths(graph = g,
                                from = nodes[nodes$case == cs, "node"],
                                to = p.select$node,
                                weights = edges$d)

    p <- names(unlist(p$vpath))

    edge.select <- vapply(seq_along(p[-1]), function(i) {
      ab <- edges$node1 %in% p[i] & edges$node2 %in% p[i + 1]
      ba <- edges$node2 %in% p[i] & edges$node1 %in% p[i + 1]
      which(ab | ba)
    }, numeric(1L))

    edges[edge.select, "id2"]
  })

  names(path.id2) <- snow.anchors

  out <- list(edges = edges, path.id2 = path.id2, snow.anchors = snow.anchors,
    latlong = latlong)
  class(out) <- "snow"
  out
}

#' Plot method for neighborhoodSnow().
#'
#' @param x An object of class "neighborhood_snow" created by \code{neighborhoodSnow()}.
#' @param type Character. "roads", "area.points" or "area.polygons".
#' @param non.snow.cases Logical. Plot anchor cases outside Snow neighborhood.
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param polygon.type Character. "border" or "solid".
#' @param polygon.col Character.
#' @param polygon.lwd Numeric.
#' @param add Logical. Add graphic to plot.
#' @param ... Additional plotting parameters.
#' @export

plot.snow <- function(x, type = "area.polygons", non.snow.cases = TRUE,
  alpha.level = 1/3, polygon.type = "solid", polygon.col = NULL,
  polygon.lwd = NULL, add = FALSE, ...) {

  if (x$latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  edges <- x$edges
  id2 <- unique(unlist(x$path.id2))

  z <- neighborhoodWalking(pump.select = 7)

  if (!add) snowMap(add.cases = FALSE, add.pumps = FALSE)

  if (type == "roads") {
    p7.col <- cholera::snowColors()["p7"]
    snow.edge <- edges[edges$id2 %in% id2, ]
    segments(snow.edge$x1, snow.edge$y1, snow.edge$x2, snow.edge$y2,
      col = p7.col, lwd = 2)
    if (!add) pumpTokens(z, type = "obseved")
    sel <- cholera::fatalities.address$anchor %in% x$snow.anchors
    points(cholera::fatalities.address[sel, vars], pch = 16, col = p7.col,
      cex = 0.5)
    if (non.snow.cases) {
      points(cholera::fatalities.address[!sel, vars],  pch = 16, col = "red",
        cex = 0.5)
    }
  } else if (type %in% c("area.points", "area.polygons")) {
    seg.data <- data.frame(do.call(rbind, strsplit(id2, "-")))
    names(seg.data) <- c("street", "subseg")
    seg.data$street <- as.numeric(seg.data$street)
    seg.data$id <- paste0(seg.data$street, "-", substr(seg.data$subseg, 1, 1))
    seg.data$subseg <- NULL
    seg.data <- cbind(seg.data, id2)
    seg.data <- seg.data[order(seg.data$street, seg.data$id2), ]

    seg.audit <- vapply(unique(seg.data$id), function(x) {
      seg.ID2 <- edges[edges$id %in% x, "id2"]
      snow.ID2 <- seg.data[seg.data$id == x, "id2"]
      all(seg.ID2 %in% snow.ID2)
    }, logical(1L))

    whole <- unique(seg.data$id)[seg.audit]

    # manual adjustment for "holes" #

    whole.segment.manual <- c("Dufours Place", "Tent Court", "Portland Mews",
      "St James Workhouse", "Bentinck Street", "Kemps Court", "St Anns Place",
      "Pulteney Court (I)", "Cock Court", "New Street/Husband Street",
      "Maidenhead Court", "Hopkins Street")
    sel <- cholera::road.segments$name %in% whole.segment.manual
    whole.segment.manual.id <- cholera::road.segments[sel, "id"]
    whole <- c(whole, setdiff(whole.segment.manual.id, whole))

    # partially transversed segments

    partial <- unique(seg.data$id)[!seg.audit]
    partial <- setdiff(partial, whole)

    transversed.subsegs <- lapply(unique(partial), function(x) {
      seg.ID2 <- edges[edges$id %in% x, "id2"]
      snow.ID2 <- seg.data[seg.data$id == x, "id2"]
      intersect(seg.ID2, snow.ID2)
    })

    names(transversed.subsegs) <- partial

    sel <- cholera::road.segments$id %in% whole
    whole.data <- rbind(
      stats::setNames(cholera::road.segments[sel, paste0(vars, 1)], vars),
      stats::setNames(cholera::road.segments[sel, paste0(vars, 2)], vars))

    sel <- cholera::sim.ortho.proj$road.segment %in% whole
    whole.case <- cholera::sim.ortho.proj[sel, "case"]

    sim.whole <- cholera::regular.cases[whole.case - 2000L, ]

    sim.partial.case <- lapply(names(transversed.subsegs), function(nm) {
      sel <- cholera::ortho.proj$road.segment == nm &
        cholera::ortho.proj$case %in% cholera::fatalities.address$anchor &
        cholera::ortho.proj$case %in% cholera::snow.neighborhood

      obs.case <- cholera::ortho.proj[sel, ]

      if (nrow(obs.case) > 1) obs.case <- obs.case[order(obs.case$x.proj), ]

      if (any(obs.case$type == "eucl")) {
        euclidean <- which(obs.case$type == "eucl")
        ts <- c(transversed.subsegs[[nm]][euclidean], transversed.subsegs[[nm]])
        seg.case <- data.frame(obs.case, id2 = ts)
      } else {
        seg.case <- data.frame(obs.case, id2 = transversed.subsegs[[nm]])
      }

      if (any(grepl("a", seg.case$id2))) {
        first.seg <- seg.case[which.max(seg.case$x), "id2"]
        exit <- 2L
      } else {
        first.seg <- seg.case[which.min(seg.case$x), "id2"]
        exit <- 1L
      }

      obs.case <- seg.case[seg.case$id2 == first.seg, ]

      sel <- cholera::sim.ortho.proj$road.segment == nm
      sim.segment <- cholera::sim.ortho.proj[sel, ]

      if (exit == 1) {
        sim.segment[sim.segment$x.proj >= obs.case$x.proj, "case"]
      } else if (exit == 2) {
        sim.segment[sim.segment$x.proj <= obs.case$x.proj, "case"]
      }
    })

    sim.partial <- cholera::regular.cases[unlist(sim.partial.case) - 2000L, ]

    sim.data <- rbind(sim.whole, sim.partial)
    p7.col <- grDevices::adjustcolor(snowColors()["p7"], alpha.f = alpha.level)

    if (type == "area.points") {
      points(sim.data, col = p7.col, pch = 16, cex = 0.25)
      if (!add) pumpTokens(z, type = "obseved")

    } else if (type == "area.polygons") {
      if (is.null(polygon.col)) polygon.col <- p7.col

      periphery.cases <- peripheryCases(row.names(sim.data))
      pearl.string <- travelingSalesman(periphery.cases,
        tsp.method = "repetitive_nn")

      if (polygon.type == "border") {
        if (is.null(polygon.lwd)) polygon.lwd <- 3
        polygon(cholera::regular.cases[pearl.string, ], border = polygon.col,
          lwd = polygon.lwd)
      } else if (polygon.type == "solid") {
        if (is.null(polygon.lwd)) polygon.lwd <- 1
        polygon(cholera::regular.cases[pearl.string, ], col = polygon.col,
          lwd = polygon.lwd)
      }

      if (!add) pumpTokens(z, type = "obseved")
    }

    if (non.snow.cases) {
      sel <- !cholera::fatalities.address$anchor %in% x$snow.anchors
      points(cholera::fatalities.address[sel, vars],  pch = 16, col = "red",
        cex = 0.5)
    }
  }
  if (!add) title("Snow's Graphical Annotation Neighborhood")
}
