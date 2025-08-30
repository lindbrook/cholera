#' Compute Snow's graphical annotation of Broad Street pump neighborhood.
#'
#' Computational approximation.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param vestry Logical. Moves Broad Street pump slightly.
#' @export

neighborhoodSnow <- function(latlong = FALSE, vestry = FALSE) {
  dat <- sohoGraph(case.set = "observed", vestry = vestry, latlong = latlong)
  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges

  p.select <- nodes[nodes$pump == 7L, ]

  sel <- cholera::snow.neighborhood %in% cholera::fatalities.anchor$anchor
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
    latlong = latlong, vestry = vestry)
  class(out) <- "snow"
  out
}

#' Plot method for neighborhoodSnow().
#'
#' @param x An object of class "neighborhood_snow" created by \code{neighborhoodSnow()}.
#' @param type Character. "roads", "area.points" or "area.polygons".
#' @param snow.cases Logical. Plot anchor cases inside the Snow neighborhood.
#' @param non.snow.cases Logical. Plot anchor cases outside the Snow neighborhood.
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param polygon.type Character. "border" or "solid".
#' @param polygon.col Character.
#' @param polygon.lwd Numeric.
#' @param data.summary Logical. Show tabulation subtitle.
#' @param add Logical. Add graphic to plot.
#' @param ... Additional plotting parameters.
#' @export

plot.snow <- function(x, type = "area.polygons", snow.cases = FALSE, 
  non.snow.cases = TRUE, alpha.level = 1/3, polygon.type = "solid", 
  polygon.col = NULL, polygon.lwd = NULL, data.summary = FALSE,
  add = FALSE, ...) {

  if (x$latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  edges <- x$edges
  id2 <- unique(unlist(x$path.id2))

  if (!add) {
    snowMap(add.cases = FALSE, add.frame = TRUE, add.pumps = FALSE, 
      latlong = x$latlong)
  }

  if (type == "roads") {
    p7.col <- cholera::snowColors()["p7"]
    snow.edge <- edges[edges$id2 %in% id2, ]
    
    if (x$latlong) {
      segments(snow.edge$lon1, snow.edge$lat1, snow.edge$lon2, snow.edge$lat2,
        col = p7.col, lwd = 2)
    } else {
      segments(snow.edge$x1, snow.edge$y1, snow.edge$x2, snow.edge$y2,
        col = p7.col, lwd = 2)  
    }
    
    sel <- cholera::fatalities.anchor$anchor %in% x$snow.anchors
    points(cholera::fatalities.anchor[sel, vars], pch = 16, col = p7.col,
      cex = 0.5)
    if (non.snow.cases) {
      points(cholera::fatalities.anchor[!sel, vars], pch = 16, col = "red",
        cex = 0.5)
    }
  
  } else if (type %in% c("area.points", "area.polygons")) {
    
    # road segments and sub-segments #

    seg.data <- data.frame(do.call(rbind, strsplit(id2, "-")))
    names(seg.data) <- c("street", "subseg")
    seg.data$street <- as.numeric(seg.data$street)
    seg.data$id <- paste0(seg.data$street, "-", substr(seg.data$subseg, 1, 1))
    seg.data$subseg <- NULL
    seg.data <- cbind(seg.data, id2)
    seg.data <- seg.data[order(seg.data$street, seg.data$id2), ]

    # all sub-segments observed in Snow neighborhood #

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
    whole <- union(whole, whole.segment.manual.id)
    
    # partially traversed segments #

    partial <- unique(seg.data$id)[!seg.audit]
    partial <- setdiff(partial, whole)

    traversed.subsegs <- lapply(partial, function(x) {
      seg.ID2 <- edges[edges$id == x, "id2"]
      snow.ID2 <- seg.data[seg.data$id == x, "id2"]
      intersect(seg.ID2, snow.ID2)
    })

    names(traversed.subsegs) <- partial

    # simulated cases on whole segments
    
    if (x$latlong) {
      sel <- cholera::latlong.sim.ortho.proj$road.segment %in% whole
      whole.case <- cholera::latlong.sim.ortho.proj[sel, "case"]
      sim.whole <- cholera::latlong.regular.cases[whole.case, ]
    } else {
      sel <- cholera::sim.ortho.proj$road.segment %in% whole
      whole.case <- cholera::sim.ortho.proj[sel, "case"]
      sim.whole <- cholera::regular.cases[whole.case - 2000L, ]
    }
    
    # simulated cases on sub-segments (partially traversed segments)

    sim.partial.case <- lapply(names(traversed.subsegs), function(nm) {
      if (x$latlong) {
        sel <- cholera::latlong.ortho.anchor$road.segment == nm &
          cholera::latlong.ortho.anchor$case %in% cholera::snow.neighborhood
        obs.case <- cholera::latlong.ortho.anchor[sel, ]
      } else {
        sel <- cholera::ortho.proj$road.segment == nm &
          cholera::ortho.proj$case %in% cholera::fatalities.anchor$anchor &
          cholera::ortho.proj$case %in% cholera::snow.neighborhood
        obs.case <- cholera::ortho.proj[sel, ]  
      }
      
      if (nrow(obs.case) > 1) obs.case <- obs.case[order(obs.case$x.proj), ]

      if (any(obs.case$type == "eucl")) {
        euclidean <- which(obs.case$type == "eucl")
        ts <- c(traversed.subsegs[[nm]][euclidean], traversed.subsegs[[nm]])
        seg.case <- data.frame(obs.case, id2 = ts)
      } else {
        seg.case <- data.frame(obs.case, id2 = traversed.subsegs[[nm]])
      }

      if (x$latlong) {
        if (any(grepl("a", seg.case$id2))) {
          first.seg <- seg.case[which.max(seg.case$lon), "id2"]
          exit <- 2L
        } else {
          first.seg <- seg.case[which.min(seg.case$lon), "id2"]
          exit <- 1L
        }
      } else {
        if (any(grepl("a", seg.case$id2))) {
          first.seg <- seg.case[which.max(seg.case$x), "id2"]
          exit <- 2L
        } else {
          first.seg <- seg.case[which.min(seg.case$x), "id2"]
          exit <- 1L
        }
      }

      obs.case <- seg.case[seg.case$id2 == first.seg, ]

      if (x$latlong) {
        sel <- cholera::latlong.sim.ortho.proj$road.segment == nm
        sim.segment <- cholera::latlong.sim.ortho.proj[sel, ]
        out.var <- "lon"
      } else {
        sel <- cholera::sim.ortho.proj$road.segment == nm
        sim.segment <- cholera::sim.ortho.proj[sel, ]
        out.var <- "x.proj"
      }
     
      if (exit == 1) {
        sim.segment[sim.segment[, out.var] >= obs.case[, out.var], "case"]
      } else if (exit == 2) {
        sim.segment[sim.segment[, out.var] <= obs.case[, out.var], "case"]
      }
    })

    if (x$latlong) {
      sim.partial <- cholera::latlong.regular.cases[unlist(sim.partial.case), ]
    } else {
      sim.partial <- cholera::regular.cases[unlist(sim.partial.case) - 2000L, ]  
    }
    
    sim.data <- rbind(sim.whole, sim.partial)
    p7.col <- grDevices::adjustcolor(snowColors()["p7"], alpha.f = alpha.level)

    if (type == "area.points") {
      points(sim.data[, vars], col = p7.col, pch = 16, cex = 0.25)
    } else if (type == "area.polygons") {
      if (is.null(polygon.col)) polygon.col <- p7.col
      
      periphery.cases <- peripheryCases(row.names(sim.data),
        latlong = x$latlong)
      
      pearl.string <- travelingSalesman(periphery.cases, latlong = x$latlong)

      if (x$latlong) {
        if (polygon.type == "border") {
          if (is.null(polygon.lwd)) polygon.lwd <- 3
          polygon(cholera::latlong.regular.cases[pearl.string, vars],
            border = polygon.col, lwd = polygon.lwd)
        } else if (polygon.type == "solid") {
          if (is.null(polygon.lwd)) polygon.lwd <- 1
          polygon(cholera::latlong.regular.cases[pearl.string, vars], 
            col = polygon.col, lwd = polygon.lwd)
        }
      } else {
        if (polygon.type == "border") {
          if (is.null(polygon.lwd)) polygon.lwd <- 3
          polygon(cholera::regular.cases[pearl.string, ], border = polygon.col,
            lwd = polygon.lwd)
        } else if (polygon.type == "solid") {
          if (is.null(polygon.lwd)) polygon.lwd <- 1
          polygon(cholera::regular.cases[pearl.string, ], col = polygon.col,
            lwd = polygon.lwd)
        }
      }
    }

    if (snow.cases) {
      sel <- cholera::fatalities.anchor$anchor %in% x$snow.anchors
      points(cholera::fatalities.anchor[sel, vars], pch = 16, col = p7.col,
        cex = 0.5)
    }

    if (non.snow.cases) {
      sel <- !cholera::fatalities.anchor$anchor %in% x$snow.anchors
      points(cholera::fatalities.anchor[sel, vars], pch = 16, col = "red",
        cex = 0.5)
    }
  }
  
  if (!add) {  
    if (x$vestry) {
      p.data <- cholera::pumps.vestry[cholera::pumps.vestry$id == 7L, vars]
    } else {
      p.data <- cholera::pumps[cholera::pumps$id == 7L, vars]
    }
    points(p.data, pch = 2)
    text(p.data, cex = 0.9, labels = "p7", pos = 1)
    title("Snow's Graphical Annotation Neighborhood")
    
    if (data.summary) {
      tabulation <- summary(x)
      string <- paste0("anchors: ",
                       names(tabulation)[1], " = ", tabulation[1], ", ",
                       names(tabulation)[2], " = ", tabulation[2])
      title(sub = string)
    }
  }
}

#' Print method for neighborhoodSnow().
#'
#' Parameter values for neighborhoodSnow().
#' @param x An object of class "snow" created by \code{neighborhoodSnow()}.
#' @param ... Additional parameters.
#' @return A list of argument values.
#' @export
#' @examples
#' \dontrun{
#' neighborhoodSnow()
#' print(neighborhoodSnow())
#' }

print.snow <- function(x, ...) {
  dat <- unlist(summary(x))
  out <- list(x[["latlong"]], dat)
  names(out) <- c("latlong", "anchor.data")
  print(out)
}

#' Summary method for neighborhoodSnow().
#'
#' Return computed counts for Snow neighborhood.
#' @param object Object. An object of class "snow" created by \code{neighborhoodSnow()}.
#' @param ... Additional parameters.
#' @return A data frame of counts inside and outside of Snow's neighborhood.
#' @export
#' @examples
#' \dontrun{
#' summary(neighborhoodSnow())
#' }

summary.snow <- function(object, ...) {
  tbl <- table(cholera::fatalities.anchor$anchor %in% object$snow.anchors)
  data.frame(inside = tbl["TRUE"], outside = tbl["FALSE"], row.names = NULL)
}
