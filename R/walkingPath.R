#' Plot the shortest walking path between cases and/or pumps.
#'
#' Beta v.1
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is NULL, which returns closest pump or "anchor" case.
#' @param type Character. "case-pump", "cases" or "pumps".
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param weighted Logical. TRUE computes shortest path weighted by road distance. FASLE computes shortes path by number of nodes.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. Meaningful only when "weighted" = TRUE. See \code{vignette("roads")} for information on unit distances.
#' @return A base R graphics plot.
#' @seealso \code{\link{fatalities}}, \code{vignette("pump.neighborhoods")}
#' @import graphics
#' @export
#' @examples
#' # path from case 1 to nearest pump.
#' walkingPath(1)
#'
#' # path from case 1 to nearest pump in meters (appox).
#' walkingPath(1, unit = "meter")
#'
#' # path from case 1 to pump 6.
#' walkingPath(1, 6)
#'
#' # exclude pump 7 from consideration.
#' walkingPath(1, -7)
#'
#' # path from case 1 to case 6.
#' walkingPath(1, 6, type = "cases")
#'
#' # path from pump 1 to pump 6.
#' walkingPath(1, 6, type = "pumps")

walkingPath <- function(origin, destination = NULL, type = "case-pump",
  zoom = TRUE, radius = 0.5, weighted = TRUE, vestry = FALSE, unit = NULL) {

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE) {
      stop('If specified, "unit" must either be "meter" or "yard".')
    }
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('"type" must be "case-pump", "cases" or "pumps".')
  }

  if (vestry) {
    colors <- cholera::snowColors(vestry = TRUE)
    node.data <- cholera::nodeData(vestry = TRUE)
    pumps <- cholera::pumps.vestry
  } else {
    colors <- cholera::snowColors()
    node.data <- cholera::nodeData()
    pumps <- cholera::pumps
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g

  if (type == "case-pump") {
    if (origin %in% 1:578 == FALSE) {
      stop('With type = "case-pump", "origin" must be between 1 and 578.')
    }

    if (!is.null(destination)) {
      if (vestry) {
        if (any(abs(destination) %in% 1:14 == FALSE)) {
          txt1 <- 'With type = "case-pump" and "vestry = TRUE",'
          txt2 <- '1 >= |destination| <= 14.'
          stop(paste(txt1, txt2))
        }
      } else {
        if (any(abs(destination) %in% 1:13 == FALSE)) {
          txt1 <- 'With type = "case-pump" and "vestry = FALSE",'
          txt2 <- '1 >= |destination| <= 13.'
          stop(paste(txt1, txt2))
        }
      }
    }

    ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
      "anchor.case"]
    ego.node <- nodes[nodes$anchor == ego.id, "node"]

    if (!is.null(destination)) {
      if (all(destination < 0)) {
        p.nodes <- nodes[nodes$pump != 0, ]
        alters <- p.nodes[p.nodes$pump %in% abs(destination) == FALSE, "node"]
      } else {
        alters <- nodes[nodes$pump %in% destination, "node"]
      }
    } else {
      alters <- nodes[nodes$pump != 0, "node"]
    }

    if (weighted) {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x, weights = edges$d)
      }, numeric(1L))
    } else {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x)
      }, numeric(1L))
    }

    nearest <- which.min(d)
    alter.id <- nodes[nodes$node %in% names(nearest), "pump"]
    alter.node <- nodes[nodes$node %in% names(nearest), "node"]

    if (weighted) {
      path <- names(unlist(igraph::shortest_paths(g, ego.node, alter.node,
        weights = edges$d)$vpath))
    } else {
      path <- names(unlist(igraph::shortest_paths(g, ego.node,
        alter.node)$vpath))
    }

    dat <- numericNodeCoordinates(path)
    n1 <- dat[1:(nrow(dat) - 1), ]
    n2 <- dat[2:nrow(dat), ]

    if (zoom) {
      x.rng <- c(min(dat$x) - radius, max(dat$x) + radius)
      y.rng <- c(min(dat$y) - radius, max(dat$y) + radius)
    } else {
      x.rng <- range(cholera::roads$x)
      y.rng <- range(cholera::roads$y)
    }

    case.color <- colors[alter.id]

    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      xlab = "x", ylab = "y", pch = 15, cex = 0.5, col = "lightgray", asp = 1)
    invisible(lapply(roads.list, lines, col = "lightgray"))
    invisible(lapply(border.list, lines))
    title(main = paste("Case", ego.id, "to Pump", alter.id))

    if (vestry) {
      pump.names <- paste0("p", cholera::pumps.vestry$id)
      points(cholera::pumps.vestry[, c("x", "y")], pch = 24, cex = 1,
        col = colors)
      text(cholera::pumps.vestry[, c("x", "y")], label = pump.names, pos = 1)
    } else {
      pump.names <- paste0("p", cholera::pumps$id)
      points(cholera::pumps[, c("x", "y")], pch = 24, cex = 1, col = colors)
      text(cholera::pumps[, c("x", "y")], label = pump.names, pos = 1)
    }

    if (zoom) {
      points(cholera::fatalities[cholera::fatalities$case == ego.id,
        c("x", "y")],col = "red")
      text(cholera::fatalities[cholera::fatalities$case == ego.id,
        c("x", "y")], labels = ego.id, pos = 1, col = "red")
      points(dat[1, c("x", "y")], col = case.color, pch = 0)
      points(dat[nrow(dat), c("x", "y")], col = case.color, pch = 0)
    } else {
      points(cholera::fatalities[cholera::fatalities$case == ego.id,
        c("x", "y")], col = "red")
      points(dat[1, c("x", "y")], col = case.color, pch = 0)
      points(dat[nrow(dat), c("x", "y")], col = case.color, pch = 0)
    }

    if (zoom) {
      arrows(n1$x, n1$y, n2$x, n2$y, col = case.color, lwd = 2, length = 0.05)
    } else {
      sel <- seq_len(nrow(dat))
      med <- round(stats::median(sel))
      selA <- sel[-med]
      selB <- sel[med]
      segments(n1$x[selA], n1$y[selA], n2$x[selA], n2$y[selA],
        col = case.color, lwd = 2)
      arrows(n1$x[selB], n1$y[selB], n2$x[selB], n2$y[selB],
        col = case.color, lwd = 2, length = 0.075)
    }

  } else if (type == "cases") {
    if (any(abs(c(origin, destination)) %in% 1:578 == FALSE)) {
      txt1 <- 'With type = "cases", the absolute value of both "origin"'
      txt2 <- 'and "destination" must be between 1 and 578.'
      stop(paste(txt1, txt2))
    }

    ego.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
      origin, "anchor.case"])
    ego.anchor <- cholera::anchor.case[cholera::anchor.case$case == ego.id,
      "anchor.case"]

    if (is.null(destination)) {
      alters.id <- cholera::fatalities.address$anchor.case
      alters.id <- alters.id[alters.id != ego.anchor]
    } else {
      if (all(destination > 0)) {
        alters.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
          destination, "anchor.case"])
        alters.id <- alters.id[alters.id != ego.anchor]
      } else if (all(destination < 0)) {
        alters.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
          abs(destination) == FALSE, "anchor.case"])
        alters.id <- alters.id[alters.id != ego.anchor]
      }
    }

    if (identical(all.equal(ego.id, alters.id), TRUE)) {
      warning('Origin and destination case at same "address".')
    }

    ego.node <- nodes[nodes$anchor == ego.id, "node"]
    alter.nodes <- nodes[nodes$anchor %in% alters.id, "node"]

    if (weighted) {
      d <- vapply(alter.nodes, function(x) {
        igraph::distances(g, ego.node, x, weights = edges$d)
      }, numeric(1L))
    } else {
      d <- vapply(alter.nodes, function(x) {
        igraph::distances(g, ego.node, x)
      }, numeric(1L))
    }

    nearest <- which.min(d)
    nearest.node <- nodes[nodes$node == names(nearest), "node"]
    nearest.id <- nodes[nodes$node == nearest.node, "anchor"]

    if (weighted) {
      path <- names(unlist(igraph::shortest_paths(g, ego.node, nearest.node,
        weights = edges$d)$vpath))
    } else {
      path <- names(unlist(igraph::shortest_paths(g, ego.node,
        alter.node)$vpath))
    }

    dat <- numericNodeCoordinates(path)
    n1 <- dat[1:(nrow(dat) - 1), ]
    n2 <- dat[2:nrow(dat), ]

    if (zoom) {
      x.rng <- c(min(dat$x) - radius, max(dat$x) + radius)
      y.rng <- c(min(dat$y) - radius, max(dat$y) + radius)
    } else {
      x.rng <- range(cholera::roads$x)
      y.rng <- range(cholera::roads$y)
    }

    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      xlab = "x", ylab = "y", pch = 15, cex = 0.5, col = "lightgray", asp = 1)
    invisible(lapply(roads.list, lines, col = "lightgray"))
    invisible(lapply(border.list, lines, col = "gray"))
    title(main = paste("Case", origin, "to Case", nearest.id))

    points(cholera::fatalities[cholera::fatalities$case == ego.id,
      c("x", "y")], col = "red")
    text(cholera::fatalities[cholera::fatalities$case == ego.id,
      c("x", "y")], labels = ego.id, pos = 1, col = "red")

    points(cholera::fatalities[cholera::fatalities$case == nearest.id,
      c("x", "y")], col = "red")
    text(cholera::fatalities[cholera::fatalities$case == nearest.id,
      c("x", "y")], labels = nearest.id, pos = 1, col = "red")

    points(dat[1, c("x", "y")], col = "blue", pch = 0)
    points(dat[nrow(dat), c("x", "y")], col = "blue", pch = 0)

    if (zoom) {
      arrows(n1$x, n1$y, n2$x, n2$y, col = "blue", lwd = 2, length = 0.05)
    } else {
      sel <- seq_len(nrow(dat))
      med <- round(stats::median(sel))
      selA <- sel[-med]
      selB <- sel[med]
      segments(n1$x[selA], n1$y[selA], n2$x[selA], n2$y[selA],
        col = "blue", lwd = 2)
      arrows(n1$x[selB], n1$y[selB], n2$x[selB], n2$y[selB],
        col = "blue", lwd = 2, length = 0.075)
    }

  } else if (type == "pumps") {
    p.nodes <- nodes[nodes$pump > 0, ]
    ego.node <- p.nodes[p.nodes$pump == origin, "node"]


    if (is.null(destination)) {
      alters  <- p.nodes[p.nodes$pump != origin, "node"]
    } else {
      if (all(destination > 0)) {
        alters  <- p.nodes[p.nodes$pump %in% destination &
                           p.nodes$pump != origin, "node"]
      } else if (all(destination < 0)) {
        alters  <- p.nodes[p.nodes$pump %in% abs(destination) == FALSE &
                           p.nodes$pump != origin, "node"]
      }
    }

    if (weighted) {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x, weights = edges$d)
      }, numeric(1L))
    } else {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x)
      }, numeric(1L))
    }

    nearest <- which.min(d)
    alter.id <- p.nodes[p.nodes$node %in% names(nearest), "pump"]
    alter.node <- p.nodes[p.nodes$node %in% names(nearest), "node"]

    if (weighted) {
      path <- names(unlist(igraph::shortest_paths(g, ego.node, alter.node,
        weights = edges$d)$vpath))
    } else {
      path <- names(unlist(igraph::shortest_paths(g, ego.node,
        alter.node)$vpath))
    }

    dat <- numericNodeCoordinates(path)
    n1 <- dat[1:(nrow(dat) - 1), ]
    n2 <- dat[2:nrow(dat), ]

    if (zoom) {
      x.rng <- c(min(dat$x) - radius, max(dat$x) + radius)
      y.rng <- c(min(dat$y) - radius, max(dat$y) + radius)
    } else {
      x.rng <- range(cholera::roads$x)
      y.rng <- range(cholera::roads$y)
    }

    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      xlab = "x", ylab = "y", pch = 15, cex = 0.5, col = "lightgray", asp = 1)
    invisible(lapply(roads.list, lines, col = "lightgray"))
    invisible(lapply(border.list, lines, col = "gray"))
    title(main = paste("Pump", origin, "to Pump", alter.id))

    if (vestry) {
      pump.names <- paste0("p", cholera::pumps.vestry$id)
      points(cholera::pumps.vestry[, c("x", "y")], pch = 24, cex = 1,
        col = colors)
      text(cholera::pumps.vestry[, c("x", "y")], label = pump.names, pos = 1)
    } else {
      pump.names <- paste0("p", cholera::pumps$id)
      points(cholera::pumps[, c("x", "y")], pch = 24, cex = 1, col = colors)
      text(cholera::pumps[, c("x", "y")], label = pump.names, pos = 1)
    }

    points(dat[1, c("x", "y")], col = "blue", pch = 0)
    points(dat[nrow(dat), c("x", "y")], col = "blue", pch = 0)

    if (zoom) {
      arrows(n1$x, n1$y, n2$x, n2$y, col = "blue", lwd = 2, length = 0.05)
    } else {
      sel <- seq_len(nrow(dat))
      med <- round(stats::median(sel))
      selA <- sel[-med]
      selB <- sel[med]
      segments(n1$x[selA], n1$y[selA], n2$x[selA], n2$y[selA],
        col = "blue", lwd = 2)
      arrows(n1$x[selB], n1$y[selB], n2$x[selB], n2$y[selB],
        col = "blue", lwd = 2, length = 0.075)
    }
  }

  if (is.null(unit)) {
    title(sub = paste(round(d[nearest], 2), "units"))
  } else if (unit == "meter") {
    title(sub = paste(round(54 * d[nearest], 2), "meters"))
  } else if (unit == "yard") {
    title(sub = paste(round(177/3 * d[nearest], 2), "yards"))
  }
}

numericNodeCoordinates <- function(x) {
  nodes <- do.call(rbind, (strsplit(x, "-")))
  data.frame(x = as.numeric(nodes[, 1]), y = as.numeric(nodes[, 2]))
}
