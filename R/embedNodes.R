#' Embed anchors, landmarks and/or pumps into road network (prototype).
#'
#' @param vestry Logical.
#' @param case.set Character. "observed" or "expected".
#' @param embed.anchor Logical. Embed all 321 or selected anchor cases into graph network.
#' @param embed.landmarks Logical or Numeric. Embed all or selected landmarks into road network.
#' @param embed.pumps Logical or Numeric. Embed all or selected pumps into road network.
#' @param latlong Logical or Numeric. Use estimated longitude and latitude.
#' @param drop.isolates Logical. Exclude Adam and Eve Court (and Pump #2) and Falconberg Court and Mews.
#' @param ellipsoid Character. "WGS" for WGS-84 or "BNG" for British National Gride (i.e., Airy 1830).
#' @importFrom geosphere distGeo
#' @noRd

embedNodes <- function(vestry = FALSE, case.set = "observed",
  embed.anchor = FALSE, embed.landmarks = FALSE, embed.pumps = FALSE,
  latlong = FALSE, drop.isolates = TRUE, ellipsoid = "WGS") {

  road.data <- cholera::road.segments

  if (drop.isolates) {
    adam.eve.ct <- "44-1"
    falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
    isolates <- c(adam.eve.ct, falconberg.ct.mews)
    road.data <- road.data[!road.data$id %in% isolates, ]
  }

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  if (ellipsoid == "WGS") {
    a <- 6378137
    f <- 1 / 298.257223563
  } else if (ellipsoid == "BNG") {
    a <- 6377563.396
    f <- 1 / 299.3249646
  } else {
    stop('ellipsoid must be "WGS" or "BNG".', call. = FALSE)
  }

  if ((isTRUE(embed.anchor) | is.numeric(embed.anchor)) &
      (isTRUE(embed.landmarks) | is.numeric(embed.landmarks)) &
      (isTRUE(embed.pumps) | is.numeric(embed.pumps))) {

    ortho.anchor <- orthoAnchor(case.set = case.set, latlong = latlong)
    ortho.land <- orthoLand(latlong = latlong)
    ortho.pump <- orthoPump(vestry = vestry, latlong = latlong,
      drop.isolates = drop.isolates)

    if (is.numeric(embed.anchor)) {
      ortho.anchor <- ortho.anchor[ortho.anchor$case %in% embed.anchor, ]
    }

    if (is.numeric(embed.landmarks)) {
      ortho.land <- ortho.land[ortho.land$case %in% embed.landmarks, ]
    }

    if (is.numeric(embed.pumps)) {
      ortho.pump <- ortho.pump[ortho.pump$id %in% embed.pumps, ]
    }

    obs.segs <- unique(c(ortho.anchor$road.segment, ortho.land$road.segment,
      ortho.pump$road.segment))

  } else if (isFALSE(embed.anchor) &
             (isTRUE(embed.landmarks) | is.numeric(embed.landmarks)) &
             (isTRUE(embed.pumps) | is.numeric(embed.pumps))) {

    ortho.land <- orthoLand(latlong = latlong)
    ortho.pump <- orthoPump(vestry = vestry, latlong = latlong,
      drop.isolates = drop.isolates)

    if (is.numeric(embed.landmarks)) {
      ortho.land <- ortho.land[ortho.land$case %in% embed.landmarks, ]
    }

    if (is.numeric(embed.pumps)) {
      ortho.pump <- ortho.pump[ortho.pump$id %in% embed.pumps, ]
    }

    obs.segs <- union(ortho.land$road.segment, ortho.pump$road.segment)

  } else if ((isTRUE(embed.anchor) | is.numeric(embed.anchor)) &
             isFALSE(embed.landmarks) &
             (isTRUE(embed.pumps) | is.numeric(embed.pumps))) {

    ortho.anchor <- orthoAnchor(case.set = case.set, latlong = latlong)
    ortho.pump <- orthoPump(vestry = vestry, latlong = latlong,
      drop.isolates = drop.isolates)

    if (is.numeric(embed.anchor)) {
      ortho.anchor <- ortho.anchor[ortho.anchor$case %in% embed.anchor, ]
    }

    if (is.numeric(embed.pumps)) {
      ortho.pump <- ortho.pump[ortho.pump$id %in% embed.pumps, ]
    }

    obs.segs <- union(ortho.anchor$road.segment, ortho.pump$road.segment)

  } else if (isFALSE(embed.anchor) &
             isFALSE(embed.landmarks) &
             (isTRUE(embed.pumps) | is.numeric(embed.pumps))) {

    ortho.pump <- orthoPump(vestry = vestry, latlong = latlong,
      drop.isolates = drop.isolates)

    if (is.numeric(embed.pumps)) {
      ortho.pump <- ortho.pump[ortho.pump$id %in% embed.pumps, ]
    }

    obs.segs <- unique(ortho.pump$road.segment)

  } else if ((isTRUE(embed.anchor) | is.numeric(embed.anchor)) &
             (isTRUE(embed.landmarks) | is.numeric(embed.landmarks)) &
             isFALSE(embed.pumps)) {

    ortho.anchor <- orthoAnchor(case.set = case.set, latlong = latlong)
    ortho.land <- orthoLand(latlong = latlong)

    if (is.numeric(embed.anchor)) {
      ortho.anchor <- ortho.anchor[ortho.anchor$case %in% embed.anchor, ]
    }

    if (is.numeric(embed.landmarks)) {
      ortho.land <- ortho.land[ortho.land$case %in% embed.landmarks, ]
    }

    obs.segs <- union(ortho.anchor$road.segment, ortho.land$road.segment)

  } else if (isFALSE(embed.anchor) &
             (isTRUE(embed.landmarks) | is.numeric(embed.landmarks)) &
             isFALSE(embed.pumps)) {

    ortho.land <- orthoLand(latlong = latlong)

    if (is.numeric(embed.landmarks)) {
      ortho.land <- ortho.land[ortho.land$case %in% embed.landmarks, ]
    }

    obs.segs <- unique(ortho.land$road.segment)

  } else if ((isTRUE(embed.anchor) | is.numeric(embed.anchor)) &
             isFALSE(embed.landmarks) &
             isFALSE(embed.pumps)) {

    ortho.anchor <- orthoAnchor(case.set = case.set, latlong = latlong)

    if (is.numeric(embed.anchor)) {
      ortho.anchor <- ortho.anchor[ortho.anchor$case %in% embed.anchor, ]
    }

    obs.segs <- unique(ortho.anchor$road.segment)

  } else if (isFALSE(embed.anchor) &
             isFALSE(embed.landmarks) &
             isFALSE(embed.pumps)) {

    obs.segs <- NULL
  }

  if (!is.null(obs.segs)) {
    no_embeds <- road.data[!road.data$id %in% obs.segs, ]
    vars2 <- c(vars, "case", "land", "pump")
    null.df <- stats::setNames(data.frame(matrix(nrow = 0, ncol = 5)), vars2)

    embeds <- lapply(obs.segs, function(s) {
      rd.tmp <- road.data[road.data$id == s, ]
      endpts <- endPoints(rd.tmp, vars, latlong = latlong)

      if (exists("ortho.anchor")) {
        anch.tmp <- ortho.anchor[ortho.anchor$road.segment == s, ]
      } else {
        anch.tmp <- null.df
      }

      if (exists("ortho.land")) {
        land.tmp <- ortho.land[ortho.land$road.segment == s, ]
      } else {
        land.tmp <- null.df
      }

      if (exists("ortho.pump")) {
        pump.tmp <- ortho.pump[ortho.pump$road.segment == s, ]
      } else {
        pump.tmp <- null.df
      }

      if (nrow(anch.tmp) > 0 & nrow(land.tmp) > 0 & nrow(pump.tmp) > 0) {
        anch.embed <- embedAnchor(anch.tmp, s, vars)
        land.embed <- embedLandmark(land.tmp, s, vars, vars2)
        pump.embed <- embedPump(pump.tmp, s, vars, vars2)
        embed.data <- rbind(endpts, anch.embed, land.embed, pump.embed)
      } else if (nrow(anch.tmp) == 0 &
                 nrow(land.tmp) > 0 &
                 nrow(pump.tmp) > 0) {
        land.embed <- embedLandmark(land.tmp, s, vars, vars2)
        pump.embed <- embedPump(pump.tmp, s, vars, vars2)
        embed.data <- rbind(endpts, land.embed, pump.embed)
      } else if (nrow(anch.tmp) > 0 &
                 nrow(land.tmp) == 0 &
                 nrow(pump.tmp) > 0) {
        anch.embed <- embedAnchor(anch.tmp, s, vars)
        pump.embed <- embedPump(pump.tmp, s, vars, vars2)
        embed.data <- rbind(endpts, anch.embed, pump.embed)
      } else if (nrow(anch.tmp) == 0 &
                 nrow(land.tmp) == 0 &
                 nrow(pump.tmp) > 0) {
        pump.embed <- embedPump(pump.tmp, s, vars, vars2)
        embed.data <- rbind(endpts, pump.embed)
      } else if (nrow(anch.tmp) > 0 &
                 nrow(land.tmp) > 0 &
                 nrow(pump.tmp) == 0) {
        anch.embed <- embedAnchor(anch.tmp, s, vars)
        land.embed <- embedLandmark(land.tmp, s, vars, vars2)
        embed.data <- rbind(endpts, anch.embed, land.embed)
      } else if (nrow(anch.tmp) == 0 &
                 nrow(land.tmp) > 0 &
                 nrow(pump.tmp) == 0) {
        land.embed <- embedLandmark(land.tmp, s, vars, vars2)
        embed.data <- rbind(endpts, land.embed)
      } else if (nrow(anch.tmp) > 0 &
                 nrow(land.tmp) == 0 &
                 nrow(pump.tmp) == 0) {
        anch.embed <- embedAnchor(anch.tmp, s, vars)
        embed.data <- rbind(endpts, anch.embed)
      } else if (nrow(anch.tmp) == 0 &
                 nrow(land.tmp) == 0 &
                 nrow(pump.tmp) == 0) {
        embed.data <- endpts
      }

      if (latlong) nodes <- embed.data[order(embed.data$lon, embed.data$lat), ]
      else nodes <- embed.data[order(embed.data$x, embed.data$y), ]

      tmp <- unique(nodes[, vars])
      tmp <- cbind(tmp[-nrow(tmp), ], tmp[-1, ])
      coord.nms <- paste0(names(tmp), c(rep(1, 2), rep(2, 2)))
      names(tmp) <- coord.nms
      tmp <- cbind(tmp, rd.tmp[, c("street", "id", "name")])
      edges <- tmp[, c("street", "id", "name", coord.nms)]

      if (case.set %in% c("observed", "snow")) {
        edges$id2 <- paste0(edges$id, letters[seq_len(nrow(edges))])
      } else if (case.set == "expected") {
        edges$id2 <- paste0(edges$id, "-", seq_len(nrow(edges)))
      }

      list(edges = edges, nodes = nodes)
    })

    no_embeds$id2 <- paste0(no_embeds$id, "a")

    edges <- do.call(rbind, lapply(embeds, function(x) x$edges))
    edges <- rbind(edges, no_embeds[, names(edges)])
    edges <- edges[order(edges$street), ]

    if (latlong) {
      edges$node1 <- paste0(edges$lon1, "_&_", edges$lat1)
      edges$node2 <- paste0(edges$lon2, "_&_", edges$lat2)
      edges$d <- vapply(seq_len(nrow(edges)), function(i) {
        p1 <- edges[i, c("lon1", "lat1")]
        p2 <- edges[i, c("lon2", "lat2")]
        geosphere::distGeo(p1, p2, a = a, f = f)
      }, numeric(1L))
    } else {
      edges$node1 <- paste0(edges$x1, "_&_", edges$y1)
      edges$node2 <- paste0(edges$x2, "_&_", edges$y2)
      edges$d <- vapply(seq_len(nrow(edges)), function(i) {
        stats::dist(rbind(stats::setNames(edges[i, paste0(vars, 1)], vars),
                          stats::setNames(edges[i, paste0(vars, 2)], vars)))
      }, numeric(1L))
    }

    edge.list <- edges[, c("node1", "node2")]
    g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

    nodes <- do.call(rbind, lapply(embeds, function(x) x$nodes))
    n1 <- stats::setNames(no_embeds[, paste0(vars, 1)], vars)
    n2 <- stats::setNames(no_embeds[, paste0(vars, 2)], vars)
    nodes.no_embeds <- rbind(n1, n2)
    nodes.no_embeds$case <- 0
    nodes.no_embeds$land <- 0
    nodes.no_embeds$pump <- 0
    nodes <- rbind(nodes, nodes.no_embeds)

    if (latlong) nodes$node <- paste0(nodes$lon, "_&_", nodes$lat)
    else nodes$node <- paste0(nodes$x, "_&_", nodes$y)

  } else {
    edges <- road.data
    if (latlong) {
      edges$node1 <- paste0(edges$lon1, "_&_", edges$lat1)
      edges$node2 <- paste0(edges$lon2, "_&_", edges$lat2)
      edges$d <- vapply(seq_len(nrow(edges)), function(i) {
        p1 <- edges[i, paste0(vars, 1)]
        p2 <- edges[i, paste0(vars, 2)]
        geosphere::distGeo(p1, p2, a = a, f = f)
      }, numeric(1L))
    } else {
      edges$node1 <- paste0(edges$x1, "_&_", edges$y1)
      edges$node2 <- paste0(edges$x2, "_&_", edges$y2)
      edges$d <- vapply(seq_len(nrow(edges)), function(i) {
        stats::dist(rbind(stats::setNames(edges[i, paste0(vars, 1)], vars),
                          stats::setNames(edges[i, paste0(vars, 2)], vars)))
      }, numeric(1L))
    }

    edge.list <- edges[, c("node1", "node2")]
    g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)

    node.nms <- c(vars, "node")
    n1 <- stats::setNames(edges[, paste0(node.nms, 1)], node.nms)
    n2 <- stats::setNames(edges[, paste0(node.nms, 2)], node.nms)
    nodes <- rbind(n1, n2)
    nodes$case <- 0
    nodes$land <- 0
    nodes$pump <- 0
    nodes <- nodes[, c(vars, "case", "land", "pump", "node")]
  }

  row.names(edges) <- NULL
  row.names(nodes) <- NULL

  list(g = g, edges = edges, nodes = nodes)
}

endPoints <- function(rd.tmp, vars, latlong = FALSE) {
  out <- data.frame(x = unlist(rd.tmp[, paste0(vars[1], 1:2)]),
                    y = unlist(rd.tmp[, paste0(vars[2], 1:2)]),
                    case = 0,
                    land = 0,
                    pump = 0,
                    row.names = NULL)
  if (latlong) names(out)[1:2] <- vars
  out
}

orthoAnchor <- function(case.set = "observed", latlong = FALSE) {
  if (latlong) {
    if (case.set == "observed") {
      out <- cholera::latlong.ortho.anchor
    } else if (case.set == "expected") {
      out <- cholera::latlong.sim.ortho.proj
    } else if (case.set == "snow") {
      sel <- cholera::latlong.ortho.anchor$case %in% cholera::snow.neighborhood
      out <- cholera::latlong.ortho.anchor[sel, ]
    }
  } else {
    if (case.set == "observed") {
      sel <- cholera::ortho.proj$case %in% cholera::fatalities.anchor$anchor
      out <- cholera::ortho.proj[sel, ]
    } else if (case.set == "expected") {
      out <- cholera::sim.ortho.proj
    } else if (case.set == "snow") {
      sel <- cholera::ortho.proj$case %in% cholera::snow.neighborhood
      out <- cholera::ortho.proj[sel, ]
    }
    sel <- names(out) %in% c("x.proj", "y.proj")
    names(out)[sel] <- c("x", "y")
    }
  out
}

orthoLand <- function(latlong = FALSE) {
  dat <- cholera::landmarks
  sel.xy <- grepl("x", names(dat)) | grepl("y", names(dat))
  sel.latlong <- grepl("lon", names(dat)) | grepl("lat", names(dat))
  if (latlong) out <- dat[, !sel.xy]
  else out <- dat[, !sel.latlong]
  names(out)[names(out) == "case"] <- "land"
  out
}

orthoPump <- function(vestry = TRUE, latlong = FALSE, drop.isolates = FALSE) {
  if (latlong) {
    if (vestry) {
      out <- cholera::latlong.ortho.pump.vestry
    } else {
      out <- cholera::latlong.ortho.pump
    }
    names(out)[names(out) == "id"] <- "pump"
  } else {
    if (vestry) {
      out <- cholera::ortho.proj.pump.vestry
    } else {
      out <- cholera::ortho.proj.pump
    }
    sel <- c("x.proj", "y.proj", "pump.id")
    names(out)[names(out) %in% sel] <- c("x", "y", "pump")
  }
  if (drop.isolates) out <- out[out$pump != 2L, ]
  out
}

embedAnchor <- function(anch.tmp, s, vars) {
  out <- anch.tmp[anch.tmp$road.segment == s, c(vars, "case")]
  out$land <- 0
  out$pump <- 0
  out
}

embedLandmark <- function(land.tmp, s, vars, vars2) {
  out <- land.tmp[land.tmp$road.segment == s, c(vars, "land")]
  out$case <- 0
  out$pump <- 0
  out[, vars2]
}

embedPump <- function(pump.tmp, s, vars, vars2) {
  out <- pump.tmp[pump.tmp$road.segment == s, c(vars, "pump")]
  out$case <- 0
  out$land <- 0
  out[, vars2]
}
