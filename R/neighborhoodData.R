#' Compute network graph of roads, cases and pumps.
#'
#' Assembles cases, pumps and road into a network graph.
#' @param vestry Logical. Use Vestry Report pump data.
#' @param case.set Character. "observed" or "expected", or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @export
#' @return An R list of nodes, edges and an 'igraph' network graph.

neighborhoodData <- function(vestry = FALSE, case.set = "observed") {
  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('"case.set" must be "observed", "expected" or "snow".')
  }

  if (case.set == "expected") {
    if (vestry) {
      node.data <- nodeData(vestry = TRUE, observed = FALSE)
    } else {
      node.data <- nodeData(observed = FALSE)
    }
  } else {
    if (vestry) {
      node.data <- nodeData(vestry = TRUE)
    } else {
      node.data <- nodeData()
    }
  }

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g
  nodes.pump <- nodes[nodes$pump != 0, ]
  nodes.pump <- nodes.pump[order(nodes.pump$pump), c("pump", "node")]
  list(g = g, nodes = nodes, edges = edges, nodes.pump = nodes.pump)
}

nodeData <- function(embed = TRUE, vestry = FALSE, observed = TRUE) {
  if (observed) {
    sel <- cholera::ortho.proj$case %in% cholera::fatalities.address$anchor.case
    case.segments <- unique(cholera::ortho.proj[sel, "road.segment"])
  } else {
    sim.proj <- simProj()
    case.segments <- unique(sim.proj$road.segment)
    case.segments <- case.segments[is.na(case.segments) == FALSE]
  }

  if (embed) {
    if (vestry) {
      ortho.pump <- cholera::ortho.proj.pump.vestry
    } else {
      ortho.pump <- cholera::ortho.proj.pump
    }

    case.pump <- intersect(ortho.pump$road.segment, case.segments)
    case.no_pump <- setdiff(case.segments, ortho.pump$road.segment)
    no_case.pump <- setdiff(ortho.pump$road.segment, case.segments)
    edits <- c(case.pump, case.no_pump, no_case.pump)

    if (observed) {
      if (vestry) {
        nodes <- lapply(edits, embedSites, vestry = TRUE)
        edges <- lapply(edits, embedSites, type = "edges", vestry = TRUE)
      } else {
        nodes <- lapply(edits, embedSites)
        edges <- lapply(edits, embedSites, type = "edges")
      }
    } else {
      if (vestry) {
        nodes <- lapply(edits, embedSites, vestry = TRUE, observed = FALSE)
        edges <- lapply(edits, embedSites, type = "edges", vestry = TRUE,
          observed = FALSE)
      } else {
        nodes <- lapply(edits, embedSites, observed = FALSE)
        edges <- lapply(edits, embedSites, type = "edges", observed = FALSE)
      }
    }

    # Edges #

    edges <- do.call(rbind, edges)
    road.segments <- cholera::road.segments
    road.segments <- road.segments[road.segments$id %in% edits == FALSE, ]
    road.segments$node1 <- paste0(road.segments$x1, "-", road.segments$y1)
    road.segments$node2 <- paste0(road.segments$x2, "-", road.segments$y2)

    if (observed) {
      road.segments$id2 <- paste0(road.segments$id, "a")
    } else {
      road.segments$id2 <- paste0(road.segments$id, "-1")
    }

    edges <- rbind(edges, road.segments)
    edges$d <- sqrt((edges$x1 - edges$x2)^2 + (edges$y1 - edges$y2)^2)

    # Nodes #

    nodes <- do.call(rbind, nodes)
    select1 <- road.segments$node1 %in% nodes$node == FALSE
    select2 <- road.segments$node2 %in% nodes$node == FALSE
    endpt.node1 <- road.segments[select1, "node1"]
    endpt.node2 <- road.segments[select2, "node2"]

    null.nodes <- lapply(union(endpt.node1, endpt.node2), function(x) {
      sel <- road.segments$node1 == x | road.segments$node2 == x
      dat <- road.segments[sel, ]

      endpt1 <- dat$node1 == x & dat$node2 != x
      endpt2 <- dat$node1 != x & dat$node2 == x

      if (any(endpt1)) {
        data.sel <- unique(dat[endpt1, c("x1", "y1")])
      } else if (any(endpt2)) {
        data.sel <- unique(dat[endpt2, c("x2", "y2")])
      }

      data.sel <- stats::setNames(data.sel, c("x.proj", "y.proj"))
      data.frame(data.sel, anchor = 0, pump = 0, node = x)
    })

    null.nodes <- do.call(rbind, null.nodes)
    nodes <- rbind(nodes, null.nodes)

    # Network Graph #

    edge.list <- edges[, c("node1", "node2")]
    g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)
    list(nodes = nodes, edges = edges, g = g)

  } else {
    road.segments$node1 <- paste0(road.segments$x1, "-", road.segments$y1)
    road.segments$node2 <- paste0(road.segments$x2, "-", road.segments$y2)

    if (observed) {
      road.segments$id2 <- paste0(road.segments$id, "a")
    } else {
      road.segments$id2 <- paste0(road.segments$id, "-1")
    }

    road.segments$d <- sqrt((road.segments$x1 - road.segments$x2)^2 +
                            (road.segments$y1 - road.segments$y2)^2)

    rs1 <- road.segments[, c("x1", "y1", "node1")]
    rs2 <- road.segments[, c("x2", "y2", "node2")]
    a <- stats::setNames(rs1, c("x.proj", "y.proj", "node"))
    b <- stats::setNames(rs2, c("x.proj", "y.proj", "node"))

    nodes <- unique(rbind(a, b))
    nodes <- data.frame(nodes[, c("x.proj", "y.proj")],
                        anchor = 0,
                        pump = 0,
                        node = nodes$node,
                        stringsAsFactors = FALSE)

    edge.list <- road.segments[, c("node1", "node2")]
    g <- igraph::graph_from_data_frame(edge.list, directed = FALSE)
    list(nodes = nodes, edges = road.segments, g = g)
  }
}

embedSites <- function(id, type = "nodes", vestry = FALSE, observed = TRUE) {
  if (id %in% cholera::road.segments$id == FALSE) {
    stop('See cholera::road.segments$id for valid "id" values.')
  }

  road.data <- cholera::road.segments[cholera::road.segments$id == id, ]

  if (observed) {
    if (road.data$id %in% cholera::ortho.proj$road.segment) {
      sel <- cholera::ortho.proj$road.segment %in% road.data$id
      road.fatalities <- cholera::ortho.proj[sel, ]

      sel <- road.fatalities$case %in% cholera::fatalities.address$anchor.case
      cases <- road.fatalities[sel, "case"]

      road.address <- road.fatalities[road.fatalities$case %in% cases, ]
      rds <- data.frame(road.address[, c("x.proj", "y.proj")],
                        anchor = road.address$case,
                        pump = 0)
    }
  } else {
    sim.proj <- simProj()

    if (road.data$id %in% sim.proj$road.segment) {
      road.fatalities <- sim.proj[sim.proj$road.segment %in% road.data$id, ]

      sel <- road.fatalities$case[road.fatalities$case %in% sim.proj$case]
      road.address <- road.fatalities[road.fatalities$case %in% sel, ]

      rds <- data.frame(road.address[, c("x.proj", "y.proj")],
                        anchor = road.address$case,
                        pump = 0)
    }
  }

  endptA <- data.frame(x.proj = road.data$x1,
                       y.proj = road.data$y1,
                       anchor = 0,
                       pump = 0)

  endptB <- data.frame(x.proj = road.data$x2,
                       y.proj = road.data$y2,
                       anchor = 0,
                       pump = 0)

  if (vestry) {
    pumps <- cholera::ortho.proj.pump.vestry
  } else {
    pumps <- cholera::ortho.proj.pump
  }

  if (observed) {
    case.seg <- road.data$id %in% cholera::ortho.proj$road.segment
  } else {
    case.seg <- road.data$id %in% sim.proj$road.segment
  }

  pump.seg <- id %in% pumps$road.segment

  if (pump.seg) {
     pump.data <- pumps[pumps$road.segment == id, ]
     ps <- data.frame(pump.data[, c("x.proj", "y.proj")],
                      anchor = 0,
                      pump = pump.data$pump.id)
  }

  if (case.seg & pump.seg) {
    dat <- rbind(rds, ps)
    nodes <- rbind(endptA, dat, endptB)
    nodes <- nodes[order(nodes$x.proj), ]
  } else if (case.seg & !pump.seg) {
    nodes <- rbind(endptA, rds, endptB)
    nodes <- nodes[order(nodes$x.proj), ]
  } else if (!case.seg & pump.seg) {
    nodes <- rbind(endptA, ps, endptB)
    nodes <- nodes[order(nodes$x.proj), ]
  } else {
    nodes <- rbind(endptA, endptB)
    nodes <- nodes[order(nodes$x.proj), ]
  }

  row.names(nodes) <- NULL
  nodes$node <- paste0(nodes$x.proj, "-", nodes$y.proj)

  # Edges

  sel <- c("x.proj", "y.proj")
  edges <- cbind(nodes[-nrow(nodes), sel], nodes[-1, sel])
  names(edges) <- c("x1", "y1", "x2", "y2")
  edges$node1 <- paste0(edges$x1, "-", edges$y1)
  edges$node2 <- paste0(edges$x2, "-", edges$y2)

  edges <- cbind(road.data[1, c("street", "id", "name")], edges,
    row.names = NULL)

  if (observed) {
    edges$id2 <- paste0(edges$id, letters[seq_len(nrow(edges))])
  } else {
    edges$id2 <- paste0(edges$id, "-", seq_len(nrow(edges)))
  }

  if (type == "nodes") {
    nodes
  } else if (type == "edges") {
    edges
  }
}

simProj <- function() {
  distanceFix <- function(case) {
    a <- unlist(cholera::regular.cases[case, ])
    b <- unlist(dat[dat$case == case, c("x.proj", "y.proj")])
    c(stats::dist(matrix(c(a, b), 2, 2, byrow = TRUE)))
  }

  orthoProjFix <- function(s, east.end = TRUE,
    nudge = 1 / (2 * cholera::unitMeter(1, "meter"))) {

    seg.data <- cholera::road.segments[cholera::road.segments$id == s,
      c("x1", "y1", "x2", "y2")]

    seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                         y = c(seg.data$y1, seg.data$y2))

    ols <- stats::lm(y ~ x, data = seg.df)
    segment.slope <- stats::coef(ols)[2]
    theta <- atan(segment.slope)
    h <- nudge
    delta.x <- h * cos(theta)
    delta.y <- h * sin(theta)

    # "East-West" ordering of segment endpoints
    EW <- which.min(seg.data[, c("x1", "x2")])

    if (EW == 1) {
      east <- seg.df[1, ] + c(delta.x, delta.y)
      west <- seg.df[2, ] - c(delta.x, delta.y)
    } else {
      east <- seg.df[2, ] + c(delta.x, delta.y)
      west <- seg.df[1, ] - c(delta.x, delta.y)
    }

    if(east.end) east else west
  }

  ## manual re-classification ##
  dat <- cholera::sim.ortho.proj

  # Sutton Street (85-1) -> Soho Square (86-1)
  # p4 -> ?
  dat[dat$case == 4497, "road.segment"] <- "86-1"
  dat[dat$case == 4497, c("x.proj", "y.proj")] <-
    orthoProjFix("86-1", east.end = TRUE)
  dat[dat$case == 4497, "ortho.dist"] <- distanceFix(4497)

  # Dufours Place (217-2) -> St James Workhouse (148-1)
  # p7 -> p7
  # St Johns Workhouse empty space problem/entrance
  dat[dat$case == 3296, "road.segment"] <- "194-1"
  dat[dat$case == 3296, c("x.proj", "y.proj")] <-
    cholera::classifierAudit(3296, "194-1", observed = FALSE,
      coordinates = TRUE)
  dat[dat$case == 3296, "ortho.dist"] <- distanceFix(3296)

  # Dufours Place (217-2) -> St James Workhouse (148-1)
  # p7 -> p7
  # St Johns Workhouse empty space problem/entrance
  dat[dat$case == 3223, "road.segment"] <- "194-1"
  dat[dat$case == 3223, c("x.proj", "y.proj")] <-
    cholera::classifierAudit(3223, "194-1", observed = FALSE,
      coordinates = TRUE)
  dat[dat$case == 3223, "ortho.dist"] <- distanceFix(3223)

  # Oxford Street (76-1) -> Blenheim Mews (106-1)
  # p1 -> p3
  # "fix" for misclassification of 4022
  dat[dat$case == 4022, "road.segment"] <- "106-1"
  dat[dat$case == 4022, c("x.proj", "y.proj")] <-
    orthoProjFix("106-1", east.end = FALSE)
  dat[dat$case == 4022, "ortho.dist"] <- distanceFix(4022)

  # Wardour Street (188-1) -> Richmond Mews (189-1)
  # p7 -> p11
  # "fix" for misclassification of 3173
  dat[dat$case == 3173, "road.segment"] <- "189-2"
  dat[dat$case == 3173, c("x.proj", "y.proj")] <-
    orthoProjFix("189-2")
  dat[dat$case == 3173, "ortho.dist"] <- distanceFix(3173)

  # Swallow Place (133-3) -> Princes Street/Hanover Square (184-1)
  # p5 -> p6
  # "fix" for misclassification of 3553
  dat[dat$case == 3553, "road.segment"] <- "184-1"
  dat[dat$case == 3553, c("x.proj", "y.proj")] <-
    orthoProjFix("184-1")
  dat[dat$case == 3553, "ortho.dist"] <- distanceFix(3553)

  # Princes Street/Hanover Square (184-1) -> Oxford Street (113-1)
  # p6 -> p5
  # "fix" for misclassification of 3772, 3699, 3626
  dat[dat$case %in% c(3772, 3699, 3626), "road.segment"] <- "113-1"
  dat[dat$case %in% c(3772, 3699, 3626), c("x.proj", "y.proj")] <-
    orthoProjFix("113-1")
  dat[dat$case %in% c(3772, 3699, 3626), "ortho.dist"] <-
    distanceFix(3772)

  # Regents Quadrant (523-2) -> Vine Street (520-2)
  # p13 -> p8
  # "fix" for misclassification of 594
  dat[dat$case == 594, "road.segment"] <- "520-2"
  dat[dat$case == 594, c("x.proj", "y.proj")] <-
    orthoProjFix("520-2")
  dat[dat$case == 594, "ortho.dist"] <- distanceFix(594)

  # Great Marlborough Street (135-1) -> Poland Street (122-1)
  dat[dat$case == 3951, "road.segment"] <- "122-1"
  dat[dat$case == 3951, c("x.proj", "y.proj")] <-
    cholera::classifierAudit(3951, "122-1", observed = FALSE,
      coordinates = TRUE)
  dat[dat$case == 3951, "ortho.dist"] <- distanceFix(3951)

  dat
}
