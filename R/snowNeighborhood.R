#' Plotting data for Snow's graphical annotation of the Broad Street pump neighborhood.
#'
#' Computes "missing" and split road segments data, and area plot data.
#' @return An R list of edge IDs and simulated case IDs.
#' @export

snowNeighborhood <- function() {
  snow <- cholera::neighborhoodWalking(case.set = "snow")
  dat <- cholera::neighborhoodData(vestry = snow$vestry)
  edges <- dat$edges

  n.paths <- lapply(snow$paths, function(neighborhood) {
    dat <- lapply(neighborhood, auditEdge, edges)
  })

  edge.data <- unname(unlist(lapply(n.paths, function(x) unique(unlist(x)))))

  ## Street Plot Data ##

  portland.mews <- which(edges$id == "160-4")
  ship.yard <- which(edges$id %in% c("163-1", "163-2"))
  tylers.court <- which(edges$id == "221-1")
  maidenhead.court <- which(edges$id == "244-1")
  cock.court <- which(edges$id == "225-1")
  hopkins.street <- which(edges$id %in% c("245-2", "265-1", "265-2"))
  unknownB <- which(edges$id == "263-1")
  duck.ham <- which(edges$id %in% paste0(198, "-", 2:4))

  dufours.place <- which(edges$id2 == "217-2c")
  silver.street <- which(edges$id2 == "275-1a")
  pulteney.court1 <- which(edges$id2 == "242-1h")
  new.husband.street <- which(edges$id2 == "259-1d")
  st.anns.place <- which(edges$id2 == "138-1a")
  hopkins.street.sub <- which(edges$id2 == "245-1c")
  kemps.court <- which(edges$id2 == "196-1d")
  bentinck.street <- which(edges$id2 == "167-1a")

  whole.segs <- c(portland.mews, ship.yard, tylers.court,
                  maidenhead.court, cock.court, hopkins.street, unknownB,
                  duck.ham)

  sub.segs <- c(dufours.place, silver.street, pulteney.court1,
                new.husband.street, st.anns.place, hopkins.street.sub,
                kemps.court, bentinck.street)

  other.edges <- c(whole.segs, sub.segs)

  ## Area Plot Data ##

  snow.edges <- edges[c(edge.data, other.edges), ]
  snow.ct <- unclass(table(snow.edges$id))
  snow.ct <- data.frame(id = names(snow.ct),
                        count = snow.ct,
                        row.names = NULL,
                        stringsAsFactors = FALSE)

  edge.ct <- vapply(unique(snow.edges$id), function(x) {
    sum(edges$id == x)
  }, numeric(1L))

  edge.ct <- data.frame(id = names(edge.ct),
                        count = edge.ct,
                        row.names = NULL,
                        stringsAsFactors = FALSE)

  audit <- merge(edge.ct, snow.ct, by = "id")
  names(audit)[-1] <- c("edge.ct", "snow.ct")

  # whole segments #

  whole.audit <- audit[audit$edge.ct == audit$snow.ct, ]
  whole.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$road.segment
    %in% whole.audit$id, "case"]

  # partial segments #

  partial <- snow.edges[snow.edges$id %in% whole.audit$id == FALSE, ]

  partial.proj <-
    cholera::sim.ortho.proj[cholera::sim.ortho.proj$road.segment %in%
                            partial$id, ]

  partial.candidates <- split(partial.proj, partial.proj$road.segment)
  partial.segments <- split(partial, partial$id)

  classifyCase <- function(i) {
    case.data <- partial.candidates[[i]]
    seg.data <- partial.segments[[i]]

    if (nrow(seg.data > 1)) {
      seg.data <- seg.data[order(seg.data$id2), ]
      classify.test <- vapply(case.data$case, function(case) {
        obs <- case.data[case.data$case == case, c("x.proj", "y.proj")]
        xs <- c(seg.data[1, "x1"], seg.data[nrow(seg.data), "x2"])
        ys <- c(seg.data[1, "y1"], seg.data[nrow(seg.data), "y2"])
        seg.df <- data.frame(x = xs, y = ys)
        data1 <- rbind(seg.df[1, ], c(obs$x.proj, obs$y.proj))
        data2 <- rbind(seg.df[2, ], c(obs$x.proj, obs$y.proj))
        seg.dist <- stats::dist(data1) + stats::dist(data2)
        signif(stats::dist(seg.df)) == signif(seg.dist)
      }, logical(1L))
    } else {
      classify.test <- vapply(case.data$case, function(case) {
        obs <- case.data[case.data$case == case, c("x.proj", "y.proj")]
        seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                             y = c(seg.data$y1, seg.data$y2))
        data1 <- rbind(seg.df[1, ], c(obs$x.proj, obs$y.proj))
        data2 <- rbind(seg.df[2, ], c(obs$x.proj, obs$y.proj))
        seg.dist <- stats::dist(data1) + stats::dist(data2)
        signif(stats::dist(seg.df)) == signif(seg.dist)
      }, logical(1L))
    }

    case.data[classify.test, "case"]
  }

  sim.case.partial <- lapply(seq_along(partial.candidates), classifyCase)
  sim.case.partial <- unlist(sim.case.partial)

  list(obs.edges = edge.data,
       other.edges = other.edges,
       sim.cases = c(whole.id, sim.case.partial))
}
