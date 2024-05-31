#' Plotting data for Snow's graphical annotation of the Broad Street pump neighborhood.
#'
#' Computes "missing" and split road segments data, and area plot data.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @return An R list of edge IDs and simulated case IDs.
#' @export

snowNeighborhood <- function(latlong = TRUE) {
  if (latlong) {
    snow <- latlongNeighborhoodWalking(pump.select = 7, case.set = "snow")
    edges <- snow$neigh.data$edges
    sim.data <- cholera::latlong.sim.ortho.proj
  } else {
    snow <- neighborhoodWalking(case.set = "snow")
    edges <- neighborhoodData(vestry = snow$vestry)$edges
    sim.data <- cholera::sim.ortho.proj
  }

  if (latlong) {
    n.paths <- lapply(snow$paths, function(neighborhood) {
      coord.paths <- lapply(neighborhood, names)
      lapply(coord.paths, auditEdge, edges, output = "id2")
    })
  } else {
    n.paths <- lapply(snow$paths, function(neighborhood) {
      lapply(neighborhood, auditEdge, edges, output = "id2")
    })
  }

  obs.edges <- unname(unlist(lapply(n.paths, function(x) unique(unlist(x)))))

  ## Street Data ##

  # id #
  portland.mews <- "160-4"
  ship.yard <- c("163-1", "163-2")
  tylers.court <- "221-1"
  maidenhead.court <- "244-1"
  cock.court <- "225-1"
  hopkins.street <- c("245-2", "265-1", "265-2")
  unknownB <- "263-1"
  duck.ham <- paste0(198, "-", 2:4)

  workhouse <- "148-1"

  # id2 #
  dufours.place <- "217-2c"
  silver.street <- "275-1a"
  pulteney.court1 <- "242-1h"
  new.husband.street <- "259-1d"
  st.anns.place <- "138-1a"
  hopkins.street.sub <- "245-1c"
  bentinck.street <- "167-1a"

  if (latlong) kemps.court <- c("196-1d", "196-1e")
  else kemps.court <- "196-1d"
  marshall.street <- "276-2d"
  new.street <- "224-1e"

  if (latlong) {
    whole.segs <- c(portland.mews, ship.yard, tylers.court, maidenhead.court,
                    cock.court, hopkins.street, unknownB, duck.ham, workhouse)
    sub.segs <- c(dufours.place, silver.street, pulteney.court1,
                  new.husband.street, st.anns.place, hopkins.street.sub,
                  bentinck.street, kemps.court, marshall.street, new.street)
  } else {
    whole.segs <- c(portland.mews, ship.yard, tylers.court, maidenhead.court,
                    cock.court, hopkins.street, unknownB, duck.ham)
    sub.segs <- c(dufours.place, silver.street, pulteney.court1,
                  new.husband.street, st.anns.place, hopkins.street.sub,
                  bentinck.street, kemps.court)
  }

  ## Area Data ##

  whole.id <- edges$id %in% whole.segs
  sub.id <- edges$id2 %in% sub.segs
  other.edges <- edges[whole.id | sub.id, "id2"]

  snow.edges <- edges[edges$id2 %in% c(obs.edges, other.edges), ]
  snow.ct <- unclass(table(snow.edges$id))
  snow.ct <- data.frame(id = names(snow.ct), count = snow.ct, row.names = NULL)

  edge.ct <- vapply(unique(snow.edges$id), function(x) {
    sum(edges$id == x)
  }, numeric(1L))

  edge.ct <- data.frame(id = names(edge.ct), count = edge.ct, row.names = NULL)

  audit <- merge(edge.ct, snow.ct, by = "id")
  names(audit)[-1] <- c("edge.ct", "snow.ct")

  # whole segments #

  whole.audit <- audit[audit$edge.ct == audit$snow.ct, ]
  whole.id <- sim.data[sim.data$road.segment %in% whole.audit$id, "case"]

  # partial segments #

  partial <- snow.edges[snow.edges$id %in% whole.audit$id == FALSE, ]
  partial.proj <- sim.data[sim.data$road.segment %in% partial$id, ]

  partial.candidates <- split(partial.proj, partial.proj$road.segment)
  partial.segments <- split(partial, partial$id)

  # sim partials #

  if (latlong) {
    sim.case.partial <- lapply(seq_along(partial.candidates), function(i) {
      latlongClassifyCase(i, partial.candidates, partial.segments)
    })
  } else {
    sim.case.partial <- lapply(seq_along(partial.candidates), function(i) {
      classifyCase(i, partial.candidates, partial.segments)
    })
  }

  sim.case.partial <- unlist(sim.case.partial)

  list(obs.edges = obs.edges,
       other.edges = other.edges,
       sim.cases = c(whole.id, sim.case.partial))
}

computeGeoCartesian <- function(dat, endpt = 1) {
  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  vars <- paste0(c("lon", "lat"), endpt)
  tmp <- dat[, vars]
  x.proj <- c(tmp$lon, origin$lat)
  y.proj <- c(origin$lon, tmp$lat)
  m.lon <- geosphere::distGeo(y.proj, tmp)
  m.lat <- geosphere::distGeo(x.proj, tmp)
  data.frame(x = m.lon, y = m.lat)
}

classifyCase <- function(i, partial.candidates, partial.segments) {
  case.data <- partial.candidates[[i]]
  seg.data <- partial.segments[[i]]

  if (nrow(seg.data) > 1) {
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

latlongClassifyCase <- function(i, partial.candidates, partial.segments) {
  case.data <- partial.candidates[[i]]
  seg.data <- partial.segments[[i]]
  vars <- c("lon", "lat")

  if (nrow(seg.data) > 1) {
    ones <- do.call(rbind, lapply(seq_along(seg.data$id2), function(i) {
      computeGeoCartesian(seg.data[i, ], endpt = 1)
    }))

    twos <- do.call(rbind, lapply(seq_along(seg.data$id2), function(i) {
      computeGeoCartesian(seg.data[i, ], endpt = 2)
    }))

    seg.data[, paste0(vars, 1)] <- stats::setNames(ones, paste0(vars, 1))
    seg.data[, paste0(vars, 2)] <- stats::setNames(twos, paste0(vars, 2))
    seg.data <- seg.data[order(seg.data$id2), ]

    classify.test <- vapply(case.data$case, function(case) {
      obs <- case.data[case.data$case == case, c("x.proj", "y.proj")]
      xs <- c(seg.data[1, "lon1"], seg.data[nrow(seg.data), "lon2"])
      ys <- c(seg.data[1, "lat1"], seg.data[nrow(seg.data), "lat2"])
      seg.df <- data.frame(x = xs, y = ys)
      data1 <- rbind(seg.df[1, ], c(obs$x.proj, obs$y.proj))
      data2 <- rbind(seg.df[2, ], c(obs$x.proj, obs$y.proj))
      seg.dist <- stats::dist(data1) + stats::dist(data2)
      signif(stats::dist(seg.df)) == signif(seg.dist)
    }, logical(1L))

  } else {
    seg.data[, paste0(vars, 1)] <- computeGeoCartesian(seg.data, endpt = 1)
    seg.data[, paste0(vars, 2)] <- computeGeoCartesian(seg.data, endpt = 2)

    classify.test <- vapply(case.data$case, function(case) {
      obs <- case.data[case.data$case == case, c("x.proj", "y.proj")]
      seg.df <- data.frame(x = c(seg.data$lon1, seg.data$lon2),
                           y = c(seg.data$lat1, seg.data$lat2))
      data1 <- rbind(seg.df[1, ], c(obs$x.proj, obs$y.proj))
      data2 <- rbind(seg.df[2, ], c(obs$x.proj, obs$y.proj))
      seg.dist <- stats::dist(data1) + stats::dist(data2)
      signif(stats::dist(seg.df)) == signif(seg.dist)
    }, logical(1L))
  }

  case.data[classify.test, "case"]
}
