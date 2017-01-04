#' Plot Voronoi neighborhoods.
#'
#' Voronoi diagram of John Snow's 1854 London cholera data.
#' @param selection Numeric. Default is NULL: all pumps are used. Ortherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param statistic Character. NULL plots address points. "address" plots the total count of addresses in a neighborhood at its pump's location. "fatality" plots the total count of fatalities in a neighborhood at its pump's location.
#' @return A base R graphics plot.
#' @seealso \code{addVoronoi()}
#' @import graphics
#' @export
#' @examples
#' neighborhoodVoronoi()
#' neighborhoodVoronoi(statistic = "address")
#' neighborhoodVoronoi(vestry = TRUE)
#' neighborhoodVoronoi(selection = 6:7)
#' neighborhoodVoronoi(selection = -6)
#' neighborhoodVoronoi(selection = -6, statistic = "fatality")

neighborhoodVoronoi <- function(selection = NULL, vestry = FALSE,
  statistic = NULL) {

  if (is.null(statistic) == FALSE) {
    if (all(statistic %in% c("address", "fatality")) == FALSE) {
      stop('If specified, "statistic" must either be "address" or "fatality".')
    }
  }

  if (vestry) {
    msg1 <- 'With "vestry = TRUE", "selection" must include at least 2 pumps:'
    msg2 <- "  two different numbers between 1 and 14."

    if (is.null(selection) == FALSE) {
      test1 <- length(unique((1:14)[selection])) < 2
      test2 <- any(abs(selection) %in% 1:14 == FALSE)

      if (test1 | test2) {
        stop(paste(msg1, "\n", msg2))
      }
    }
  } else {
    msg1 <- 'With "vestry = FALSE", "selection" must include at least 2 pumps:'
    msg2 <- "  two different numbers between 1 and 13."

    if (is.null(selection) == FALSE ) {
      test1 <- length(unique((1:13)[selection])) < 2
      test2 <- any(abs(selection) %in% 1:13 == FALSE)

      if (test1 | test2) {
        stop(paste(msg1, "\n", msg2))
      }
    }
  }

  roadsB <- cholera::roads[cholera::roads$street %in%
                           cholera::border == FALSE, ]

  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(roadsB[, c("x", "y")], roadsB$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.rng <- range(cholera::roads$x)
  y.rng <- range(cholera::roads$y)

  if (is.null(selection)) {
    if (vestry) {
      pump.id <- cholera::pumps.vestry$id
      voronoi <- deldir::deldir(cholera::pumps.vestry[, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
      snow.colors <- snowColors(vestry = TRUE)
    } else {
      pump.id <- cholera::pumps$id
      voronoi <- deldir::deldir(cholera::pumps[, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
      snow.colors <- snowColors()
    }
  } else {
    if (vestry) {
      pump.id <- cholera::pumps.vestry$id[selection]
      voronoi <- deldir::deldir(cholera::pumps.vestry[selection, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
      snow.colors <- snowColors(vestry = TRUE)[selection]
      select.string <- paste(sort(selection), collapse = ", ")
    } else {
      pump.id <- cholera::pumps$id[selection]
      voronoi <- deldir::deldir(cholera::pumps[selection, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
      snow.colors <- snowColors()[selection]
      select.string <- paste(sort(selection), collapse = ", ")
    }
  }

  cell.data <- voronoi$dirsgs
  # cell.id <- sort(unique(c(cell.data$ind1, cell.data$ind2)))
  voronoi.order <- as.numeric(rownames(voronoi$summary))

  if (vestry) {
    pump.sel <- cholera::pumps.vestry$id
  } else {
    pump.sel <- cholera::pumps$id
  }

  if (is.null(selection)) {
    expected.data <- data.frame(pump = pump.sel[voronoi.order],
                                area = voronoi$summary$dir.area)
  } else {
    expected.data <- data.frame(pump = pump.sel[selection][voronoi.order],
                                area = voronoi$summary$dir.area)
  }

  expected.data$pct <- expected.data$area / sum(expected.data$area)

  # Polygon coordinates
  coordinates <- polygonCoordinates(pump.id, cell.data, vestry)

  plot(cholera::fatalities.address[, c("x", "y")], xlim = x.rng, ylim = y.rng,
    pch = NA, asp = 1)
  # invisible(lapply(roads.list, lines, col = "lightgray"))
  # invisible(lapply(border.list, lines))
  # plot(voronoi, add = TRUE, wline = "tess", wpoints = "none", lty = "solid")

  if (is.null(statistic)) {
    if (is.null(selection)) {
      if (!vestry) {
        points(cholera::pumps[, c("x", "y")], pch = 2, col = snow.colors)
        text(cholera::pumps[, c("x", "y")], label = pump.id, pos = 1)
      } else {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 2, col = snow.colors)
        text(cholera::pumps.vestry[, c("x", "y")], label = pump.id, pos = 1)
      }
    } else {
      if (!vestry) {
        points(cholera::pumps[selection, c("x", "y")], pch = 2,
          col = snow.colors)
        text(cholera::pumps[selection, c("x", "y")], label = pump.id, pos = 1)
      } else {
        points(cholera::pumps.vestry[selection, c("x", "y")], pch = 2,
          col = snow.colors)
        text(cholera::pumps.vestry[selection, c("x", "y")], label = pump.id,
          pos = 1)
      }
    }

    voronoi.address <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
                           cholera::fatalities.address$y, cell$x, cell$y)
    })

    voronoi.address.id <- lapply(voronoi.address, function(x) which(x == 1))
    elements <- length(unlist(voronoi.address.id))
    voronoi.colors <- vector(length = elements)

    for (i in seq_along(voronoi.address.id)) {
      id <- voronoi.address.id[[i]]
      voronoi.colors[id] <- snow.colors[i]
    }

    invisible(lapply(roads.list, lines, col = "lightgray"))
    invisible(lapply(border.list, lines))
    plot(voronoi, add = TRUE, wline = "tess", wpoints = "none", lty = "solid")

    points(cholera::fatalities.address[, c("x", "y")], col = voronoi.colors,
      pch = 20, cex = 0.75)

    caption <- "Snow Addresses by Neighborhood"

    if (is.null(selection)) {
      title(main = caption)
    } else {
      title(main = paste0(caption, "\n", "Pumps ", select.string))
    }

  } else if (statistic == "address") {
    voronoi.address <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
                           cholera::fatalities.address$y, cell$x, cell$y)
    })

    address.count <- vapply(voronoi.address, sum, numeric(1L))

    stat.data <- data.frame(pump.id = as.numeric(names(address.count)),
                            Count = address.count,
                            Percent = round(100 * address.count /
                              sum(address.count), 2))

    stat.data <- merge(stat.data, expected.data[, c("pump", "pct")],
      by.x = "pump.id", by.y = "pump")

    stat.data$Expected <- stat.data$pct * sum(stat.data$Count)
    stat.data$pct <- NULL

    pearson.resid0 <- round((stat.data$Count - stat.data$Expected) /
      sqrt(stat.data$Expected))

    pearson.resid2 <- round((stat.data$Count - stat.data$Expected) /
      sqrt(stat.data$Expected), 2)

    polygon.cols <- polygonColors(pearson.resid0)

    invisible(lapply(seq_along(coordinates), function(i) {
      polygon(coordinates[[i]]$x, coordinates[[i]]$y, col = polygon.cols[i],
        border = NA)
    }))

    invisible(lapply(roads.list, lines, col = "white"))
    invisible(lapply(border.list, lines))
    plot(voronoi, add = TRUE, wline = "tess", wpoints = "none", lty = "solid")

    if (is.null(selection)) {
      if (!vestry) {
        text(cholera::pumps[, c("x", "y")], label = address.count)
        text(cholera::pumps[, c("x", "y")], label = pearson.resid2, pos = 1,
          cex = 0.8, col = "blue")
      } else {
        text(cholera::pumps.vestry[, c("x", "y")], label = address.count)
        text(cholera::pumps.vestry[, c("x", "y")], label = pearson.resid2,
          pos = 1, cex = 0.8, col = "blue")
      }
    } else {
      if (!vestry) {
        text(cholera::pumps[selection, c("x", "y")], label = address.count)
        text(cholera::pumps[selection, c("x", "y")], label = pearson.resid2,
          pos = 1, cex = 0.8, col = "blue")
      } else {
        text(cholera::pumps.vestry[selection, c("x", "y")],
          label = address.count)
        text(cholera::pumps.vestry[selection, c("x", "y")],
          label = pearson.resid2, pos = 1, cex = 0.8, col = "blue")
      }
    }

    caption <- "Snow Address Count by Pump Neighborhood"

    if (is.null(selection)) {
      title(main = caption)
    } else {
      title(main = paste0(caption, "\n", "Pumps ", select.string))
    }

  } else if (statistic == "fatality") {
    voronoi.fatalities <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.unstacked$x,
                           cholera::fatalities.unstacked$y, cell$x, cell$y)
    })

    fatality.count <- vapply(voronoi.fatalities, sum, numeric(1L))

    stat.data <- data.frame(pump.id = as.numeric(names(fatality.count)),
                            Count = fatality.count,
                            Percent = round(100 * fatality.count /
                              sum(fatality.count), 2))

    stat.data <- merge(stat.data, expected.data[, c("pump", "pct")],
      by.x = "pump.id", by.y = "pump")

    stat.data$Expected <- stat.data$pct * sum(stat.data$Count)
    stat.data$pct <- NULL

    pearson.resid0 <- round((stat.data$Count - stat.data$Expected) /
      sqrt(stat.data$Expected))

    pearson.resid2 <- round((stat.data$Count - stat.data$Expected) /
      sqrt(stat.data$Expected), 2)


    polygon.cols <- polygonColors(pearson.resid0)

    invisible(lapply(seq_along(coordinates), function(i) {
      polygon(coordinates[[i]]$x, coordinates[[i]]$y, col = polygon.cols[i],
              border = NA)
    }))

    invisible(lapply(roads.list, lines, col = "white"))
    invisible(lapply(border.list, lines))
    plot(voronoi, add = TRUE, wline = "tess", wpoints = "none", lty = "solid")

    if (is.null(selection)) {
      if (!vestry) {
        text(cholera::pumps[, c("x", "y")], label = fatality.count)
        text(cholera::pumps[, c("x", "y")], label = pearson.resid2, pos = 1,
          cex = 0.8, col = "blue")
        caption <- "Snow Fatalities Count by Pump Neighborhood"

        if (is.null(selection)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }

      } else {
        text(cholera::pumps.vestry[, c("x", "y")], label = fatality.count)
        text(cholera::pumps.vestry[, c("x", "y")], label = pearson.resid2,
          pos = 1, cex = 0.8, col = "blue")
        caption <- "Vestry Fatalities Count by Pump Neighborhood"
        if (is.null(selection)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }
      }
    } else {
      if (!vestry) {
        text(cholera::pumps[selection, c("x", "y")], label = fatality.count)
        text(cholera::pumps[selection, c("x", "y")], label = pearson.resid2,
          pos = 1, cex = 0.8, col = "blue")
        caption <- "Snow Fatalities Count by Pump Neighborhood"
        if (is.null(selection)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }
      } else {
        text(cholera::pumps.vestry[selection, c("x", "y")],
          label = fatality.count)
        text(cholera::pumps.vestry[selection, c("x", "y")],
          label = pearson.resid2, pos = 1, cex = 0.8, col = "blue")
        caption <- "Vestry Fatalities Count by Pump Neighborhood"
        if (is.null(selection)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }
      }
    }
  }
}

#' Summary statistics for Voronoi neighborhoods.
#'
#' @param selection Numeric. Default is NULL: all pumps are used. Ortherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param statistic Character. "address" plots the total count of addresses at pumps' locations. "fatality" plots the total count of fatalities at pumps' locations.
#' @return A data frame with observed and expected counts, observed percentage, and Pearson residual (obs - exp) / sqrt(exp).
#' @export
#' @examples
#' neighborhoodVoronoiCensus()
#' neighborhoodVoronoiCensus(selection = 6:7)
#' neighborhoodVoronoiCensus(selection = -6)
#' neighborhoodVoronoiCensus(statistic = "address")

neighborhoodVoronoiCensus <- function(selection = NULL, vestry = FALSE,
  statistic = "address") {

  if (all(statistic %in% c("address", "fatality")) == FALSE) {
    stop('If specified, "statistic" must either be "address" or "fatality".')
  }

  if (vestry) {
    msg1 <- 'With "vestry = TRUE", "selection" must include at least 2 pumps:'
    msg2 <- "  two different numbers between 1 and 14."

    if (is.null(selection) == FALSE) {
      test1 <- length(unique((1:14)[selection])) < 2
      test2 <- any(abs(selection) %in% 1:14 == FALSE)

      if (test1 | test2) {
        stop(paste(msg1, "\n", msg2))
      }
    }
  } else {
    msg1 <- 'With "vestry = FALSE", "selection" must include at least 2 pumps:'
    msg2 <- "  two different numbers between 1 and 13."

    if (is.null(selection) == FALSE ) {
      test1 <- length(unique((1:13)[selection])) < 2
      test2 <- any(abs(selection) %in% 1:13 == FALSE)

      if (test1 | test2) {
        stop(paste(msg1, "\n", msg2))
      }
    }
  }

  x.rng <- range(cholera::roads$x)
  y.rng <- range(cholera::roads$y)

  if (is.null(selection)) {
    if (vestry) {
      pump.id <- cholera::pumps.vestry$id
      voronoi <- deldir::deldir(cholera::pumps.vestry[, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
    } else {
      pump.id <- cholera::pumps$id
      voronoi <- deldir::deldir(cholera::pumps[, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
    }
  } else {
    if (vestry) {
      pump.id <- cholera::pumps.vestry$id[selection]
      voronoi <- deldir::deldir(cholera::pumps.vestry[selection, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
    } else {
      pump.id <- cholera::pumps$id[selection]
      voronoi <- deldir::deldir(cholera::pumps[selection, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
    }
  }

  cell.data <- voronoi$dirsgs
  voronoi.order <- as.numeric(rownames(voronoi$summary))

  if (vestry) {
    pump.sel <- cholera::pumps.vestry$id
  } else {
    pump.sel <- cholera::pumps$id
  }

  if (is.null(selection)) {
    expected.data <- data.frame(pump = pump.sel[voronoi.order],
                                area = voronoi$summary$dir.area)
  } else {
    expected.data <- data.frame(pump = pump.sel[selection][voronoi.order],
                                area = voronoi$summary$dir.area)
  }

  expected.data$pct <- expected.data$area / sum(expected.data$area)

  coordinates <- polygonCoordinates(pump.id, cell.data, vestry)

  if (statistic == "address") {
    voronoi.address <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
                           cholera::fatalities.address$y,
                           cell$x, cell$y)
    })

    address.count <- vapply(voronoi.address, sum, numeric(1L))

    stat.data <- data.frame(
      pump.id = as.numeric(names(address.count)),
      Count = address.count,
      Percent = round(100 * address.count / sum(address.count), 2))

    stat.data <- merge(stat.data, expected.data[, c("pump", "pct")],
      by.x = "pump.id", by.y = "pump")

    stat.data$Expected <- stat.data$pct * sum(stat.data$Count)
    stat.data$pct <- NULL
    stat.data$Pearson <- (stat.data$Count - stat.data$Expected) /
                          sqrt(stat.data$Expected)
    stat.data

  } else if (statistic == "fatality") {
    voronoi.fatalities <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.unstacked$x,
                           cholera::fatalities.unstacked$y,
                           cell$x, cell$y)
    })

    fatality.count <- vapply(voronoi.fatalities, sum, numeric(1L))

    stat.data <- data.frame(
      pump.id = as.numeric(names(fatality.count)),
      Count = fatality.count,
      Percent = round(100 * fatality.count / sum(fatality.count), 2))

    stat.data <- merge(stat.data, expected.data[, c("pump", "pct")],
      by.x = "pump.id", by.y = "pump")

    stat.data$Expected <- stat.data$pct * sum(stat.data$Count)
    stat.data$pct <- NULL
    stat.data$Pearson <- (stat.data$Count - stat.data$Expected) /
                          sqrt(stat.data$Expected)
    stat.data
  }
}

snowColors <- function(vestry = FALSE) {
  colors.pair <- RColorBrewer::brewer.pal(10, "Paired")
  colors.dark <- RColorBrewer::brewer.pal(8, "Dark2")
  if (!vestry) {
    c("dodgerblue", "gray", colors.dark[1:4], colors.pair[2], colors.dark[5:8],
      "red", colors.pair[1])
  } else {
    c("dodgerblue", "gray", colors.dark[1:4], colors.pair[2], colors.dark[5:8],
      "red", colors.pair[1], "darkorange")
  }
}

fourCorners <- function() {
  nw <- cholera::roads[cholera::roads$id == 69, c("x", "y")]
  ne <- cholera::roads[cholera::roads$id == 28, c("x", "y")]
  se <- cholera::roads[cholera::roads$id == 1137, c("x", "y")]
  sw <- cholera::roads[cholera::roads$id == 1211, c("x", "y")]
  list(northwest = nw, northeast = ne, southeast = se, southwest = sw)
}

segmentIntersection <- function(x1, y1, x2, y2, a1, b1, a2, b2) {
  # returns the point of intersection between two segments or NA if none.
  # http://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r
  # x1, y1, x2, y2 coordinates of first segment's endpoints.
  # a1, b1, a2, b2 coordinates of second segment's endpoints.
  denom <- (b2 - b1) * (x2 - x1) - (a2 - a1) * (y2 - y1)
  denom[abs(denom) < 1e-10] <- NA # parallel lines
  ua <- ((a2 - a1) * (y1 - b1) - (b2 - b1) * (x1 - a1)) / denom
  ub <- ((x2 - x1) * (y1 - b1) - (y2 - y1) * (x1 - a1)) / denom
  x <- x1 + ua * (x2 - x1)
  y <- y1 + ua * (y2 - y1)
  inside <- (ua >= 0) & (ua <= 1) & (ub >= 0) & (ub <= 1)
  data.frame(x = ifelse(inside, x, NA), y = ifelse(inside, y, NA))
}

polygonCoordinates <- function(pump.id, cell.data, vestry,
  four.corners = fourCorners()) {
  # coordinates of Voronoi cell polygons for sp::point.in.polygon()

  coordinates <- lapply(pump.id, function(i) {
    if (vestry) {
      pump <- cholera::pumps.vestry[cholera::pumps.vestry$id == i, c("x", "y")]
    } else {
      pump <- cholera::pumps[cholera::pumps$id == i, c("x", "y")]
    }

    dat <- cell.data[cell.data$ind1 == which(pump.id == i) |
                     cell.data$ind2 == which(pump.id == i), ]

    a <- dat[, c("x1", "y1")]
    b <- dat[, c("x2", "y2")]
    names(a) <- c("x", "y")
    names(b) <- c("x", "y")

    coords <- unique(rbind(a, b))

    # test for "open" polygons
    test1 <- any(dat$thirdv1 < 0 | dat$thirdv2 < 0)
    test2 <- unlist(dat[, c("thirdv1", "thirdv2")])
    test2 <- length(unique(test2[test2 < 0])) != 1

    # close "open" polygons at corners of deldir::deldir()'s rectangular window
    if (test1 & test2) {
      # four.corners <- fourCorners()
      # test by negation:
      # does segment from pump to corner intersect any of the polygon's sides?
      corners <- lapply(seq_len(nrow(dat)), function(j) {
        intersection.points <- lapply(four.corners, function(corner) {
          segmentIntersection(pump$x, pump$y, corner$x, corner$y,
            dat[j, "x1"], dat[j, "y1"], dat[j, "x2"], dat[j, "y2"])
        })

        vapply(intersection.points, function(x) all(is.na(x)) == FALSE,
               logical(1L))
      })

      # If a "corner" returns FALSE, include that corner as a vertex
      corner.id <- which(colSums(do.call(rbind, corners)) == 0)
      corner.solution <- four.corners[corner.id]

      if (length(corner.solution) > 1) {
        coords <- rbind(coords, do.call(rbind, corner.solution))
      } else {
        coords <- rbind(coords, unlist(corner.solution))
      }
    }

    # center vertices relative to pump's coordinates
    coords.centered <- data.frame(x = coords$x - pump$x, y = coords$y - pump$y)

    # transform coordinates from cartesian to polar
    # sort vertices by phi (angle); returns vertices in counter-clockwise order
    idx <- order(apply(coords.centered, 1, pracma::cart2pol)[1, ])
    coords <- coords[idx, ]

    # adds first vertex to last to close polygon
    rbind(coords, coords[1, ])
  })

  names(coordinates) <- pump.id
  coordinates
}

polygonColors <- function(resid.vector, upper.limit = 70, alpha = FALSE) {
  vec <- upper.limit:-upper.limit
  color.map <- data.frame(resid = vec, id = rev(order(vec)))
  color.id <- vapply(round(resid.vector), function(x) {
    color.map[color.map$resid == x, "id"]
  }, integer(1L))

  if (alpha) {
    scales::alpha(scales::col_numeric("RdBu",
      domain = NULL)(seq_along(vec))[color.id], 0.5)
  }
  else {
    scales::col_numeric("RdBu", domain = NULL)(seq_along(vec))[color.id]
  }

}
