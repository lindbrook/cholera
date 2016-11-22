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
#' # neighborhoodVoronoi()
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
      colors <- snowColors(vestry = TRUE)
    } else {
      pump.id <- cholera::pumps$id
      voronoi <- deldir::deldir(cholera::pumps[, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
      colors <- snowColors()
    }
  } else {
    if (vestry) {
      pump.id <- cholera::pumps.vestry$id[selection]
      voronoi <- deldir::deldir(cholera::pumps.vestry[selection, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
      colors <- snowColors(vestry = TRUE)[selection]
      select.string <- paste(sort(selection), collapse = ", ")
    } else {
      pump.id <- cholera::pumps$id[selection]
      voronoi <- deldir::deldir(cholera::pumps[selection, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
      colors <- snowColors()[selection]
      select.string <- paste(sort(selection), collapse = ", ")
    }
  }

  cell.data <- voronoi$dirsgs
  cell.id <- sort(unique(c(cell.data$ind1, cell.data$ind2)))

  # Polygon coordinates

  coordinates <- lapply(cell.id, function(i) {
    pump <- cholera::pumps[pump.id[i], c("x", "y")]
    cell <- cell.data[cell.data$ind1 == i | cell.data$ind2 == i, ]
    a <- cell[, c("x1", "y1")]
    b <- cell[, c("x2", "y2")]
    names(a) <- c("x", "y")
    names(b) <- c("x", "y")

    # "Open" polygon test

    test1 <- any(cell$thirdv1 < 0 | cell$thirdv2 < 0)

    test2 <- unlist(cell[, c("thirdv1", "thirdv2")])
    test2 <- length(unique(test2[test2 < 0])) != 1

    # Close "open" polygons at corners

    if (test1 & test2) {
      four.corners <- fourCorners()
      corners <- lapply(seq_len(nrow(cell)), function(j) {
        intersection.points <- lapply(four.corners, function(corner) {
          segmentIntersection(pump$x, pump$y, corner$x, corner$y,
            cell[j, "x1"], cell[j, "y1"], cell[j, "x2"], cell[j, "y2"])
        })
        vapply(intersection.points, function(x) all(is.na(x)) == FALSE,
          logical(1L))
      })

      corner.id <- which(colSums(do.call(rbind, corners)) == 0)
      corner.solution <- four.corners[corner.id]
      a <- rbind(a, b[nrow(b), ], corner.solution[[1]])
      b <- rbind(b, do.call(rbind, corner.solution))
    }

    coords <- unique(rbind(a, b))
    coords.centered <- data.frame(x = coords$x - pump$x, y = coords$y - pump$y)

    # transform cartesian to polar coordinates
    # then sort vertices by phi (angle); counter-clockwise order
    idx <- order(apply(coords.centered, 1, pracma::cart2pol)[1, ])
    coords <- coords[idx, ]

    # adds first vertex to last to close polygon
    rbind(coords, coords[1, ])
  })

  names(coordinates) <- pump.id

  plot(cholera::fatalities.address[, c("x", "y")], xlim = x.rng, ylim = y.rng,
    pch = NA, asp = 1)
  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(border.list, lines))
  plot(voronoi, add = TRUE, wline = "tess", wpoints = "none", lty = "solid")

  if (is.null(statistic)) {
    if (is.null(selection)) {
      if (!vestry) {
        points(cholera::pumps[, c("x", "y")], pch = 2, col = colors)
        text(cholera::pumps[, c("x", "y")], label = pump.id, pos = 1)
      } else {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 2, col = colors)
        text(cholera::pumps.vestry[, c("x", "y")], label = pump.id, pos = 1)
      }
    } else {
      if (!vestry) {
        points(cholera::pumps[selection, c("x", "y")], pch = 2, col = colors)
        text(cholera::pumps[selection, c("x", "y")], label = pump.id, pos = 1)
      } else {
        points(cholera::pumps.vestry[selection, c("x", "y")], pch = 2,
          col = colors)
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
      voronoi.colors[id] <- colors[i]
    }

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

    if (is.null(selection)) {
      if (!vestry) {
        text(cholera::pumps[, c("x", "y")], label = address.count)
      } else {
        text(cholera::pumps.vestry[, c("x", "y")], label = address.count)
      }
    } else {
      if (!vestry) {
        text(cholera::pumps[selection, c("x", "y")], label = address.count)
      } else {
        text(cholera::pumps.vestry[selection, c("x", "y")],
          label = address.count)
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

    if (is.null(selection)) {
      if (!vestry) {
        text(cholera::pumps[, c("x", "y")], label = fatality.count)
        caption <- "Snow Fatalities Count by Pump Neighborhood"
        if (is.null(selection)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }
      } else {
        text(cholera::pumps.vestry[, c("x", "y")], label = fatality.count)
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
        caption <- "Snow Fatalities Count by Pump Neighborhood"
        if (is.null(selection)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }
      } else {
        text(cholera::pumps.vestry[selection, c("x", "y")],
          label = fatality.count)
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
#' @return A data frame.
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
  cell.id <- sort(unique(c(cell.data$ind1, cell.data$ind2)))

  # Polygon coordinates

  coordinates <- lapply(cell.id, function(i) {
    pump <- cholera::pumps[pump.id[i], c("x", "y")]
    cell <- cell.data[cell.data$ind1 == i | cell.data$ind2 == i, ]
    a <- cell[, c("x1", "y1")]
    b <- cell[, c("x2", "y2")]
    names(a) <- c("x", "y")
    names(b) <- c("x", "y")

    # "Open" polygon test

    test1 <- any(cell$thirdv1 < 0 | cell$thirdv2 < 0)

    test2 <- unlist(cell[, c("thirdv1", "thirdv2")])
    test2 <- length(unique(test2[test2 < 0])) != 1

    # Close "open" polygons at corners

    if (test1 & test2) {
      four.corners <- fourCorners()
      corners <- lapply(seq_len(nrow(cell)), function(j) {
        intersection.points <- lapply(four.corners, function(corner) {
          segmentIntersection(pump$x, pump$y, corner$x, corner$y,
            cell[j, "x1"], cell[j, "y1"], cell[j, "x2"], cell[j, "y2"])
        })
        vapply(intersection.points, function(x) all(is.na(x)) == FALSE,
          logical(1L))
      })

      corner.id <- which(colSums(do.call(rbind, corners)) == 0)
      corner.solution <- four.corners[corner.id]
      a <- rbind(a, b[nrow(b), ], corner.solution[[1]])
      b <- rbind(b, do.call(rbind, corner.solution))
    }

    coords <- unique(rbind(a, b))
    coords.centered <- data.frame(x = coords$x - pump$x, y = coords$y - pump$y)

    # transform cartesian to polar coordinates
    # then sort vertices by phi (angle); counter-clockwise order
    idx <- order(apply(coords.centered, 1, pracma::cart2pol)[1, ])
    coords <- coords[idx, ]

    # adds first vertex to last to close polygon
    rbind(coords, coords[1, ])
  })

  names(coordinates) <- pump.id

  if (statistic == "address") {
    voronoi.address <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
                           cholera::fatalities.address$y, cell$x, cell$y)
    })

    address.count <- vapply(voronoi.address, sum, numeric(1L))

    data.frame(pump.id = as.numeric(names(address.count)),
               Count = address.count,
               Percent = round(100 * address.count / sum(address.count), 2))

  } else if (statistic == "fatality") {
    voronoi.fatalities <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.unstacked$x,
                           cholera::fatalities.unstacked$y, cell$x, cell$y)
    })

    fatality.count <- vapply(voronoi.fatalities, sum, numeric(1L))

    data.frame(pump.id = as.numeric(names(fatality.count)),
               Count = fatality.count,
               Percent = round(100 * fatality.count / sum(fatality.count), 2))
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

# Point of intersection between two segments.

segmentIntersection <- function(x0, y0, x1, y1, s0, t0, s1, t1) {
  # returns the point of intersection between two segments or NA if none.
  # http://stackoverflow.com/questions/20519231/finding-point-of-intersection-in-r
  # x0, y0, x1, x2 Coordinates of first segment's endpoints.
  # s0, t0, s1, t2 Coordinates of second segment's endpoints.
  denom <- (t1 - t0) * (x1 - x0) - (s1 - s0) * (y1 - y0)
  denom[abs(denom) < 1e-10] <- NA # parallel lines
  ua <- ((s1 - s0) * (y0 - t0) - (t1 - t0) * (x0 - s0)) / denom
  ub <- ((x1 - x0) * (y0 - t0) - (y1 - y0) * (x0 - s0)) / denom
  x <- x0 + ua * (x1 - x0)
  y <- y0 + ua * (y1 - y0)
  inside <- (ua >= 0) & (ua <= 1) & (ub >= 0) & (ub <= 1)
  data.frame(x = ifelse(inside, x, NA), y = ifelse(inside, y, NA))
}
