#' Plot Voronoi diagram.
#'
#' Plot  Voronoi diagrams of John Snow's 1854 London cholera data.
#' @param select Numeric vector. Selection by numeric id: 1 to 13 for "pumps"; 1 to 14 for "pumps.vestry". Default is NULL; all pumps are used.
#' @param vestry Logical. Use the 14 pumps from the Vestry Report rather than the 13 in the original map.
#' @param output Character. NULL plots address points. "addresses" plots the total count of addresses at pumps' locations. "fatalities" plots the total count of fatalities at pumps' locations.
#' @return A base R graphics plot.
#' @seealso \code{addVoronoi()}
#' @import graphics
#' @export
#' @examples
#' voronoiPlot()
#' voronoiPlot(output = "addresses")
#' voronoiPlot(vestry = TRUE)
#' voronoiPlot(select = 6:7)
#' voronoiPlot(select = -6)
#' voronoiPlot(select = -6, output = "fatalities")


voronoiPlot <- function(select = NULL, vestry = FALSE, output = NULL) {
  if (is.null(output) == FALSE) {
    if (all(output %in% c("addresses", "fatalities") == FALSE)) {
      stop('If specified, "output" must either be "addresses" or "fatalities".')
    }
  }

  roadsB <- cholera::roads[cholera::roads$street %in%
                           cholera::border == FALSE, ]

  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(roadsB[, c("x", "y")], roadsB$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.rng <- range(cholera::roads$x)
  y.rng <- range(cholera::roads$y)

  if (is.null(select)) {
    if (vestry == FALSE) {
      pump.id <- cholera::pumps$id
      voronoi <- deldir::deldir(cholera::pumps[, c("x", "y")],
        rw = c(x.rng, y.rng))
      colors <- snowColors()
    } else {
      pump.id <- cholera::pumps.vestry$id
      voronoi <- deldir::deldir(cholera::pumps.vestry[, c("x", "y")],
        rw = c(x.rng, y.rng))
      colors <- snowColors(vestry = TRUE)
    }
  } else {
    if (vestry == FALSE) {
      pump.id <- cholera::pumps$id[select]
      voronoi <- deldir::deldir(cholera::pumps[select, c("x", "y")],
        rw = c(x.rng, y.rng))
      colors <- snowColors()[select]
      select.string <- paste(sort(select), collapse = ", ")
    } else {
      pump.id <- cholera::pumps.vestry$id[select]
      voronoi <- deldir::deldir(cholera::pumps.vestry[select, c("x", "y")],
        rw = c(x.rng, y.rng))
      colors <- snowColors(vestry = TRUE)[select]
      select.string <- paste(sort(select), collapse = ", ")
    }
  }

  cell.data <- voronoi$dirsgs
  cell.id <- sort(unique(c(cell.data$ind1, cell.data$ind2)))

  coordinates <- lapply(cell.id, function(i) {
    pump <- cholera::pumps[pump.id[i], c("x", "y")]
    cell <- cell.data[cell.data$ind1 == i | cell.data$ind2 == i, ]

    a <- cell[, c("x1", "y1")]
    b <- cell[, c("x2", "y2")]
    names(a) <- c("x", "y")
    names(b) <- c("x", "y")

    test1 <- any(cell$thirdv1 < 0 | cell$thirdv2 < 0)

    test2 <- unlist(cell[, c("thirdv1", "thirdv2")])
    test2 <- length(unique(test2[test2 < 0])) != 1

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
  plot(voronoi, add = TRUE, wline = "tess", wpoints = "none",lty = "solid")

  if (is.null(output)) {
    if (is.null(select)) {
      if (vestry == FALSE) {
        points(cholera::pumps[, c("x", "y")], pch = 2, col = colors)
        text(cholera::pumps[, c("x", "y")], label = pump.id, pos = 1)
      } else {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 2, col = colors)
        text(cholera::pumps.vestry[, c("x", "y")], label = pump.id, pos = 1)
      }
    } else {
      if (vestry == FALSE) {
        points(cholera::pumps[select, c("x", "y")], pch = 2, col = colors)
        text(cholera::pumps[select, c("x", "y")], label = pump.id, pos = 1)
      } else {
        points(cholera::pumps.vestry[select, c("x", "y")], pch = 2,
          col = colors)
        text(cholera::pumps.vestry[select, c("x", "y")], label = pump.id,
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

    if (is.null(select)) {
      title(main = caption)
    } else {
      title(main = paste0(caption, "\n", "Pumps ", select.string))
    }

  } else if (output == "addresses") {
    voronoi.address <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
                           cholera::fatalities.address$y, cell$x, cell$y)
    })

    address.count <- vapply(voronoi.address, sum, numeric(1L))

    if (is.null(select)) {
      if (vestry == FALSE) {
        text(cholera::pumps[, c("x", "y")], label = address.count)
      } else {
        text(cholera::pumps.vestry[, c("x", "y")], label = address.count)
      }
    } else {
      if (vestry == FALSE) {
        text(cholera::pumps[select, c("x", "y")], label = address.count)
      } else {
        text(cholera::pumps.vestry[select, c("x", "y")], label = address.count)
      }
    }

    caption <- "Snow Address Count by Pump Neighborhood"
    if (is.null(select)) {
      title(main = caption)
    } else {
      title(main = paste0(caption, "\n", "Pumps ", select.string))
    }

  } else if (output == "fatalities") {
    voronoi.fatalities <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.unstacked$x,
                           cholera::fatalities.unstacked$y, cell$x, cell$y)
    })

    fatality.count <- vapply(voronoi.fatalities, sum, numeric(1L))

    if (is.null(select)) {
      if (vestry == FALSE) {
        text(cholera::pumps[, c("x", "y")], label = fatality.count)
        caption <- "Snow Fatalities Count by Pump Neighborhood"

        if (is.null(select)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }

      } else {
        text(cholera::pumps.vestry[, c("x", "y")], label = fatality.count)
        caption <- "Vestry Fatalities Count by Pump Neighborhood"

        if (is.null(select)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }
      }

    } else {
      if (vestry == FALSE) {
        text(cholera::pumps[select, c("x", "y")], label = fatality.count)
        caption <- "Snow Fatalities Count by Pump Neighborhood"

        if (is.null(select)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }

      } else {
        text(cholera::pumps.vestry[select, c("x", "y")], label = fatality.count)
        caption <- "Vestry Fatalities Count by Pump Neighborhood"

        if (is.null(select)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }
      }
    }
  }
}

snowColors <- function(vestry = FALSE) {
  colors.pair <- RColorBrewer::brewer.pal(10, "Paired")
  colors.dark <- RColorBrewer::brewer.pal(8, "Dark2")
  if (vestry == FALSE) {
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
#
# Returns the point of intersection between two segments or NA if none.
# @param x0, y0, x1, x2 Coordinates of first segment's endpoints.
# @param s0, t0, s1, t2 Coordinates of second segment's endpoints.
# @return A data frame.
# @seealso \url{http://stackoverflow.com/questions/20519231/finding-point-of-intersection-in-r}
segmentIntersection <- function(x0, y0, x1, y1, s0, t0, s1, t1){
  denom <- (t1 - t0) * (x1 - x0) - (s1 - s0) * (y1 - y0)
  denom[abs(denom) < 1e-10] <- NA # parallel lines
  ua <- ((s1 - s0) * (y0 - t0) - (t1 - t0) * (x0 - s0)) / denom
  ub <- ((x1 - x0) * (y0 - t0) - (y1 - y0) * (x0 - s0)) / denom
  x <- x0 + ua * (x1 - x0)
  y <- y0 + ua * (y1 - y0)
  inside <- (ua >= 0) & (ua <= 1) & (ub >= 0) & (ub <= 1)
  data.frame(x = ifelse(inside, x, NA), y = ifelse(inside, y, NA))
}
