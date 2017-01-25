#' Voronoi neighborhood object.
#'
#' Data for Voronoi tessellation of John Snow's 1854 London cholera data.
#' @param selection Numeric. Default is NULL: all pumps are used. Ortherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param polygon.vertices Logical. TRUE returns a list of x-y coordinates of the vertices of Voronoi cells. Useful for sp::point.in.polygon() and used in choleraSummary().
#' @return A list of data and parameters of computed Voronoi neighborhoods. "voronoi" class.
#' @seealso \code{addVoronoi()}
#' \code{choleraSummary()}
#' \code{plot.voronoi()}
#' @export
#' @examples
#' neighborhoodVoronoi()
#' neighborhoodVoronoi(vestry = TRUE)
#' neighborhoodVoronoi(selection = 6:7)
#' neighborhoodVoronoi(selection = -6)
#' neighborhoodVoronoi(selection = -6, polygon.vertices = TRUE)
#'
#' # coordinate for vertices also available in object
#' dat <- neighborhoodVoronoi(selection = -6)
#' dat$coordinates

neighborhoodVoronoi <- function(selection = NULL, vestry = FALSE,
  polygon.vertices = FALSE) {

  if (is.null(selection) == FALSE) {
    msg1 <- 'If specified, "selection" must include at least 2 different pumps'

    if (vestry) {
      if (length(unique((1:14)[selection])) < 2) {
        stop(msg1)
      }
      if (any(abs(selection) %in% 1:14 == FALSE)) {
        stop('With "vestry = TRUE", 1 >= |"selection"| <= 14')
      }
    } else {
      if (length(unique((1:13)[selection])) < 2) {
        stop(msg1)
      }
      if (any(abs(selection) %in% 1:13 == FALSE)) {
        stop('With "vestry = FALSE", 1 >= |"selection"| <= 13')
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
      snow.colors <- snowColors(vestry = TRUE)
      select.string <- NULL
    } else {
      pump.id <- cholera::pumps$id
      voronoi <- deldir::deldir(cholera::pumps[, c("x", "y")],
        rw = c(x.rng, y.rng), suppressMsge = TRUE)
      snow.colors <- snowColors()
      select.string <- NULL
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

  output <- list(pump.id = pump.id, voronoi = voronoi, snow.colors = snow.colors,
     x.rng = x.rng, y.rng = y.rng, select.string = select.string,
     expected.data = expected.data, coordinates = coordinates,
     selection = selection, vestry = vestry)

  class(output) <- "voronoi"

  if (polygon.vertices) {
    output$coordinates
  } else {
    output
  }
}

#' Plot Voronoi neighborhoods.
#'
#' @param x An object of class "voronoi" created by neighborhoodVoronoi().
#' @param statistic NULL or Character. NULL plots the address, a stack's base case. "address" plots the number of addresses and its Pearson residual in each selected pump neighborhood. "fatality" plots the number of fatalities and its Pearson resiudual in pump neighborhoods.
#' @param ... Additional plotting parameters.
#' @return A base R graph.
#' @seealso
#'
#' \code{neighborhoodVornoi()}
#'
#' \code{addVoronoi()}
#'
#' @export
#' @examples
#' dat <- neighborhoodVoronoi()
#' plot(x = dat)
#'
#' plot(x = dat, statistic = "fatality")

plot.voronoi <- function(x, statistic = NULL, ...) {
  if (class(x) != "voronoi") {
    stop('Input object\'s class needs to be "voronoi".')
  }

  if (all(statistic %in% c("address", "fatality")) == FALSE) {
    stop('If specified, "statistic" must be either "address" or "fatality".')
  }

  dat <- x

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  plot(cholera::fatalities.address[, c("x", "y")], xlim = dat$x.rng,
    ylim = dat$y.rng, pch = NA, asp = 1)

  if (is.null(statistic)) {
    if (is.null(dat$selection)) {
      if (dat$vestry) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 2,
          col = dat$snow.colors)
        text(cholera::pumps.vestry[, c("x", "y")], pos = 1,
          label = paste0("p", dat$pump.id))
      } else {
        points(cholera::pumps[, c("x", "y")], pch = 2, col = dat$snow.colors)
        text(cholera::pumps[, c("x", "y")], label = paste0("p", dat$pump.id),
          pos = 1)
      }
    } else {
      if (dat$vestry) {
        points(cholera::pumps.vestry[dat$selection, c("x", "y")], pch = 2,
          col = dat$snow.colors)
        text(cholera::pumps.vestry[dat$selection, c("x", "y")],
          label = paste0("p", dat$pump.id), pos = 1)
      } else {
        points(cholera::pumps[dat$selection, c("x", "y")], pch = 2,
          col = dat$snow.colors)
        text(cholera::pumps[dat$selection, c("x", "y")],
          label = paste0("p", dat$pump.id), pos = 1)
      }
    }

    voronoi.case.id <- cholera::pumpCases(dat, statistic = "address")

    elements <- length(unlist(voronoi.case.id))
    voronoi.colors <- vector(length = elements)

    for (i in seq_along(voronoi.case.id)) {
      id <- voronoi.case.id[[i]]
      voronoi.colors[id] <- dat$snow.colors[i]
    }

    invisible(lapply(roads.list, lines, col = "lightgray"))
    invisible(lapply(border.list, lines))

    plot(dat$voronoi, add = TRUE, wline = "tess", wpoints = "none",
      lty = "solid")
    points(cholera::fatalities.address[, c("x", "y")], col = voronoi.colors,
      pch = 20, cex = 0.75)

    caption <- "Snow Addresses by Neighborhood"

    if (is.null(dat$selection)) {
      title(main = caption)
    } else {
      title(main = paste0(caption, "\n", "Pumps ", dat$select.string))
    }

  } else {
    if (is.null(dat$selection)) {
      if (dat$vestry) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 2,
          col = dat$snow.colors)
        text(cholera::pumps.vestry[, c("x", "y")], pos = 1,
          label = paste0("p", dat$pump.id))
      } else {
        points(cholera::pumps[, c("x", "y")], pch = 2, col = dat$snow.colors)
        text(cholera::pumps[, c("x", "y")], label = paste0("p", dat$pump.id),
          pos = 1)
      }
    } else {
      statistic <- match.arg(statistic, c("address", "fatality"))
      stat.data <- cholera::pumpSummary(dat, statistic = statistic)
      polygon.cols <- polygonColors(stat.data$Pearson)

      invisible(lapply(seq_along(dat$coordinates), function(i) {
        polygon(dat$coordinates[[i]]$x, dat$coordinates[[i]]$y,
          col = polygon.cols[i], border = NA)
      }))

      invisible(lapply(roads.list, lines, col = "gray"))
      invisible(lapply(border.list, lines))

      plot(dat$voronoi, add = TRUE, wline = "tess", wpoints = "none",
        lty = "solid")

      if (is.null(dat$selection)) {
        if (dat$vestry) {
          text(cholera::pumps.vestry[, c("x", "y")], label = stat.data$Count)
          text(cholera::pumps.vestry[, c("x", "y")], pos = 1, cex = 0.8,
            col = "blue", label = round(stat.data$Pearson, 2))
        } else {
          text(cholera::pumps[, c("x", "y")], label = stat.data$Count)
          text(cholera::pumps[, c("x", "y")], pos = 1, cex = 0.8, col = "blue",
            label = round(stat.data$Pearson, 2))
        }
      } else {
        if (dat$vestry) {
          text(cholera::pumps.vestry[dat$selection, c("x", "y")],
            label = stat.data$Count)
          text(cholera::pumps.vestry[dat$selection, c("x", "y")], pos = 1, cex = 0.8,
            col = "blue", label = round(stat.data$Pearson, 2))
        } else {
          text(cholera::pumps[dat$selection, c("x", "y")],
            label = stat.data$Count)
          text(cholera::pumps[dat$selection, c("x", "y")], pos = 1, cex = 0.8,
            col = "blue", label = round(stat.data$Pearson, 2))
        }
      }

      if (statistic == "address") {
        caption <- "Snow Address Count by Pump Neighborhood"
      } else if (statistic == "fatality") {
        caption <- "Snow Fatality Count by Pump Neighborhood"
      }

      if (is.null(dat$selection)) {
        title(main = caption)
      } else {
        title(main = paste0(caption, "\n", "Pumps ", dat$select.string))
      }
    }
  }
}

fourCorners <- function() {
  nw <- cholera::roads[cholera::roads$id == 69, c("x", "y")]
  ne <- cholera::roads[cholera::roads$id == 28, c("x", "y")]
  se <- cholera::roads[cholera::roads$id == 1137, c("x", "y")]
  sw <- cholera::roads[cholera::roads$id == 1211, c("x", "y")]
  list(northwest = nw, northeast = ne, southeast = se, southwest = sw)
}

# coordinates of Voronoi cell polygons for sp::point.in.polygon()
polygonCoordinates <- function(pump.id, cell.data, vestry,
  four.corners = fourCorners()) {

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

    # "open" polygon tests
    test1 <- any(dat$thirdv1 < 0 | dat$thirdv2 < 0)
    test2 <- unlist(dat[, c("thirdv1", "thirdv2")])
    test2 <- length(unique(test2[test2 < 0])) != 1

    # close "open" polygons at corners of deldir::deldir()'s rectangular window
    if (test1 & test2) {
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

polygonColors <- function(resid.vector, upper.limit = 67, alpha = FALSE) {
  # colors for Voronoi cells based on Pearson residuals.
  vec <- upper.limit:-upper.limit
  color.map <- data.frame(resid = vec, id = rev(order(vec)))
  color.id <- vapply(round(resid.vector), function(x) {
    color.map[color.map$resid == x, "id"]
  }, integer(1L))

  if (alpha) {
    col <- scales::col_numeric("RdBu", domain = NULL)(seq_along(vec))[color.id]
    scales::alpha(col, 0.5)
  } else {
    col <- scales::col_numeric("RdBu", domain = NULL)(seq_along(vec))[color.id]
    scales::alpha(col, 0.5)
  }
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
