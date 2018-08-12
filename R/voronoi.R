#' Compute Voronoi pump neighborhoods.
#'
#' Group cases into neighborhoods using Voronoi tessellation.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param statistic NULL or Character. \code{NULL}, the default, makes no summary computation. "address" computes the number of addresses in each selected pump neighborhood. "fatality" computes the number of fatalities in pump neighborhoods.
#' @param polygon.vertices Logical. \code{TRUE} returns a list of x-y coordinates of the vertices of Voronoi cells. Useful for \code{sp::point.in.polygon()} as used in \code{print.voronoi()} method.
#' @return An R list with 12 objects.
#' \itemize{
#'   \item{\code{pump.id}: vector of selected pumps}
#'   \item{\code{voronoi}: output from deldir::deldir().}
#'   \item{\code{snow.colors}: neighborhood color based on snowColors().}
#'   \item{\code{x.rng}: range of x for plot.}
#'   \item{\code{y.rng}: range of y for plot.}
#'   \item{\code{select.string}: description of "pump.select" for plot title.}
#'   \item{\code{expected.data}: expected neighborhood fatality counts, based on Voronoi cell area.}
#'   \item{\code{coordinates}: polygon vertices of Voronoi cells.}
#'   \item{\code{statistic.data}: observed neighborhood fatality counts.}
#'   \item{\code{pump.select}: "pump.select" from neighborhoodVoronoi().}
#'   \item{\code{statistic}: "statistic" from neighborhoodVoronoi().}
#'   \item{\code{vestry}: "vestry" from neighborhoodVoronoi().}
#' }
#' @seealso \code{\link{addVoronoi}}, \code{\link{plot.voronoi}}, \code{\link{print.voronoi}}, \code{vignette("pump.neighborhoods")}
#' @export
#' @examples
#' neighborhoodVoronoi()
#' neighborhoodVoronoi(vestry = TRUE)
#' neighborhoodVoronoi(pump.select = 6:7)
#' neighborhoodVoronoi(pump.select = -6)
#' neighborhoodVoronoi(pump.select = -6, polygon.vertices = TRUE)
#'
#' # coordinates for vertices also available in the returned object.
#' dat <- neighborhoodVoronoi(pump.select = -6)
#' dat$coordinates

neighborhoodVoronoi <- function(pump.select = NULL, vestry = FALSE,
  statistic = NULL, polygon.vertices = FALSE) {

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  if (is.null(pump.select) == FALSE) {
    if (is.numeric(pump.select) == FALSE) stop("pump.select must be numeric.")
    p.count <- nrow(pump.data)
    p.ID <- seq_len(p.count)

    if (any(abs(pump.select) %in% p.ID == FALSE)) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ', p.count)
    }

    msg1 <- 'If specified,'
    msg2 <- 'pump.select must include at least 2 different pumps.'
    if (length(unique(p.ID[pump.select])) < 2) stop(msg1, msg2)
  }

  if (is.null(statistic) == FALSE) {
    if (all(statistic %in% c("address", "fatality")) == FALSE) {
      stop('If specified, statistic must either be "address" or "fatality".')
    }
  }

  x.rng <- range(cholera::roads$x)
  y.rng <- range(cholera::roads$y)

  if (is.null(pump.select)) {
    pump.id <- pump.data$id
    voronoi <- deldir::deldir(pump.data[, c("x", "y")], rw = c(x.rng, y.rng),
      suppressMsge = TRUE)
    snow.colors <- cholera::snowColors(vestry = TRUE)
    select.string <- NULL
  } else {
    pump.id <- pump.data$id[pump.select]
    voronoi <- deldir::deldir(pump.data[pump.select, c("x", "y")],
      rw = c(x.rng, y.rng), suppressMsge = TRUE)
    snow.colors <- cholera::snowColors(vestry = TRUE)[pump.select]
    select.string <- paste(sort(pump.select), collapse = ", ")
  }

  cell.data <- voronoi$dirsgs
  voronoi.order <- as.numeric(rownames(voronoi$summary))

  pump.sel <- pump.data$id

  if (is.null(pump.select)) {
    expected.data <- data.frame(pump = pump.sel[voronoi.order],
                                area = voronoi$summary$dir.area)
  } else {
    expected.data <- data.frame(pump = pump.sel[pump.select][voronoi.order],
                                area = voronoi$summary$dir.area)
  }

  expected.data$pct <- expected.data$area / sum(expected.data$area)
  coordinates <- polygonCoordinates(pump.id, cell.data, vestry)

  if (is.null(statistic)) {
    statistic.data <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
        cholera::fatalities.address$y, cell$x, cell$y)
    })
  } else {
    if (statistic == "address") {
      statistic.data <- lapply(coordinates, function(cell) {
        sp::point.in.polygon(cholera::fatalities.address$x,
          cholera::fatalities.address$y, cell$x, cell$y)
      })
    }

    if (statistic == "fatality") {
      statistic.data <- lapply(coordinates, function(cell) {
        sp::point.in.polygon(cholera::fatalities.unstacked$x,
          cholera::fatalities.unstacked$y, cell$x, cell$y)
      })
    }
  }

  output <- list(pump.id = pump.id, voronoi = voronoi,
    snow.colors = snow.colors, x.rng = x.rng, y.rng = y.rng,
    select.string = select.string, expected.data = expected.data,
    coordinates = coordinates, statistic.data = statistic.data,
    pump.select = pump.select, vestry = vestry, statistic = statistic)

  class(output) <- "voronoi"

  if (polygon.vertices) {
    output$coordinates
  } else {
    output
  }
}

#' Plot Voronoi neighborhoods.
#'
#' @param x An object of class "voronoi" created by \code{neighborhoodVoronoi()}.
#' @param voronoi.cells Logical. Plot Voronoi tessellation cells.
#' @param euclidean.paths Logical. Plot all Euclidean paths (star graph).
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
#' plot(neighborhoodVoronoi())

plot.voronoi <- function(x, voronoi.cells = TRUE,
  euclidean.paths = FALSE, ...) {

  if (class(x) != "voronoi") {
    stop('"x"\'s class needs to be "voronoi".')
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  if (x$vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  plot(cholera::fatalities.address[, c("x", "y")], xlim = x$x.rng,
    ylim = x$y.rng, pch = NA, asp = 1)
  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(border.list, lines))

  if (is.null(x$statistic)) {
    if (is.null(x$pump.select)) {
      points(pump.data[, c("x", "y")], pch = 2, col = x$snow.colors)
      text(pump.data[, c("x", "y")], pos = 1, label = paste0("p", x$pump.id))
      title(main = "Pump Neighborhoods: Voronoi (address)")
    } else {
      points(pump.data[x$pump.select, c("x", "y")], pch = 2,
        col = x$snow.colors)
      text(pump.data[x$pump.select, c("x", "y")],
        label = paste0("p", x$pump.id), pos = 1)
      title(main = paste0("Pump Neighborhoods: Voronoi (address)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    }

    if (voronoi.cells) {
      plot(x$voronoi, add = TRUE, wline = "tess", wpoints = "none",
        lty = "solid")
    }

    voronoi.case.id <- cholera::pumpCase(x)
    voronoi.colors <- vector(length = length(unlist(voronoi.case.id)))
    names(voronoi.colors) <- cholera::fatalities.address$anchor.case

    for (i in seq_along(voronoi.case.id)) {
      id <- voronoi.case.id[[i]]
      voronoi.colors[names(voronoi.colors) %in% id] <- x$snow.colors[i]
    }

    if (euclidean.paths) {
      invisible(lapply(names(voronoi.case.id), function(nm) {
        p.data <- pump.data[paste0("p", pump.data$id) == nm, ]
        sel <- cholera::fatalities.address$anchor.case %in%
          voronoi.case.id[[nm]]
        n.data <- cholera::fatalities.address[sel, ]
        n.color <- x$snow.colors[nm]
        lapply(n.data$anchor.case, function(case) {
          c.data <- n.data[n.data$anchor.case == case, ]
          segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color,
            lwd = 0.5)
        })
      }))
    } else {
      points(cholera::fatalities.address[, c("x", "y")], col = voronoi.colors,
        pch = 20, cex = 0.75)
    }

  } else {
    stat.data <- summary(x)
    polygon.cols <- polygonColors(stat.data$Pearson)

    invisible(lapply(seq_along(x$coordinates), function(i) {
      polygon(x$coordinates[[i]]$x, x$coordinates[[i]]$y,
        col = polygon.cols[i], border = NA)
    }))

    plot(x$voronoi, add = TRUE, wline = "tess", wpoints = "none", lty = "solid")
    invisible(lapply(roads.list, lines, col = "lightgray"))
    invisible(lapply(border.list, lines))

    if (x$statistic == "address") {
      caption <- "Pump Neighborhoods: Voronoi (address count)"
    } else if (x$statistic == "fatality") {
      caption <- "Pump Neighborhoods: Voronoi (fatality count)"
    }

    if (is.null(x$pump.select)) {
      text(pump.data[, c("x", "y")], label = stat.data$Count)
      text(pump.data[, c("x", "y")], pos = 1, cex = 0.8,
        col = "blue", label = round(stat.data$Pearson, 2))
      title(main = caption)
    } else {
      text(pump.data[x$pump.select, c("x", "y")], label = stat.data$Count)
      text(pump.data[x$pump.select, c("x", "y")], pos = 1, cex = 0.8,
        col = "blue", label = round(stat.data$Pearson, 2))
      title(main = paste0(caption, "\n", "Pumps ", x$select.string))
    }
  }
}

#' Print method for neighborhoodVoronoi().
#'
#' Return summary statistics for Voronoi neighborhoods.
#' @param x An object of class "voronoi" created by \code{neighborhoodVoronoi()}.
#' @param ... Additional arguments.
#' @return A data frame with observed and expected counts, observed percentage, and the Pearson residual, (observed - expected) / sqrt(expected).
#' @seealso \code{addVoronoi()}
#' \code{plot.voronoi()}
#' @export
#' @examples
#' neighborhoodVoronoi()
#' print(neighborhoodVoronoi())

print.voronoi <- function(x, ...) {
  if (class(x) != "voronoi") {
    stop('x\'s class needs to be "voronoi".')
  }

  output <- summary(x)
  print(output)
}

summary.voronoi <- function(x, ...) {
  if (class(x) != "voronoi") {
    stop('x\'s class needs to be "voronoi".')
  }

  census <- x$statistic.data
  count <- vapply(census, sum, numeric(1L))

  output <- data.frame(pump.id = as.numeric(names(count)),
                       Count = count,
                       Percent = round(100 * count / sum(count), 2))

  output <- merge(output, x$expected.data[, c("pump", "pct")],
    by.x = "pump.id", by.y = "pump")

  output$Expected <- output$pct * sum(output$Count)
  output$pct <- NULL
  output$Pearson <- (output$Count - output$Expected) / sqrt(output$Expected)
  output
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
    # scales::alpha(col, 0.5)
    grDevices::adjustcolor(col, alpha.f = 0.75)
  } else {
    scales::col_numeric("RdBu", domain = NULL)(seq_along(vec))[color.id]
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
