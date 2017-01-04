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

  if (is.null(selection) == FALSE) {
    msg1 <- '"selection" must include at least 2 different pumps'
    if (vestry) {
      if (length(unique((1:14)[selection])) < 2) {
        stop(msg1)
      }
      if (any(abs(selection) %in% 1:14 == FALSE)) {
        stop('With "vestry = TRUE", 1 >= |selection| <= 14')
      }
    } else {
      if (length(unique((1:13)[selection])) < 2) {
        stop(msg1)
      }
      if (any(abs(selection) %in% 1:13 == FALSE)) {
        stop('With "vestry = FALSE", 1 >= |selection| <= 13')
      }
    }
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
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

  plot(cholera::fatalities.address[, c("x", "y")], xlim = x.rng, ylim = y.rng,
    pch = NA, asp = 1)

  if (is.null(statistic)) {
    if (is.null(selection)) {
      if (vestry) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 2, col = snow.colors)
        text(cholera::pumps.vestry[, c("x", "y")], label = paste0("p", pump.id),
          pos = 1)
      } else {
        points(cholera::pumps[, c("x", "y")], pch = 2, col = snow.colors)
        text(cholera::pumps[, c("x", "y")], label = paste0("p", pump.id),
          pos = 1)
      }
    } else {
      if (vestry) {
        points(cholera::pumps.vestry[selection, c("x", "y")], pch = 2,
          col = snow.colors)
        text(cholera::pumps.vestry[selection, c("x", "y")],
          label = paste0("p", pump.id), pos = 1)
      } else {
        points(cholera::pumps[selection, c("x", "y")], pch = 2,
          col = snow.colors)
        text(cholera::pumps[selection, c("x", "y")],
          label = paste0("p", pump.id), pos = 1)
      }
    }

    voronoi.address <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
                           cholera::fatalities.address$y,
                           cell$x, cell$y)
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
      if (vestry) {
        text(cholera::pumps.vestry[, c("x", "y")], label = address.count)
        text(cholera::pumps.vestry[, c("x", "y")], label = pearson.resid2,
          pos = 1, cex = 0.8, col = "blue")
      } else {
        text(cholera::pumps[, c("x", "y")], label = address.count)
        text(cholera::pumps[, c("x", "y")], label = pearson.resid2, pos = 1,
          cex = 0.8, col = "blue")
      }
    } else {
      if (vestry) {
        text(cholera::pumps.vestry[selection, c("x", "y")],
          label = address.count)
        text(cholera::pumps.vestry[selection, c("x", "y")],
          label = pearson.resid2, pos = 1, cex = 0.8, col = "blue")
      } else {
        text(cholera::pumps[selection, c("x", "y")], label = address.count)
        text(cholera::pumps[selection, c("x", "y")], label = pearson.resid2,
          pos = 1, cex = 0.8, col = "blue")
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
      if (vestry) {
        text(cholera::pumps.vestry[, c("x", "y")], label = fatality.count)
        text(cholera::pumps.vestry[, c("x", "y")], label = pearson.resid2,
          pos = 1, cex = 0.8, col = "blue")
        caption <- "Vestry Fatalities Count by Pump Neighborhood"
        if (is.null(selection)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }
      } else {
        text(cholera::pumps[, c("x", "y")], label = fatality.count)
        text(cholera::pumps[, c("x", "y")], label = pearson.resid2, pos = 1,
          cex = 0.8, col = "blue")
        caption <- "Snow Fatalities Count by Pump Neighborhood"
        if (is.null(selection)) {
          title(main = caption)
        } else {
          title(main = paste0(caption, "\n", "Pumps ", select.string))
        }
      }
    } else {
      if (vestry) {
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
      } else {
        text(cholera::pumps[selection, c("x", "y")], label = fatality.count)
        text(cholera::pumps[selection, c("x", "y")], label = pearson.resid2,
          pos = 1, cex = 0.8, col = "blue")
        caption <- "Snow Fatalities Count by Pump Neighborhood"
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
    stop('"statistic" must either be "address" or "fatality".')
  }

  if (is.null(selection) == FALSE) {
    msg1 <- '"selection" must include at least 2 different pumps'
    if (vestry) {
      if (length(unique((1:14)[selection])) < 2) {
        stop(msg1)
      }
      if (any(abs(selection) %in% 1:14 == FALSE)) {
        stop('With "vestry = TRUE", 1 >= |selection| <= 14')
      }
    } else {
      if (length(unique((1:13)[selection])) < 2) {
        stop(msg1)
      }
      if (any(abs(selection) %in% 1:13 == FALSE)) {
        stop('With "vestry = FALSE", 1 >= |selection| <= 13')
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
