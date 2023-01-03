#' Compute Voronoi pump neighborhoods (lat-long prototype).
#'
#' Group cases into neighborhoods using Voronoi tessellation.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @export

latlongNeighborhoodVoronoi <- function(pump.select = NULL, vestry = FALSE) {
  cells <- latlongVoronoi(pump.select = pump.select, vestry = vestry)

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  if (!is.null(pump.select)) {
    pump.id <- selectPump(pump.data, pump.select = pump.select,
      metric = "euclidean", vestry = vestry)
  } else {
    pump.id <- pump.select
  }

  statistic.data <- lapply(cells, function(c) {
      sp::point.in.polygon(cholera::fatalities.address$lon,
        cholera::fatalities.address$lat, c$lon, c$lat)
    })

  out <- list(pump.select = pump.id, vestry = vestry, cells = cells,
    pump.data = pump.data, statistic.data = statistic.data)
  class(out) <- "latlongNeighborhoodVoronoi"
  out
}

#' Plot method for latlongNeighborhoodVoronoi()
#' @param x Object. Currently separate classification check.
#' @param add.cases Logical.
#' @param add.pumps Logical.
#' @param euclidean.paths Logical. Currently separate classification check.
#' @param ... Additional plotting parameters.
#' @export

plot.latlongNeighborhoodVoronoi <- function(x, add.cases = TRUE,
  add.pumps = TRUE, euclidean.paths = FALSE, ...) {

  pump.select <- x$pump.select
  vars <- c("lon", "lat")

  snowMap(vestry = x$vestry, latlong = TRUE, add.cases = add.cases,
    add.pumps = FALSE)
  invisible(lapply(x$cells, function(x) polygon(x[, vars])))
  if (add.pumps) addPump(pump.select, vestry = x$vestry, latlong = TRUE)

  if (!is.null(pump.select)) {
    unselected <- x$pump.data[!x$pump.data$id %in% pump.select, ]
    names(x$statistic.data) <- pump.select
    snow.colors <- snowColors(vestry = x$vestry)[paste0("p", pump.select)]
    points(unselected[, vars], pch = 2, col = "gray")
    text(unselected[, vars], labels = paste0("p", unselected$id), pos = 1,
      col = "gray")
  } else {
    snow.colors <- snowColors(vestry = x$vestry)
  }

  if (euclidean.paths) {
    cases <- cholera::fatalities.address

    if (is.null(pump.select)) p.id <- x$pump.data$id
    else p.id <- pump.select

    nearest.pump <- do.call(rbind, lapply(cases$anchor, function(a) {
      m1 <- as.matrix(cases[cases$anchor == a, vars])
      d <- vapply(p.id, function(p) {
        m2 <- as.matrix(x$pump.data[x$pump.data$id == p, vars])
        sp::spDistsN1(m1, m2, longlat = TRUE) * 1000L
      }, numeric(1L))
      near.id <- which.min(d)
      if (is.null(pump.select)) p.nr <- x$pump.data$id[near.id]
      else p.nr <- p.id[near.id]
      data.frame(case = a, pump = p.nr, meters = d[near.id])
    }))

    invisible(lapply(nearest.pump$case, function(c) {
      ego <- cases[cases$anchor == c, vars]
      p <- nearest.pump[nearest.pump$case == c, "pump"]
      alter <- x$pump.data[x$pump.data$id == p, vars]
      segments(ego$lon, ego$lat, alter$lon, alter$lat,
               col = snow.colors[paste0("p", p)], lwd = 0.5)
    }))
  } else {
    case.partition <- lapply(x$statistic.data, function(dat) {
      cholera::fatalities.address$anchor[dat == 1]
    })
    invisible(lapply(seq_along(case.partition), function(i) {
      sel <- cholera::fatalities.address$anchor %in% case.partition[[i]]
      points(cholera::fatalities.address[sel, vars], col = snow.colors[i],
        pch = 20, cex = 0.75)
    }))
  }
}

#' Print method for latlongNeighborhoodVoronoi().
#'
#' Parameter values for latlongNeighborhoodVoronoi().
#' @param x An object of class "latlongNeighborhoodVoronoi" created by \code{latlongNeighborhoodVoronoi()}.
#' @param ... Additional arguments.
#' @return A list of argument values.
#' @export

print.latlongNeighborhoodVoronoi <- function(x, ...) {
  print(x[c("pump.select", "vestry")])
}
