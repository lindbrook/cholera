#' Compute Voronoi pump neighborhoods (lat-long prototype).
#'
#' Group cases into neighborhoods using Voronoi tessellation.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param euclidean.paths Logical. Currently separate classification check.
#' @export

neighborhoodVoronoiB <- function(pump.select = NULL, vestry = FALSE,
  euclidean.paths = FALSE) {

  snowMap(vestry = vestry, latlong = TRUE, add.cases = FALSE, add.pumps = FALSE)
  cells <- latlongVoronoiC(pump.select = pump.select, vestry = vestry)
  invisible(lapply(cells, function(x) polygon(x[, c("lon", "lat")])))
  addPump(pump.select = pump.select, vestry = vestry, latlong = TRUE)

  if (vestry) {
    pmp <- cholera::pumps.vestry
  } else {
    pmp <- cholera::pumps
  }

  if (!is.null(pump.select)) {
    if (any(pump.select < 0)) {
      if (all(pump.select < 0)) {
        pump.select <- setdiff(pmp$id, -pump.select)
      } else {
        stop("'pump.select' must be all postive or negative.", call. = FALSE)
      }
    }
  }

  if (!is.null(pump.select)) unselected <- pmp[!pmp$id %in% pump.select, ]

  statistic.data <- lapply(cells, function(c) {
      sp::point.in.polygon(cholera::fatalities.address$lon,
        cholera::fatalities.address$lat, c$lon, c$lat)
    })

  if (!is.null(pump.select)) names(statistic.data) <- pump.select

  case.partition <- lapply(statistic.data, function(dat) {
    cholera::fatalities.address$anchor[dat == 1]
  })

  if (!is.null(pump.select)) {
    snow.colors <- snowColors(vestry = vestry)[paste0("p", pump.select)]
  } else {
    snow.colors <- snowColors(vestry = vestry)
  }

  vars <- c("lon", "lat")

  if (!is.null(pump.select))  {
    points(unselected[, vars], pch = 2, col = "gray")
    text(unselected[, vars], labels = paste0("p", unselected$id), pos = 1,
      col = "gray")
  }

  if (euclidean.paths) {
    cases <- cholera::fatalities.address
    if (is.null(pump.select)) p.id <- pmp$id
    else p.id <- pump.select

    nearest.pump <- do.call(rbind, lapply(cases$anchor, function(x) {
      m1 <- as.matrix(cases[cases$anchor == x, vars])
      d <- vapply(p.id, function(p) {
        m2 <- as.matrix(pmp[pmp$id == p, vars])
        sp::spDistsN1(m1, m2, longlat = TRUE) * 1000L
      }, numeric(1L))
      near.id <- which.min(d)
      if (is.null(pump.select)) p.nr <- pmp$id[near.id]
      else p.nr <- p.id[near.id]
      data.frame(case = x, pump = p.nr, meters = d[near.id])
    }))

    invisible(lapply(nearest.pump$case, function(x) {
      ego <- cases[cases$anchor == x, vars]
      p <- nearest.pump[nearest.pump$case == x, "pump"]
      alter <- pmp[pmp$id == p, vars]
      segments(ego$lon, ego$lat, alter$lon, alter$lat,
               col = snow.colors[paste0("p", p)], lwd = 0.5)
    }))
  } else {
    invisible(lapply(seq_along(case.partition), function(i) {
      sel <- cholera::fatalities.address$anchor %in% case.partition[[i]]
      points(cholera::fatalities.address[sel, vars], col = snow.colors[i],
        pch = 20, cex = 0.75)
    }))
  }
}
