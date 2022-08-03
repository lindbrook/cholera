#' Compute Voronoi pump neighborhoods (lat-long prototype).
#'
#' Group cases into neighborhoods using Voronoi tessellation.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @export

neighborhoodVoronoiB <- function(pump.select = NULL, vestry = FALSE) {
  snowMap(vestry = vestry, latlong = TRUE, add.cases = FALSE, add.pumps = FALSE)
  cells <- latlongVoronoiB(pump.select = pump.select, vestry = vestry)
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

  snow.colors <- snowColors()[paste0("p", pump.select)]
  vars <- c("lon", "lat")

  if (!is.null(pump.select))  {
    points(unselected[, vars], pch = 2, col = "gray")
    text(unselected[, vars], labels = paste0("p", unselected$id), pos = 1,
      col = "gray")
  }

  invisible(lapply(seq_along(case.partition), function(i) {
    sel <- cholera::fatalities.address$anchor %in% case.partition[[i]]
    points(cholera::fatalities.address[sel, vars], col = snow.colors[i],
      pch = 20, cex = 0.75)
  }))
}
