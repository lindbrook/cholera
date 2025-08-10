#' Add Voronoi cells.
#'
#' @param pump.select Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Exclusion (negative selection) is possible (e.g., -6).
#' @param vestry Logical. \code{FALSE} for original 13 pumps. TRUE for 14 pumps in Vestry Report.
#' @param case.location Character. For \code{observed = FALSE}: "address" or "nominal". "nominal" is the x-y coordinates of \code{regular.cases}.
#' @param color Character. Color of cell edges.
#' @param line.type Character. Type of line for cell edges: lty.
#' @param line.width Numeric. Width of cell edges: lwd.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @note This function uses \code{deldir::deldir()}.
#' @import graphics
#' @export
#' @examples
#' snowMap()
#' # addVoronoi()

addVoronoi <- function(pump.select = NULL, vestry = FALSE,
  case.location = "nominal", color = "black", line.type = "solid",
  line.width = 1, latlong = FALSE) {

  if (!is.null(pump.select)) {
    if (!is.numeric(pump.select)) {
      stop("pump.select must be numeric.", call. = FALSE)
    }
    if (length(pump.select) < 2 & all(pump.select > 0)) {
      stop("With Voronoi diagram, use at least 2 pumps for pump.select.",
        call. = FALSE)
    }
  }

  if (latlong) {
    cells <- latlongVoronoiVertices(pump.select = pump.select,
      vestry = vestry)$cells
    invisible(lapply(cells, function(x) {
      polygon(x[, c("lon", "lat")], border = color, lty = line.type, 
        lwd = line.width)
    }))
  } else {
    rng <- mapRange(latlong = latlong)
    
    if (case.location %in% c("address", "nominal") == FALSE) {
      stop('case.location must be "address" or "nominal".', call. = FALSE)
    }

    vars <- c("x", "y")

    if (case.location == "address") {
      if (vestry) {
        p.data <- cholera::ortho.proj.pump.vestry
        p.data$street <- cholera::pumps.vestry$street
      } else {
        p.data <- cholera::ortho.proj.pump
        p.data$street <- cholera::pumps$street
        names(p.data)[names(p.data) %in% c("x.proj", "y.proj")] <- vars
      }
    } else if (case.location == "nominal") {
      if (vestry) {
        p.data <- cholera::pumps.vestry
      } else {
        p.data <- cholera::pumps
      }
    }

    if (is.null(pump.select)) {
      pump.data <- p.data[, vars]
    } else {
      if (any(abs(pump.select) %in% p.data$id == FALSE)) {
        stop('With vestry = ', vestry, ", 1 >= |pump.select| <= ", nrow(p.data),
          ".", call. = FALSE)
      } else pump.data <- cholera::pumps[pump.select, vars]
    }

    dat <- deldir::deldir(pump.data, rw = unlist(rng), suppressMsge = TRUE)

    plot(dat, add = TRUE, wline = "tess", showpoints = FALSE, cmpnt_col = color,
      cmpnt_lty = line.type, lwd = line.width)
      # cmpnt_col = c(tri = color) wants number 1:8 ...
  }
}
