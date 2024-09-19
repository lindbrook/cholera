#' Add Delaunay triangles.
#'
#' @param pump.select Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Exclusion (negative selection) is possible (e.g., -6).
#' @param vestry Logical. \code{FALSE} for original 13 pumps. TRUE for 14 pumps in Vestry Report.
#' @param color Character. Color of triangle edges.
#' @param line.type Character. Type of line for triangle edges.
#' @param line.width Numeric. Width of cell edges: lwd.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @note This function uses \code{deldir::deldir()}.
#' @importFrom deldir deldir
#' @export
#' @examples
#' snowMap()
#' addDelaunay()

addDelaunay <- function(pump.select = NULL, vestry = FALSE, color = "black",
  line.type = "solid", line.width = 1, latlong = FALSE) {

  if (latlong) {
    tri <- latlongVoronoiVertices(pump.select = pump.select,
      vestry = vestry)$triangles
    invisible(lapply(tri, function(x) {
      polygon(x[, c("lon", "lat")], border = color, lty = line.type, 
        lwd = line.width)
    }))
  } else {
    if (vestry) {
      p.data <- cholera::pumps.vestry
    } else {
      p.data <- cholera::pumps
    }

    p.count <- nrow(p.data)
    p.ID <- seq_len(p.count)

    if (is.null(pump.select)) {
      pump.data <- p.data[, c("x", "y")]
    } else {
      if (is.numeric(pump.select) == FALSE) stop("pump.select must be numeric.")
      if (any(abs(pump.select) %in% p.ID == FALSE)) {
        stop('With vestry = ', vestry, ", 1 >= |pump.select| <= ", p.count, ".")
      }
      pump.data <- cholera::pumps[pump.select, c("x", "y")]
    }

    dat <- deldir::deldir(pump.data, rw = c(range(cholera::roads$x),
      range(cholera::roads$y)), suppressMsge = TRUE)

    plot(dat, add = TRUE, wline = "triang", showpoints = FALSE,
      cmpnt_col = c(tri = color), cmpnt_lty = line.type)
  }
}
