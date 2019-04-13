#' Extract vertices of Delauny triangles and Dirichelet (Voronoi) tiles.
#'
#' For construction and plotting of Delauny and Voronoi polygons.
#' @param sites Object. Data frame of sites to compute Delauny triangulation and Dirichelet (Voronoi) tessellation with variables "x" and "y".
#' @param rw.data Object. Data frame of secondary source of data to set the rectangular window or bounding box: observations, cases, etc. with variables "x" and "y".
#' @param rw Numeric. Alternative to rw.data: vector of corners to define the rectangular window or bounding box: xmin, xmax, ymin, ymax.
#' @param type Character. "tiles" (tessellation) or "triangles" (triangulation) vertices.
#' @param output Character. "vertices" or "polygons". "vertices" re "polygons" will draw base R polygons() to an existing plot.
#' @return An R list of data frames or base R graphics polygon()'s'.
#' @note This function relies on the 'deldir' package.
#' @export
#' @examples
#' snowMap()
#' voronoiPolygons(pumps, output = "polygons")
#'
#' snowMap()
#' voronoiPolygons(pumps, roads, output = "polygons")
#'
#' snowMap()
#' voronoiPolygons(pumps, roads, type = "triangles", output = "polygons")
#'
#' vertices <- voronoiPolygons(pumps, roads)
#' snow.colors <- grDevices::adjustcolor(snowColors(), alpha.f = 1/3)
#' snowMap(add.cases = FALSE)
#' invisible(lapply(seq_along(vertices), function(i) {
#'   polygon(vertices[[i]], col = snow.colors[[i]])
#' }))

voronoiPolygons <- function(sites, rw.data = NULL, rw = NULL, type = "tiles",
  output = "vertices") {

  if (type %in% c("tiles", "triangles") == FALSE) {
    stop('type must be "tiles" or "triangles".')
  }

  if (is.null(rw.data) & is.null(rw)) {
    x.rng <- range(sites$x)
    y.rng <- range(sites$y)
  } else if (is.null(rw.data) == FALSE & is.null(rw)) {
    x.rng <- range(rw.data$x)
    y.rng <- range(rw.data$y)
  } else if (is.null(rw.data) & is.null(rw) == FALSE) {
    x.rng <- rw[1:2]
    y.rng <- rw[3:4]
  } else stop("Use either 'rw.data' or 'rw', not both.")

  tile.triangle <- deldir::deldir(sites[, c("x", "y")], rw = c(x.rng, y.rng),
    suppressMsge = TRUE)

  if (type == "tiles") {
    vertex.data <- deldir::tile.list(tile.triangle)
  } else if (type == "triangles") {
    vertex.data <- deldir::triang.list(tile.triangle)
  }

  vertices <- lapply(vertex.data, function(dat) {
    data.frame(x = dat$x, y = dat$y)
  })

  if (output == "vertices") vertices
  else if (output == "polygons") invisible(lapply(vertices, polygon))
  else stop('output must either be "vertices" or "polygons".')
}
