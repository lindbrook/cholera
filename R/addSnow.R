#' Adds Snow's graphical annotation of the Broad Street pump walking neighborhood.
#'
#' @param type Character. Type of annotation plot: "area", "perimeter" or "street".
#' @param color Character. Neighborhood color.
#' @param alpha.level Numeric. Alpha level transparency: a value in [0, 1].
#' @param line.width Numeric. Line width for \code{type = "street"} and \code{type = "perimeter"}.
#' @import graphics
#' @export
#' @examples
#' \dontrun{
#'
#' plot(neighborhoodVoronoi())
#' addSnow()
#' }

addSnow <- function(type = "area", color = "dodgerblue", alpha.level = 0.25,
  line.width = 2) {

  if (type %in% c("area", "perimeter", "street") == FALSE) {
    stop('type must be "area", "perimeter" or "street".')
  }

  snow.col <- grDevices::adjustcolor(color, alpha.f = alpha.level)
  edges <- neighborhoodData(case.set = "snow")$edges
  snow <- snowNeighborhood()

  if (type == "street") {
    invisible(lapply(c(snow$obs.edges, snow$other.edges), function(x) {
      n.edges <- edges[x, ]
      segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = line.width,
        col = color)
    }))
  } else if (type == "area" | type == "perimeter") {
    snow.area <- cholera::regular.cases[snow$sim.case, ]
    radius <- c(stats::dist(cholera::regular.cases[c(1, 3), ]))
    periphery.cases <- peripheryCases(row.names(snow.area), pearlStringRadius())
    pearl.string <- pearlString(periphery.cases, pearlStringRadius())

    if (type == "perimeter") {
      polygon(cholera::regular.cases[pearl.string, ], border = snow.col,
        lwd = line.width)
    } else if (type == "area") {
      polygon(cholera::regular.cases[pearl.string, ], border = "black",
        col = snow.col)
    }
  }
}
