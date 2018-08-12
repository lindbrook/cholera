#' Adds Snow's graphical annotation of the Broad Street pump walking neighborhood.
#'
#' @param type Character. Type of annotation plot: "area", "boundary" or "street".
#' @param color Character. Neighborhood color.
#' @param alpha.level Numeric. Alpha level transparency: a value in [0, 1].
#' @param line.width Numeric. Line width for \code{type = "street"} and \code{type = "boundary"}.
#' @param ... Additional plotting parameters.
#' @seealso \code{\link{snowMap}},
#' \code{\link{addIndexCase}},
#' \code{\link{addKernelDensity}},
#' \code{\link{addLandmarks}},
#' \code{\link{addPlaguePit}},
#' \code{\link{addVoronoi}},
#' \code{\link{addWhitehead}}
#' @import graphics
#' @export
#' @examples
#' \dontrun{
#'
#' plot(neighborhoodVoronoi())
#' addSnow()
#' }

addSnow <- function(type = "area", color = "dodgerblue", alpha.level = 0.25,
  line.width = 2, ...) {

  if (type %in% c("area", "boundary", "street") == FALSE) {
    stop('type must be "area", "boundary" or "street".')
  }

  edges <- cholera::neighborhoodData(case.set = "snow")$edges
  snow <- cholera::snowNeighborhood()

  if (type == "street") {
    invisible(lapply(c(snow$obs.edges, snow$other.edges), function(x) {
      n.edges <- edges[x, ]
      segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = line.width,
        col = color)
    }))
  } else if (type == "area" | type == "boundary") {
    snow.area <- cholera::regular.cases[snow$sim.case, ]
    radius <- c(stats::dist(cholera::regular.cases[c(1, 3), ]))
    periphery.cases <- peripheryCases(row.names(snow.area), pearlStringRadius())
    pearl.string <- pearlString(periphery.cases, pearlStringRadius())

    if (type == "boundary") {
      polygon(cholera::regular.cases[pearl.string, ], border = color, col = NA)
    } else if (type == "area") {
      polygon(cholera::regular.cases[pearl.string, ], border = "black",
        col = grDevices::adjustcolor(color, alpha.f = alpha.level))
    }
  }
}
