#' Adds Snow's graphical annotation of the Broad Street pump walking neighborhood.
#'
#' Uses alphahull::ashape().
#' @param type Character. Type of annotation plot: "area", "boundary" or "street".
#' @param color Character. Neighborhood color.
#' @param alpha.level Numeric. Alpha level transparency: a value in [0, 1].
#' @param line.width Numeric. Line width for "street" and "boundary".
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
#' # plot(neighborhoodVoronoi())
#' # addSnow()

addSnow <- function(type = "street", color = "dodgerblue", alpha.level = 0.25,
  line.width = 2, ...) {

  if (type %in% c("area", "boundary", "street") == FALSE) {
    stop('"type" must be "area", "boundary" or "street".')
  }

  edges <- neighborhoodData()$edges
  snow <- cholera::snowNeighborhood()

  if (type == "street") {
    invisible(lapply(c(snow$obs.edges, snow$other.edges), function(x) {
      n.edges <- edges[x, ]
      segments(n.edges$x1, n.edges$y1, n.edges$x2, n.edges$y2, lwd = line.width,
        col = color)
    }))
  } else if (type == "area" | type == "boundary") {
    snow.area <- cholera::regular.cases[snow$sim.case, ]
    snow.hull <- suppressWarnings(alphahull::ashape(snow.area, alpha = 0.2))
    e <- data.frame(snow.hull$edges)

    if (type == "boundary") {
      invisible(lapply(seq_len(nrow(e)), function(i) {
        segments(e[i, "x1"], e[i, "y1"], e[i, "x2"], e[i, "y2"], col = color,
          lwd = line.width)
      }))
    } else if (type == "area") {
      e2 <- e[, c("ind1", "ind2")]
      ordered.vertices <- vector(length = nrow(e))

      # manually set a first node.
      ordered.vertices[1] <- 3

      for (i in 2:length(ordered.vertices)) {
        dat <- e2[e2$ind1 == ordered.vertices[i - 1] |
                  e2$ind2 == ordered.vertices[i - 1], ]
        if (!all(dat[1, ] %in% ordered.vertices)) {
          ordered.vertices[i] <- unlist(dat[1, ][dat[1, ] %in%
            ordered.vertices == FALSE])
        } else if (!all(dat[2, c("ind1", "ind2")] %in% ordered.vertices)) {
          ordered.vertices[i] <- unlist(dat[2, ][dat[2, ] %in%
            ordered.vertices == FALSE])
        }
      }

      polygon(snow.area[ordered.vertices, ],
        col = grDevices::adjustcolor(color, alpha.level))
    }
  }
}
