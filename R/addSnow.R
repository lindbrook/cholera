#' Adds Snow's Annotation  of the Broad Street pump walking neighborhood.
#'
#' Reproduces Snow's graphic annotation in the Vestry Report.
#' @param streets Logical. TRUE plots streets. FALSE plots orthogonal area.
#' @param color Character. Color for neighborhood annotation.
#' @param alpha.st Numeric. A value in [0, 1] to set alpha level for street neighborhood annotation.
#' @param alpha.area Numeric. A value in [0, 1] to set alpha level for area neighborhood annotation.
#' @param ... Additional plotting parameters.
#' @seealso \code{\link{snowMap}},
#' \code{\link{addKernelDensity}},
#' \code{\link{addLandmarks}},
#' \code{\link{addPlaguePit}},
#' \code{\link{addVoronoi}},
#' \code{\link{addWhitehead}}
#' @import graphics
#' @export
#' @examples
#' plot(neighborhoodVoronoi())
#' addSnow()

addSnow <- function(streets = TRUE, color = "dodgerblue", alpha.st = 0.75,
  alpha.area = 1/3, ...) {

  if (streets) {
    snow <- sysdata[["snow"]]$pump.seg$p7
    for (i in seq_along(snow$id)) {
      segments(snow[i, "x1"], snow[i, "y1"], snow[i, "x2"], snow[i, "y2"],
        lwd = 6, col = scales::alpha(color, alpha.st))
    }
  } else {
    snow <- sysdata[["snow"]]$pump.seg$p7
    trimmed <- snow[snow$trimmed == TRUE, ]
    trim.seg <- trimmed$id
    whole.seg <- snow[snow$trimmed == FALSE, "id"]

    sel <- cholera::sim.ortho.proj$road.segment %in% trim.seg
    trim.reg.proj <- cholera::sim.ortho.proj[sel, ]

    snow.trim <- lapply(trimmed$id, function(x) {
      a <- trim.reg.proj[trim.reg.proj$road.segment == x, ]
      b <- trimmed[trimmed$id == x, ]
      xs <- unlist(sort(b[, c("x1", "x2")]))
      ys <- unlist(sort(b[, c("y1", "y2")]))
      x.test <- a$x.proj >= xs[1] & a$x.proj <= xs[2]
      y.test <- a$y.proj >= ys[1] & a$y.proj <= ys[2]
      sel <- x.test & y.test
      a[sel, "case"]
    })

    points(cholera::regular.cases[unlist(snow.trim), ], pch = 15, cex = 1,
      col = scales::alpha(color, alpha.area))
    sel <- cholera::sim.ortho.proj$road.segment %in% whole.seg
    snow.whole <- cholera::sim.ortho.proj[sel, "case"]
    points(cholera::regular.cases[snow.whole, ], pch = 15, cex = 1,
      col = scales::alpha(color, alpha.area))
  }
}
