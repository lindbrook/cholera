#' Adds Snow's Annotation  of the Broad Street pump walking neighborhood.
#'
#' Reproduces Snow's graphic annotation in the Vestry Report.
#' @param streets Logical. TRUE plots streets. FALSE plots orthogonal area.
#' @param color Character. Color for neighborhood annotation.
#' @param ... Additional plotting parameters.
#' @import graphics
#' @export
#' @examples
#' plot(neighborhoodVoronoi())
#' addSnow()

addSnow <- function(streets = TRUE, color = "dodgerblue", ...) {
  if (streets) {
    for (i in seq_along(cholera::snow.trimmed.segments$road.segment)) {
      segments(cholera::snow.trimmed.segments[i, "x1"],
               cholera::snow.trimmed.segments[i, "y1"],
               cholera::snow.trimmed.segments[i, "x2"],
               cholera::snow.trimmed.segments[i, "y2"],
               lwd = 6, col = scales::alpha(color, 0.75))
    }
  } else {
    snow <- cholera::snow.trimmed.segments
    trimmed <- snow[snow$trimmed == TRUE, ]
    trim.seg <- trimmed$road.segment
    whole.seg <- snow[snow$trimmed == FALSE, "road.segment"]

    sel <- cholera::ortho.proj.sp$road.segment %in% trim.seg
    trim.reg.proj <- cholera::ortho.proj.sp[sel, ]

    snow.trim <- lapply(trimmed$road.segment, function(x) {
      a <- trim.reg.proj[trim.reg.proj$road.segment == x, ]
      b <- trimmed[trimmed$road.segment == x, ]
      xs <- unlist(sort(b[, c("x1", "x2")]))
      ys <- unlist(sort(b[, c("y1", "y2")]))
      x.test <- a$x.proj >= xs[1] & a$x.proj <= xs[2]
      y.test <- a$y.proj >= ys[1] & a$y.proj <= ys[2]
      sel <- x.test & y.test
      a[sel, "case"]
    })

    points(cholera::regular.cases[unlist(snow.trim), ], pch = 15, cex = 1,
      col = scales::alpha(color, 1/3))
    sel <- cholera::ortho.proj.sp$road.segment %in% whole.seg
    snow.whole <- cholera::ortho.proj.sp[sel, "case"]
    points(cholera::regular.cases[snow.whole, ], pch = 15, cex = 1,
      col = scales::alpha(color, 1/3))
  }
}
