#' Plot Snow's Broad Street pump walking neighborhood.
#'
#' Reproduces Snow's graphic annotation in the Vestry Report.
#' @param zoom Logical.
#' @param streets Logical. TRUE plots streets. FALSE plots orthogonal area.
#' @param add.address Logical. Plots the address of cases both in and out of the neighborhood. These are the base case in a stack of fatalities.
#' @param add.landmarks Logical. Include landmarks.
#' @import graphics
#' @export
#' @examples
#' neighborhoodSnow()
#' neighborhoodSnow(streets = FALSE)
#' neighborhoodSnow(zoom = FALSE, add.address = TRUE); addKernelDensity()

neighborhoodSnow <- function(zoom = TRUE, streets = TRUE, add.address = FALSE,
  add.landmarks = TRUE) {

  roadsB <- cholera::roads[cholera::roads$street %in%
                           cholera::border == FALSE, ]

  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(roadsB[, c("x", "y")], roadsB$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  sel <- cholera::fatalities.unstacked$case %in% cholera::snow.neighborhood

  if (zoom) {
    # x.rng <- range(cholera::fatalities.unstacked[sel, "x"])
    # y.rng <- range(cholera::fatalities.unstacked[sel, "y"])
    x.rng <- range(cholera::snow.trimmed.segments[, c("x1", "x2")])
    y.rng <- range(cholera::snow.trimmed.segments[, c("y1", "y2")])
  } else {
    x.rng <- range(cholera::roads$x)
    y.rng <- range(cholera::roads$y)
  }

  plot(cholera::fatalities.unstacked[sel, c("x", "y")],
       xlim = x.rng,
       ylim = y.rng,
       pch = NA, asp = 1, cex = 1)

  invisible(lapply(border.list, lines))

  if (streets) {
    invisible(lapply(roads.list, lines, col = "gray"))

    for (i in seq_along(cholera::snow.trimmed.segments$road.segment)) {
      segments(cholera::snow.trimmed.segments[i, "x1"],
               cholera::snow.trimmed.segments[i, "y1"],
               cholera::snow.trimmed.segments[i, "x2"],
               cholera::snow.trimmed.segments[i, "y2"],
               lwd = 6, col = "dodgerblue")
    }

    if (add.address) {
      id <- cholera::fatalities.address$anchor.case %in%
        cholera::snow.neighborhood
      if (zoom) {
        points(cholera::fatalities.address[id, c("x", "y")], pch = 16,
          col = "dodgerblue")
          points(cholera::fatalities.address[!id, c("x", "y")], pch = 16,
            col = "gray")
      } else  {
        points(cholera::fatalities.address[id, c("x", "y")], cex = 0.5,
          col = "dodgerblue")
        points(cholera::fatalities.address[!id, c("x", "y")], cex = 0.5,
          col = "red")
      }
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

    if (zoom) {
      points(cholera::regular.cases[unlist(snow.trim), ], pch = 15,
        col = scales::alpha("dodgerblue", 0.33), cex = 2.5)
      sel <- cholera::ortho.proj.sp$road.segment %in% whole.seg
      snow.whole <- cholera::ortho.proj.sp[sel, "case"]
      points(cholera::regular.cases[snow.whole, ], pch = 15,
        col = scales::alpha("dodgerblue", 0.33), cex = 2.5)
    } else {
      points(cholera::regular.cases[unlist(snow.trim), ], pch = 15, cex = 2,
        col = "dodgerblue")
      sel <- cholera::ortho.proj.sp$road.segment %in% whole.seg
      snow.whole <- cholera::ortho.proj.sp[sel, "case"]
      points(cholera::regular.cases[snow.whole, ], pch = 15, cex = 2,
        col = "dodgerblue")
    }
    invisible(lapply(roads.list, lines))

    for (i in seq_along(cholera::snow.trimmed.segments$road.segment)) {
      segments(cholera::snow.trimmed.segments[i, "x1"],
               cholera::snow.trimmed.segments[i, "y1"],
               cholera::snow.trimmed.segments[i, "x2"],
               cholera::snow.trimmed.segments[i, "y2"],
               col = "black", lwd = 4)
    }

    if (add.address) {
      id <- cholera::fatalities.address$anchor.case %in%
        cholera::snow.neighborhood
      if (zoom) {
        points(cholera::fatalities.address[id, c("x", "y")], pch = 16)
          points(cholera::fatalities.address[!id, c("x", "y")], pch = 16,
            col = "gray")
      } else  {
        points(cholera::fatalities.address[id, c("x", "y")], cex = 0.5)
        points(cholera::fatalities.address[!id, c("x", "y")], cex = 0.5,
          col = "red")
      }
    }

  }

  points(cholera::pumps[7, c("x", "y")], pch = 24, col = "dodgerblue",
    bg = "white", cex = 1.5)
  points(cholera::pumps[7, c("x", "y")], pch = ".")
  title(main = "Snow's Broad Street Pump Walking Neighborhood")
  if (add.landmarks) cholera::addLandmarks()
}
