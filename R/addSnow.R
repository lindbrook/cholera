#' Adds Snow's graphical annotation of the Broad Street pump walking neighborhood.
#'
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

  peripheryCases <- function(n.points, radius) {
    n.area <- cholera::regular.cases[n.points, ]
    periphery.test <- vapply(seq_len(nrow(n.area)), function(i) {
      case.point <- n.area[i, ]

      N <- signif(case.point$x) == signif(n.area$x) &
           signif(case.point$y + radius) == signif(n.area$y)

      E <- signif(case.point$x + radius) == signif(n.area$x) &
           signif(case.point$y) == signif(n.area$y)

      S <- signif(case.point$x) == signif(n.area$x) &
           signif(case.point$y - radius) == signif(n.area$y)

      W <- signif(case.point$x - radius) == signif(n.area$x) &
           signif(case.point$y) == signif(n.area$y)

      sum(c(N, E, S, W)) == 4
    }, logical(1L))

    row.names(n.area[which(periphery.test == FALSE), ])
  }

  pearlString <- function(vertices) {
    dat <- cholera::regular.cases[vertices, ]
    dat <- dat[order(dat$y), ] # set southernmost point as prime.
    pearl.string <- vector(mode = "character", length = length(vertices))
    pearl.string[1] <- row.names(dat[1, ])

    for (j in 2:length(pearl.string)) {
      added.pearls <- pearl.string[pearl.string != ""]
      ego.case <- added.pearls[length(added.pearls)]
      alter.sel <- row.names(dat) %in% added.pearls == FALSE
      alters <- dat[alter.sel, ]

      S  <- signif(alters$x) == signif(dat[ego.case, "x"]) &
            signif(alters$y) == signif(dat[ego.case, "y"] - radius)

      SW <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
            signif(alters$y) == signif(dat[ego.case, "y"] - radius)

      W  <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
            signif(alters$y) == signif(dat[ego.case, "y"])

      NW <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
            signif(alters$y) == signif(dat[ego.case, "y"] + radius)

      N  <- signif(alters$x) == signif(dat[ego.case, "x"]) &
            signif(alters$y) == signif(dat[ego.case, "y"] + radius)

      NE <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
            signif(alters$y) == signif(dat[ego.case, "y"] + radius)

      E  <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
            signif(alters$y) == signif(dat[ego.case, "y"])

      SE <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
            signif(alters$y) == signif(dat[ego.case, "y"] - radius)

      master.list <- list(S, SE, E, NE, N, NW, W, SW)
      names(master.list) <- c("S", "SE", "E", "NE", "N", "NW", "W", "SW")

      if (j > 2) {
        delta <- dat[ego.case, ] -
                 dat[added.pearls[(length(added.pearls) - 1)], ]

        # Prev:South
        if (delta$x == 0 & delta$y > 0) {
          lst <- master.list[-1]

        # Prev:South-East
        } else if (delta$x < 0 & delta$y > 0) {
          lst <- master.list[c(3:length(master.list), 1)]

        # Prev:East
        } else if (delta$x < 0 & delta$y == 0) {
          lst <- master.list[c(4:length(master.list), 1:2)]

        # Prev:North-East
        } else if (delta$x < 0 & delta$y < 0) {
          lst <- master.list[c(5:length(master.list), 1:3)]

        # Prev:North
        } else if (delta$x == 0 & delta$y < 0) {
          lst <- master.list[c(6:length(master.list), 1:4)]

        # Prev:North-West
        } else if (delta$x > 0 & delta$y < 0) {
          lst <- master.list[c(7:length(master.list), 1:5)]

        # Prev:West
        } else if (delta$x > 0 & delta$y == 0) {
          lst <- master.list[c(8, 1:6)]

        # Prev:North-East
        } else if (delta$x > 0 & delta$y > 0) {
          lst <- master.list[-length(master.list)]
        }

        candidates <- vapply(lst, any, logical(1L))
      } else {
        candidates <- vapply(master.list, any, logical(1L))
      }

      sel <- which(get(names(which(candidates)[1])))
      pearl.string[j] <- row.names(alters[sel, ])
    }

    pearl.string
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
    periphery.cases <- peripheryCases(row.names(snow.area), radius)
    pearl.string <- pearlString(periphery.cases)

    if (type == "boundary") {
      polygon(cholera::regular.cases[pearl.string, ], border = color, col = NA)
    } else if (type == "area") {
      polygon(cholera::regular.cases[pearl.string, ], border = "black",
        col = grDevices::adjustcolor(color, alpha.f = alpha.level))
    }
  }
}
