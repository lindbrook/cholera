#' String of pearls functions.
#'
#' @noRd

pearlStringFunctions <- function() NULL

pearlStringRadius <- function() {
  c(stats::dist(cholera::regular.cases[c(1, 2), ]))
}

# remove observations with neighbors at each of the 4 cardinal directions
peripheryCases <- function(n.points, radius = pearlStringRadius()) {
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

# Add pearls to string to form a closed, non-intersecting polygon.
# Default method of neighborhoodWalking().
pearlString <- function(vertices, radius = pearlStringRadius(),
  orientation = "clockwise") {

  dat <- cholera::regular.cases[vertices, ]
  dat <- dat[order(dat$y, dat$x), ] # set most SW point as first observation.

  pearl.string <- vector(mode = "character", length = length(vertices))
  pearl.string[1] <- row.names(dat[1, ])

  for (j in 2:length(pearl.string)) {
    added.pearls <- pearl.string[pearl.string != ""]
    ego.case <- added.pearls[length(added.pearls)]
    alters <- dat[row.names(dat) %in% added.pearls == FALSE, ]

    N  <- signif(alters$x) == signif(dat[ego.case, "x"]) &
          signif(alters$y) == signif(dat[ego.case, "y"] + radius)

    NE <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
          signif(alters$y) == signif(dat[ego.case, "y"] + radius)

    E  <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
          signif(alters$y) == signif(dat[ego.case, "y"])

    SE <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
          signif(alters$y) == signif(dat[ego.case, "y"] - radius)

    S  <- signif(alters$x) == signif(dat[ego.case, "x"]) &
          signif(alters$y) == signif(dat[ego.case, "y"] - radius)

    SW <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
          signif(alters$y) == signif(dat[ego.case, "y"] - radius)

    W  <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
          signif(alters$y) == signif(dat[ego.case, "y"])

    NW <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
          signif(alters$y) == signif(dat[ego.case, "y"] + radius)

    master.list <- list(N = N, NE = NE, E = E, SE = SE, S = S, SW = SW, W = W,
      NW = NW)

    if (j > 2) {
      clockwise.compass <- lapply(-seq_len(length(master.list)), function(i) {
        vec <- names(master.list)[i]
        if (abs(i) == 1 | abs(i) == length(master.list)) vec
        else vec[c(abs(i):length(vec), 1:(abs(i) - 1))]
      })

      counterclockwise.compass <- lapply(clockwise.compass, rev)
      names(clockwise.compass) <- names(master.list)
      names(counterclockwise.compass) <- names(master.list)

      if (orientation == "clockwise") {
        compass <- clockwise.compass
      } else if (orientation == "counterclockwise") {
        compass <- counterclockwise.compass
      }

      delta <- dat[ego.case, ] - dat[added.pearls[(length(added.pearls) - 1)], ]

      if (delta$x == 0 & delta$y < 0) {
        lst <- compass["N"]  # Prev: North
      } else if (delta$x < 0 & delta$y < 0) {
        lst <- compass["NE"] # Prev: North-East
      } else if (delta$x < 0 & delta$y == 0) {
        lst <- compass["E"]  # Prev: East
      } else if (delta$x < 0 & delta$y > 0) {
        lst <- compass["SE"] # Prev: South-East
      } else if (delta$x == 0 & delta$y > 0) {
        lst <- compass["S"]  # Prev: South
      } else if (delta$x > 0 & delta$y > 0) {
        lst <- compass["SW"] # Prev: South-West
      } else if (delta$x > 0 & delta$y == 0) {
        lst <- compass["W"]  # Prev: West
      } else if (delta$x > 0 & delta$y < 0) {
        lst <- compass["NW"] # Prev: North-West
      }

      candidates <- vapply(master.list, any, logical(1L))[unlist(lst)]

      # Exception to consider second-order candidates for pearl string.
      if (all(candidates == FALSE)) {
        n   <- signif(alters$x) == signif(dat[ego.case, "x"]) &
               signif(alters$y) == signif(dat[ego.case, "y"] + 2 * radius)

        nne <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + 2 * radius)

        ne  <- signif(alters$x) == signif(dat[ego.case, "x"] + 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + 2 * radius)

        ene <- signif(alters$x) == signif(dat[ego.case, "x"] + 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + radius)

        e   <- signif(alters$x) == signif(dat[ego.case, "x"] + 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"])

        ese <- signif(alters$x) == signif(dat[ego.case, "x"] + 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - radius)

        se  <- signif(alters$x) == signif(dat[ego.case, "x"] + 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - 2 * radius)

        sse <- signif(alters$x) == signif(dat[ego.case, "x"] + radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - 2 * radius)

        s   <- signif(alters$x) == signif(dat[ego.case, "x"]) &
               signif(alters$y) == signif(dat[ego.case, "y"] - 2 * radius)

        ssw <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - 2 * radius)

        sw  <- signif(alters$x) == signif(dat[ego.case, "x"] - 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - 2 * radius)

        wsw <- signif(alters$x) == signif(dat[ego.case, "x"] - 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] - radius)

        w   <- signif(alters$x) == signif(dat[ego.case, "x"] - 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"])

        wnw <- signif(alters$x) == signif(dat[ego.case, "x"] - 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + radius)

        nw  <- signif(alters$x) == signif(dat[ego.case, "x"] - 2 * radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + 2 * radius)

        nnw <- signif(alters$x) == signif(dat[ego.case, "x"] - radius) &
               signif(alters$y) == signif(dat[ego.case, "y"] + 2 * radius)

        # closest second order neighbors
        master.listB <- list(n = n, e = e, s = s, w = w,
                             nne = nne, ene = ene,
                             ese = ese, sse = sse,
                             ssw = ssw, wsw = wsw,
                             wnw = wnw, nnw = nnw,
                             ne = ne, se = se, sw = sw, nw = nw)

        idx <- -seq_len(length(master.listB))

        clockwise.compassB <- lapply(idx, function(i) {
          vec <- names(master.listB)[i]
          if (abs(i) == 1 | abs(i) == length(master.listB)) vec
          else vec[c(abs(i):length(vec), 1:(abs(i) - 1))]
        })

        counterclockwise.compassB <- lapply(clockwise.compassB, rev)
        names(clockwise.compassB) <- names(master.listB)
        names(counterclockwise.compassB) <- names(master.listB)

        if (orientation == "clockwise") {
          compassB <- clockwise.compassB
        } else if (orientation == "counterclockwise") {
          compassB <- counterclockwise.compassB
        }

        # increment by one compass point
        if (delta$x == 0 & delta$y < 0) {
          lstB <- compassB["n"]  # Prev: North
        } else if (delta$x < 0 & delta$y < 0) {
          lstB <- compassB["ne"] # Prev: North-East
        } else if (delta$x < 0 & delta$y == 0) {
          lstB <- compassB["e"]  # Prev: East
        } else if (delta$x < 0 & delta$y > 0) {
          lstB <- compassB["se"] # Prev: South-East
        } else if (delta$x == 0 & delta$y > 0) {
          lstB <- compassB["s"]  # Prev: South
        } else if (delta$x > 0 & delta$y > 0) {
          lstB <- compassB["sw"] # Prev: South-West
        } else if (delta$x > 0 & delta$y == 0) {
          lstB <- compassB["w"]  # Prev: West
        } else if (delta$x > 0 & delta$y < 0) {
          lstB <- compassB["nw"] # Prev: North-West
        }

        candidatesB <- vapply(master.listB, any, logical(1L))[unlist(lstB)]
        sel <- which(get(names(which(candidatesB)[1])))

      } else {
        sel <- which(get(names(which(candidates)[1])))
      }

    } else {
      candidates <- vapply(master.list, any, logical(1L))

      if (orientation == "clockwise") {
        second.pearl <- vapply(c("W", "NW", "N", "NE"), function(x) {
          x %in% names(candidates[candidates])
        }, logical(1L))
      } else if (orientation == "counterclockwise") {
        second.pearl <- vapply(c("E", "NE", "N", "NW"), function(x) {
          x %in% names(candidates[candidates])
        }, logical(1L))
      }

      sel <- which(get(names(second.pearl[second.pearl])))
    }
    pearl.string[j] <- row.names(alters[sel, ])
  }
  pearl.string
}

#' Compute polygon vertices via 'TSP' package.
#'
#' @param vertices Object. Polygon vertices candidates.
#' @param tsp.method Character. Traveling saleman algorithm. See TSP::solve_TSP() for details. Default method is repetitive nearest neighbor: "repetitive_nn".
#' @note Default method for neighborhoodEuclidean().
#' @noRd

travelingSalesman <- function(vertices, tsp.method = "repetitive_nn") {
  methods <- c("identity", "random", "nearest_insertion", "farthest_insertion",
    "cheapest_insertion", "arbitrary_insertion", "nn", "repetitive_nn")

  if (tsp.method %in% methods == FALSE) {
    stop('tsp.method must be "identity", "random", "nearest_insertion",
         "farthest_insertion", "cheapest_insertion", "arbitrary_insertion",
         "nn", or "repetitive_nn".')
  }

  d <- stats::dist(cholera::regular.cases[vertices, ])
  distances <- data.frame(t(utils::combn(vertices, 2)), c(d),
    stringsAsFactors = FALSE)
  names(distances) <- c("a", "b", "dist")
  distances$pathID <- paste0(distances$a, "-", distances$b)
  distances$rev.pathID <- paste0(distances$b, "-", distances$a)
  tsp <- TSP::TSP(d, labels = vertices)
  soln <- TSP::solve_TSP(tsp, method = tsp.method)
  names(soln)
}

## diagnostic plots ##

#' Plot periphery cases.
#'
#' @param x Object. Neighborhood data.
#' @param i Numeric. Neighborhood ID.
#' @param pch Numeric.
#' @param cex Numeric.
#' @noRd

peripheryAudit <- function(x, i = 1, pch = 16, cex = 0.5) {
  nearest.pump <- x$nearest.pump
  p.num <- sort(unique(nearest.pump))
  neighborhood.cases <- lapply(p.num, function(n) {
    which(nearest.pump == n)
  })

  periphery.cases <- parallel::mclapply(neighborhood.cases, peripheryCases,
    mc.cores = x$cores)
  points(cholera::regular.cases[periphery.cases[[i]], ], pch = pch, cex = cex,
    col = snowColors()[i])
}

#' Plot neighborhood polygon.
#'
#' @param x Object. Neighborhood data.
#' @param i Numeric. Neighborhood ID.
#' @noRd

polygonAudit <- function(x, i = 1) {
  nearest.pump <- x$nearest.pump
  p.num <- sort(unique(nearest.pump))
  neighborhood.cases <- lapply(p.num, function(n) {
    which(nearest.pump == n)
  })

  periphery.cases <- parallel::mclapply(neighborhood.cases, peripheryCases,
    mc.cores = x$cores)
  pearl.string <- parallel::mclapply(periphery.cases, pearlString,
    mc.cores = x$cores)
  names(pearl.string) <- p.num
  polygon(cholera::regular.cases[pearlString(periphery.cases[[i]]), ],
    col = grDevices::adjustcolor(snowColors()[i], alpha.f = 2/3))
}
