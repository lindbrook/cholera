#' String of pearls functions.
#'
#' @noRd

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
  pearl.string <- parallel::mclapply(periphery.cases, travelingSalesman,
    mc.cores = x$cores)
  names(pearl.string) <- p.num
  polygon(cholera::regular.cases[travelingSalesman(periphery.cases[[i]]), ],
    col = grDevices::adjustcolor(snowColors()[i], alpha.f = 2/3))
}
