#' String of pearls functions.
#'
#' @param latlong Logical. Use estimated longitude and latitude.
#' @noRd

pearlStringRadius <- function(latlong = FALSE) {
  if (latlong) {
    dat <- cholera::latlong.regular.cases[, c("x", "y")]
  } else {
    dat <- cholera::regular.cases
  }
  c(stats::dist(dat[c(1, 2), ]))
}

peripheryCases <- function(n.points, latlong = FALSE) {
  radius <- pearlStringRadius(latlong = latlong)

  if (latlong) {
    n.area <- cholera::latlong.regular.cases[n.points, c("x", "y")]
  } else {
    n.area <- cholera::regular.cases[n.points, ]
  }

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
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param tsp.method Character. Traveling saleman algorithm. See TSP::solve_TSP() for details. Default method is repetitive nearest neighbor: "repetitive_nn".
#' @importFrom TSP TSP
#' @importFrom TSP solve_TSP
#' @noRd

travelingSalesman <- function(vertices, latlong = FALSE,
  tsp.method = "repetitive_nn") {

  methods <- c("identity", "random", "nearest_insertion", "farthest_insertion",
    "cheapest_insertion", "arbitrary_insertion", "nn", "repetitive_nn")
    # "two_opt", "concorde", "linkern") # parallelization doesn't work.

  if (tsp.method %in% methods == FALSE) {
    stop('tsp.method must be "identity", "random", "nearest_insertion",
         "farthest_insertion", "cheapest_insertion", "arbitrary_insertion",
         "nn", or "repetitive_nn".')
  }

  if (latlong) {
    dat <- cholera::latlong.regular.cases[, c("x", "y")]
  } else {
    dat <- cholera::regular.cases
  }

  d <- stats::dist(dat[vertices, ])
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

#' Identify and visualize von Neumann neighbor outliers.
#'
#' @param plot Logical. TRUE plots data; FALSE returns case IDs.
#' @param pump.select Integer. Pump IDs to consider.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @noRd

polygonVertexOutliers <- function(plot = TRUE, pump.select = NULL, 
  vestry = FALSE, multi.core = FALSE) {

  if (.Platform$OS.type == "windows") {
    cores <- 1L
  } else {
    cores <- multiCore(multi.core)
  }
  
  if (is.null(pump.select)) {
    x <- neighborhoodWalking(case.set = "expected", vestry = vestry, 
      multi.core = TRUE)
    if (vestry) {
      pmp <- 1:14
    } else {
      pmp <- 1:13
    }   
  } else {
    x <- neighborhoodWalking(pump.select = pump.select, case.set = "expected", 
      vestry = vestry, multi.core = TRUE)
    pmp <- as.character(x$pump.select)
  }
  
  neighborhood.cases <- lapply(x$exp.pump.case, function(x) x - 2000L)

  vars <- c("x", "y")
  radius <- pearlStringRadius()

  neighborhood.data <-lapply(pmp, function(p) {
    n.sel <- neighborhood.cases[[p]]
    n.area <- data.frame(case = n.sel, cholera::regular.cases[n.sel, ])
    
    vonNeumann.neighborhood <- parallel::mclapply(n.area$case, function(case) {
      case.point <- n.area[n.area$case == case, vars]
      N <- signif(case.point$x) == signif(n.area$x) &
           signif(case.point$y + radius) == signif(n.area$y)
      E <- signif(case.point$x + radius) == signif(n.area$x) &
           signif(case.point$y) == signif(n.area$y)
      S <- signif(case.point$x) == signif(n.area$x) &
           signif(case.point$y - radius) == signif(n.area$y)
      W <- signif(case.point$x - radius) == signif(n.area$x) &
           signif(case.point$y) == signif(n.area$y)
      data.frame(case = case, neighbors = sum(c(N, E, S, W)))
    }, mc.cores = cores)
    
    vonNeumann.neighborhood <- do.call(rbind, vonNeumann.neighborhood)
    id <- vonNeumann.neighborhood$neighbors == 1
    list(n.area = n.area, id = id)
  })
  
  names(neighborhood.data) <- pmp

  if (plot) {
    invisible(lapply(pmp, function(nm) {
      n.area <- neighborhood.data[[nm]]$n.area
      id <- neighborhood.data[[nm]]$id
      plot(n.area[, vars], asp = 1, main = nm)
      points(n.area[id, vars], pch = 16, col = "red")    
    }))
  } else {
    out <- lapply(pmp, function(nm) {
      n.area <- neighborhood.data[[nm]]$n.area
      id <- neighborhood.data[[nm]]$id
      n.area[id, "case"]
    })
    names(out) <- pmp
    out
  }
}
