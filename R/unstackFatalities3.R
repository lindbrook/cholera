#' Unstack "stacks" in Snow's cholera map.
#'
#' Unstacks fatalities data by 1) assigning the coordinates of the base case to all cases in a stack and 2) setting the base case as an "address" and making the number of fatalities an attribute.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @param fatalities Corrected fatalities data from \code{cholera::fixFatalities()}. For original data, use \code{HistData::Snow.deaths}.
#' @seealso \code{vignette("unstacking.fatalities")}
#' @return An R list that includes \code{anchor.case}, \code{fatalities.address}, \code{fatalities.unstacked} and \code{ortho.proj}.
#' @section Notes: This function is computationally intensive. On a 2.3 GHz Intel Core i7 with R version 3.6.0, it takes approximately 156 seconds to run on one core and approximately 37 seconds to run on eight logical (four physical) cores. These functions document the code that generates \code{\link{anchor.case}}, \code{\link{fatalities.address}}, \code{\link{fatalities.unstacked}} and \code{\link{ortho.proj}}.
#' @export

unstackFatalities3 <- function(multi.core = FALSE,
  fatalities = fixFatalities()) {

  cores <- multiCore(multi.core)
  case.id <- fatalities$case

  # if(.Platform$OS.type == "windows") {
  # }

  cl <- parallel::makeCluster(cores)

  parallel::clusterExport(cl = cl, varlist = "fatalities",
    envir = environment())

  orthogonal.projection <- parallel::parLapply(cl, case.id, function(case) {
    case.data <- fatalities[fatalities$case == case, c("x", "y")]

    within.radius <- lapply(cholera::road.segments$id, function(id) {
      seg.data <- cholera::road.segments[cholera::road.segments$id == id, ]
      test1 <- cholera::withinRadius(case.data, seg.data[, c("x1", "y1")])
      test2 <- cholera::withinRadius(case.data, seg.data[, c("x2", "y2")])
      if (any(test1, test2)) unique(seg.data$id)
    })

    within.radius <- unlist(within.radius)

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      ortho.data <- cholera::orthogonalProjection(case, seg.id)
      x.proj <- ortho.data$x.proj
      y.proj <- ortho.data$y.proj

      seg.data <- cholera::road.segments[cholera::road.segments$id == seg.id,
        c("x1", "y1", "x2", "y2")]

      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      # segment bisection/intersection test
      distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
               stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

      bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

      if (bisect.test) {
        dat <- rbind(c(case.data$x, case.data$y), c(x.proj, y.proj))
        ortho.dist <- c(stats::dist(dat))
        ortho.pts <- data.frame(x.proj, y.proj)
        data.frame(road.segment = seg.id, ortho.pts, ortho.dist,
          stringsAsFactors = FALSE)
      } else {
        null.out <- data.frame(matrix(NA, ncol = 4))
        names(null.out) <- c("road.segment", "x.proj", "y.proj", "ortho.dist")
        null.out
      }
    })

    out <- do.call(rbind, ortho.proj.test)

    if (all(is.na(out)) == FALSE) {
      sel <- which.min(out$ortho.dist)
      out[sel, ]
    } else {
      out[1, ] # all candidate roads are NA; arbitrarily choose first obs.
    }
  })

  parallel::stopCluster(cl)
  orthogonal.projection
}
