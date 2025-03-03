#' Project observed fatalities onto road network
#'
#' Finds the "addresses" of those cases via orthogonal projection or simple proximity to road graph network. These data are used to generate "observed" pump neighborhoods.
#' @param case.type Character. "address" or "fatality".
#' @param radius Numeric. Radius to check for nearby road segments. Nominal Dodson and Tobler units.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores (rounds with \code{as.integer()}). See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @noRd

orthogonalProjectionFatalities <- function(case.type = "fatality", radius = 2,
  multi.core = FALSE) {

  vars <- c("x", "y")
  cores <- multiCore(multi.core)
  manual.classification <- caseRoadClassificationFix()

  if (case.type == "address") {
    obs.cases <- cholera::fatalities.address
    ids <- obs.cases$anchor
  } else if (case.type == "fatality") {
    obs.cases <- cholera::fatalities
    ids <- obs.cases$case
  }

  # set St James Workhouse cases at Poland Street "entrance".

  sel <- cholera::anchor.case$anchor == 369
  workhouse.cases <- cholera::anchor.case[sel, "case"]

  poland.seg <- "194-1"
  sel <- cholera::road.segments$id == poland.seg
  poland.st.north <- cholera::road.segments[sel, paste0(vars, 1)]
  names(poland.st.north) <- vars

  sel <- ids[ids %in% workhouse.cases]
  workhouse <- cholera::fatalities[cholera::fatalities$case %in% sel, vars]

  ds <- as.matrix(stats::dist(rbind(poland.st.north, workhouse)))[-1, 1]

  ortho.proj.workhouse <- data.frame(case = sel,
                                     road.segment = poland.seg,
                                     x.proj = poland.st.north$x,
                                     y.proj = poland.st.north$y,
                                     dist = ds,
                                     type = "eucl")

  ids <- ids[!ids %in% workhouse.cases]

  # compute other orthogonal coordianates

  orthogonal.projection <- parallel::mclapply(ids, function(x) {
    if (case.type == "address") {
      case <- obs.cases[obs.cases$anchor == x, ]
      case.id <- case$anchor
    } else if (case.type == "fatality") {
      case <- obs.cases[obs.cases$case == x, ]
      case.id <- case$case
    }

    if (case.id %in% unlist(manual.classification)) {
      sel <- vapply(manual.classification, function(x) {
        case.id %in% x
      }, logical(1L))
      within.radius <- names(manual.classification[sel])
    } else {
      ones <- rbind(case[, vars],
        stats::setNames(cholera::road.segments[, paste0(vars, 1)], vars))
      twos <- rbind(case[, vars],
        stats::setNames(cholera::road.segments[, paste0(vars, 2)], vars))
      d1 <- as.matrix(stats::dist(ones))[-1, 1]
      d2 <- as.matrix(stats::dist(twos))[-1, 1]
      within.radius <- cholera::road.segments$id[d1 <= radius & d2 <= radius]
    }

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      ortho.data <- orthogonalProjection(case = case.id, segment.id = seg.id, 
        observed = TRUE, case.data = case)
      x.proj <- ortho.data$x.proj
      y.proj <- ortho.data$y.proj

      sel <- cholera::road.segments$id == seg.id
      seg.data <- cholera::road.segments[sel, c("x1", "y1", "x2", "y2")]

      seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                           y = c(seg.data$y1, seg.data$y2))

      ## segment bisection/intersection test ##

      distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
               stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

      bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

      if (bisect.test) {
        ortho.dist <- c(stats::dist(rbind(c(case$x, case$y),
          c(x.proj, y.proj))))
        ortho.pts <- data.frame(x.proj, y.proj)
        data.frame(road.segment = seg.id, ortho.pts, ortho.dist)
      }
    })

    ortho <- do.call(rbind, ortho.proj.test)

    if (any(!is.na(ortho$ortho.dist))) {
      projection <- ortho[which.min(ortho$ortho.dist), ]
      names(projection)[names(projection) == "ortho.dist"] <- "dist"
      projection$type <- "ortho"
    } else {
      ## nearest endpoint of nearest road segment (sum of distance to endpts) ##

      if (case.id %in% unlist(manual.classification)) {
        sel <- cholera::road.segments$id == within.radius
        candidates <- cholera::road.segments[sel, ]

        ones <- rbind(case[, vars],
                      stats::setNames(candidates[, paste0(vars, 1)], vars))
        twos <- rbind(case[, vars],
                      stats::setNames(candidates[, paste0(vars, 2)], vars))
        d1 <- stats::dist(ones)
        d2 <- stats::dist(twos)
        nr.ep <- which.min(c(d1, d2))
        nr.coords <- candidates[, paste0(vars, nr.ep)]

        projection <- data.frame(road.segment = within.radius,
                                 x.proj = unname(nr.coords[1]),
                                 y.proj = unname(nr.coords[2]),
                                 dist = get(paste0("d", nr.ep)),
                                 type = "eucl")
      } else {
        unbisected.segs <- setdiff(within.radius, ortho$road.segment)

        sel <- cholera::road.segments$id %in% unbisected.segs
        candidates <- cholera::road.segments[sel, ]

        ones <- rbind(case[, vars],
                      stats::setNames(candidates[, paste0(vars, 1)], vars))
        twos <- rbind(case[, vars],
                      stats::setNames(candidates[, paste0(vars, 2)], vars))

        ep.dist <- data.frame(seg = unbisected.segs,
                              d1 = as.matrix(stats::dist(ones))[-1, 1],
                              d2 = as.matrix(stats::dist(twos))[-1, 1])

        # select segment by minimum total euclidean distance to endpoints
        nr.seg <- ep.dist[which.min(rowSums(ep.dist[, c("d1", "d2")])), ]

        # select closest endpoint of nearest segment
        nr.ep <- which.min(nr.seg[ c("d1", "d2")])

        # extract endpoint coordinates
        nr.coords <- candidates[candidates$id == nr.seg$seg, paste0(vars, nr.ep)]

        projection <- data.frame(road.segment = nr.seg$seg,
                                 x.proj = unname(nr.coords[1]),
                                 y.proj = unname(nr.coords[2]),
                                 dist = nr.seg[, paste0("d", nr.ep)],
                                 type = "eucl")
      }
    }
    projection
  }, mc.cores = cores)

  ortho.proj <- data.frame(case = ids, do.call(rbind, orthogonal.projection))
  ortho.proj <- rbind(ortho.proj, ortho.proj.workhouse)
  ortho.proj <- ortho.proj[order(ortho.proj$case), ]
  row.names(ortho.proj) <- NULL
  ortho.proj
}
