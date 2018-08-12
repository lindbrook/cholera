#' Add distance or time based "mileposts" to a walking neighborhood plot.
#'
#' @param pump.subset Numeric. Vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} uses all pumps in \code{pump.select}.
#' @param pump.select Numeric. Numeric vector of pumps to define possible pump neighborhoods (i.e. the "population"). Negative selection is possible. NULL selects all "observed" pumps (i.e., pumps with at least one case).
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. FALSE uses the 13 from the original map.
#' @param unit Character. Milepost unit of measurement: "distance" or "time".
#' @param interval Numeric. Interval between mileposts: 50 meters for "distance";  60 seconds for "time".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param type Character. "arrows" or "points".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @return R base graphics arrows or points.
#' @export

addMilePosts <- function(pump.subset = NULL, pump.select = NULL,
  vestry = FALSE, unit = "distance", interval = NULL, walking.speed = 5,
  type = "arrows", multi.core = FALSE) {

  if (type %in% c("arrows", "points") == FALSE) {
    stop('type must either be "arrows" or "points"')
  }

  cores <- multiCore(multi.core)
  x <- cholera::neighborhoodWalking(pump.select, vestry, multi.core = cores)
  dat <- cholera::neighborhoodData(vestry = x$vestry, case.set = "observed")
  edges <- dat$edges
  nodes <- dat$nodes
  p.data <- dat$nodes.pump

  if (is.null(x$pump.select)) {
    p.node <- p.data$node
    p.name <- p.data$pump
  } else {
    if (all(x$pump.select > 0)) {
      p.data <- p.data[p.data$pump %in% x$pump.select, ]
    } else if (all(x$pump.select < 0)) {
      p.data <- p.data[p.data$pump %in% abs(x$pump.select) == FALSE, ]
    }
    p.node <- p.data$node
    p.name <- p.data$pump
  }

  # vector of nodes for the 321 observed anchor cases
  n.path.edges <- parallel::mclapply(x$paths, function(neighborhood) {
    lapply(neighborhood, auditEdge, edges, output = "id2")
  }, mc.cores = x$cores)

  if (!is.null(pump.subset)) {
    if (all(pump.subset > 0)) {
      sel <- paste(pump.subset)
    } else if (all(pump.select < 0)) {
      sel <- setdiff(names(x$paths), paste(abs(pump.subset)))
    }

    # path's case edge and path's other, remaining edges
    case.edge <- lapply(n.path.edges[sel], function(n) {
      vapply(n, function(x) x[1], character(1L))
    })

    # path's case edge and path's other, remaining edges
    noncase.edges <- lapply(n.path.edges[sel], function(n) {
      lapply(n, function(x) x[-1])
    })

    neighborhood.names <- sel

  } else {
    case.edge <- lapply(n.path.edges, function(n) {
      vapply(n, function(x) x[1], character(1L))
    })

    noncase.edges <- lapply(n.path.edges, function(n) {
      lapply(n, function(x) x[-1])
    })

    neighborhood.names <- names(x$path)
  }

  # potential neighborhood periphery edges
  candidate.case.edge <- lapply(seq_along(case.edge), function(i) {
    case.edge[[i]][case.edge[[i]] %in% unlist(noncase.edges[[i]]) == FALSE]
  })

  candidate.case.edge <- stats::setNames(candidate.case.edge,
    neighborhood.names)

  candidate.data <- lapply(seq_along(candidate.case.edge), function(i) {
    sel <- setdiff(candidate.case.edge[[i]], unlist(noncase.edges[[i]]))
    edges[edges$id2 %in% sel, ]
  })

  candidate.data <- stats::setNames(candidate.data, neighborhood.names)

  candidateID <- lapply(seq_along(case.edge), function(i) {
    case.edge[[i]] %in% candidate.case.edge[[i]]
  })

  candidateID <- stats::setNames(candidateID, neighborhood.names)

  endpt.paths <- lapply(names(candidateID), function(nm) {
    n.path <- x$paths[[nm]]
    c.id <- candidateID[[nm]]
    n.path[c.id]
  })

  endpt.paths <- stats::setNames(endpt.paths, neighborhood.names)
  edge.data <- edgeData(endpt.paths, edges)

  if (is.null(interval)) {
    if (unit == "distance") interval <- 50
    else if (unit == "time") interval <- 60
  }

  if (type == "arrows") {
    coords <- parallel::mclapply(names(endpt.paths), function(nm) {
      lapply(edge.data[[nm]], postCoordinates, unit, interval, walking.speed,
        arrow.data = TRUE)
    }, mc.cores = cores)

    coords <- stats::setNames(coords, names(endpt.paths))

    # test for no mileposts or timeposts
    no.posts <- vapply(coords, function(x) {
      is.null(nrow(do.call(rbind, x)))
    }, logical(1L))

    invisible(lapply(names(coords)[!no.posts], function(nm) {
      dat <- unique(do.call(rbind, coords[[nm]]))
      color <- cholera::snowColors()[paste0("p", nm)]
      zero.length.x <- round(abs(dat$x0 - dat$x), 2) == 0
      zero.length.y <- round(abs(dat$y0 - dat$y), 2) == 0

      # fix for zero-length arrows
      if (any(zero.length.x | zero.length.y)) {
        zero <- zero.length.x | zero.length.y
        if (length(zero) == 1) {
          text(dat[zero, c("x", "y")], labels = ">", srt = dat[zero, "angle"],
            col = color)
          arrows(dat[!zero, "x0"], dat[!zero, "y0"],
                 dat[!zero, "x"],  dat[!zero, "y"],
                 lwd = 2, length = 0.065, code = 2, col = color)
        } else {
          invisible(lapply(which(zero), function(i) {
            text(dat[i, c("x", "y")], labels = ">", srt = dat[i, "angle"],
              col = color, cex = 1.5)
          }))

          arrows(dat[!zero, "x0"], dat[!zero, "y0"],
                 dat[!zero, "x"],  dat[!zero, "y"],
                 lwd = 2, length = 0.065, code = 2, col = color)
        }

      } else {
        arrows(dat$x0, dat$y0, dat$x, dat$y, lwd = 2, length = 0.065, code = 2,
          col = color)
      }
    }))

  } else if (type == "points") {
    coords <- parallel::mclapply(names(endpt.paths), function(nm) {
      lapply(edge.data[[nm]], postCoordinates, unit, interval, walking.speed)
    }, mc.cores = cores)

    coords <- stats::setNames(coords, names(endpt.paths))

    invisible(lapply(coords, function(z) {
      dat <- unique(do.call(rbind, z))
      points(dat[, c("x", "y")], pch = 22, bg = "white", cex = 2/3)
    }))
  }
}
