#' Add observed cases by walking neighborhood.
#'
#' Add cases to a plot as "address" or "fatalities" and as points or IDs.
#' @param pump.subset Numeric. Vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} uses all pumps in \code{pump.select}.
#' @param pump.select Numeric. Numeric vector of pump IDs that define which pump neighborhoods to consider (i.e., specify the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param type Character. Type of case: "address" (base of stack), "fatalities" (entire stack), or "expected" for simulated data.
#' @param token Character. Type of token to plot: "point" or "id".
#' @param text.size Numeric. Size of case ID text.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param color Character. Use a single color for all paths. \code{NULL} uses neighborhood colors defined by \code{snowColors().}
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @param ... Additional plotting parameters.
#' @export
#' @examples
#' \dontrun{
#'
#' snowMap(add.cases = FALSE)
#' addNeighborhoodCases(pump.subset = c(6, 10))
#'
#' snowMap(add.cases = FALSE)
#' addNeighborhoodCases(pump.select = c(6, 10))
#' }

addNeighborhoodCases <- function(pump.subset = NULL, pump.select = NULL,
  type = "address", token = "point", text.size = 0.5, vestry = FALSE,
  weighted = TRUE, color = NULL, multi.core = FALSE, ...) {

  if (type %in% c("address", "fatalities", "expected") == FALSE) {
    stop('type must be "address", "fatalities" or "expected".')
  }

  if (token %in% c("id", "point") == FALSE) {
    stop('token must be "id" or "point".')
  }

  cores <- multiCore(multi.core)

  arguments <- list(pump.select = pump.select,
                    vestry = vestry,
                    weighted = weighted,
                    case.set = "observed",
                    multi.core = cores)

  nearest.path <- do.call("nearestPump", c(arguments, output = "path"))

  if (vestry) {
    nearest.pump <- vapply(nearest.path, function(paths) {
      sel <- cholera::ortho.proj.pump.vestry$node %in% paths[length(paths)]
      cholera::ortho.proj.pump.vestry[sel, "pump.id"]
    }, numeric(1L))
  } else {
    nearest.pump <- vapply(nearest.path, function(paths) {
      sel <- cholera::ortho.proj.pump$node %in% paths[length(paths)]
      cholera::ortho.proj.pump[sel, "pump.id"]
    }, numeric(1L))
  }

  nearest.pump <- data.frame(case = cholera::fatalities.address$anchor.case,
                             pump = nearest.pump)

  snow.colors <- cholera::snowColors(vestry)

  if (!is.null(color)) {
    snow.colors <- stats::setNames(rep(color, length(snow.colors)),
      names(snow.colors))
  }

  selected.pumps <- unique(nearest.pump$pump)

  if (is.null(pump.subset) == FALSE) {
    if (all(pump.subset > 0)) {
      if (all(pump.subset %in% selected.pumps) == FALSE) {
        stop("pump.subset must be a subset of selected.pumps.")
      }
    } else if (all(pump.subset < 0)) {
      if (all(abs(pump.subset) %in% selected.pumps) == FALSE) {
        stop("|pump.subset| must be a subset of selected.pumps.")
      }
    } else {
      stop("Use all positive or all negative numbers for pump.subset.")
    }
  }

  if (type == "address") {
    if (is.null(pump.subset)) {
      invisible(lapply(selected.pumps, function(x) {
        addr <- nearest.pump[nearest.pump$pump == x, "case"]
        sel <- cholera::fatalities.address$anchor.case %in% addr

        if (token == "point") {
          points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
            cex = 0.75, col = snow.colors[paste0("p", x)])
        } else if (token == "id") {
          text(cholera::fatalities.address[sel, c("x", "y")],
            cex = text.size, col = snow.colors[paste0("p", x)],
            labels = cholera::fatalities.address[sel, "anchor.case"])
        }
      }))
    } else {
      if (all(pump.subset > 0)) {
        select <- selected.pumps[selected.pumps %in% pump.subset]
      } else if (all(pump.subset < 0)) {
        select <- selected.pumps[selected.pumps %in% abs(pump.subset) == FALSE]
      } else {
        stop("Use all positive or all negative numbers for pump.subset.")
      }

      if (token == "point") {
        invisible(lapply(select, function(x) {
          addr <- nearest.pump[nearest.pump$pump == x, "case"]
          sel <- cholera::fatalities.address$anchor.case %in% addr
          points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
            cex = 0.75, col = snow.colors[paste0("p", x)])
        }))
      } else if (token == "id") {
        invisible(lapply(select, function(x) {
          addr <- nearest.pump[nearest.pump$pump == x, "case"]
          sel <- cholera::fatalities.address$anchor.case %in% addr
          text(cholera::fatalities.address[sel, c("x", "y")],
            cex = text.size, col = snow.colors[paste0("p", x)],
            labels = cholera::fatalities.address[sel, "anchor.case"])
        }))
      }
    }

  } else if (type == "fatalities") {
    if (is.null(pump.subset)) {
      invisible(lapply(selected.pumps, function(x) {
        addr <- nearest.pump[nearest.pump$pump == x, "case"]
        fatal <- cholera::anchor.case[cholera::anchor.case$anchor.case %in%
          addr, "case"]
        sel <- cholera::fatalities$case %in% fatal
        points(cholera::fatalities[sel, c("x", "y")], pch = 20,
          cex = 0.75, col = snow.colors[paste0("p", x)])
      }))
    } else {
      if (all(pump.subset > 0)) {
        select <- selected.pumps[selected.pumps %in% pump.subset]
      } else if (all(pump.subset < 0)) {
        select <- selected.pumps[selected.pumps %in% abs(pump.subset) == FALSE]
      } else {
        stop("Use all positive or all negative numbers for pump.subset.")
      }

      invisible(lapply(select, function(x) {
        addr <- nearest.pump[nearest.pump$pump == x, "case"]
        fatal <- cholera::anchor.case[cholera::anchor.case$anchor.case %in%
          addr, "case"]
        sel <- cholera::fatalities$case %in% fatal
        points(cholera::fatalities[sel, c("x", "y")], pch = 20,
          cex = 0.75, col = snow.colors[paste0("p", x)])
      }))
    }

  } else if (type == "expected") {
    x <- neighborhoodWalking(pump.select = pump.select, vestry = vestry)
    OE <- observedExpected(x)
    wholes <- OE$expected.wholes
    splits <- OE$exp.splits
    splits.pump <- OE$exp.splits.pump
    splits.segs <- OE$exp.splits.segs

    sim.proj <- cholera::sim.ortho.proj
    sim.proj.segs <- unique(sim.proj$road.segment)

    if (OE$obs.split.test > 0 | OE$unobs.split.test > 0) {
      split.outcome <- parallel::mclapply(seq_along(splits.segs), function(i) {
        id <- sim.proj$road.segment == splits.segs[i] &
              is.na(sim.proj$road.segment) == FALSE

        sim.data <- sim.proj[id, ]
        split.data <- splits[[i]]

        sel <- vapply(seq_len(nrow(sim.data)), function(j) {
          obs <- sim.data[j, c("x.proj", "y.proj")]
          distance <- vapply(seq_len(nrow(split.data)), function(k) {
            stats::dist(matrix(c(obs, split.data[k, ]), 2, 2, byrow = TRUE))
          }, numeric(1L))

          test1 <- signif(sum(distance[1:2])) ==
            signif(c(stats::dist(split.data[c(1, 2), ])))
          test2 <- signif(sum(distance[3:4])) ==
            signif(c(stats::dist(split.data[c(3, 4), ])))

          ifelse(any(c(test1, test2)), which(c(test1, test2)), NA)
        }, integer(1L))

        data.frame(case = sim.data$case, pump = splits.pump[[i]][sel])
      }, mc.cores = x$cores)

      split.outcome <- do.call(rbind, split.outcome)
      split.outcome <- split.outcome[!is.na(split.outcome$pump), ]
      split.cases <- lapply(sort(unique(split.outcome$pump)), function(p) {
        split.outcome[split.outcome$pump == p, "case"]
      })

      names(split.cases) <- sort(unique(split.outcome$pump))
    }

    ap <- areaPointsData(sim.proj.segs, wholes, snow.colors, sim.proj,
      split.cases)
    points(cholera::regular.cases[ap$sim.proj.wholes$case, ],
      col = grDevices::adjustcolor(ap$sim.proj.wholes$color, alpha.f = 1/2),
      pch = 15, cex = 1.25)
    points(cholera::regular.cases[ap$sim.proj.splits$case, ],
      col = grDevices::adjustcolor(ap$sim.proj.splits$color, alpha.f = 1/2),
      pch = 15, cex = 1.25)
  }
}
