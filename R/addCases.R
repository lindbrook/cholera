#' Add observed cases by walking neighborhood.
#'
#' Add cases, as "address" or "fatalities" as points or IDs, to a plot.
#' @param pump.subset Numeric. Vector of numeric pump IDs to select (subset) from the neighborhoods defined by "pump.select". Negative selection possible. NULL selects all pumps in "pump.select".
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. NULL selects all pumps.
#' @param type Character. Type of case: "address" (base of stack) or "fatalities" (entire stack).
#' @param token Character. Type of token to plot: "point" or "id".
#' @param text.size Numeric. Size of case ID text.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path weighted by road length. FALSE computes shortest path in terms of the number of nodes.
#' @param color Character. Use a single color for all paths. NULL uses neighborhood colors defined by snowColors().
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @param ... Additional plotting parameters.
#' @export
#' @examples
#' \dontrun{
#' snowMap()
#' addCases(pump.susbet = c(6, 10))
#'
#' snowMap()
#' addCases(pump.select = c(6, 10))
#' }

addCases <- function(pump.subset = NULL, pump.select = NULL, type = "address",
  token = "id", text.size = 0.5, vestry = FALSE, weighted = TRUE, color = NULL,
  multi.core = FALSE, ...) {

  if (type %in% c("address", "fatalities") == FALSE) {
    stop('"type" must be "address" or "fatalities".')
  }

  if (token %in% c("id", "point") == FALSE) {
    stop('"id" must be "id" or "point".')
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
        stop('"pump.subset" must be a subset of "selected.pumps".')
      }
    } else if (all(pump.subset < 0)) {
      if (all(abs(pump.subset) %in% selected.pumps) == FALSE) {
        stop('"|pump.subset|" must be a subset of "selected.pumps".')
      }
    } else {
      stop('Use all positive or all negative "pump.subset"!')
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
        stop('Use all positive or all negative "pump.subset"!')
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
        stop('Use all positive or all negative "pump.subset"!')
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
  }
}
