#' Add observed cases.
#'
#' Plots address or unstacked fatalities.
#' @param pump.subset Numeric. Vector of pumps to select (subset) from neighborhoods defined by "pump.select". Negative selection possible. NULL selects all pumps in "pump.select".
#' @param pump.select Numeric. Numeric vector of pumps to define pump neighborhoods (i.e. the "population"). Negative selection possible. NULL selects all pumps.
#' @param type Character. Type of cases: "address" or "fatalities".
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path weighted by road length. FALSE computes shortest path in terms of the number of nodes.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @param ... Additional plotting parameters.
#' @export
#' @examples
#' snowMap()
#' addCases(pump.susbet = c(6, 10))
#'
#' snowMap()
#' addCases(pump.select = c(6, 10))

addCases <- function(pump.subset = NULL, pump.select = NULL, type = "address",
  vestry = FALSE, weighted = TRUE, multi.core = FALSE, ...) {

  if (type %in% c("address", "fatalities") == FALSE) {
    stop('"type" must be "address" or "fatalities".')
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
  selected.pumps <- unique(nearest.pump$pump)

  if (type == "address") {
    if (is.null(pump.subset)) {
      invisible(lapply(selected.pumps, function(x) {
        addr <- nearest.pump[nearest.pump$pump == x, "case"]
        sel <- cholera::fatalities.address$anchor.case %in% addr
        points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
          cex = 0.75, col = snow.colors[paste0("p", x)])
      }))
    } else {
      if (all(pump.subset > 0)) {
        select <- selected.pumps[selected.pumps %in% pump.subset]
        invisible(lapply(select, function(x) {
          addr <- nearest.pump[nearest.pump$pump == x, "case"]
          sel <- cholera::fatalities.address$anchor.case %in% addr
          points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
            cex = 0.75, col = snow.colors[paste0("p", x)])
        }))
      } else if (all(pump.subset < 0)) {
        select <- selected.pumps[selected.pumps %in% abs(pump.subset) == FALSE]
        invisible(lapply(select, function(x) {
          addr <- nearest.pump[nearest.pump$pump == x, "case"]
          sel <- cholera::fatalities.address$anchor.case %in% addr
          points(cholera::fatalities.address[sel, c("x", "y")], pch = 20,
            cex = 0.75, col = snow.colors[paste0("p", x)])
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
        invisible(lapply(select, function(x) {
          addr <- nearest.pump[nearest.pump$pump == x, "case"]
          fatal <- cholera::anchor.case[cholera::anchor.case$anchor.case %in%
            addr, "case"]
          sel <- cholera::fatalities$case %in% fatal
          points(cholera::fatalities[sel, c("x", "y")], pch = 20,
            cex = 0.75, col = snow.colors[paste0("p", x)])
        }))
      } else if (all(pump.subset < 0)) {
        select <- selected.pumps[selected.pumps %in% abs(pump.subset) == FALSE]
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
}
