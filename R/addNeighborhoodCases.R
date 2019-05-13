#' Add observed cases by neighborhood.
#'
#' Add cases to a plot as "address" or "fatalities" and as points or IDs.
#' @param pump.subset Numeric. Vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} uses all pumps in \code{pump.select}.
#' @param pump.select Numeric. Numeric vector of pump IDs that define which pump neighborhoods to consider (i.e., specify the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param metric Character. Type of neighborhood: "euclidean" or "walking".
#' @param type Character. Type of case: "stack.base" (base of stack), or "stack" (entire stack). For observed = TRUE.
#' @param token Character. Type of token to plot: "point" or "id".
#' @param text.size Numeric. Size of case ID text.
#' @param pch Numeric.
#' @param point.size Numeric.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest walking path weighted by road length. \code{FALSE} computes shortest walking path in terms of the number of nodes.
#' @param color Character. Use a single color for all paths. \code{NULL} uses neighborhood colors defined by \code{snowColors().}
#' @param case.location Character. For \code{metric = "euclidean"}: "address" uses \code{ortho.proj}; "nominal" uses \code{fatalities}.
#' @param observed Logical. TRUE is observed; FALSE is expected or simulated.
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @export
#' @examples
#' \donttest{
#'
#' snowMap(add.cases = FALSE)
#' addNeighborhoodCases(pump.subset = c(6, 10))
#'
#' snowMap(add.cases = FALSE)
#' addNeighborhoodCases(pump.select = c(6, 10))
#' }

addNeighborhoodCases <- function(pump.subset = NULL, pump.select = NULL,
  metric = "walking", type = "stack.base", token = "point", text.size = 0.5,
  pch = 16, point.size = 0.5, vestry = FALSE, weighted = TRUE, color = NULL,
  case.location = "nominal", observed = TRUE, alpha.level = 0.5,
  multi.core = FALSE) {

  if (metric %in% c("euclidean", "walking") == FALSE) {
    stop('metric must be "euclidean" or "walking".')
  }

  if (observed) {
    if (type %in% c("stack.base", "stack") == FALSE) {
      stop('type must be "stack.base", "stack" or "simulated".')
    }
  }

  if (token %in% c("id", "point") == FALSE) {
    stop('token must be "id" or "point".')
  }

  if (case.location %in% c("address", "nominal") == FALSE) {
    stop('case.location must be "address" or "nominal".')
  }

  cores <- multiCore(multi.core)

  arguments <- list(pump.select = pump.select,
                    vestry = vestry,
                    case.location = case.location,
                    case.set = ifelse(observed, "observed", "expected"),
                    multi.core = cores)

  if (metric == "euclidean") {
    eucl.data <- do.call("neighborhoodEuclidean", arguments)
    nearest.pump <- data.frame(case = eucl.data$anchor,
                               pump = eucl.data$nearest.pump)
  } else if (metric == "walking") {
    arguments$case.location <- NULL
    walk.data <- do.call("nearestPump", arguments)
    nearest.pump <- walk.data[, c("case", "pump")]
  }

  snow.colors <- snowColors(vestry)

  if (!is.null(color)) {
    snow.colors <- stats::setNames(rep(color, length(snow.colors)),
      names(snow.colors))
  }

  selected.pumps <- unique(nearest.pump$pump)

  if (is.null(pump.subset) == FALSE) {
    if (all(pump.subset > 0)) {
      if (all(pump.subset %in% selected.pumps)) {
        selected.pumps <- selected.pumps[selected.pumps %in% pump.subset]
      } else {
        stop("pump.subset must be a subset of selected.pumps.")
      }
    } else if (all(pump.subset < 0)) {
      if (all(abs(pump.subset) %in% selected.pumps)) {
        selected.pumps <- selected.pumps[selected.pumps %in%
          abs(pump.subset) == FALSE]
      } else {
        stop("|pump.subset| must be a subset of selected.pumps.")
      }
    } else {
      stop("Use all positive or all negative numbers for pump.subset.")
    }
  }

  if (observed) {
    addr.data <- cholera::ortho.proj
    nom.data <- cholera::fatalities
  } else {
    addr.data <- cholera::sim.ortho.proj
    nom.data <- cholera::regular.cases
    nom.data$case <- seq_len(nrow(nom.data))
  }

  if (case.location == "address") {
    case.data <- addr.data
    vars <- c("x.proj", "y.proj")
  } else if (case.location == "nominal") {
    case.data <- nom.data
    vars <- c("x", "y")
  } else stop("Invalid case.location!")

  if (observed) {
    invisible(lapply(selected.pumps, function(x) {
      addr <- nearest.pump[nearest.pump$pump == x, "case"]
      case.sel <- cholera::anchor.case$anchor %in% addr

      if (type == "stack.base") {
        id <- unique(cholera::anchor.case[case.sel, "anchor"])
      } else if (type == "stack") {
        id <- cholera::anchor.case[case.sel, "case"]
      } else stop("Invalid type")

      sel <- case.data$case %in% id

      if (token == "point") {
        points(case.data[sel, vars], pch = pch, cex = point.size,
          col = snow.colors[paste0("p", x)])
      } else if (token == "id") {
        text(case.data[sel, vars], cex = text.size,
          col = snow.colors[paste0("p", x)], labels = case.data[sel, "case"])
      } else stop("Invalid token!")
    }))

  } else {
    invisible(lapply(selected.pumps, function(x) {
      addr <- nearest.pump[nearest.pump$pump == x, "case"]
      sel <- case.data$case %in% addr
      neighborhood.color <- snow.colors[paste0("p", x)]

      if (token == "point") {
        points(case.data[sel, vars], pch = pch, cex = point.size,
          col = grDevices::adjustcolor(neighborhood.color,
            alpha.f = alpha.level))
      } else if (token == "id") {
        text(case.data[sel, vars], cex = text.size,
          col = neighborhood.color, labels = case.data[sel, "case"])
      } else stop("Invalid token!")
    }))
  }
}
