#' Add observed cases by neighborhood.
#'
#' Add cases to a plot as "nominal" or "fatalities" and as points or IDs.
#' @param pump.subset Numeric. Vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} uses all pumps in \code{pump.select}.
#' @param pump.select Numeric. Numeric vector of pump IDs that define which pump neighborhoods to consider (i.e., specify the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param metric Character. Type of neighborhood: "euclidean" or "walking".
#' @param case.set Character. "observed" or "expected".
#' @param location Character. "nominal", "anchor" or "orthogonal".
#' @param token Character. Type of token to plot: "point" or "id".
#' @param text.size Numeric. Size of case ID text.
#' @param pch Numeric.
#' @param point.size Numeric.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest walking path weighted by road length. \code{FALSE} computes shortest walking path in terms of the number of nodes.
#' @param color Character. Use a single color for all paths. \code{NULL} uses neighborhood colors defined by \code{snowColors().}
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export
#' @examples
#' \dontrun{
#' snowMap(add.cases = FALSE)
#' addNeighborhoodCases(pump.subset = c(6, 10))
#'
#' snowMap(add.cases = FALSE)
#' addNeighborhoodCases(pump.select = c(6, 10))
#' }

addNeighborhoodCases <- function(pump.subset = NULL, pump.select = NULL,
  metric = "walking", case.set = "observed", location = "nominal",
  token = "point", text.size = 0.5, pch = 16, point.size = 0.5, vestry = FALSE,
  weighted = TRUE, color = NULL, alpha.level = 0.5, multi.core = TRUE) {

  if (metric %in% c("euclidean", "walking") == FALSE) {
    stop('metric must be "euclidean" or "walking".', call. = FALSE)
  }

  if (case.set %in% c("observed", "expected") == FALSE) {
    stop('case.set must be "observed" or "expected".', call. = FALSE)
  }

   if (location %in% c("nominal", "anchor", "orthogonal") == FALSE) {
    stop('location must be "nominal", "anchor", or "orthogonal".',
      call. = FALSE)
  }

  if (token %in% c("id", "point") == FALSE) {
    stop('token must be "id" or "point".', call. = FALSE)
  }

  cores <- multiCore(multi.core)

  arguments <- list(pump.select = pump.select,
                    vestry = vestry,
                    location = location,
                    multi.core = cores)

  if (metric == "euclidean") {
    arguments <- c(arguments, case.set = case.set)
    eucl.data <- do.call("neighborhoodEuclidean", arguments)
    nearest.pump <- data.frame(case = eucl.data$case.num,
                               pump = eucl.data$nearest.pump)
  } else if (metric == "walking") {
    arguments$location <- NULL
    walk.data <- do.call("nearestPump", arguments)
    nearest.pump <- walk.data$distance[, c("case", "pump")]
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

  if (location %in% c("nominal", "anchor")) {
    vars <- c("x", "y")
  } else if (location == "orthogonal") {
    vars <- c("x.proj", "y.proj")
  }

  if (location == "nominal") {
    case.data <- cholera::fatalities
  } else if (location == "anchor") {
    sel <- cholera::fatalities.address$anchor
    case.data <-  cholera::fatalities[cholera::fatalities$case %in% sel, ]
  } else if (location == "orthogonal") {
    case.data <- cholera::ortho.proj
  } else stop("Invalid 'location'!")

  invisible(lapply(selected.pumps, function(x) {
    p.neighborhood <- nearest.pump[nearest.pump$pump == x, "case"]
    sel <- case.data$case %in% p.neighborhood

    if (token == "point") {
      points(case.data[sel, vars], pch = pch, cex = point.size,
        col = snow.colors[paste0("p", x)])
    } else if (token == "id") {
      text(case.data[sel, vars], cex = text.size,
        col = snow.colors[paste0("p", x)], labels = case.data[sel, "case"])
    } else stop("Invalid token!")
  }))
}
