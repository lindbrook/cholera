#' 3D Profile.
#'
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param pump.subset Numeric. Vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} selects all pumps in \code{pump.select}.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param drop.neg.subset Logical. Drop negative subset selection
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export
#' @examples
#' \dontrun{
#' profile3D(pump.select = 6:7)
#' profile3D(pump.subset = -7)
#' profile3D(pump.subset = -7, drop.neg.subset = TRUE)
#' }

profile3D <- function(pump.select = NULL, pump.subset = NULL, vestry = FALSE,
  drop.neg.subset = FALSE, multi.core = TRUE) {

  if (vestry) {
    pump.id <- cholera::pumps.vestry$id
  } else {
    pump.id <- cholera::pumps$id
  }

  cores <- multiCore(multi.core)

  if (is.null(pump.select) == FALSE) {
    if (is.numeric(pump.select) == FALSE) {
      stop("pump.select must be numeric.", call. = FALSE)
    } else if (any(abs(pump.select) %in% pump.id) == FALSE) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ',
        length(pump.id),  ".", call. = FALSE)
    }

    if (all(pump.select > 0)) {
      pump.id <- pump.id[pump.select]
    } else if (all(pump.select < 0)) {
      pump.id <- pump.id[pump.id %in% abs(pump.select) == FALSE]
    } else {
      stop("Use all positive or all negative numbers for pump.select.",
        call. = FALSE)
    }
  }

  nearest.pump <- nearestPump(pump.id, multi.core = cores)$distance

  x <- cholera::fatalities.address$x
  y <- cholera::fatalities.address$y
  z <- cholera::fatalities.address$case.count

  snow.colors <- snowColors()[paste0("p", nearest.pump$pump)]

  if (is.null(pump.subset)) {
    threejs::scatterplot3js(x, y, z, cex = 0.5,
      color = grDevices::adjustcolor(unname(snow.colors), alpha.f = 2/3))
  } else {
    if (!all(abs(pump.subset) %in% pump.id)) {
      stop("pump.subset must be a subset of pump.select.", call. = FALSE)
    }

    if (all(pump.subset < 0)) {
      neg.selection <- pump.id[pump.id %in% abs(pump.subset) == FALSE]
      sel <- names(snow.colors) %in% paste0("p", neg.selection)
    } else if (all(pump.subset > 0)) {
      sel <- names(snow.colors) %in% paste0("p", pump.subset) == FALSE
    }

    if (drop.neg.subset) {
      threejs::scatterplot3js(x[sel], y[sel], z[sel], cex = 0.5,
        color = grDevices::adjustcolor(unname(snow.colors[sel]),
        alpha.f = 2/3))
    } else {
      snow.colors[!sel] <- "lightgray"
      threejs::scatterplot3js(x, y, z, cex = 0.5,
        color = grDevices::adjustcolor(unname(snow.colors), alpha.f = 2/3))
    }
  }
}
