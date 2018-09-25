#' 3D Profile.
#'
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @export

profile3D <- function(pump.select = NULL, vestry = FALSE, multi.core = FALSE) {
  if (vestry) {
    pump.id <- cholera::pumps.vestry$id
  } else {
    pump.id <- cholera::pumps$id
  }

  cores <- multiCore(multi.core)

  if (is.null(pump.select) == FALSE) {
    if (is.numeric(pump.select) == FALSE) {
      stop("pump.select must be numeric.")
    } else if (any(abs(pump.select) %in% pump.id) == FALSE) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ',
        length(pump.id),  ".")
    }

    if (all(pump.select > 0)) {
      pump.id <- pump.id[pump.select]
    } else if (all(pump.select < 0)) {
      pump.id <- pump.id[pump.id %in% abs(pump.select) == FALSE]
    } else {
      stop("Use all positive or all negative numbers for pump.select.")
    }
  }

  nearest.pump <- cholera::nearestPump(pump.id, multi.core = cores)

  x <- cholera::fatalities.address$x
  y <- cholera::fatalities.address$y
  z <- cholera::fatalities.address$case.count

  snow.colors <- cholera::snowColors()[paste0("p", nearest.pump$pump)]

  profile <- list(x = x,
                  y = y,
                  z = z,
                  pump.id = pump.id,
                  snow.colors = snow.colors)
                  
  class(profile) <- "profile3D"
  profile
}

#' Plot method for profile3D().
#'
#' @param x An object of class "profile3D" created by \code{profile3D()}.
#' @param pump.subset Numeric. Vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} selects all pumps in \code{pump.select}.
#' @param drop.neg.selection Logical. Drop negative selection.
#' @param ... Additional plotting parameters.
#' @export

plot.profile3D <- function(x, pump.subset = NULL, drop.neg.selection = FALSE,
  ...) {

  if (is.null(pump.subset)) {
    threejs::scatterplot3js(x$x, x$y, x$z, cex = 0.5,
      color = grDevices::adjustcolor(unname(x$snow.colors), alpha.f = 2/3))
  } else {
    if (!all(abs(pump.subset) %in% x$pump.id)) {
      stop("pump.subset must be a subset of pump.select.")
    }

    if (all(pump.subset < 0)) {
      neg.selection <- x$pump.id[x$pump.id %in% abs(pump.subset) == FALSE]
      sel <- names(x$snow.colors) %in% paste0("p", neg.selection)
    } else if (all(pump.subset > 0)) {
      sel <- names(x$snow.colors) %in% paste0("p", pump.subset) == FALSE
    }

    if (drop.neg.selection) {
      threejs::scatterplot3js(x$x[sel], x$y[sel], x$z[sel], cex = 0.5,
        color = grDevices::adjustcolor(unname(x$snow.colors[sel]),
        alpha.f = 2/3))
    } else {
      x$snow.colors[!sel] <- "lightgray"
      threejs::scatterplot3js(x$x, x$y, x$z, cex = 0.5,
        color = grDevices::adjustcolor(unname(x$snow.colors), alpha.f = 2/3))
    }
  }
}

#' Print method for profile3D().
#'
#' @param x An object of class "profile3D" created by \code{profile3D()}.
#' @param ... Additional parameters.
#' @export

print.profile3D <- function(x, ...) {
  neighborhood.ct <- table(names(x$snow.colors))
  id <- order(as.numeric(do.call(rbind,
    strsplit(names(neighborhood.ct), "p"))[, 2]))
  print(neighborhood.ct[id])
}
