#' Compute walking or Eucldean distances within pump neighborhoods.
#'
#' @param pump.select Numeric. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param statistic Character. "address" computes the number of addresses in each selected pump neighborhood. "fatality" computes the number of fatalities in pump neighborhoods.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of nodes.
#' @param metric Character. Type of distance: "walking" or "Euclidean".
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. Meaningful only when "weighted" is TRUE.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. With Numeric, you specify the number logical cores (rounds with as.integer()). On Windows, only "multi.core = FALSE" is available.
#' @note Documents "neighborhood.distances".
#' @export

neighborhoodDistances <- function(pump.select = NULL, statistic = "address",
  vestry = FALSE, metric = "walking", weighted = TRUE, unit = NULL,
  multi.core = FALSE) {

  if (is.logical(multi.core)) {
    if (multi.core == TRUE) {
      cores <- parallel::detectCores()
    } else {
      if (is.numeric(multi.core)) {
        if (is.integer(multi.core)) {
          cores <- multi.core
        } else {
          cores <- as.integer(multi.core)
        }
      } else {
        cores <- 1L
      }
    }
  } else if (is.numeric(multi.core)) {
    if (is.integer(multi.core)) {
      cores <- multi.core
    } else {
      cores <- as.integer(multi.core)
    }
  }

  if (statistic == "address") {
    case.select <- cholera::fatalities.address$anchor.case
  } else if (statistic == "fatality") {
    case.select <- cholera::fatalities$case
  }

  if (is.null(pump.select) == FALSE) {
    if (vestry) {
      pumps <- cholera::pumps.vestry[pump.select, "id"]
    } else {
      pumps <- cholera::pumps[pump.select, "id"]
    }
  }

  if (metric %in% c("walking", "euclidean") == FALSE) {
    stop('"metric" must either be "walking" or "euclidean".')
  } else {
    if (metric == "walking") {
      fn <- cholera::walkingDistance
    } else if (metric == "euclidean") {
      fn <- cholera::euclideanDistance
    }
  }

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  if (is.null(pump.select)) {
    if (vestry) {
      if (metric == "walking") {
        if (weighted) {
          nearest <- parallel::mclapply(case.select, fn, vestry = TRUE,
            mc.cores = cores)
        } else {
          nearest <- parallel::mclapply(case.select, fn, vestry = TRUE,
            weighted = FALSE, mc.cores = cores)
        }
      } else if (metric == "euclidean") {
        nearest <- parallel::mclapply(case.select, fn, vestry = TRUE,
          mc.cores = cores)
      }
    } else {
      if (metric == "walking") {
        if (weighted) {
          nearest <- parallel::mclapply(case.select, fn, mc.cores = cores)
        } else {
          nearest <- parallel::mclapply(case.select, fn, weighted = FALSE,
            mc.cores = cores)
        }
      } else if (metric == "euclidean") {
        nearest <- parallel::mclapply(case.select, fn, mc.cores = cores)
      }
    }
  } else {
    if (vestry) {
      if (metric == "walking") {
        if (weighted) {
          nearest <- parallel::mclapply(case.select, fn, destination = pumps,
            vestry = TRUE, mc.cores = cores)
        } else {
          nearest <- parallel::mclapply(case.select, fn, destination = pumps,
            vestry = TRUE, weighted = FALSE, mc.cores = cores)
        }
      } else if (metric == "euclidean") {
        nearest <- parallel::mclapply(case.select, fn, destination = pumps,
          vestry = TRUE, mc.cores = cores)
      }
    } else {
      if (metric == "walking") {
        if (weighted) {
          nearest <- parallel::mclapply(case.select, fn, destination = pumps,
            mc.cores = cores)
        } else {
          nearest <- parallel::mclapply(case.select, fn, destination = pumps,
            weighted = FALSE, mc.cores = cores)
        }
      } else if (metric == "euclidean") {
        nearest <- parallel::mclapply(case.select, fn, destination = pumps,
          mc.cores = cores)
      }
    }
  }

  out <- do.call(rbind, lapply(nearest, function(x) x$summary))

  if (!is.null(unit)) {
    if (unit == "yard") {
      out$distance <- out$distance * 177 / 3
    } else if (unit == "meter") {
      out$distance <- out$distance * 54
    }
  }

  out
}
