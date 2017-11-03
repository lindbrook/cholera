#' Compute distances within pump neighborhoods.
#'
#' Compute walking or Eucldean distances between the 578 observed cases and their nearest pump.
#' @param metric Character. Type of distance: "walking" or "Euclidean".
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. Meaningful only when "weighted" is TRUE.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. With Numeric, you specify the number logical cores (rounds with as.integer()). On Windows, only "multi.core = FALSE" is available.
#' @note Documents "neighborhood.distances".
#' @export

neighborhoodDistances <- function(metric = "walking", unit = NULL,
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

  if (metric == "walking") {
    nearest <- parallel::mclapply(cholera::fatalities.unstacked$case,
      cholera::walkingDistance, mc.cores = cores)
  } else if (metric == "euclidean") {
    nearest <- parallel::mclapply(cholera::fatalities.unstacked$case,
      cholera::euclideanDistance, mc.cores = cores)
  }

  do.call(rbind, nearest)
}
