#' Set or compute the number of cores for parallelized functions.
#'
#' @param x Logical or Numeric. \code{TRUE} returns the number of logical cores. \code{FALSE} uses one, single core. You can also specify the number of cores.
#' @noRd

multiCore <- function(x = TRUE) {
  if (is.logical(x) | is.numeric(x)) {
    if (is.logical(x)) {
      if (x == TRUE) {
        cores <- parallel::detectCores()
      } else {
        cores <- 1L
      }
    } else if (is.numeric(x)) {
      obs.cores <- parallel::detectCores()
      if (x > obs.cores) {
        msg <- 'For your system, the number of specified cores must be <= '
        stop(paste0(msg, obs.cores, "."))
      } else if (x <= 0) {
        stop("You must specify at least one core.")
      } else {
        cores <- as.integer(x)
      }
    }
    cores
  } else stop("'x' must either be logical or numeric.")
}
