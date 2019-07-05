#' Set or compute the number of cores for parallelized functions.
#'
#' For parallel::mclapply() and parallel::parLapply().
#' @param x Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number of logical cores to use.
#' @note Returns physical cores for Windows, for parallel::parLapply(), and logical cores for macOS and unix, for parallel::mclapply().
#' @noRd

multiCore <- function(x = TRUE) {
  logical.cores <- ifelse(.Platform$OS.type == "windows", FALSE, TRUE)
  if (is.logical(x) | is.numeric(x)) {
    if (is.logical(x)) {
      if (x == TRUE) {
        cores <- parallel::detectCores(logical = logical.cores)
      } else {
        cores <- 1L
      }
    } else if (is.numeric(x)) {
      obs.cores <- parallel::detectCores(logical = logical.cores)
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
