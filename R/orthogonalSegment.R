#' Find a case's closest segment (orthogonal distance).
#'
#' @param case Integer. Numerical case fatality ID.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @noRd

orthogonalSegment <- function(case = 145, multi.core = FALSE) {
  fixed.fatalities <- fixFatalities() # fixed HistData fatalities.
  cores <- multiCore(multi.core)
  out <- orthoProj(fixed.fatalities, case.id = case, cores, dev.mode = FALSE)
  data.frame(case = case, do.call(rbind, out))
}



