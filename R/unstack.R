#' Unstack "stacks" in Snow's cholera map.
#'
#' Unstacks fatalities data by 1) assigning the coordinates of the base case to all cases in a stack and 2) setting the base case as an "address" and making the number of fatalities an attribute.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @seealso \code{vignette("unstacking.fatalities")}
#' @return An R list.
#' @note This function documents the code that generates \code{\link{anchor.case}},  \code{\link{fatalities.address}}, \code{\link{fatalities.unstacked}} and \code{\link{ortho.proj}}.
#' @export

unstackFatalities <- function(multi.core = FALSE, dev.mode = FALSE) {
  cores <- multiCore(multi.core)
  fixed.fatalities <- fixFatalities()
  ortho.proj <- orthogonalProjectionFatalities(fixed.fatalities, cores)

  ## Single and Multiple ##

  road.incidence <- c(table(ortho.proj$road.segment))
  road.incidence <- data.frame(id = names(road.incidence),
    count = road.incidence)

  single.obs <- road.incidence[road.incidence$count == 1, ]
  single.address <- lapply(single.obs$id, function(id) {
    sel <- ortho.proj$road.segment == id
    data.frame(id = id, case = ortho.proj[sel, "case"])
  })

  cutpoint <- 0.05
  multiple.obs <- road.incidence[road.incidence$count > 1, ]
  multiple.address <- multipleAddress(multiple.obs, ortho.proj,
    fixed.fatalities, cutpoint, cores, dev.mode)

  multiple.unstacked <- multipleUnstack(multiple.address, cores, dev.mode)

  single.unstacked <- do.call(rbind, single.address)
  single.unstacked[ c("group", "anchor",  "case.count")] <- 1
  single.unstacked$anchor <- single.unstacked$case
  single.unstacked$multiple.obs.seg <- "No"

  unstacked <- rbind(multiple.unstacked, single.unstacked)
  unstacked <- merge(unstacked, fixed.fatalities, by.x = "anchor",
    by.y = "case")

  fatalities.unstacked <- unstacked[, c("case", "x", "y")]
  idx <- order(fatalities.unstacked$case)
  fatalities.unstacked <- fatalities.unstacked[idx, ]
  row.names(fatalities.unstacked) <- NULL

  vars <- c("case", "x", "y", "case.count")
  fatalities.anchor <- unstacked[unstacked$anchor == unstacked$case, vars]
  names(fatalities.anchor)[1] <- "anchor"
  row.names(fatalities.anchor) <- NULL

  anchor.case <- unstacked[, c("anchor", "case")]

  list(anchor.case = anchor.case,
       fatalities = fixed.fatalities,
       fatalities.unstacked = fatalities.unstacked,
       fatalities.anchor = fatalities.anchor,
       ortho.proj = ortho.proj)
}
