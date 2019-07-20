#' Compute shortest distances or paths to selected pumps.
#'
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param weighted Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected" or "snow". "snow" captures John Snow's annotation of the Broad Street pump neighborhood printed in the Vestry report version of the map.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export

nearestPumpX <- function(pump.select = NULL, vestry = FALSE,
  weighted = TRUE, case.set = "observed", multi.core = FALSE,
  dev.mode = FALSE) {

  x <- neighborhoodWalking(pump.select = pump.select, vestry = vestry,
    weighted = weighted, case.set = case.set, multi.core = multi.core,
    dev.mode = dev.mode)

  n.data <- neighborhoodPathData(x)
  OE <- observedExpected(x, n.data)

  wholes <- OE$expected.wholes
  splits <- OE$exp.splits
  splits.pump <- OE$exp.splits.pump
  splits.segs <- OE$exp.splits.segs

  sim.proj <- cholera::sim.ortho.proj
  sim.proj.segs <- unique(sim.proj$road.segment)

  if (OE$obs.split.test > 0 | OE$unobs.split.test > 0) {
    split.outcome <- splitOutcomes(x, splits.segs, sim.proj, splits,
      splits.pump)
    split.outcome <- do.call(rbind, split.outcome)
    split.outcome <- split.outcome[!is.na(split.outcome$pump), ]
    split.cases <- lapply(sort(unique(split.outcome$pump)), function(p) {
      split.outcome[split.outcome$pump == p, "case"]
    })

    names(split.cases) <- sort(unique(split.outcome$pump))
  }

  ap <- areaPointsData(sim.proj.segs, wholes, x$snow.colors, sim.proj,
    split.cases)

  ap <- do.call(rbind, ap)

  ap <- ap[, c("case", "pump")]
  row.names(ap) <- NULL
  ap
}
