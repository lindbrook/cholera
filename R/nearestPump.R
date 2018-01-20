#' Compute shortest walking path distance from case to pump.
#'
#' Compute path distance from anchor case to nearest pump (or among selected pumps).
#' @param pump.select Numeric. Pump candidates to consider. Default is NULL: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of the number of nodes.
#' @param case.set Character. "observed", "expected", or "snow".
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. Meaningful only when "weighted" is TRUE. See \code{vignette("roads")} for information on unit distances.
#' @param cores  Numeric. The number logical cores to use. Default is 1, which is the only possible value for Windows.
#' @export
#' @return An R data frame.

nearestPump <- function(pump.select = NULL, vestry = FALSE, weighted = TRUE,
  case.set = "observed", unit = NULL, cores = 1L) {

  if (case.set %in% c("observed", "expected", "snow") == FALSE) {
    stop('"case.set" must be "observed", "expected" or "snow".')
  }

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  dat <- neighborhoodData(vestry)
  path.data <- pathData(dat, weighted, case.set, cores)
  distances <- path.data$distances
  nodes.pump <- dat$nodes.pump

  if (is.null(pump.select)) {
    dat <- lapply(distances, function(x) {
      data.frame(pump = as.numeric(names(which.min(x))),
                 distance = x[which.min(x)])
    })

  } else {
    if (all(pump.select > 0)) {
      dat <- lapply(distances, function(x) {
        candidates <- x[names(x) %in% pump.select]
        dat <- candidates[which.min(candidates)]
        data.frame(pump = as.numeric(names(dat)),
                   distance = dat)
      })
    } else if (all(pump.select < 0)) {
      dat <- lapply(distances, function(x) {
        candidates <- x[names(x) %in% abs(pump.select) == FALSE]
        dat <- candidates[which.min(candidates)]
        data.frame(pump = as.numeric(names(dat)),
                   distance = dat)
      })
    }
  }

  if (case.set == "observed") {
    out <- data.frame(anchor = cholera::fatalities.address$anchor.case,
      do.call(rbind, dat), row.names = NULL)
  } else if (case.set == "expected"){
    out <- data.frame(anchor = seq_along(dat), do.call(rbind, dat),
      row.names = NULL)
  } else if (case.set == "snow") {
    snow <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
      cholera::snow.neighborhood, "anchor.case"])
    out <- data.frame(anchor = snow, do.call(rbind, dat), row.names = NULL)
  }

  out$pump.name <- NA

  if (vestry) {
    for (p in unique(out$pump)) {
      out[out$pump == p, "pump.name"] <-
        cholera::pumps.vestry[cholera::pumps.vestry$id == p, "street"]
    }
  } else {
    for (p in unique(out$pump)) {
      out[out$pump == p, "pump.name"] <-
        cholera::pumps[cholera::pumps$id == p, "street"]
    }
  }

  out <- out[, c("anchor", "pump", "pump.name", "distance")]

  if (!is.null(unit)) {
    if (unit == "meter") {
      out$distance <- cholera::unitMeter(out$distance, "meter")
    } else if (unit == "yard") {
      out$distance <- cholera::unitMeter(out$distance, "yard")
    }
  }

  out
}
