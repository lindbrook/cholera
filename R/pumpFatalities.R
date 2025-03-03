#' Compute fatalities by pump.
#'
#' @param pump.select Numeric. Pump candidates to consider. Default is \code{NULL}: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param metric Character. "euclidean" or "walking".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export
#' @examples
#' \dontrun{
#' pumpFatalities(pump.select = -7)
#' pumpFatalities(latlong = TRUE)
#' pumpFatalities(metric = "euclidean", vestry = TRUE)
#' }

pumpFatalities <- function(pump.select = NULL, metric = "walking",
  vestry = FALSE, latlong = FALSE, multi.core = FALSE) {

  if (!metric %in% c("euclidean", "walking")) {
    stop('metric must be "euclidean" or "walking".', call. = FALSE)
  }

  args <- list(pump.select = pump.select, metric = metric, vestry = vestry)

  if (latlong) {
    args <- c(args, list(latlong = TRUE))
  }

  nr.pump <- do.call("nearestPump", args)

  if (metric == "euclidean") {
    sel <- names(nr.pump) %in% c("origin", "destination")
    names(nr.pump)[sel] <- c("case", "pump")
  }

  tbl <- table(cholera::anchor.case$anchor)
  anchor.case.ct <- data.frame(anchor = as.integer(names(tbl)), ct = c(tbl))

  p.fatality <- merge(nr.pump[, c("case", "pump")], anchor.case.ct,
    by.x = "case", by.y = "anchor")

  fatality.ct <- lapply(p.fatality$case, function(c) {
    z <- p.fatality[p.fatality$case == c, ]
    rep(z$pump, z$ct)
  })

  table(unlist(fatality.ct))
}
