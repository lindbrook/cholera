#' Compute fatalities by pump.
#'
#' @param pump.select Numeric. Pump candidates to consider. Default is \code{NULL}: all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}. Negative selection allowed.
#' @param metric Character. "eucldidean" or "walking".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @export
#' @examples
#' \dontrun{
#' pumpFatalities(pump.select = -7)
#' pumpFatalities(metric = "euclidean")
#' pumpFatalities(metric = "euclidean", vestry = TRUE)
#' }

pumpFatalities <- function(pump.select = NULL, metric = "walking",
  vestry = FALSE, latlong = FALSE) {

  if (latlong) {
    nr.pump <- latlongNearestPump(pump.select = pump.select, metric = metric,
        vestry = vestry)
    if (metric == "walking") nr.pump <- nr.pump$distance  
  } else {
    nr.pump <- nearestPump(pump.select = pump.select, metric = metric,
      vestry = vestry)$distance    
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
