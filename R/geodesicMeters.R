#' Decompose geodesic distances into horizontal and vertical components.
#'
#' Compute geodesic distance from origin to pump and decompose result into
#` horizontal (East-West) and vertical (North-South) components.
#' @param dat Object. Data.
#' @param case.address Logical. Use fatalities.address$anchor
#' @noRd

geodesicMeters <- function(dat = cholera::pumps, case.address = FALSE) {
  origin <- data.frame(lon = min(cholera::roads$lon),
                       lat = min(cholera::roads$lat))
  if (case.address) {
    dat <- cholera::fatalities.address
    names(dat)[names(dat) == "anchor"] <- "id"
  }
  do.call(rbind, lapply(dat$id, function(x) {
    tmp <- dat[dat$id == x, c("lon", "lat")]
    x.proj <- c(tmp$lon, origin$lat)
    y.proj <- c(origin$lon, tmp$lat)
    m.lon <- geosphere::distGeo(y.proj, tmp)
    m.lat <- geosphere::distGeo(x.proj, tmp)
    data.frame(id = x, x = m.lon, y = m.lat)
  }))
}
