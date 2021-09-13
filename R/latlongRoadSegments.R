#' Reshape 'roads' data frame into 'road.segments' data frame.
#'
#' Used to integrate pumps and cases into road network when computing walking neighborhoods.
#' @param path Character. e.g., "~/Documents/Data/"
#' @return An R data frame.
#' @note This function documents the code that generates \code{\link[cholera]{road.segments}}.
#' @export

latlongRoadSegments <- function(path) {
  dat <- latitudeLongitudeRoads(path)
  out <- lapply(unique(dat$street), function(i) {
    st <- dat[dat$street == i, ]
    names(st)[names(st) %in% c("long", "lat")] <- c("long1", "lat1")
    seg.end <- st[-1, c("long1", "lat1")]
    names(seg.end) <- c("long2", "lat2")
    st <- cbind(st[-nrow(st), c("street", "id", "name")],
                st[-nrow(st), c("long1", "lat1")],
                seg.end)
    st$id <- paste0(st$street, "-", seq_len(nrow(st)))
    st
  })
  do.call(rbind, out)
}
