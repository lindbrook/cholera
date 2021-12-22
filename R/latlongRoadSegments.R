#' Compute longitude and latitude version of 'road.segments' data frame.
#'
#' Used to integrate pumps and cases into road network when computing walking neighborhoods.
#' @param path Character. e.g., "~/Documents/Data/"
#' @return An R data frame.
#' @export

latlongRoadSegments <- function(path) {
  vars <- c("lon", "lat")
  dat <- latlongRoads(path)
  out <- lapply(unique(dat$street), function(x) { # minor benefit from mclapply
    st <- dat[dat$street == x, ]
    names(st)[names(st) %in% vars] <- paste0(vars, 1)
    seg.end <- st[-1, paste0(vars, 1)]
    names(seg.end) <- paste0(vars, 2)
    st <- cbind(st[-nrow(st), c("street", "id", "name")],
                st[-nrow(st), paste0(vars, 1)],
                seg.end)
    st$id <- paste0(st$street, "-", seq_len(nrow(st)))
    st
  })
  out <- do.call(rbind, out)
  out$distance <- segmentDistance(out, latlong = TRUE)
  out
}
