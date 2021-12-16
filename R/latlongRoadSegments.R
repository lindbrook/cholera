#' Compute longitude and latitude version of 'road.segments' data frame.
#'
#' Used to integrate pumps and cases into road network when computing walking neighborhoods.
#' @param path Character. e.g., "~/Documents/Data/"
#' @return An R data frame.
#' @export

latlongRoadSegments <- function(path) {
  vars <- c("lon", "lat")
  dat <- latlongRoads(path)
  out <- lapply(unique(dat$street), function(i) { # minor benefit from mclapply
    st <- dat[dat$street == i, ]
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
  out$distance <- segmentDistance(out)
  out
}

segmentDistance <- function(dat) {
  vars <- c("lon", "lat")
  vapply(seq_len(nrow(dat)), function(i) {
    p1 <- dat[i, paste0(vars, 1)]
    p2 <- dat[i, paste0(vars, 2)]
    names(p1) <- vars
    names(p2) <- vars
    sp::spDistsN1(as.matrix(p1), as.matrix(p2), longlat = TRUE) * 1000L
  }, numeric(1L))
}
