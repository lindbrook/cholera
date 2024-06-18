#' Reshape 'roads' data frame into 'road.segments' data frame.
#'
#' Used to integrate pumps and cases into road network when computing walking neighborhoods.
#' @return An R data frame.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @note This function documents the code that generates \code{\link[cholera]{road.segments}}.
#' @export

roadSegments <- function(latlong = FALSE) {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")
  dat <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  out <- lapply(unique(dat$street), function(s) {
    st <- dat[dat$street == s, ]
    names(st)[names(st) %in% vars] <- paste0(vars, 1)
    seg.end <- st[-1, paste0(vars, 1)]
    names(seg.end) <- paste0(vars, 2)
    st <- cbind(st[-nrow(st), c("street", "id", "name")],
                st[-nrow(st), paste0(vars, 1)],
                seg.end)
    st$id <- paste0(st$street, "-", seq_len(nrow(st)))
    st
  })
  do.call(rbind, out)
}
