#' Reshape 'roads' data frame into 'road.segments' data frame.
#'
#' Used to integrate pumps and cases into road network when computing walking neighborhoods.
#' @return An R data frame.
#' @note This function documents the code that generates \code{\link[cholera]{road.segments}}.
#' @export

roadSegments <- function() {
  dat <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  out <- lapply(unique(dat$street), function(i) {
   st <- dat[dat$street == i, ]
   names(st)[names(st) %in% c("x", "y")] <- c("x1", "y1")
   seg.end <- st[-1, c("x1", "y1")]
   names(seg.end) <- c("x2", "y2")
   st <- cbind(st[-nrow(st), c("street", "id", "name")],
               st[-nrow(st), c("x1", "y1")],
               seg.end)
   st$id <- paste0(st$street, "-", seq_len(nrow(st)))
   st
  })
  do.call(rbind, out)
}
