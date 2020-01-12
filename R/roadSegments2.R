#' Reshape 'roads2' data frame into 'road.segments2' data frame.
#'
#' Used to integrate pumps and cases into road network when computing walking neighborhoods.
#' @return An R data frame.
#' @note This function documents the code that generates \code{\link[cholera]{road.segments2}}.
#' @export

roadSegments2 <- function() {
  dat <- cholera::roads2
  vars <- c("longitude", "latitude")
  out <- lapply(unique(dat$street), function(id) {
   st <- dat[dat$street == id, ]
   names(st)[names(st) %in% c("longitude", "latitude")] <- paste0(vars, 1)
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

# usethis::use_data(road.segments2)
# usethis::use_data(road.segments2, overwrite = TRUE)
