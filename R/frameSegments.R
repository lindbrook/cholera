#' Reshape "Map Frame" streets from 'roads' data frame into 'frame.segments' data frame.
#'
#' @return An R data frame.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @note This function documents the code that generates the frame.segments data frame..
#' @noRd

frameSegments <- function(latlong = FALSE) {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")
  dat <- cholera::roads[cholera::roads$street %in% cholera::border, ]
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

# frame.segments <- cholera:::frameSegments()
# geo <- cholera:::frameSegments(latlong = TRUE)
# vars <- c("id", "lon1", "lat1", "lon2", "lat2")
# frame.segments <- merge(frame.segments, geo[, vars], by = "id")
# vars <- c("street", "id", "name", "x1", "y1", "x2", "y2", "lon1", "lat1",
#   "lon2", "lat2")
# frame.segments <- frame.segments[order(frame.segments$street), vars]
# usethis::use_data(frame.segments)
# usethis::use_data(frame.segments, overwrite = TRUE)
