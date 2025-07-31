#' Add plague pit (Marshall Street).
#'
#' Draws a polygon that approximates the plague pit located around Marshall Street. From Vestry Report map.
#' @param latlong Logical. Use estimated longitude and latitude.
#' @param color Character. Color of polygon.
#' @param line.type Character. Polygon line type.
#' @return Adds a polygon to a graphics plot.
#' @note In progress.
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addPlaguePit()

addPlaguePit <- function(latlong = FALSE, color = "black",
  line.type = "solid") {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")
  polygon(cholera::plague.pit[, vars], border = color, lty = line.type)
}

#' Plague pit line segment data.
#' @noRd

plaguePitSegments <- function() {
  vars <- c("x", "y")
  newvars <- c(paste0(vars, 1), paste0(vars, 2))
  pit <- cholera::plague.pit
  seg.data <- do.call(rbind, lapply(seq_len(nrow(pit) - 1), function(i) {
    stats::setNames(cbind(pit[i, vars], pit[i + 1, vars]), newvars) 
  }))
  seg.data$id <- seq_len(nrow(seg.data))
  seg.data[, c("id", newvars)]
}

# usethis::use_data(plague.pit.segments)
# usethis::use_data(plague.pit.segments, overwrite = TRUE)
