#' Rectangle to filter map data (prototype).
#'
#' @noRd

rectangleFilter <- function() {
  x <- c(-0.145, -0.13)
  y <- c(51.508, 51.518)
  out <- stats::setNames(expand.grid(x, y), c("x", "y"))
  out[c(1, 2, 4, 3), ]
}

# rectangle.filter <- rectangleFilter()
# usethis::use_data(rectangle.filter, overwrite = TRUE)
