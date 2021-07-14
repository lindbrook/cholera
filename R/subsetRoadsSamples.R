#' Sample for road segment endpoints.
#'
#' For endpoints with 1 or 3 intersections.
#' @export

subsetRoadsSamples <- function() {
  framework <- cholera::roads[cholera::roads$name != "Map Frame", ]
  dat <- framework
  dat$point.id <- paste0(dat$x, "-", dat$y)
  intersections <- table(dat$point.id)
  # > table(intersections)
  # intersections
  #   1   2   3   4
  # 276  10 221  44
  one <- intersections[intersections == 1]
  three <- intersections[intersections == 3]
  list(one = sample(names(one)), three = sample(names(three)))
}

# usethis::use_data(rd.sample, overwrite = TRUE)
