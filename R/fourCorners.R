fourCorners <- function() {
  nw <- cholera::roads[cholera::roads$id == 69, c("x", "y")]
  ne <- cholera::roads[cholera::roads$id == 28, c("x", "y")]
  se <- cholera::roads[cholera::roads$id == 1137, c("x", "y")]
  sw <- cholera::roads[cholera::roads$id == 1211, c("x", "y")]
  list(northwest = nw, northeast = ne, southeast = se, southwest = sw)
}
