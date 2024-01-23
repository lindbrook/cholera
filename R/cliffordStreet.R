#' Estimate native coordinates for missing Clifford Street segment.
#'
#' Segment between Old Burlington and Saville Row.
#' @noRd

cliffordStreet <- function() {
  # Clifford Street Eastern segment
  clifford <- cholera::roads[cholera::roads$street == 407, ]
  saville <- cholera::roads[cholera::roads$name == "Saville Row", ]

  saville.ols <- stats::lm(y ~ x, data = saville[, c("x", "y")])
  clifford.ols <- stats::lm(y ~ x, data = clifford[, c("x", "y")])

  xs <- stats::coef(clifford.ols)["x"] - 
        stats::coef(saville.ols)["x"]

  cs <- stats::coef(saville.ols)["(Intercept)"] - 
        stats::coef(clifford.ols)["(Intercept)"]

  x.intersection <- cs / xs
  y.intersection <- x.intersection * stats::coef(saville.ols)["x"] +
    stats::coef(saville.ols)["(Intercept)"]

  data.frame(x = x.intersection, y = y.intersection, row.names = NULL)
}

#' Append "missing" Clifford Street segment to cholera::roads
#' @noRd

appendCliffordStreet <- function() {
  tmp2 <- cholera::roads[cholera::roads$id == 942, ]
  row.names(tmp2) <- NULL
  tmp2[, c("id", "street")] <- c(1243, 529)
  tmp2[, c("lon", "lat")] <- NULL

  tmp1 <- tmp2
  tmp1$id <- tmp1$id - 1
  tmp1[, c("x", "y")] <- cliffordStreet()

  rds <- cholera::roads[, !names(cholera::roads) %in% c("lon", "lat")]
  rbind(rds, tmp1, tmp2)
}

# roads <- cholera:::appendCliffordStreet()
# usethis::use_data(roads, overwrite = TRUE)
# road.segments <- roadSegments()
# usethis::use_data(road.segments, overwrite = TRUE)



