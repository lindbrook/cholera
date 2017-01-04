polygonColors <- function(resid.vector, upper.limit = 67, alpha = FALSE) {
  # colors for Voronoi cells based on Pearson residuals.
  vec <- upper.limit:-upper.limit
  color.map <- data.frame(resid = vec, id = rev(order(vec)))
  color.id <- vapply(round(resid.vector), function(x) {
    color.map[color.map$resid == x, "id"]
  }, integer(1L))

  if (alpha) {
    col <- scales::col_numeric("RdBu", domain = NULL)(seq_along(vec))[color.id]
    scales::alpha(col, 0.5)
  } else {
    color <- scales::col_numeric("RdBu", domain = NULL)(seq_along(vec))[color.id]
    scales::alpha(color, 0.5)
  }
}
