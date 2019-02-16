#' Create a set of colors for pump neighborhoods.
#'
#' Uses \code{RColorBrewer::brewer.pal()}.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps in the Vestry Report. \code{FALSE} uses the original 13.
#' @return A character vector of colors.
#' @note Built with 'RColorBrewer' package.
#' @export

snowColors <- function(vestry = FALSE) {
  colors.pair <- RColorBrewer::brewer.pal(10, "Paired")
  colors.dark <- RColorBrewer::brewer.pal(8, "Dark2")
  out <- c("dodgerblue", "gray", colors.dark[1:4], colors.pair[2],
    colors.dark[5:8], "red", colors.pair[1])
  if (vestry) {
    out <- c(out, "darkorange")
    p.count <- nrow(cholera::pumps.vestry)
  } else {
    p.count <- nrow(cholera::pumps)
  }
  names(out) <- paste0("p", seq_len(p.count))
  out
}
