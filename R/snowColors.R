#' Create a set of colors for pump neighborhoods.
#'
#' Uses \code{RColorBrewer::brewer.pal()}.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps in the Vestry Report. \code{FALSE} uses the original 13.
#' @param palette Character. "RColorBrewer" or "viridis".
#' @return A character vector of colors.
#' @note Built with 'RColorBrewer' package.
#' @export

snowColors <- function(vestry = FALSE, palette = "RColorBrewer") {
  if (palette %in% c("RColorBrewer", "viridis") == FALSE) {
    stop('palette must be "RColorBrewer" or "viridis".')
  }

  if (palette == "RColorBrewer") {
    colors.pair <- RColorBrewer::brewer.pal(10, "Paired")
    colors.dark <- RColorBrewer::brewer.pal(8, "Dark2")
    if (vestry) {
      out <- c("dodgerblue", "gray", colors.dark[1:4], colors.pair[2],
        colors.dark[5:8], "red", colors.pair[1], "darkorange")
      names(out) <- paste0("p", seq_len(nrow(cholera::pumps.vestry)))
    } else {
      out <- c("dodgerblue", "gray", colors.dark[1:4], colors.pair[2],
        colors.dark[5:8], "red", colors.pair[1])
      names(out) <- paste0("p", seq_len(nrow(cholera::pumps)))
    }
  } else if (palette == "viridis") {
    # scales::show_col(viridis::viridis_pal()(14))
    viridis.colors <- rev(viridis::viridis_pal()(14))
    nearest.pump <- nearestPump()
    order.nm <- names(sort(table(nearest.pump$pump), decreasing = TRUE))
    leftovers <- setdiff(1:14, unique(nearest.pump$pump))
    names(viridis.colors) <-  paste0("p", c(order.nm, leftovers))
    viridis.colors <- viridis.colors[order(c(as.numeric(order.nm), leftovers))]

    if (vestry) {
      out <- viridis.colors
    } else {
      out <- viridis.colors[1:13]
    }
  }

  out
}
