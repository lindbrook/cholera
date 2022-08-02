#' Add selected pump(s) to plot.
#'
#' @param pump.select Numeric or Integer. Vector of water pump numerical ID(s). With \code{vestry = TRUE}, whole number(s) between 1 and 14. With \code{vestry = FALSE}, whole number(s) between 1 and 13. See \code{pumps.vestry} and \code{pumps} for IDs and details about specific pumps. \code{NULL} plots all pumps. Negative selection allowed.
#' @param vestry Logical. \code{TRUE} for the 14 pumps from Vestry Report. \code{FALSE} for the original 13 pumps.
#' @param col Character. Color of pump points.
#' @param pch Numeric. Shape of point character.
#' @param label Logical. TRUE adds text label.
#' @param pos Numeric. Position of label.
#' @param cex Numeric. point cex.
#' @param latlong Logical. Use c("lon". "lat") or c("x", "y").
#' @export

addPump <- function(pump.select = NULL, vestry = FALSE, col = NULL, pch = 24,
  label = TRUE, pos = 1, cex = 1, latlong = FALSE) {

  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }

  if (latlong) {
    vars <- c("lon", "lat")
  } else {
    vars <- c("x", "y")
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  if (is.null(pump.select) == FALSE) {
    if (is.numeric(pump.select) == FALSE) {
      stop('pump.select must be numeric.', call. = FALSE)
    }

    if (any(abs(pump.select) %in% p.ID == FALSE)) {
      stop("With vestry = ", vestry, ", ", "1 >= |pump.select| <= ", p.count,
        ".", call. = FALSE)
    }

    if (all(pump.select > 0)) {
      sel <- p.data$id %in% pump.select
    } else if (all(pump.select < 0)) {
      sel <- p.data$id %in% abs(pump.select) == FALSE
    }

    if (is.null(col)) {
      sel.col <- snowColors(vestry)[paste0("p", p.ID[sel])]
      points(p.data[sel, vars], pch = pch, col = sel.col, cex = cex)

      if (label) {
        text(p.data[sel, vars], pos = pos, labels = paste0("p", p.ID[sel]))
      }
    } else {
      points(p.data[sel, vars], pch = pch, col = col, cex = cex)

      if (label) {
        text(p.data[sel, vars], pos = pos, labels = paste0("p", p.ID[sel]),
          col = col)
      }
    }

  } else {
    if (is.null(col)) {
      sel.col <- snowColors(vestry)[paste0("p", p.ID)]
      points(p.data[, vars], pch = pch, col = sel.col, cex = cex)
    } else {
      points(p.data[, vars], pch = pch, col = col, cex = cex)
    }

    if (label) {
      text(p.data[, vars], pos = pos, labels = paste0("p", p.ID), col = col)
    }
  }
}
