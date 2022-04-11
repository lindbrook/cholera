#' Add observed case(s) to plot.
#'
#' Add case(s), as "address" or "fatalities" as points or IDs, to a plot.
#' @param case Numeric or Character. Vector of case ID(s). "all" plots all cases. "anchor" plots anchor cases.
#' @param type Character. Type of case: "observed" or "expected".
#' @param token Character. Type of token to plot: "point", "id" or "both".
#' @param text.size Numeric. Size of case ID text.
#' @param pch Numeric. pch.
#' @param cex Numeric. cex.
#' @param point.lwd Numeric. Point lwd.
#' @param col Character. Color.
#' @param pos Numeric. Text position.
#' @note type, token, text.size, pch, cex, point.lwd and pos relevant only when case is numeric.
#' @export
#' @examples
#' snowMap(add.cases = FALSE)
#' addCase(1)
#'
#' snowMap(add.cases = FALSE)
#' addCase(100)

addCase <- function(case = 1, type = "observed", token = "both",
  text.size = 0.5, pch = 1, cex = 1, point.lwd = 2, col = "black", pos = 1) {

  if (is.character(case)) {
    if (!case %in% c("all", "anchor")) {
      stop('If non-numeric, case must be "all" or "anchor".', call. = FALSE)
    } else {
      if (case == "all") {
        points(cholera::fatalities[, c("x", "y")], pch = 16, cex = 0.5,
          col = col)
      } else if (case == "anchor") {
        points(cholera::fatalities.address[, c("x", "y")], pch = 16, cex = 0.5,
          col = col)
      }
    }
  } else if (is.numeric(case)) {
    if (type == "observed") {
      dat <- cholera::fatalities
    } else if (type == "expected") {
      dat <- cholera::regular.cases
    } else stop('type must be "observed" or "expected".', call. = FALSE)

    if (any(!case %in% seq_len(nrow(dat)))) {
      stop('With type = ', type, ', case must be between 1 and ', nrow(dat),
        ".", call. = FALSE)
    } else {
      if (token %in% c("id", "point", "both") == FALSE) {
        stop('token must be "id", "point" or "both".', call. = FALSE)
      }

      if (!pos %in% 1:4) stop("pos must be 1, 2, 3, or 4.", call. = FALSE)

      if (token == "point") {
        points(dat[case, c("x", "y")], pch = pch, cex = cex, lwd = point.lwd,
          col = col)
      } else if (token == "id") {
        text(dat[case, c("x", "y")], cex = text.size, col = col, labels = case)
      } else if (token == "both") {
        points(dat[case, c("x", "y")], pch = pch, cex = cex, lwd = point.lwd,
          col = col)
        text(dat[case, c("x", "y")], cex = text.size, col = col, labels = case,
          pos = pos)
      }
    }
  }
}
