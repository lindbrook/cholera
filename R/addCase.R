#' Add observed case(s).
#'
#' Add case(s), as "address" or "fatalities" as points or IDs, to a plot.
#' @param case Numeric. Vector of case ID(s).
#' @param type Character. Type of case: "observed" or "expected".
#' @param token Character. Type of token to plot: "point", "id" or "both".
#' @param text.size Numeric. Size of case ID text.
#' @param col Character. Color.
#' @param pos Numeric. Text position.
#' @export
#' @examples
#' snowMap(add.cases = FALSE)
#' addCase(1)
#'
#' snowMap(add.cases = FALSE)
#' addCase(100)

addCase <- function(case = 1, type = "observed", token = "both",
  text.size = 0.5, col = "red", pos = 1) {

  if (type %in% c("observed", "expected") == FALSE) {
    stop('type must be "observed" or "expected".')
  }

  if (token %in% c("id", "point", "both") == FALSE) {
    stop('token must be "id", "point" or "both".')
  }

  if (type == "observed") {
    dat <- cholera::fatalities
  } else if (type == "expected") {
    dat <- cholera::regular.cases
  }

  if (pos %in% 1:4 == FALSE) stop("pos must be 1, 2, 3, or 4.")
  
  if (any(case %in% seq_len(nrow(dat)) == FALSE)) {
    stop('With type = ', type, ', case must be between 1 and ', nrow(dat), ".")
  } else {
    if (token == "point") {
      points(dat[case, c("x", "y")], lwd = 2, col = col)
    } else if (token == "id") {
      text(dat[case, c("x", "y")], cex = text.size, col = col, labels = case)
    } else if (token == "both") {
      points(dat[case, c("x", "y")], lwd = 2, col = col)
      text(dat[case, c("x", "y")], cex = text.size, col = col, labels = case,
        pos = pos)
    }
  }
}
