#' Add observed case(s) to plot.
#'
#' Add case(s), as "anchor", "fatality" or "orthogonal" as points or IDs, to a plot.
#' @param case Numeric or Character. Vector of case ID(s). "anchor" plots anchor cases; "fatality" plots all cases; "orthogonal" plot projected addresses.
#' @param latlong Logical.
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

addCase <- function(case = 1, latlong = FALSE, type = "observed",
  token = "both", text.size = 0.5, pch = 1, cex = 1, point.lwd = 2,
  col = "black", pos = 1) {

  if (is.character(case) & type == "expected") {
    msg1 <- 'Note: case %in% c("anchor", "fatality" or "orthogonal") only works'
    msg2 <- 'with type = "observed".'
    message(paste(msg1, msg2))
  }

  if (latlong) {
    vars <- c("lon", "lat")
    vars.proj <- vars # !
    proj.data <- cholera::latlong.ortho.addr
    regular.data <- cholera::latlong.regular.cases
  } else {
    vars <- c("x", "y")
    vars.proj <- paste0(vars, ".proj")
    proj.data <- cholera::ortho.proj
    regular.data <- cholera::regular.cases
  }

  if (is.character(case)) {
    if (!case %in% c("anchor", "fatality", "orthogonal")) {
      stop('If non-numeric, case must be "anchor", "fatality" or "orthogonal".',
        call. = FALSE)
    } else {
      if (case == "anchor") {
        points(cholera::fatalities.address[, vars], pch = 16, cex = 0.5,
          col = col)
      } else if (case == "fatality") {
        points(cholera::fatalities[, vars], pch = 16, cex = 0.5,
          col = col)
      } else if (case %in% c("orthogonal")) {
        if (latlong) {
          sel <- proj.data$case %in% cholera::fatalities.address$anchor
          points(proj.data[sel, vars.proj], pch = 16, cex = 0.5, col = col)
        } else {
          points(proj.data[, vars.proj], pch = 16, cex = 0.5, col = col)
        }
      }
    }
  } else if (is.numeric(case)) {
    if (type == "observed") {
      dat <- cholera::fatalities
    } else if (type == "expected") {
      dat <- regular.data
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
        points(dat[case, vars], pch = pch, cex = cex, lwd = point.lwd,
          col = col)
      } else if (token == "id") {
        text(dat[case, vars], cex = text.size, col = col, labels = case)
      } else if (token == "both") {
        points(dat[case, vars], pch = pch, cex = cex, lwd = point.lwd,
          col = col)
        text(dat[case, vars], cex = text.size, col = col, labels = case,
          pos = pos)
      }
    }
  }
}
