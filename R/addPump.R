#' Add water pump by numerical ID.
#'
#' Add water pump to existing plot.
#' @param id Numeric or Integer. With "vestry = TRUE", a whole number between 1 and 14. With "vestry = FALSE", a whole number between 1 and 13. See cholera::pumps.vestry and cholera::pumps for IDs and details about specific pumps.
#' @param vestry Logical. TRUE for the 14 pumps from Vestry Report. FALSE for the original 13 pumps.
#' @param col Character. Color of point.
#' @param pch Numeric. Shape of point character.
#' @param label Logical. TRUE adds text label.
#' @param pos Numeric. Position of label.
#' @param ... Additional plotting parameters.
#' @export

addPump <- function(id, vestry = FALSE, col = "red", pch = 17, label = TRUE,
  pos = 1, ...) {

  if (is.numeric(id) == FALSE) {
    stop('"id" must be numeric.')
  }

  if (!vestry & id %in% cholera::pumps$id == FALSE) {
    stop('For original pumps, "id" must be a whole number between 1 and 13.')
  }

  if (vestry & id %in% cholera::pumps.vestry$id == FALSE) {
    stop('For vestry pumps, "id" must lie be a whole number 1 and 14.')
  }

  if (vestry) {
    dat <- cholera::pumps.vestry
  } else {
    dat <- cholera::pumps
  }

  points(dat[dat$id == id, c("x", "y")], pch = pch, col = col)

  if (label) {
    text(dat[dat$id == id, c("x", "y")], pos = pos, labels = paste0("p", id))
  }
}
