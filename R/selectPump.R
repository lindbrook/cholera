#' Select desired pump neighborhoods.
#'
#' Allows negative selection.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param metric Character. "eucldidean" or "walking".
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.

selectPump <- function(pump.select = NULL, metric = "walking", vestry = FALSE) {
  if (is.character(pump.select)) {
    stop('pump.select must be numeric', call. = FALSE)
  }

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  if (is.numeric(pump.select)) {
    if (any(abs(pump.select) %in% pump.data$id) == FALSE) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ', 
        nrow(pump.data), ".", call. = FALSE)
    }
  }

  if (metric == "walking") {
    if (length(pump.select) == 1) {
      if (pump.select == 2) {
        msg1 <- "You can't just select the pump on Adam and Eve Court (#2).\n"
        msg2 <- " It's an isolate, unreachable for observed fatalities."
        stop(msg1, msg2, call. = FALSE)
      }
    }
  }

  if (is.null(pump.select)) {
    pump.id <- pump.data$id
  } else {
    if (all(pump.select > 0)) {
      sel <- pump.data$id %in% pump.select
    } else if (all(pump.select < 0)) {
      sel <- pump.data$id %in% abs(pump.select) == FALSE
    } else {
      stop("Use all positive or all negative numbers for pump.select.",
        call. = FALSE)
    }
    pump.id <- pump.data$id[sel]
  }

  pump.id
}
