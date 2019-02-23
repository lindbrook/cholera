#' Fix errors in Dodson and Tobler's digitization of Snow's map.
#'
#' Fixes two apparent coding errors using three misplaced cases.
#' @seealso \code{vignette("duplicate.missing.cases")}
#' @return An R data frame.
#' @export

fixFatalities <- function() {
  data.fix <- data.frame(x = c(12.56974, 12.53617, 12.33145),
                         y = c(11.51226, 11.58107, 14.80316))
  fatalities <- HistData::Snow.deaths
  fatalities[c(91, 93, 209), c("x", "y")] <- data.fix
  fatalities
}
