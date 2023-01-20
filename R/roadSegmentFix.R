#' Bar orientation classification errors.
#'
#' @note Bars lie parallel to the road where that fatality is observed. This can lead to (classification) errors when using orthogonal projection to assign a street address: the closest road is not always the right road. This R list manually assigns those problematic bars to their "correct" road segment.
#' @export

roadSegmentFix <- function() {
  list("216-1" = c(290, 61, 174, 547, 523, 521, 138, 59, 340, 508),
       "290-1" = c(409, 131, 18, 575, 566, 518, 297),
       "259-1" = c(440, 145),
       "231-1" = c(329, 248, 408, 471),
       "340-2" = c(172, 62, 111),
       "128-1" = 302,
       "141-1" = 163,
       "169-1" = 516,
       "188-1" = 372,
       "222-1" = 520,
       "237-1" = 308,
       "330-1" = 453,
       "207-1" = 277,
       "196-1" = 346,
       "186-1" = 278,
       "261-1" = 69,
       "270-1" = 267,
       "159-1" = 165,
       "193-1" = c(463, 423),
       "216-1" = c(122, 91),
       "203-1" = 287,
       "259-2" = c(303, 513, 405, 175),
       "297-1" = 117,
       "224-1" = c(355, 253),
       "234-1" = c(254, 367, 492, 406),
       "193-1" = c(180, 452, 551),
       "178-1" = 85,
       "231-1" = 341,
       "160-3" = 558,
       "269-1" = 462,
       "326-2" = 483)
}