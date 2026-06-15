#' Add observed case(s) to plot.
#'
#' Add case(s), as "anchor", "fatality" or "orthogonal" as points or IDs, to a plot.
#' @param case Numeric or Character. Vector of case ID(s). "anchor" plots anchor cases; "fatality" plots all cases; "orthogonal" plot projected addresses.
#' @param latlong Logical.
#' @param case.set Character. Type of case: "observed" or "expected".
#' @param token Character. Type of token to plot: "point", "id" or "both".
#' @param text.size Numeric. Size of case ID text.
#' @param pch Numeric. pch.
#' @param cex Numeric. cex.
#' @param point.lwd Numeric. Point lwd.
#' @param col Character. Color.
#' @param pos Numeric. Text position.
#' @note case.set, token, text.size, pch, cex, point.lwd and pos relevant only when case is numeric.
#' @export
#' @examples
#' \dontrun{
#' snowMap(add.cases = FALSE)
#' addCase(1)
#'
#' snowMap(add.cases = FALSE)
#' addCase(100)
#' }

addCase <- function(case = 1, latlong = FALSE, case.set = "observed",
  token = "both", text.size = 0.5, pch = 1, cex = 1, point.lwd = 2,
  col = "black", pos = 1) {

  if (token %in% c("id", "point", "both") == FALSE) {
    stop('token must be "id", "point" or "both".', call. = FALSE)
  }

  if (!pos %in% 1:4) stop("pos must be 1, 2, 3, or 4.", call. = FALSE)

  if (latlong) {
    vars <- c("lon", "lat")
    vars.proj <- vars # !
    proj.data <- cholera::latlong.ortho.anchor
    regular.data <- cholera::latlong.regular.cases
  } else {
    vars <- c("x", "y")
    vars.proj <- paste0(vars, ".proj")
    proj.data <- cholera::ortho.proj
    regular.data <- cholera::regular.cases
  }

  # TRUE group value(s) "anchor", "fatality", "orthogonal"
  # FALSE e.g., "1" or "100"
  character_caseID <- is.na(suppressWarnings(as.integer(case)))
  if (!character_caseID | is.numeric(case)) case <- as.integer(case)

  if (case.set == "observed") {
    dat <- cholera::fatalities
    if (is.integer(case)) {
      if (case %in% unique(cholera::fatalities$case) == FALSE) {
        stop("Observed case must be a whole number between 1 and 578.",
          call. = FALSE)
      }  
    }
  } else if (case.set == "expected") {
    # case <- case - 2000L
    dat <- regular.data
    sim.case <- cholera::sim.ortho.proj$case
    min.sim <- format(min(sim.case), big.mark = ",")
    max.sim <- format(max(sim.case), big.mark = ",")
    if (is.numeric(case)) {
      if (case %in% sim.case == FALSE) {
        stop("Expected (simulated) case must be a whole number between ",
          min.sim, " and ", max.sim, ".", call. = FALSE)
      }
    }
  } else stop('case.set must be "observed" or "expected".', call. = FALSE)

  if (is.character(case) & character_caseID) {

  if (case.set == "expected" & 
      case %in% c("observed", "fatality", "orthogonal")) {
      
      txt1 <- 'case %in% c("anchor", "fatality" or "orthogonal") '
      txt2 <- 'only for case.set == "observed".'
      stop(txt1, txt2, call. = FALSE)

    } else if (case == "anchor") {
      points(cholera::fatalities.anchor[, vars], pch = 16, cex = 0.5,
        col = col)
    
    } else if (case == "fatality") {
      points(cholera::fatalities[, vars], pch = 16, cex = 0.5,
        col = col)
    
    } else if (case == "orthogonal") {
      if (latlong) {
        sel <- proj.data$case %in% cholera::fatalities.anchor$anchor
        points(proj.data[sel, vars.proj], pch = 16, cex = 0.5, col = col)
      } else {
        points(proj.data[, vars.proj], pch = 16, cex = 0.5, col = col)
      }

    } else if (!case %in% c("observed", "fatality", "orthogonal")) {
      txt1 <- 'Non-numeric character case should be "anchor",'
      txt2 <- '"fatality" or "orthogonal". For case.set == "observed".'
      stop(txt1, txt2, call. = FALSE)
    }

  } else if (is.integer(case) | !character_caseID) {
    case.id <- ifelse(case.set == "expected", case - 2000L, case)
    
    if (token == "point") {
      points(dat[case.id, vars], pch = pch, cex = cex, lwd = point.lwd,
        col = col)
    } else if (token == "id") {
      text(dat[case.id, vars], cex = text.size, col = col, labels = case)
    } else if (token == "both") {
      points(dat[case.id, vars], pch = pch, cex = cex, lwd = point.lwd,
        col = col)
      text(dat[case.id, vars], cex = text.size, col = col, labels = case,
        pos = pos)
    }
  }
}
