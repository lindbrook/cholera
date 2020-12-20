#' Locate case by numerical ID.
#'
#' Highlight selected observed or simulated case and its home road segment.
#' @param case Numeric or Integer. Whole number between 1 and 578.
#' @param zoom Logical or Numeric.A numeric value >= 0 controls the degree of zoom. The default is 1.
#' @param observed Logical. \code{TRUE} for observed. \code{FALSE} for simulated.
#' @param add.title Logical. Include title.
#' @param highlight.segment Logical. Highlight case's segment.
#' @param data Logical. Output data.
#' @param add Logical. Add to existing plot or separate plot.
#' @param col Character. Point color.
#' @return A base R graphics plot.
#' @import graphics
#' @export
#' @examples
#' caseLocator(290)
#' caseLocator(290, zoom = TRUE)
#' caseLocator(290, observed = FALSE)

caseLocator <- function(case = 1, zoom = 1, observed = TRUE, add.title = TRUE,
  highlight.segment = TRUE, data = FALSE, add = FALSE, col = "red") {

  if (!is.numeric(case)) stop("case must be numeric.", call. = FALSE)

  if (observed) {
    if (case %in% unique(cholera::fatalities$case) == FALSE) {
      stop("Observed case must be a whole number between 1 and 578.",
        call. = FALSE)
    }
  } else {
    if (case %in% seq_len(nrow(cholera::regular.cases)) == FALSE) {
      stop("Simulated case must be a whole number between 1 and 19,993.",
        call. = FALSE)
    }
  }

  if (add == TRUE) {
    if (observed) {
      points(cholera::fatalities[cholera::fatalities$case == case,
        c("x", "y")], col = col, lwd = 2)
    } else {
      points(cholera::regular.cases[case, c("x", "y")],
        col = col, lwd = 2)
    }
  } else {
    if (observed) {
      case.seg <- cholera::ortho.proj[cholera::ortho.proj$case == case,
        "road.segment"]
      seg.data <- cholera::road.segments[cholera::road.segments$id ==
        case.seg, ]
    } else {
      case.seg <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case == case,
        "road.segment"]
      seg.data <- cholera::road.segments[cholera::road.segments$id ==
        case.seg, ]
    }

    if (data == FALSE) {
      if ((is.logical(zoom) & zoom == TRUE) | is.numeric(zoom)) {
        sel <- cholera::fatalities$case == case

        if (is.logical(zoom)) {
          padding <- 0.1

          if (observed) {
            x.rng <- c(cholera::fatalities[sel, "x"] - padding,
                       cholera::fatalities[sel, "x"] + padding)
            y.rng <- c(cholera::fatalities[sel, "y"] - padding,
                       cholera::fatalities[sel, "y"] + padding)
          } else {
            x.rng <- c(cholera::regular.cases[case, "x"] - padding,
                       cholera::regular.cases[case, "x"] + padding)
            y.rng <- c(cholera::regular.cases[case, "y"] - padding,
                       cholera::regular.cases[case, "y"] + padding)
          }
        } else if (is.numeric(zoom)) {
          if (zoom >= 0) {
            if (observed) {
              x.rng <- c(cholera::fatalities[sel, "x"] - zoom,
                         cholera::fatalities[sel, "x"] + zoom)
              y.rng <- c(cholera::fatalities[sel, "y"] - zoom,
                         cholera::fatalities[sel, "y"] + zoom)
            } else {
              x.rng <- c(cholera::regular.cases[case, "x"] - zoom,
                         cholera::regular.cases[case, "x"] + zoom)
              y.rng <- c(cholera::regular.cases[case, "y"] - zoom,
                         cholera::regular.cases[case, "y"] + zoom)
            }
          } else stop("If numeric, zoom must be >= 0.")
        } else stop("zoom must either be logical or numeric.")

      } else {
        x.rng <- range(cholera::roads$x)
        y.rng <- range(cholera::roads$y)
      }

      roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)

      plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
        pch = 15, cex = 0.5, col = "gray", asp = 1)
      invisible(lapply(roads.list, lines, col = "gray"))
      points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
      text(cholera::pumps[, c("x", "y")], label = cholera::pumps$id,
        pos = 1)

      if (observed) {
        points(cholera::fatalities[cholera::fatalities$case == case,
          c("x", "y")], col = col, lwd = 2)

        if (zoom) {
          if (highlight.segment) {
            segments(seg.data$x1, seg.data$y1, seg.data$x2, seg.data$y2,
              col = "red", lwd = 2)
          }
          if (add.title) {
            title(main = paste0("Observed Case #", case, "; ", seg.data$name,
              " ", seg.data$id))
          }
        } else {
          if (add.title) {
            title(main = paste0("Observed Case #", case, "; ", seg.data$name))
          }
        }

      } else {
        points(cholera::regular.cases[case, c("x", "y")],
          col = col, lwd = 2)

        if (zoom) {
          if (highlight.segment) {
            segments(seg.data$x1, seg.data$y1, seg.data$x2, seg.data$y2,
              col = "red", lwd = 2)
          }
          if (add.title) {
            title(main = paste0("Simulated Case #", case, "; ", seg.data$name,
              " ", seg.data$id))
          }
        } else {
          if (add.title) {
            title(main = paste0("Simulated Case #", case, "; ", seg.data$name))
          }
        }
      }
    } else list(case = case, segment.data = seg.data)
  }
}
