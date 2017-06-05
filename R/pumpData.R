#' Compute pump coordinates.
#'
#' Returns the Dodson and Tobler coordinates for the original thirteen pumps, appended with name of nearest road. Adds "approximate" locations for the fourteenth pump and the relocated Broad Street pump that are included in Snow's second version of the pump in the Vestry report.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry report. FALSE uses the 13 in the original map.
#' @param orthogonal Logical. TRUE returns pump "addresses": the coordinates of the orthogonal projection from a pump's location onto the network of roads. FALSE returns pump location coordinates.
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. With Numeric, you specify the number logical cores (rounds with as.integer()). On Windows, only "multi.core = FALSE" is available.
#' @seealso\code{\link{pumpLocator}}
#' @return An R dataframe.
#' @section Notes: This function documents the code that generates \code{\link{pumps}}, \code{\link{pumps.vestry}}, \code{\link{ortho.proj.pump}} and \code{\link{ortho.proj.pump.vestry}}.
#' @export

pumpData <- function(vestry = FALSE, orthogonal = FALSE, multi.core = FALSE) {
  pumps <- HistData::Snow.pumps
  pumps$label <- c("Market Place", "Adam and Eve Court", "Berners Street",
    "Newman Street", "Marlborough Mews", "Little Marlborough Street",
    "Broad Street", "Warwick Street", "Bridle Street", "Rupert Street",
    "Dean Street", "Tichborne Street", "Vigo Street")

  names(pumps)[names(pumps) == "label"] <- "street"
  names(pumps)[names(pumps) == "pump"] <- "id"

  if (vestry == FALSE) {
    pumps
  } else {
    # approximate location of 14th pump
    p14 <- data.frame(id = 14,
                      street = "Hanover Street",
                      x = 3.707649,
                      y = 12.12859)

    pumps <- rbind(pumps, p14)

    # approximate "corrected" location of the Broad Street pump
    pumps[pumps$id == 7, c("x", "y")] <- c(12.47044, 11.67793)
    pumps
  }

  if (orthogonal == FALSE) {
    pumps
  } else {
    if (is.logical(multi.core)) {
      if (multi.core == TRUE) {
        cores <- parallel::detectCores()
      } else {
        if (is.numeric(multi.core)) {
          if (is.integer(multi.core)) {
            cores <- multi.core
          } else {
            cores <- as.integer(multi.core)
          }
        } else {
          cores <- 1L
        }
      }
    } else if (is.numeric(multi.core)) {
      if (is.integer(multi.core)) {
        cores <- multi.core
      } else {
        cores <- as.integer(multi.core)
      }
    }

    rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
    map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
    roads.list <- split(rd[, c("x", "y")], rd$street)
    border.list <- split(map.frame[, c("x", "y")], map.frame$street)

    road.segments <- parallel::mclapply(unique(rd$street), function(i) {
      dat <- rd[rd$street == i, ]
      names(dat)[names(dat) %in% c("x", "y")] <- c("x1", "y1")
      seg.data <- dat[-1, c("x1", "y1")]
      names(seg.data) <- c("x2", "y2")
      dat <- cbind(dat[-nrow(dat), ], seg.data)
      dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
      dat
    }, mc.cores = cores)

    road.segments <- do.call(rbind, road.segments)

    orthogonal.projection <- parallel::mclapply(pumps$id, function(i) {
      case <- pumps[pumps$id == i, c("x", "y")]

      within.radius <- lapply(road.segments$id, function(x) {
        dat <- road.segments[road.segments$id == x, ]
        test1 <- withinRadius(case, dat[, c("x1", "y1")])
        test2 <- withinRadius(case, dat[, c("x2", "y2")])
        if (any(test1, test2)) unique(dat$id)
      })

      within.radius <- unlist(within.radius)

      ortho.proj.test <- lapply(within.radius, function(x) {
        seg.data <- road.segments[road.segments$id == x,
          c("x1", "y1", "x2", "y2")]

        seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                             y = c(seg.data$y1, seg.data$y2))

        ols <- stats::lm(y ~ x, data = seg.df)
        segment.slope <- stats::coef(ols)[2]
        segment.intercept <- stats::coef(ols)[1]
        orthogonal.slope <- -1 / segment.slope
        orthogonal.intercept <- case$y - orthogonal.slope * case$x

        x.proj <- (orthogonal.intercept - segment.intercept) /
                  (segment.slope - orthogonal.slope)

        y.proj <- segment.slope * x.proj + segment.intercept

        # segment bisection/intersection test
        distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
          stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

        bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

        if (bisect.test) {
          ortho.dist <- c(stats::dist(rbind(c(case$x, case$y),
            c(x.proj, y.proj))))
          ortho.pts <- data.frame(x.proj, y.proj)
          data.frame(road.segment = x, ortho.pts, ortho.dist,
            stringsAsFactors = FALSE)
        } else {
          null.out <- data.frame(matrix(NA, ncol = 4))
          names(null.out) <- c("road.segment", "x.proj", "y.proj", "ortho.dist")
          null.out
        }
      })

      out <- do.call(rbind, ortho.proj.test)

      if (all(is.na(out)) == FALSE) {
        sel <- which.min(out$ortho.dist)
        out <- out[sel, ]
      } else {
        # all candidate roads are NA so arbitrarily choose the first obs.
        out <- out[1, ]
      }

      out$pump.id <- i
      row.names(out) <- NULL
      out
    }, mc.cores = cores)

  do.call(rbind, orthogonal.projection)
  }
}

withinRadius <- function(a, b, radius = 2) {
  (a$x - b$x)^2 + (a$y - b$y)^2 <= radius^2
}
