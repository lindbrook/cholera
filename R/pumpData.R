#' Compute pump coordinates.
#'
#' Returns either the set of x-y coordinates for the pumps themselves or for their orthogonally projected "addresses" on the network of roads.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param orthogonal Logical. \code{TRUE} returns pump "addresses": the coordinates of the orthogonal projection from a pump's location onto the network of roads. \code{FALSE} returns pump location coordinates.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores (rounds with \code{as.integer()}). See \code{vignette("Parallelization")} for details.
#' @seealso\code{\link{pumpLocator}}
#' @return An R data frame.
#' @note Note: The location of the fourteenth pump, at Hanover Square, and the "correct" location of the Broad Street pump are approximate. This function documents the code that generates \code{\link{pumps}}, \code{\link{pumps.vestry}}, \code{\link{ortho.proj.pump}} and \code{\link{ortho.proj.pump.vestry}}.
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

    # approximate "corrected" location of Broad Street pump
    pumps[pumps$id == 7, c("x", "y")] <- c(12.47044, 11.67793)
    pumps
  }

  if (orthogonal == FALSE) {
    pumps
  } else {
    cores <- multiCore(multi.core)
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

    orthogonal.projection <- parallel::mclapply(pumps$id, function(pump) {
      case <- pumps[pumps$id == pump, c("x", "y")]

      within.radius <- lapply(road.segments$id, function(x) {
        seg.data <- cholera::road.segments[cholera::road.segments$id == x, ]
        test1 <- withinRadius(case, seg.data[, c("x1", "y1")])
        test2 <- withinRadius(case, seg.data[, c("x2", "y2")])
        if (any(test1, test2)) unique(seg.data$id)
      })

      within.radius <- unlist(within.radius)

      ortho.proj.test <- lapply(within.radius, function(seg.id) {
        ortho.data <- orthogonalProjection(pump, seg.id, use.pump = TRUE,
          vestry = vestry)
        x.proj <- ortho.data$x.proj
        y.proj <- ortho.data$y.proj

        seg.data <- cholera::road.segments[cholera::road.segments$id == seg.id,
          c("x1", "y1", "x2", "y2")]

        seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                             y = c(seg.data$y1, seg.data$y2))

        # segment bisection/intersection test
        distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
                 stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

        bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

        if (bisect.test) {
          ortho.dist <- c(stats::dist(rbind(c(case$x, case$y),
            c(x.proj, y.proj))))
          ortho.pts <- data.frame(x.proj, y.proj)
          data.frame(road.segment = seg.id, ortho.pts, ortho.dist,
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

      out$node <- paste0(out$x.proj, "_&_", out$y.proj)
      out$pump.id <- pump
      row.names(out) <- NULL
      out
    }, mc.cores = cores)

  do.call(rbind, orthogonal.projection)
  }
}

# ortho.proj.pump <- pumpData(orthogonal = TRUE)
# ortho.proj.pump.vestry <- pumpData(orthogonal = TRUE, vestry = TRUE)
# usethis::use_data(ortho.proj.pump, overwrite = TRUE)
# usethis::use_data(ortho.proj.pump.vestry, overwrite = TRUE)

