#' Compute pump coordinates.
#'
#' Returns either the set of x-y coordinates for the pumps themselves or for their orthogonally projected "addresses" on the network of roads.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param orthogonal Logical. \code{TRUE} returns pump "addresses": the coordinates of the orthogonal projection from a pump's location onto the network of roads. \code{FALSE} returns pump location coordinates.
#' @seealso\code{\link{pumpLocator}}
#' @return An R data frame.
#' @note Note: The location of the fourteenth pump (from Snow's map in the Vestry report), at Hanover Square, and the "correct" location of the Broad Street pump are approximate. This function documents the code that generates \code{\link{pumps}}, \code{\link{pumps.vestry}}, \code{\link{ortho.proj.pump}} and \code{\link{ortho.proj.pump.vestry}}.
#' @export

pumpData <- function(vestry = FALSE, orthogonal = FALSE) {
  pumps <- HistData::Snow.pumps
  pumps$label <- c("Market Place", "Adam and Eve Court", "Berners Street",
    "Newman Street", "Marlborough Mews", "Little Marlborough Street",
    "Broad Street", "Warwick Street", "Bridle Street", "Rupert Street",
    "Dean Street", "Tichborne Street", "Vigo Street")

  names(pumps)[names(pumps) == "label"] <- "street"
  names(pumps)[names(pumps) == "pump"] <- "id"

  if (vestry) {
    # approximate location of 14th pump
    p14 <- data.frame(id = 14,
                      street = "George Street",
                      x = 3.707649,
                      y = 12.12859)

    pumps <- rbind(pumps, p14)

    # approximate "corrected" location of Broad Street pump
    pumps[pumps$id == 7, c("x", "y")] <- c(12.47044, 11.67793)
  }

  if (orthogonal == FALSE) {
    pumps
  } else {
    rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]

    road.segments <- lapply(unique(rd$street), function(st) {
      dat <- rd[rd$street == st, ]
      names(dat)[names(dat) %in% c("x", "y")] <- c("x1", "y1")
      seg.data <- dat[-1, c("x1", "y1")]
      names(seg.data) <- c("x2", "y2")
      dat <- cbind(dat[-nrow(dat), ], seg.data)
      dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
      dat
    })

    road.segments <- do.call(rbind, road.segments)

    orthogonal.projection <- lapply(pumps$id, function(p) {
      p.data <- pumps[pumps$id == p, ]
      coords <- p.data[c("x", "y")]
      st.segs <- road.segments[road.segments$name == p.data$street, "id"]

      within.radius <- lapply(st.segs, function(id) {
        seg.data <- cholera::road.segments[cholera::road.segments$id == id, ]
        test1 <- withinRadius(coords, seg.data[, c("x1", "y1")])
        test2 <- withinRadius(coords, seg.data[, c("x2", "y2")])
        if (any(test1, test2)) unique(seg.data$id)
      })

      within.radius <- unlist(within.radius)

      ortho.proj.test <- lapply(within.radius, function(id) {
        ortho.data <- orthogonalProjection(p, id, use.pump = TRUE,
          vestry = vestry)
        x.proj <- ortho.data$x.proj
        y.proj <- ortho.data$y.proj

        sel <- cholera::road.segments$id == id
        seg.data <- cholera::road.segments[sel, c("x1", "y1", "x2", "y2")]

        seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                             y = c(seg.data$y1, seg.data$y2))

        # segment bisection/intersection test
        distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
                 stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

        bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

        if (bisect.test) {
          ortho.dist <- c(stats::dist(rbind(coords, c(x.proj, y.proj))))
          ortho.pts <- data.frame(x.proj, y.proj)
          data.frame(road.segment = id, ortho.pts, ortho.dist)
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
      out$pump.id <- p
      row.names(out) <- NULL
      out
    })

  do.call(rbind, orthogonal.projection)
  }
}

# ortho.proj.pump <- pumpData(orthogonal = TRUE)
# ortho.proj.pump.vestry <- pumpData(orthogonal = TRUE, vestry = TRUE)
# usethis::use_data(ortho.proj.pump, overwrite = TRUE)
# usethis::use_data(ortho.proj.pump.vestry, overwrite = TRUE)
