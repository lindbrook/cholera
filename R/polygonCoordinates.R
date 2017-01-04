polygonCoordinates <- function(pump.id, cell.data, vestry,
  four.corners = fourCorners()) {
  # coordinates of Voronoi cell polygons for sp::point.in.polygon()

  coordinates <- lapply(pump.id, function(i) {
    if (vestry) {
      pump <- cholera::pumps.vestry[cholera::pumps.vestry$id == i, c("x", "y")]
    } else {
      pump <- cholera::pumps[cholera::pumps$id == i, c("x", "y")]
    }

    dat <- cell.data[cell.data$ind1 == which(pump.id == i) |
                     cell.data$ind2 == which(pump.id == i), ]

    a <- dat[, c("x1", "y1")]
    b <- dat[, c("x2", "y2")]
    names(a) <- c("x", "y")
    names(b) <- c("x", "y")

    coords <- unique(rbind(a, b))

    # test for "open" polygons
    test1 <- any(dat$thirdv1 < 0 | dat$thirdv2 < 0)
    test2 <- unlist(dat[, c("thirdv1", "thirdv2")])
    test2 <- length(unique(test2[test2 < 0])) != 1

    # close "open" polygons at corners of deldir::deldir()'s rectangular window
    if (test1 & test2) {
      # four.corners <- fourCorners()
      # test by negation:
      # does segment from pump to corner intersect any of the polygon's sides?
      corners <- lapply(seq_len(nrow(dat)), function(j) {
        intersection.points <- lapply(four.corners, function(corner) {
          segmentIntersection(pump$x, pump$y, corner$x, corner$y,
            dat[j, "x1"], dat[j, "y1"], dat[j, "x2"], dat[j, "y2"])
        })

        vapply(intersection.points, function(x) all(is.na(x)) == FALSE,
               logical(1L))
      })

      # If a "corner" returns FALSE, include that corner as a vertex
      corner.id <- which(colSums(do.call(rbind, corners)) == 0)
      corner.solution <- four.corners[corner.id]

      if (length(corner.solution) > 1) {
        coords <- rbind(coords, do.call(rbind, corner.solution))
      } else {
        coords <- rbind(coords, unlist(corner.solution))
      }
    }

    # center vertices relative to pump's coordinates
    coords.centered <- data.frame(x = coords$x - pump$x, y = coords$y - pump$y)

    # transform coordinates from cartesian to polar
    # sort vertices by phi (angle); returns vertices in counter-clockwise order
    idx <- order(apply(coords.centered, 1, pracma::cart2pol)[1, ])
    coords <- coords[idx, ]

    # adds first vertex to last to close polygon
    rbind(coords, coords[1, ])
  })

  names(coordinates) <- pump.id
  coordinates
}
