#' Compute orthogonal projection of fatality addresses (prototype).
#'
#' Projection from fatality address to nearest road segment.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param radius Numeric.
#' @return An R data frame.
#' @export

latlongOrthoProj <- function(path, multi.core = TRUE, radius = 0.001) {
  cores <- multiCore(multi.core)
  vars <- c("long", "lat")
  addr <- latitudeLongitudeAddress(path)
  rds <- latitudeLongitudeRoads(path)

  rd.segs <- lapply(unique(rds$street), function(i) {
    st <- rds[rds$street == i, ]
    names(st)[names(st) %in% vars] <- paste0(vars, 1)
    seg.end <- st[-1, paste0(vars, 1)]
    names(seg.end) <- paste0(vars, 2)
    st <- cbind(st[-nrow(st), c("street", "id", "name")],
                st[-nrow(st), paste0(vars, 1)],
                seg.end)
    st$id <- paste0(st$street, "-", seq_len(nrow(st)))
    st
  })

  rd.segs <- do.call(rbind, rd.segs)

  soln <- parallel::mclapply(unique(addr$anchor), function(case) {
    case.data <- addr[addr$anchor == case, vars]

    within.radius <- lapply(rd.segs$id, function(id) {
      seg.data <- rd.segs[rd.segs$id == id, ]
      test1 <- cholera::withinRadius(case.data, seg.data[, paste0(vars, 1)],
        radius)
      test2 <- cholera::withinRadius(case.data, seg.data[, paste0(vars, 2)],
        radius)
      if (any(test1, test2)) unique(seg.data$id)
    })

    within.radius <- unlist(within.radius)

    ortho.proj.test <- lapply(within.radius, function(seg.id) {
      sel <- rd.segs$id == seg.id
      segment.data <- rd.segs[sel, c(paste0(vars, 1), paste0(vars, 2))]
      road.segment <- data.frame(x = c(segment.data$long1, segment.data$long2),
                                 y = c(segment.data$lat1, segment.data$lat2))

      ols <- stats::lm(y ~ x, data = road.segment)
      road.intercept <- stats::coef(ols)[1]
      road.slope <- stats::coef(ols)[2]
      ortho.slope <- -1 / road.slope
      ortho.intercept <- case.data$lat - ortho.slope * case.data$long

      long.proj <- (ortho.intercept - road.intercept) /
                   (road.slope - ortho.slope)
      lat.proj <- road.slope * long.proj + road.intercept
      seg.df <- data.frame(x = c(segment.data$long1, segment.data$long2),
                           y = c(segment.data$lat1, segment.data$lat2))

      dist <- stats::dist(rbind(seg.df[1, ], c(long.proj, lat.proj))) +
              stats::dist(rbind(seg.df[2, ], c(long.proj, lat.proj)))

      bisect.segment <- signif(stats::dist(seg.df)) == signif(dist)
      bisect.test <- ifelse(is.na(bisect.segment), FALSE, bisect.segment)

      if (bisect.test) {
        dat <- rbind(c(case.data$long, case.data$lat), c(long.proj, lat.proj))
        ortho.dist <- c(stats::dist(dat))
        ortho.pts <- data.frame(long.proj, lat.proj)
        data.frame(road.segment = seg.id, ortho.pts, ortho.dist, case)
      } else {
        null.out <- data.frame(matrix(NA, ncol = 5))
        names(null.out) <- c("road.segment", "long.proj", "lat.proj",
          "ortho.dist", "case")
        null.out
      }
    })

    out <- do.call(rbind, ortho.proj.test)
    out[which.min(out$ortho.dist), ]
  }, mc.cores = cores)

  soln <- do.call(rbind, soln)
  row.names(soln) <- NULL

  ## Second pass: classification error fix ##
  latlong.ortho <- soln

  sel <- cholera::ortho.proj$case %in% cholera::fatalities.address$anchor
  xy.ortho <- cholera::ortho.proj[sel, ]

  ## "Misclassified" addresses

  vars <- c("case", "road.segment")
  seg.test <- merge(xy.ortho[, vars], latlong.ortho[, vars], by = "case")
  chk <- seg.test[seg.test$road.segment.x != seg.test$road.segment.y, ]

  theta <- vapply(seq_along(chk$case), function(i) {
    segmentTheta(chk[i, "road.segment.x"], chk[i, "road.segment.y"])
  }, numeric(1L))

  # keep original xy segment (due to bar orientation) and new ortho coords
  case.keep.segment <- chk$case[theta < 120]

  vars <- c("long", "lat")

  keep.segment <- lapply(case.keep.segment, function(case) {
    case.data <- addr[addr$anchor == case, vars]
    correct.seg <- xy.ortho[xy.ortho$case == case, "road.segment"]
    sel <- rd.segs$id == correct.seg
    segment.data <- rd.segs[sel, c(paste0(vars, 1), paste0(vars, 2))]
    road.segment <- data.frame(x = c(segment.data$long1, segment.data$long2),
                               y = c(segment.data$lat1, segment.data$lat2))
    ols <- stats::lm(y ~ x, data = road.segment)
    road.intercept <- stats::coef(ols)[1]
    road.slope <- stats::coef(ols)[2]
    ortho.slope <- -1 / road.slope
    ortho.intercept <- case.data$lat - ortho.slope * case.data$long
    long.proj <- (ortho.intercept - road.intercept) / (road.slope - ortho.slope)
    lat.proj <- road.slope * long.proj + road.intercept

    seg.df <- data.frame(x = c(segment.data$long1, segment.data$long2),
                         y = c(segment.data$lat1, segment.data$lat2))

    dist <- stats::dist(rbind(seg.df[1, ], c(long.proj, lat.proj))) +
            stats::dist(rbind(seg.df[2, ], c(long.proj, lat.proj)))

    data.frame(road.segment = correct.seg, long.proj, lat.proj,
      ortho.dist = c(dist), case = case, row.names = NULL)
  })

  keep.segment <- do.call(rbind, keep.segment)

  vars <- c("road.segment", "long.proj", "lat.proj", "ortho.dist")

  for (case in case.keep.segment) {
    tmp <- keep.segment[keep.segment$case == case, vars]
    latlong.ortho[latlong.ortho$case == case, vars] <- tmp
  }

  latlong.ortho[order(latlong.ortho$case), ]
}
