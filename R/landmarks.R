#' Landmark data (prototype).
#'
#' Nominal and orthogonal coordinates
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @export

# ortho.proj.landmarks <- landmarks()
# devtools::use_data(ortho.proj.landmarks)
# devtools::use_data(ortho.proj.landmarks, overwrite = TRUE)

landmarks <- function(multi.core = FALSE) {
  marx <- data.frame(x = 17.3855, y = 13.371)
  snow <- data.frame(x = 10.22414, y = 4.383851)
  st.lukes.church <- data.frame(x = 14.94156, y = 11.25313)
  # soho.sq <- data.frame(x = 18.07044, y = 15.85703)
  # golden.sq <- data.frame(x = 11.90927, y = 8.239483)
  huggins.brewery <- data.frame(x = 13.9022, y = 11.87315)

  pantheon.bazaar <- cholera::road.segments[cholera::road.segments$name ==
    "Winsley Street", c("x2", "y2")]
  names(pantheon.bazaar) <- c("x", "y")

  st.james.workhouse <- cholera::road.segments[cholera::road.segments$name ==
    "St James Workhouse", c("id", "x1", "y1", "name")]
  names(st.james.workhouse)[1:3] <- c("road.segment", "x.proj", "y.proj")
  st.james.workhouse$ortho.dist <- 0
  vars <- c("road.segment", "x.proj", "y.proj", "ortho.dist", "name")
  st.james.workhouse <- st.james.workhouse[, vars]

  ## Argyll House ##

  # NW <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
  #   "116-2", c("x2", "y2")], nm)
  # NE <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
  #   "144-1", c("x2", "y2")], nm)
  # SW <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
  #   "161-1", c("x2", "y2")], nm)
  # SE <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
  #   "161-1", c("x1", "y1")], nm)
  # aberdeen <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x, SW$y)
  # argyll.house <- data(x = aberdeen$x, y = aberdeen$y)

  # landmarks <- list(marx, snow, st.lukes.church, soho.sq, golden.sq,
  #   huggins.brewery, pantheon.bazaar)
  #
  # landmark.names <- c("Karl Marx", "John Snow", "St Luke's Church",
  #   "Soho Square", "Golden Square", "Lion Brewery", "The Pantheon")

  landmarks <- list(marx, snow, st.lukes.church, huggins.brewery,
    pantheon.bazaar)

  landmark.names <- c("Karl Marx", "John Snow", "St Luke's Church",
    "Lion Brewery", "The Pantheon")

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  cores <- multiCore(multi.core)

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

  orthogonal.projection <- parallel::mclapply(landmarks, function(x) {
    case <- x

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
      out[sel, ]
    } else {
      out[1, ] # all candidate roads are NA; arbitrarily choose first obs.
    }
  }, mc.cores = cores)

  ortho.proj <- do.call(rbind, orthogonal.projection)
  row.names(ortho.proj) <- NULL
  out <- data.frame(ortho.proj, name = landmark.names)
  out <- rbind(out, st.james.workhouse)
  row.names(out) <- NULL
  out$case <- seq(1001, 1000 + nrow(out))
  out
}
