#' Landmark data.
#'
#' Nominal and orthogonal coordinates
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @export

landmarkData <- function(multi.core = FALSE) {
  marx <- data.frame(x = 17.3855, y = 13.371)
  snow <- data.frame(x = 10.22414, y = 4.383851)
  st.lukes.church <- data.frame(x = 14.94156, y = 11.25313)
  huggins.brewery <- data.frame(x = 13.9022, y = 11.87315)

  ## Squares ##

  squareExits <- function(nm = "Golden Square") {
    dat <- cholera::road.segments[cholera::road.segments$name == nm, ]

    pasteCoords <- function(var1 = "x1", var2 = "y1") {
      vapply(seq_len(nrow(dat)), function(i) {
        paste(dat[i, c(var1, var2)], collapse = "-")
      }, character(1L))
    }

    left <- pasteCoords()
    right <- pasteCoords("x2", "y2")
    mat <- do.call(rbind, lapply(strsplit(union(left, right), "-"), as.numeric))
    data.frame(x = mat[, 1], y = mat[, 2])
  }

  golden.square <- squareExits()
  golden.square$name <- c("W", "E", "S", "N")

  soho.square <- squareExits("Soho Square")
  soho.square$name <- c("E", "NE", "N", "NW", "S3", "SE", "S2", "S1", "W", "SW")
  soho.square <- soho.square[soho.square$name %in%
    c("NE", "NW", "SE", "SW") == FALSE, ]

  ## ##

  pantheon.bazaar <- cholera::road.segments[cholera::road.segments$name ==
    "Winsley Street", c("x2", "y2")]
  names(pantheon.bazaar) <- c("x", "y")

  st.james.workhouse <- cholera::road.segments[cholera::road.segments$name ==
    "St James Workhouse", c("id", "x1", "y1", "name")]
  names(st.james.workhouse)[1:3] <- c("road.segment", "x.proj", "y.proj")
  st.james.workhouse$ortho.dist <- 0
  vars <- c("x.proj", "y.proj")
  st.james.workhouse <- stats::setNames(st.james.workhouse[, vars], c("x", "y"))

  ## Argyll House : Lord Aberdeen ##

  nm <- c("x", "y")

  NW <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
    "116-2", c("x2", "y2")], nm)
  NE <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
    "144-1", c("x2", "y2")], nm)
  SW <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
    "161-1", c("x2", "y2")], nm)
  SE <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
    "161-1", c("x1", "y1")], nm)

  aberdeen <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y,
    SW$x, SW$y)
  argyll.house <- data.frame(x = aberdeen$x, y = aberdeen$y)

  ## Model Lodging ##

  sel <- cholera::road.segments$name == "Cock Court"
  rd.data <- cholera::road.segments[sel, c("x2", "y2")]
  NW <- stats::setNames(rd.data, nm)

  sel <- cholera::road.segments$name == "Cock Court"
  rd.data <- cholera::road.segments[sel, c("x1", "y1")]
  NE <- stats::setNames(rd.data, nm)

  sel <- cholera::road.segments$id == "259-1"
  rd.data <- cholera::road.segments[sel, c("x2", "y2")]
  SW <- stats::setNames(rd.data, nm)

  sel <- cholera::road.segments$id == "259-1"
  rd.data <- cholera::road.segments[sel, c("x1", "y1")]
  SE <- stats::setNames(rd.data, nm)

  model.lodging <- segmentIntersection(NW$x, NW$y, SE$x, SE$y,
                                       NE$x, NE$y, SW$x, SW$y)

  ## Craven Chapel (Wesleyan) ##

  ep1 <- cholera::road.segments[cholera::road.segments$name == "Lowndes Court",
    c("x2", "y2")]
  ep2 <- cholera::road.segments[cholera::road.segments$id == "201-1",
    c("x2", "y2")]
  dat <- stats::setNames(rbind(ep1, ep2), nm)
  h <- c(stats::dist(dat))
  ols <- stats::lm(y ~ x, dat)
  segment.slope <- stats::coef(ols)[2]
  theta <- atan(segment.slope)
  delta.x <- (h / 2) * cos(theta)
  delta.y <- (h / 2) * sin(theta)
  x.new <- dat[1, "x"] + delta.x
  y.new <- dat[1, "y"] + delta.y
  craven.chapel <- data.frame(x = x.new, y = y.new)

  ##

  soho <- lapply(soho.square$name, function(nm) {
    soho.square[soho.square$name == nm, c("x", "y")]
  })

  golden <- lapply(golden.square$name, function(nm) {
    golden.square[golden.square$name == nm, c("x", "y")]
  })

  landmarks <- list(marx, snow, st.lukes.church, huggins.brewery,
    pantheon.bazaar, st.james.workhouse, argyll.house, model.lodging,
    craven.chapel)
  landmarks <- append(landmarks, soho)
  landmarks <- append(landmarks, golden)

  landmark.names <- c("Karl Marx", "John Snow", "St Luke's Church",
    "Lion Brewery", "The Pantheon", "St James Workhouse", "Argyll House",
    "Model Lodging", "Craven Chapel", paste0("Soho Square-", soho.square$name),
    paste0("Golden Square-", golden.square$name))

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  cores <- multiCore(multi.core)

  orthogonal.projection <- parallel::mclapply(landmarks, function(case) {
    within.radius <- lapply(cholera::road.segments$id, function(x) {
      dat <- cholera::road.segments[cholera::road.segments$id == x, ]
      test1 <- withinRadius(case, dat[, c("x1", "y1")])
      test2 <- withinRadius(case, dat[, c("x2", "y2")])
      if (any(test1, test2)) unique(dat$id)
    })

    within.radius <- unlist(within.radius)

    ortho.proj.test <- lapply(within.radius, function(x) {
      seg.data <- cholera::road.segments[cholera::road.segments$id == x,
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
  out <- data.frame(ortho.proj, do.call(rbind, landmarks),
    name = landmark.names, stringsAsFactors = FALSE)
  row.names(out) <- NULL
  out$case <- seq(20001, 20000 + nrow(out))
  out
}

landmarksSquares <- function() {
  golden.sq <- data.frame(name = "Golden Square", x = 11.90927, y = 8.239483,
    stringsAsFactors = FALSE)
  soho.sq <- data.frame(name = "Soho Square", x = 18.07044, y = 15.85703,
    stringsAsFactors = FALSE)
  out <- rbind(golden.sq, soho.sq)
  start <- max(cholera::landmarks$case) + 1
  stop <- start + nrow(out) - 1
  out$case <- seq(start, stop)
  out
}

segmentIntersection <- function(x1, y1, x2, y2, a1, b1, a2, b2) {
  # returns the point of intersection between two segments or NA if none.
  # http://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r
  # x1, y1, x2, y2 coordinates of first segment's endpoints.
  # a1, b1, a2, b2 coordinates of second segment's endpoints.
  denom <- (b2 - b1) * (x2 - x1) - (a2 - a1) * (y2 - y1)
  denom[abs(denom) < 1e-10] <- NA # parallel lines
  ua <- ((a2 - a1) * (y1 - b1) - (b2 - b1) * (x1 - a1)) / denom
  ub <- ((x2 - x1) * (y1 - b1) - (y2 - y1) * (x1 - a1)) / denom
  x <- x1 + ua * (x2 - x1)
  y <- y1 + ua * (y2 - y1)
  inside <- (ua >= 0) & (ua <= 1) & (ub >= 0) & (ub <= 1)
  data.frame(x = ifelse(inside, x, NA), y = ifelse(inside, y, NA))
}
