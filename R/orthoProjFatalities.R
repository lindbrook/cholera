#' Orthogonal projection of case fatalities to road segments (address).
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @note Documents ortho.proj data frame (unstack6c.R).
#' @export

orthoProjFatalities <- function(multi.core = TRUE) {
  cores <- multiCore(multi.core)
  orthogonal.projection <- parallel::mclapply(cholera::fatalities$case,
    orthogonalProjectionB, mc.cores = cores)
  orthogonal.projection <- do.call(rbind, orthogonal.projection)
  row.names(orthogonal.projection) <- NULL
  orthogonal.projection$case <- cholera::fatalities$case

  ## Classification errors - due to bar orientation ##

  road.segment.fix <- list(
    "216-1" = c(290, 61, 174, 547, 523, 521, 138, 59, 340, 508),
    "290-1" = c(409, 131, 18, 575, 566, 518, 297),
    "259-1" = 145,
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

  # Recompute orthogonal distances

  ortho.projB <- lapply(seq_along(road.segment.fix), function(i) {
    case <- cholera::fatalities[unlist(road.segment.fix[[i]]), ]
    seg.id <- names(road.segment.fix[i])
    seg.data <- cholera::road.segments[cholera::road.segments$id == seg.id, ]
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

    proj.data <- lapply(1:nrow(case), function(j) {
      dat <- rbind(case[j, c("x", "y")], c(x.proj[j], y.proj[j]))
      cbind(x.proj[j], y.proj[j], c(stats::dist(dat)))
    })

    out <- data.frame(seg.id, do.call(rbind, proj.data), case$case)
    names(out) <- c("road.segment", "x.proj", "y.proj", "ortho.dist", "case")
    out
  })

  ortho.projB <- do.call(rbind, ortho.projB)
  row.names(ortho.projB) <- NULL

  ## Manual fix of endpoint cases ##

  # Portland Mews: 286, 558 #

  #       56
  # 558  286  anchor
  # --------

  old.st <- "160-2"
  new.st <- "160-3"

  case.select <- 286
  case <- cholera::fatalities[case.select, ]
  x.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "x2"]
  y.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "y2"]
  ortho.dist <- stats::dist(rbind(case[, c("x", "y")], c(x.proj, y.proj)))
  data.fix <- data.frame(road.segment = new.st, x.proj, y.proj,
    ortho.dist = c(ortho.dist), case = case.select)

  ortho.projB <- rbind(ortho.projB, data.fix)

  case.select <- 56
  case <- cholera::fatalities[case.select, ]
  x.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "x2"]
  y.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "y2"]
  ortho.dist <- stats::dist(rbind(case[, c("x", "y")], c(x.proj, y.proj)))
  data.fix <- data.frame(road.segment = new.st, x.proj, y.proj,
    ortho.dist = c(ortho.dist), case = case.select)

  ortho.projB <- rbind(ortho.projB, data.fix)

  # William and Mary Yard: 440 #
  # orthogonal.projection[orthogonal.projection$case == 440, ]

  # 259-2: |
  # 259-1:  _

  old.st <- "317-3"
  new.st <- "259-1"

  case.select <- 440
  case <- cholera::fatalities[case.select, ]
  x.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "x2"]
  y.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "y2"]
  ortho.dist <- stats::dist(rbind(case[, c("x", "y")], c(x.proj, y.proj)))
  data.fix <- data.frame(road.segment = new.st, x.proj, y.proj,
    ortho.dist = c(ortho.dist), case = case.select)

  ortho.projB <- rbind(ortho.projB, data.fix)

  ## Re-assembly ##

  sel <- orthogonal.projection$case %in% ortho.projB$case
  orthogonal.projection[sel, ] <- ortho.projB

  orthogonal.projection <-
    orthogonal.projection[order(orthogonal.projection$case), ]

  row.names(orthogonal.projection) <- NULL

  ## St James Workhouse fix ##

  st.james.seg <- "148-1"
  st.james <- orthogonal.projection$case %in% c(11, 53, 193, 369, 434)
  orthogonal.projection[st.james, "road.segment"] <- st.james.seg
  sel <- cholera::road.segments$id == st.james.seg
  vars <- c("x1", "y1")
  orthogonal.projection[st.james, c("x.proj", "y.proj")] <-
    cholera::road.segments[sel, vars]
  orthogonal.projection
}

# ortho.proj <- cholera::orthoProjFatalities()
# usethis::use_data(ortho.proj, overwrite = TRUE)

orthogonalProjectionB <- function(i, data.source = "observed") {
  if (data.source == "regular") {
    case <- cholera::regular.cases[i, ]
  } else if (data.source == "pumps") {
    case <- cholera::pumps[cholera::pumps$id == i, ]
  } else if (data.source == "pumps.vestry") {
    case <- cholera::pumps.vestry[cholera::pumps.vestry$id == i, ]
  } else if (data.source == "observed") {
    case <- cholera::fatalities[cholera::fatalities$case == i, c("x", "y")]
  }

  within.radius <- lapply(cholera::road.segments$id, function(x) {
    dat <- cholera::road.segments[cholera::road.segments$id == x, ]
    test1 <- withinRadius(case, dat[, c("x1", "y1")])
    test2 <- withinRadius(case, dat[, c("x2", "y2")])
    if (any(test1, test2)) unique(dat$id)
  })

  within.radius <- unlist(within.radius)

  orthogonal.projection.test <- lapply(within.radius, function(x) {
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
      ortho.dist <- c(stats::dist(rbind(c(case$x, case$y), c(x.proj, y.proj))))
      ortho.pts <- data.frame(x.proj, y.proj)
      data.frame(road.segment = x, ortho.pts, ortho.dist)
    } else {
      null.out <- data.frame(matrix(NA, ncol = 4))
      names(null.out) <- c("road.segment", "x.proj", "y.proj", "ortho.dist")
      null.out
    }
  })

  out <- do.call(rbind, orthogonal.projection.test)

  if (all(is.na(out)) == FALSE) {
    sel <- which.min(out$ortho.dist)
    out[sel, ]
  } else {
    # all candidate roads are NA so arbitrarily choose the first obs.
    out[1, ]
  }
}

#' Visually check orthogonal projection of case fatalities to road segments (address).
#'
#' @param case Numeric. case ID from \code{fatalities}.
#' @param rd.seg Character. Road segment ID.
#' @export

orthoProjector <- function(case = 483, rd.seg = "326-2") {
  case.data <- cholera::fatalities[cholera::fatalities$case == case, ]
  seg.data <- cholera::road.segments[cholera::road.segments$id == rd.seg,
    c("x1", "y1", "x2", "y2")]
  seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                       y = c(seg.data$y1, seg.data$y2))
  ols <- stats::lm(y ~ x, data = seg.df)
  segment.slope <- stats::coef(ols)[2]
  segment.intercept <- stats::coef(ols)[1]
  orthogonal.slope <- -1 / segment.slope
  orthogonal.intercept <- case.data$y - orthogonal.slope * case.data$x

  x.proj <- (orthogonal.intercept - segment.intercept) /
            (segment.slope - orthogonal.slope)
  y.proj <- segment.slope * x.proj + segment.intercept

  x.rng <- range(seg.data[, c("x1", "x2")], case.data$x, x.proj)
  y.rng <- range(seg.data[, c("y1", "y2")], case.data$y, y.proj)

  distB <- stats::dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
    stats::dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

  bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

  plot(seg.data[, c("x1", "y1")], xlim = x.rng, ylim = y.rng, xlab = "x",
    ylab = "y", asp = 1)
  points(seg.data[, c("x2", "y2")])
  segments(seg.data$x1, seg.data$y1, seg.data$x2, seg.data$y2)
  text(case.data$x, case.data$y, pos = 1, labels = case)
  title(main = rd.seg)

  if (bisect.test) {
    points(case.data$x, case.data$y, col = "green", pch = 16)
    arrows(case.data$x, case.data$y, x.proj, y.proj, length = 0.1,
      col = "green")
  } else {
    points(case.data$x, case.data$y, col = "red", pch = 16)
    segments(seg.data$x2, seg.data$y2, x.proj, y.proj, lty = "dotted",
      col = "gray")
    arrows(case.data$x, case.data$y, x.proj, y.proj, length = 0.1, col = "red")
  }
}
