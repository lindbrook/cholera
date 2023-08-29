#' Landmark data.
#'
#' Nominal and orthogonal coordinates
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @noRd
#' @note Uses road segments that enter square(s) as entry points.

landmarkDataB <- function(multi.core = TRUE, dev.mode = FALSE) {
  marx <- data.frame(x = 17.3855, y = 13.371)
  snow <- data.frame(x = 10.22414, y = 4.383851)
  st.lukes.church <- data.frame(x = 14.94156, y = 11.25313)
  huggins.brewery <- data.frame(x = 13.9022, y = 11.87315)

  ## Squares ##

  golden.square <- squareExitsB("Golden Square")
  golden.square$name <- paste0("Golden Square-", c("W", "E", "S", "N"))

  soho.square <- squareExitsB("Soho Square")
  soho.square$name <- paste0("Soho Square-", c("E", "N", "S3", "S2", "S1", "W"))

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
    "Model Lodging", "Craven Chapel", soho.square$name, golden.square$name)

  cores <- multiCore(multi.core)
  orthogonal.projection <- orthoProjLandmarks(landmarks, cores, dev.mode)

  ortho.proj <- do.call(rbind, orthogonal.projection)
  row.names(ortho.proj) <- NULL
  out <- data.frame(ortho.proj, do.call(rbind, landmarks),
    name = landmark.names)
  row.names(out) <- NULL
  out$case <- seq(20001, 20000 + nrow(out))

  # Soho Square fix

  soho.tmp <- out[out$name %in% soho.square$name, ]
  soho.fix <- merge(soho.square[, c("id", "name")],
                    soho.tmp[names(soho.tmp) != "road.segment"],
                    by = "name")

  names(soho.fix)[names(soho.fix) == "id"] <- "road.segment"
  soho.fix <- soho.fix[, names(out)]
  out[out$name %in% soho.square$name, ] <- soho.fix

  # Golden Square fix

  golden.tmp <- out[out$name %in% golden.square$name, ]
  golden.fix <- merge(golden.square[, c("id", "name")],
                      golden.tmp[names(golden.tmp) != "road.segment"],
                      by = "name")

  names(golden.fix)[names(golden.fix) == "id"] <- "road.segment"
  golden.fix <- golden.fix[, names(out)]
  out[out$name %in% golden.square$name, ] <- golden.fix

  #

  out[order(out$case), ]
}

pasteCoordsB <- function(dat, var1 = "x1", var2 = "y1") {
  vapply(seq_len(nrow(dat)), function(i) {
    paste(dat[i, c(var1, var2)], collapse = "-")
  }, character(1L))
}

squareExitsB <- function(nm = "Golden Square") {
  dat <- cholera::road.segments[cholera::road.segments$name == nm, ]
  left <- pasteCoordsB(dat)
  right <- pasteCoordsB(dat, "x2", "y2")
  mat <- do.call(rbind, lapply(strsplit(union(left, right), "-"), as.numeric))
  exit.nodes <- data.frame(x = mat[, 1], y = mat[, 2])

  do.call(rbind, lapply(seq_len(nrow(exit.nodes)), function(i) {
    sel1 <- cholera::road.segments$x1 == exit.nodes[i, ]$x &
            cholera::road.segments$y1 == exit.nodes[i, ]$y
    sel2 <- cholera::road.segments$x2 == exit.nodes[i, ]$x &
            cholera::road.segments$y2 == exit.nodes[i, ]$y

    node.segs <- cholera::road.segments[sel1 | sel2, ]
    candidate <- node.segs[!grepl(nm, node.segs$name), ]
    square.segs <- node.segs[grepl(nm, node.segs$name), ]

    vars0 <- c("x", "y")

    sq.coords <- unique(rbind(
      stats::setNames(square.segs[, paste0(vars0, 1)], vars0),
      stats::setNames(square.segs[, paste0(vars0, 2)], vars0)
    ))

    ones <- vapply(seq_len(nrow(sq.coords)), function(i) {
      sq.tmp <- sq.coords[i, vars0]
      candidate.tmp <- candidate[, paste0(vars0, 1)]
      identical(candidate.tmp$x1, sq.tmp$x) & identical(candidate.tmp$y1,
        sq.tmp$y)
    }, logical(1L))

    twos <- vapply(seq_len(nrow(sq.coords)), function(i) {
      sq.tmp <- sq.coords[i, vars0]
      candidate.tmp <- candidate[, paste0(vars0, 2)]
      identical(candidate.tmp$x2, sq.tmp$x) & identical(candidate.tmp$y2,
        sq.tmp$y)
    }, logical(1L))

    vars <- names(dat)[!grepl("x", names(dat)) & !grepl("y", names(dat))]

    if (any(ones)) {
      candidate <- candidate[, c(vars, paste0(vars0, 1))]
      names(candidate)[grep(1, names(candidate))] <- vars0
    } else if (any(twos)) {
      candidate <- candidate[, c(vars, paste0(vars0, 2))]
      names(candidate)[grep(2, names(candidate))] <- vars0
    }

    candidate
  }))
}

