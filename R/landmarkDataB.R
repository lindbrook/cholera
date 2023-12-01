#' Landmark data.
#'
#' Nominal and orthogonal coordinates
#' @noRd
#' @note Uses road segments that enter square(s) as entry points.

landmarkDataB <- function() {
  ## squares ##

  golden.square <- Squares("Golden Square")
  soho.square <- Squares("Soho Square")

  ## other landmarks #

  # Argyll House -- Lord Aberdeen #
  # The London Palladium
  # https://www.british-history.ac.uk/survey-london/vols31-2/pt2/pp284-307#h3-0010
  # "frontages in both Argyll Street and Great Marlborough Street."
  # entrance on Argyll Street
  argyll.house <- argyllHouse()

  # Craven Chapel #
  # Berwick Street
  craven.chapel <- cravenChapel()

  # Lion Brewery #
  # 50 Broad Street; Huggins Brewery (?)
  lion.brewery <- lionBrewery()

  # Marlborough Street Magistrates Court #
  # 19–21 Great Marlborough Street
  # 51°30′51.62″N 0°8′22.13″W
  magistrates.court <- magistratesCourt()

  # Karl Marx #
  # 28 Dean Street, 174-1
  # cholera:::addressProportion("174-1", "Karl Marx") # 0.5000003
  marx <- karlMarx()

  # Model Lodging Houses #
  # Hopkins Street "The Cholera in Berwick Street" by Rev. Henry Whitehead
  # segment IDs: "245-1"
  # Ingestre Buildings
  # New Street/Husband Street -> Ingestre Place (now)
  model.lodging.houses <- modelLodgingHouses()

  # The Pantheon #
  # Today Marks & Spencers at 173 Oxford Street
  # placed at intersection of Oxford and Winsley
  pantheon.bazaar <- pantheonBazaar()

  # John Snow #
  # H: 18 Sackville Street, "508-1"
  # cholera:::addressProportion("508-1", "John Snow") # 0.4999993
  snow <- johnSnow()

  # St James Workhouse #
  # address set on Poland Street
  st.james.workhouse <- stJamesWorkhouse()

  # St Luke's Church #
  # Berwick Street, currently Kemp House across from Tyler's Court
  st.lukes.church <- stLukesChurch()


  out <- rbind(golden.square, soho.square, argyll.house, craven.chapel,
               lion.brewery, magistrates.court, marx, model.lodging.houses,
               pantheon.bazaar, snow, st.james.workhouse, st.lukes.church)

  row.names(out) <- NULL
  out
}

## Square(s) Functions ##

landmarkSquares <- function() {
  golden <- Squares("Golden Square", label.coord = TRUE)
  soho <- Squares("Soho Square", label.coord = TRUE)
  data.frame(case = 1000L:1001L, x = c(golden$x, soho$x),
    y = c(golden$y, soho$y), name = c("Golden Square", "Soho Square"))
}

# landmark.squares <- landmarkSquares()
# usethis::use_data(landmark.squares, overwrite = TRUE)

squareCenterB <- function(NS, EW, latlong = FALSE) {
  if (latlong) {
    line.NS <- stats::lm(lat ~ lon, data = NS)
    line.EW <- stats::lm(lat ~ lon, data = EW)
  } else {
    line.NS <- stats::lm(y.proj ~ x.proj, data = NS)
    line.EW <- stats::lm(y.proj ~ x.proj, data = EW)
  }

  slope.delta <- stats::coef(line.NS)[2] - stats::coef(line.EW)[2]
  int.delta <- stats::coef(line.EW)[1] - stats::coef(line.NS)[1]
  x.val <- int.delta / slope.delta
  y.val <- stats::coef(line.EW)[2] * x.val + stats::coef(line.EW)[1]

  out <- data.frame(x = x.val, y = y.val, row.names = NULL)
  if (latlong) names(out) <- c("lon", "lat")
  out
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
      names(candidate)[grep(1, names(candidate))] <- paste0(vars0, ".proj")
    } else if (any(twos)) {
      candidate <- candidate[, c(vars, paste0(vars0, 2))]
      names(candidate)[grep(2, names(candidate))] <- paste0(vars0, ".proj")
    }

    candidate
  }))
}

Squares <- function(nm = "Golden Square", label.coord = FALSE) {
  vars <- c("x.proj", "y.proj")
  sq <- squareExitsB(nm)

  if (nm == "Golden Square") {
    exits <- c("W", "E", "S", "N")
    start <- 1002L
  } else if (nm == "Soho Square") {
    exits <- c("E", "N", "S3", "S2", "S1", "W")
    start <- 1006L
  } else stop('nm must be "Golden Square" or "Soho Square"', call. = FALSE)

  case <- seq(start, start + length(exits) - 1)
  sq$name <- paste0(nm, "-", exits)

  if (label.coord) {
    if (nm == "Golden Square") {
      sel <- sq$name %in% paste0(paste0(nm, "-"), c("N", "S"))
    } else if (nm == "Soho Square") {
      sel <- sq$name %in% paste0(paste0(nm, "-"), c("N", "S2"))
    }

    NS <- sq[sel, vars]
    sel <- sq$name %in% paste0(paste0(nm, "-"), c("E", "W"))
    EW <- sq[sel, vars]
    coords <- squareCenterB(NS, EW)
  } else {
    coords <- data.frame(case = case, road.segment = sq$id, x = sq$x.proj,
      y = sq$y.proj, x.proj = sq$x.proj, y.proj = sq$y.proj, name = sq$name)
  }
  coords
}

## Landmark Functions ##

argyllHouse <- function() {
  NW <- roadSegmentData(seg.id = "116-2", var.sel = 2L)
  NE <- roadSegmentData(seg.id = "144-1", var.sel = 2L)
  SW <- roadSegmentData(seg.id = "161-1", var.sel = 2L)
  SE <- roadSegmentData(seg.id = "161-1", var.sel = 1L)
  argyll <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x, SW$y)
  argyll.house <- data.frame(x = argyll$x, y = argyll$y)
  seg.id <- "162-1"
  proj <- segmentTrigonometryAddress(seg.id = seg.id, delta = "neg")
  data.frame(case = 1011L, road.segment = seg.id, argyll.house,
    x.proj = proj$x, y.proj = proj$y, name = "Argyll House")
}

cravenChapel <- function() {
  vars <- c("x", "y")
  seg.id <- "229-1" # Fouberts Place

  sel <- cholera::road.segments$id == seg.id
  fouberts.pl <- cholera::road.segments[sel, ]

  fouberts <- rbind(stats::setNames(fouberts.pl[, paste0(vars, 1)], vars),
                    stats::setNames(fouberts.pl[, paste0(vars, 2)], vars))

  delta <- trignometricDelta(fouberts)
  left <- fouberts[which.min(fouberts$x), ] # West end
  x.new <- left$x + delta$x
  y.new <- left$y + delta$y

  ols <- stats::lm(y ~ x, data = fouberts)
  slope <- stats::coef(ols)[2]
  ortho.slope <- -1 / slope
  ortho.intercept <- y.new - ortho.slope * x.new

  theta <- ifelse(is.na(ortho.slope), pi / 2, atan(ortho.slope))
  # unitMeter(1, "yard") * 3 is appox. 177.2 ft
  # 0.5 nominal units approx 88 feet
  delta.x <- 0.5 * cos(theta)
  delta.y <- 0.5 * sin(theta)
  x.label <- x.new + delta.x
  y.label <- y.new + delta.y

  data.frame(case = 1012L, road.segment = seg.id, x = x.label, y = y.label,
    x.proj = x.new, y.proj = y.new, name = "Craven Chapel", row.names = NULL)
}

johnSnow <- function() {
  snow <- segmentTrigonometryAddress(seg.id = "508-1", factor = 2L)
  data.frame(case = 1013L, road.segment = "508-1", snow, x.proj = snow$x,
    y.proj = snow$y, name = "John Snow")
}

karlMarx <- function() {
  marx <- segmentTrigonometryAddress(seg.id = "174-1", factor = 2L)
  data.frame(case = 1014L, road.segment = "174-1", marx, x.proj = marx$x,
    y.proj = marx$y, name = "Karl Marx")
}

lionBrewery <- function() {
  vars <- c("x", "y")
  seg.id <- "187-1"
  data.label <- data.frame(x = 13.9022, y = 11.87315)

  sel <- cholera::road.segments$id == seg.id
  broad.st <- cholera::road.segments[sel, ]

  broad <- rbind(stats::setNames(broad.st[, paste0(vars, 1)], vars),
                stats::setNames(broad.st[, paste0(vars, 2)], vars))

  delta <- trignometricDelta(broad)

  left <- broad[which.min(broad$x), ]
  x.new <- left$x + delta$x
  y.new <- left$y + delta$y

  data.frame(case = 1015L, road.segment = seg.id, x = data.label$x,
    y = data.label$y, x.proj = x.new, y.proj = y.new, name = "Lion Brewery",
    row.names = NULL)
}

magistratesCourt <- function() {
  vars <- c('x', "y")

  # Great Marlborough Street #
  gt.marlb.st <- cholera::road.segments[cholera::road.segments$street == 151, ]
  gt.marlb <- rbind(stats::setNames(gt.marlb.st[, paste0(vars, 1)], vars),
                    stats::setNames(gt.marlb.st[, paste0(vars, 2)], vars))

  delta <- trignometricDelta(gt.marlb, factor = 3L) # appox 1/3 way along road
  x.est <- gt.marlb.st$x1 + delta$x
  y.est <- gt.marlb.st$y1 + delta$y

  ## Great Marlborough Street label coordinate ##
  ortho.slope <- -1 / roadTheta(gt.marlb)
  ortho.intercept <- y.est - ortho.slope * x.est

  # Marlbrough Mews - parallel road (same block) north of Great Marlborough #
  marlb.mews <- cholera::road.segments[cholera::road.segments$id == "116-2", ]
  mews <- rbind(stats::setNames(marlb.mews[, paste0(vars, 1)], vars),
                stats::setNames(marlb.mews[, paste0(vars, 2)], vars))

  ols <- stats::lm(y ~ x, data = mews)

  # orthogonal point of intersection from Magistrates Court on Marlborough Mews
  ortho.x <- (ortho.intercept - stats::coef(ols)[1]) /
             (stats::coef(ols)[2] - ortho.slope)
  ortho.y <- stats::coef(ols)["x"] * ortho.x + stats::coef(ols)["(Intercept)"]

  ortho.data <- rbind(data.frame(x = c(ortho.x), y = c(ortho.y)),
                      data.frame(x = c(x.est), y = c(y.est)))

  delta <- trignometricDelta(ortho.data)
  x.lab <- x.est - delta$x
  y.lab <- y.est - delta$y

  data.frame(case = 1016L, road.segment = "151-1", x = x.lab, y = y.lab,
    x.proj = x.est, y.proj = y.est, name = "Magistrates Court")
}

modelLodgingHouses <- function() {
  NW <- roadSegmentData(seg.id = "225-1", var.sel = 2L)
  NE <- roadSegmentData(seg.id = "225-1", var.sel = 1L)
  SW <- roadSegmentData(seg.id = "259-1", var.sel = 2L)
  SE <- roadSegmentData(seg.id = "259-1", var.sel = 1L)
  label.data <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x,
    SW$y)
  dat <- data.frame(case = 1017L, road.segment = "259-2", label.data)
  out <- assignLandmarkAddress(dat)
  out$name = "Model Lodging Houses"
  out
}

pantheonBazaar <- function() {
  vars <- c("x", "y")
  sel <- cholera::road.segments$name == "Winsley Street"
  pantheon.bazaar <- cholera::road.segments[sel, paste0(vars, 2)]
  names(pantheon.bazaar) <- vars
  data.frame(case = 1018L, road.segment = "73-1", pantheon.bazaar,
    x.proj = pantheon.bazaar$x, y.proj = pantheon.bazaar$y,
    name = "The Pantheon")
}

stJamesWorkhouse <- function() {
  vars <- c("x", "y")
  vars.proj <- paste0(vars, ".proj")
  sel <- cholera::road.segments$name == "St James Workhouse"
  workhouse <- cholera::road.segments[sel, c("id", paste0(vars, 1), "name")]
  names(workhouse)[1:3] <- c("road.segment", vars.proj)
  right <- cholera::road.segments[sel, paste0(vars, 1)]
  left <- cholera::road.segments[cholera::road.segments$id == "201-1",
    paste0(vars, 2)]
  dat <- stats::setNames(data.frame(rbind(unlist(right), unlist(left))), vars)
  h <- c(stats::dist(dat))
  ols <- stats::lm(y ~ x, dat)
  segment.slope <- stats::coef(ols)[2]
  theta <- atan(segment.slope)
  delta.x <- (h / 2) * cos(theta)
  delta.y <- (h / 2) * sin(theta)
  x.lab <- left$x2 + delta.x
  y.lab <- left$y2 + delta.y
  data.frame(case = 1019L, road.segment = "148-1",x = x.lab, y = y.lab,
    workhouse[, c("x.proj", "y.proj")], name = "St James Workhouse",
    row.names = NULL)
}

stLukesChurch <- function() {
  dat <- data.frame(case = 1019L, road.segment = "222-1", x = 14.94156,
    y = 11.25313)
  out <- assignLandmarkAddress(dat)
  out$name <- "St Luke's Church"
  out
}

## Auxilliary Functions ##

addressProportion <- function(seg.id = "174-1", landmark = "Karl Marx") {
  vars <- c("x", "y")
  seg <- cholera::road.segments[cholera::road.segments$id == seg.id, ]
  alpha <- stats::setNames(seg[, paste0(vars, 1)], vars)
  omega <- stats::setNames(seg[, paste0(vars, 2)], vars)
  seg.dist <- stats::dist(rbind(alpha, omega))
  lndmrk <- cholera::landmarks[cholera::landmarks$name == landmark, vars]
  lndmrk.dist <- stats::dist(rbind(alpha, lndmrk))
  c(lndmrk.dist / seg.dist)
}

assignLandmarkAddress <- function(dat) {
  vars <- c("x", "y")
  lndmrk <- dat[, vars]

  sel <- cholera::road.segments$id == dat$road.segment
  st.data <- cholera::road.segments[sel, ]
  st <- rbind(stats::setNames(st.data[, paste0(vars, 1)], vars),
              stats::setNames(st.data[, paste0(vars, 2)], vars))

  ols.st <- stats::lm(y ~ x, data = st)
  ortho.slope <- -1 / stats::coef(ols.st)[2]
  ortho.intercept <- lndmrk$y - ortho.slope * lndmrk$x

  x.proj <- (stats::coef(ols.st)[1] -  ortho.intercept) /
            (ortho.slope - stats::coef(ols.st)[2])
  y.proj <- x.proj * ortho.slope + ortho.intercept
  data.frame(dat, x.proj = x.proj, y.proj = y.proj, row.names = NULL)
}

pasteCoordsB <- function(dat, var1 = "x1", var2 = "y1") {
  vapply(seq_len(nrow(dat)), function(i) {
    paste(dat[i, c(var1, var2)], collapse = "-")
  }, character(1L))
}

roadSegmentData <- function(seg.id = "116-2", var.sel = 2L) {
  vars <- c("x", "y")
  sel <- cholera::road.segments$id == seg.id
  seg.data <- cholera::road.segments[sel, paste0(vars, var.sel)]
  out <- stats::setNames(seg.data, vars)
  row.names(out) <- NULL
  out
}

roadTheta <- function(dat) {
   ols <- stats::lm(y ~ x, data = dat)
   slope <- stats::coef(ols)[2]
   ifelse(is.na(slope), pi / 2, atan(slope))
}

segmentTrigonometryAddress <- function(seg.id = "174-1", factor = 2L,
  delta = "pos") {
  vars <- c("x", "y")
  seg <- cholera::road.segments[cholera::road.segments$id == seg.id, ]
  alpha <- stats::setNames(seg[, paste0(vars, 1)], vars)
  omega <- stats::setNames(seg[, paste0(vars, 2)], vars)
  dat <- rbind(alpha, omega)
  h <- c(stats::dist(dat)) / factor
  theta <- roadTheta(dat)
  delta.x <- h * cos(theta)
  delta.y <- h * sin(theta)
  if (delta == "pos") {
    data.frame(x = alpha$x + delta.x, y = alpha$y + delta.y, row.names = NULL)
  } else if (delta == "neg") {
    data.frame(x = alpha$x - delta.x, y = alpha$y - delta.y, row.names = NULL)
  }
}

trignometricDelta <- function(dat, factor = 2L) {
   h <- c(stats::dist(dat))
   theta <- roadTheta(dat)
   delta.x <- (h / factor) * cos(theta)
   delta.y <- (h / factor) * sin(theta)
   data.frame(x = delta.x, y = delta.y, row.names = NULL)
}
