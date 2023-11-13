#' Landmark data.
#'
#' Nominal and orthogonal coordinates
#' @noRd
#' @note Uses road segments that enter square(s) as entry points.

landmarkDataB <- function() {
  vars <- c("x", "y")
  vars.proj <- c("x.proj", "y.proj")

  # Karl Marx #
  # marx <- data.frame(x = 17.3855, y = 13.371) # 28 Dean Street, 174-1
  # points(marx, pch = 16, cex = 0.5)
  # text(marx, label = "Karl\n Marx", cex = 0.5)
  # cholera:::addressProportion("174-1", "Karl Marx") # 0.5000003
  marx <- segmentTrignometryAddress(seg.id = "174-1", factor = 2L)
  points(marx, pch = 16, cex = 0.5)
  text(marx, label = "Karl\n Marx", cex = 0.5)

  # John Snow #
  # snow <- data.frame(x = 10.22414, y = 4.383851) # H: 18 Sackville Street, 508-1
  # points(snow, pch = 16, cex = 0.5)
  # text(snow, label = "John\n Snow", cex = 0.5)
  # snow.office <- data.frame(x = , y = ) # O: 54 Frith Street
  # cholera:::addressProportion("508-1", "John Snow") # 0.4999993
  snow <- segmentTrignometryAddress(seg.id = "508-1", factor = 2L)
  points(snow, pch = 16, cex = 0.5)
  text(snow, label = "John\n Snow", cex = 0.5)

  # St Luke's Church #
  # Berwick Street, currently Kemp House across from Tyler's Court
  st.lukes.church <- data.frame(x = 14.94156, y = 11.25313)
  points(st.lukes.church, pch = 16, cex = 0.5)
  text(st.lukes.church, label = "St\n Lukes", cex = 0.5)
  st.lukes.church.proj <- assignLandmarkAddress(seg.id = "222-1",
    landmark.id = 20003L)
  points(st.lukes.church.proj[, c("x.proj", "y.proj")], pch = 0, cex = 0.5)

  # Lion Brewery #
  # 50 Broad Street; Huggins Brewery (?)
  lion.brewery <- data.frame(x = 13.9022, y = 11.87315)
  points(lion.brewery, pch = 16, cex = 0.5)
  text(lion.brewery, label = "Lion\n Brewery", cex = 0.5)
  lion.brewery.proj <- lionBrewery()
  points(lion.brewery.proj[, c("x.proj", "y.proj")], pch = 0, cex = 0.5)

  # The Pantheon #
  # Today Marks & Spencers at 173 Oxford Street
  # I placed at intersection of Oxford and Winsley
  pantheon.bazaar <- cholera::road.segments[cholera::road.segments$name ==
    "Winsley Street", paste0(vars, 2)]
  names(pantheon.bazaar) <- vars

  # St James Workhouse #
  st.james.workhouse <- cholera::road.segments[cholera::road.segments$name ==
    "St James Workhouse", c("id", "x1", "y1", "name")]
  names(st.james.workhouse)[1:3] <- c("road.segment", vars.proj)
  st.james.workhouse$ortho.dist <- 0
  st.james.workhouse <- stats::setNames(st.james.workhouse[, vars.proj], vars)

  # Argyll House -- Lord Aberdeen #
  # The London Palladium
  # https://www.british-history.ac.uk/survey-london/vols31-2/pt2/pp284-307#h3-0010
  # "frontages in both Argyll Street and Great Marlborough Street."
  # entrance on Argyll Street
  NW <- roadSegmentData(seg.id = "116-2", var.sel = 2L)
  NE <- roadSegmentData(seg.id = "144-1", var.sel = 2L)
  SW <- roadSegmentData(seg.id = "161-1", var.sel = 2L)
  SE <- roadSegmentData(seg.id = "161-1", var.sel = 1L)
  argyll <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x, SW$y)
  argyll.house <- data.frame(x = argyll$x, y = argyll$y)

  # Model Lodging Houses #
  # Hopkins Street "The Cholera in Berwick Street" by Rev. Henry Whitehead
  # segment IDs: "245-1"
  # Ingestre Buildings
  # New Street/Husband Street -> Ingestre Place (now)
  NW <- roadSegmentData(seg.id = "225-1", var.sel = 2L)
  NE <- roadSegmentData(seg.id = "225-1", var.sel = 1L)
  SW <- roadSegmentData(seg.id = "259-1", var.sel = 2L)
  SE <- roadSegmentData(seg.id = "259-1", var.sel = 1L)
  model.lodging <- segmentIntersection(NW$x, NW$y, SE$x, SE$y,
                                       NE$x, NE$y, SW$x, SW$y)
  model.lodging.proj <- assignLandmarkAddress(seg.id = "245-1",
    landmark.id = 20008L)

  # Craven Chapel #
  # Berwick Street
  craven.chapel <- cravenChapel()

  ## Squares ##

  golden.square <- squareExitsB("Golden Square")
  golden.square$name <- paste0("Golden Square-", c("W", "E", "S", "N"))

  sel <- golden.square$name %in% c("Golden Square-N", "Golden Square-S")
  golden.NS <- golden.square[sel, vars]
  sel <- golden.square$name %in% c("Golden Square-E", "Golden Square-W")
  golden.EW <- golden.square[sel, vars]

  golden <- squareCenterB(golden.NS, golden.EW)
  text(golden, labels = "Golden\nSquare")

  #

  soho.square <- squareExitsB("Soho Square")
  soho.square$name <- paste0("Soho Square-", c("E", "N", "S3", "S2", "S1", "W"))

  sel <- soho.square$name %in% c("Soho Square-N", "Soho Square-S2")
  soho.NS <- soho.square[sel, vars]
  sel <- soho.square$name %in% c("Soho Square-E", "Soho Square-W")
  soho.EW <- soho.square[sel, vars]

  soho <- squareCenterB(soho.NS, soho.EW)
  text(soho, labels = "Soho\nSquare")

  soho <- lapply(soho.square$name, function(nm) {
    soho.square[soho.square$name == nm, c("x", "y")]
  })

  golden <- lapply(golden.square$name, function(nm) {
    golden.square[golden.square$name == nm, c("x", "y")]
  })

  landmarks <- list(marx, snow, st.lukes.church, lion.brewery, pantheon.bazaar,
    st.james.workhouse, argyll.house, model.lodging, craven.chapel)
  landmarks <- append(landmarks, soho)
  landmarks <- append(landmarks, golden)

  landmark.names <- c("Karl Marx", "John Snow", "St Luke's Church",
    "Lion Brewery", "The Pantheon", "St James Workhouse", "Argyll House",
    "Model Lodging", "Craven Chapel", soho.square$name, golden.square$name)

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

  # Marlborough Street Magistrates Court #
  # 19–21 Great Marlborough Street
  # 51°30′51.62″N 0°8′22.13″W
  magistrates.court <- magistratesCourt()
  out <- rbind(out, magistrates.court)
  out <- out[order(out$case), ]
  row.names(out) <- NULL
}

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

argyllHouse <- function() {
  NW <- roadSegmentData(seg.id = "116-2", var.sel = 2L)
  NE <- roadSegmentData(seg.id = "144-1", var.sel = 2L)
  SW <- roadSegmentData(seg.id = "161-1", var.sel = 2L)
  SE <- roadSegmentData(seg.id = "161-1", var.sel = 1L)
  argyll <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x, SW$y)
  data.frame(x = argyll$x, y = argyll$y) 
}

# Default is St Luke's Church
assignLandmarkAddress <- function(seg.id = "222-1", landmark.id = 20003L) {
  vars <- c("x", "y")

  sel <- cholera::road.segments$id == seg.id
  st.data <- cholera::road.segments[sel, ]
  st <- rbind(stats::setNames(st.data[, paste0(vars, 1)], vars),
              stats::setNames(st.data[, paste0(vars, 2)], vars))

  sel <- cholera::landmarks$case == landmark.id
  lndmrk <- cholera::landmarks[sel, vars]

  ols.st <- stats::lm(y ~ x, data = st)
  ortho.slope <- -1 / stats::coef(ols.st)[2]
  ortho.intercept <- lndmrk$y - ortho.slope * lndmrk$x

  x.proj <- (stats::coef(ols.st)[1] -  ortho.intercept) /
            (ortho.slope - stats::coef(ols.st)[2])
  y.proj <- x.proj * ortho.slope + ortho.intercept

  data.frame(case = landmark.id, road.segment = seg.id, x.proj = x.proj,
             y.proj = y.proj, ortho.dist = 0, row.names = NULL)
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

  data.frame(case = 20009L, road.segment = seg.id, x = x.label, y = y.label,
    x.proj = x.new, y.proj = y.new, ortho.dist = 0, row.names = NULL)
}

lionBrewery <- function() {
   vars <- c("x", "y")
   seg.id <- "187-1"

   sel <- cholera::road.segments$id == seg.id
   broad.st <- cholera::road.segments[sel, ]

   broad <- rbind(stats::setNames(broad.st[, paste0(vars, 1)], vars),
                  stats::setNames(broad.st[, paste0(vars, 2)], vars))

   delta <- trignometricDelta(broad)

   left <- broad[which.min(broad$x), ]
   x.new <- left$x + delta$x
   y.new <- left$y + delta$y

   data.frame(case = 20004L, road.segment = seg.id, x.proj = x.new,
      y.proj = y.new, ortho.dist = 0)
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

  data.frame(case = 20020L, road.segment = "151-1", x.proj = x.est,
    y.proj = y.est, ortho.dist = 0, x = x.lab, y = y.lab,
    name = "Magistrates Court")
}

modelLodgingHouses <- function() {
  NW <- roadSegmentData(seg.id = "225-1", var.sel = 2L)
  NE <- roadSegmentData(seg.id = "225-1", var.sel = 1L)
  SW <- roadSegmentData(seg.id = "259-1", var.sel = 2L)
  SE <- roadSegmentData(seg.id = "259-1", var.sel = 1L)
  label.data <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x,
    SW$y)
  proj <- assignLandmarkAddress(seg.id = "245-1",
    landmark.id = 20008L)  
}

pantheonBazaar <- function() {
  vars <- c("x", "y")
  sel <- cholera::road.segments$name == "Winsley Street"
  out <- cholera::road.segments[sel, paste0(vars, 2)]
  names(out) <- vars
  out
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

squareCenterB <- function(NS, EW, latlong = FALSE) {
  if (isTRUE(latlong)) {
    line.NS <- stats::lm(lat ~ lon, data = NS)
    line.EW <- stats::lm(lat ~ lon, data = EW)
  } else {
    line.NS <- stats::lm(y ~ x, data = NS)
    line.EW <- stats::lm(y ~ x, data = EW)
  }

  slope.delta <- stats::coef(line.NS)[2] - stats::coef(line.EW)[2]
  int.delta <- stats::coef(line.EW)[1] - stats::coef(line.NS)[1]
  x.val <- int.delta / slope.delta
  y.val <- stats::coef(line.EW)[2] * x.val + stats::coef(line.EW)[1]

  out <- data.frame(x = x.val, y = y.val, row.names = NULL)
  if (isTRUE(latlong)) names(out) <- c("lon", "lat")
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
      names(candidate)[grep(1, names(candidate))] <- vars0
    } else if (any(twos)) {
      candidate <- candidate[, c(vars, paste0(vars0, 2))]
      names(candidate)[grep(2, names(candidate))] <- vars0
    }

    candidate
  }))
}

segmentTrigonometryAddress <- function(seg.id = "174-1", factor = 2L) {
  vars <- c("x", "y")
  seg <- cholera::road.segments[cholera::road.segments$id == seg.id, ]
  alpha <- stats::setNames(seg[, paste0(vars, 1)], vars)
  omega <- stats::setNames(seg[, paste0(vars, 2)], vars)
  dat <- rbind(alpha, omega)
  h <- c(stats::dist(dat)) / factor
  theta <- roadTheta(dat)
  delta.x <- h * cos(theta)
  delta.y <- h * sin(theta)
  data.frame(x = alpha$x + delta.x, y = alpha$y + delta.y, row.names = NULL)
}

stJamesWorkhouse <- function() {
  vars <- c("x", "y")
  vars.proj <- c("x.proj", "y.proj")
  workhouse <- cholera::road.segments[cholera::road.segments$name ==
    "St James Workhouse", c("id", paste0(vars, 2), "name")]
  names(workhouse)[1:3] <- c("road.segment", vars.proj)
  workhouse$ortho.dist <- 0
  stats::setNames(workhouse[, vars.proj], vars)
}

stLukesChurch <- function() {
  dat <- data.frame(x = 14.94156, y = 11.25313)
  proj <- assignLandmarkAddress(seg.id = "222-1", landmark.id = 20003L)
  cbind(proj[, c("case", "road.segment")], dat, 
    proj[, c("x.proj", "y.proj", "ortho.dist")])
}

trignometricDelta <- function(dat, factor = 2L) {
   h <- c(stats::dist(dat))
   theta <- roadTheta(dat)
   delta.x <- (h / factor) * cos(theta)
   delta.y <- (h / factor) * sin(theta)
   data.frame(x = delta.x, y = delta.y, row.names = NULL)
}