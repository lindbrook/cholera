#' Landmark data.
#'
#' Nominal and orthogonal coordinates
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @noRd
#' @note Uses road segments that enter square(s) as entry points.

landmarkData <- function(multi.core = FALSE) {
  cores <- multiCore(multi.core)

  ## squares ##

  # golden.square <- Squares("Golden Square")
  # soho.square <- Squares("Soho Square")

  ## other landmarks #

  # Argyll House -- Lord Aberdeen #
  # The London Palladium
  # https://www.british-history.ac.uk/survey-london/vols31-2/pt2/pp284-307#h3-0010
  # "frontages in both Argyll Street and Great Marlborough Street."
  # entrance on Argyll Street
  # argyll.house <- argyllHouse()

  # Craven Chapel #
  # Berwick Street
  # craven.chapel <- cravenChapel()

  # Lion Brewery #
  # 50 Broad Street; Huggins Brewery (?)
  # lion.brewery <- lionBrewery()

  # Marlborough Street Magistrates Court #
  # 19–21 Great Marlborough Street
  # 51°30′51.62″N 0°8′22.13″W
  # magistrates.court <- magistratesCourt()

  # Karl Marx #
  # 28 Dean Street, 174-1
  # cholera:::addressProportion("174-1", "Karl Marx") # 0.5000003
  # marx <- karlMarx()

  # Model Lodging Houses #
  # Hopkins Street "The Cholera in Berwick Street" by Rev. Henry Whitehead
  # segment IDs: "245-1"
  # Ingestre Buildings
  # New Street/Husband Street -> Ingestre Place (now)
  # model.lodging.houses <- modelLodgingHouses()

  # The Pantheon #
  # Today Marks & Spencers at 173 Oxford Street
  # placed at intersection of Oxford and Winsley
  # pantheon.bazaar <- pantheonBazaar()

  # St James Workhouse #
  # address set on Poland Street
  # st.james.workhouse <- stJamesWorkhouse()

  # St Luke's Church #
  # Berwick Street, currently Kemp House across from Tyler's Court
  # st.lukes.church <- stLukesChurch()

  # John Snow #
  # H: 18 Sackville Street, "508-1"
  # cholera:::addressProportion("508-1", "John Snow") # 0.4999993
  # snow <- johnSnow()

  # out <- rbind(golden.square, soho.square, argyll.house, craven.chapel,
  #              lion.brewery, magistrates.court, marx, model.lodging.houses,
  #              pantheon.bazaar, st.james.workhouse, st.lukes.church, snow)
  # row.names(out) <- NULL
  # out

  sq.data <- lapply(list("Golden Square", "Soho Square"), Squares)

  fns <- list(argyllHouse,
              cravenChapel,
              lionBrewery,
              magistratesCourt,
              karlMarx,
              modelLodgingHouses,
              pantheonBazaar,
              stJamesWorkhouse,
              stLukesChurch,
              johnSnow)

  other.lndmrks <- parallel::mclapply(seq_along(fns), function(i) fns[[i]](),
    mc.cores = cores)

  do.call(rbind, c(sq.data, other.lndmrks))
}

# landmarks <- cholera:::landmarkData()
# usethis::use_data(landmarks, overwrite = TRUE)

## Square(s) Functions ##

landmarkSquares <- function() {
  golden <- Squares("Golden Square", label.coord = TRUE)
  soho <- Squares("Soho Square", label.coord = TRUE)
  rbind(golden, soho)
}

# landmark.squares <- cholera:::landmarkSquares()
# usethis::use_data(landmark.squares, overwrite = TRUE)

squareCenter <- function(NS, EW, latlong = FALSE) {
  if (latlong) {
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
  if (latlong) names(out) <- c("lon", "lat")
  out
}

squareExits <- function(nm = "Golden Square", latlong = FALSE) {
  if (latlong) {
    rd.seg <- roadSegments(latlong = TRUE)
    dat <- rd.seg[rd.seg$name == nm, ]
    vars <- c("lon", "lat")
  } else {
    dat <- cholera::road.segments[cholera::road.segments$name == nm, ]
    vars <- c("x", "y")
  }

  if (latlong) {
    left <- pasteCoordsB(dat, paste0(vars[1], 1), paste0(vars[2], 1))
    right <- pasteCoordsB(dat, paste0(vars[1], 2), paste0(vars[2], 2))
  } else {
    left <- pasteCoordsB(dat)
    right <- pasteCoordsB(dat, paste0(vars[1], 2), paste0(vars[2], 2))
  }

  mat <- do.call(rbind, lapply(strsplit(union(left, right), "_&_"), as.numeric))

  if (latlong) exit.nodes <- data.frame(lon = mat[, 1], lat = mat[, 2])
  else exit.nodes <- data.frame(x = mat[, 1], y = mat[, 2])

  do.call(rbind, lapply(seq_len(nrow(exit.nodes)), function(i) {
    if (latlong) {
      sel1 <- signif(rd.seg$lon1) == signif(exit.nodes[i, ]$lon) &
              signif(rd.seg$lat1) == signif(exit.nodes[i, ]$lat)
      sel2 <- signif(rd.seg$lon2) == signif(exit.nodes[i, ]$lon) &
              signif(rd.seg$lat2) == signif(exit.nodes[i, ]$lat)
      node.segs <- rd.seg[sel1 | sel2, ]
    } else {
      sel1 <- cholera::road.segments$x1 == exit.nodes[i, ]$x &
              cholera::road.segments$y1 == exit.nodes[i, ]$y
      sel2 <- cholera::road.segments$x2 == exit.nodes[i, ]$x &
              cholera::road.segments$y2 == exit.nodes[i, ]$y
      node.segs <- cholera::road.segments[sel1 | sel2, ]
    }

    candidate <- node.segs[!grepl(nm, node.segs$name), ]
    square.segs <- node.segs[grepl(nm, node.segs$name), ]

    sq.coords <- unique(rbind(
      stats::setNames(square.segs[, paste0(vars, 1)], vars),
      stats::setNames(square.segs[, paste0(vars, 2)], vars)
    ))

    ones <- vapply(seq_len(nrow(sq.coords)), function(i) {
      sq.tmp <- sq.coords[i, vars]
      cand.tmp <- candidate[, paste0(vars, 1)]
      if (latlong) {
          identical(cand.tmp$lon1, sq.tmp$lon) &
          identical(cand.tmp$lat1, sq.tmp$lat)
        } else {
          identical(cand.tmp$x1, sq.tmp$x) & identical(cand.tmp$y1, sq.tmp$y)
        }
    }, logical(1L))

    twos <- vapply(seq_len(nrow(sq.coords)), function(i) {
      sq.tmp <- sq.coords[i, vars]
      cand.tmp <- candidate[, paste0(vars, 2)]
      if (latlong) {
          identical(cand.tmp$lon2, sq.tmp$lon) &
          identical(cand.tmp$lat2, sq.tmp$lat)
        } else {
          identical(cand.tmp$x2, sq.tmp$x) & identical(cand.tmp$y2, sq.tmp$y)
        }
    }, logical(1L))

    if (latlong) {
      sel <- !grepl("lon", names(dat)) & !grepl("lat", names(dat))
    } else {
      sel <- !grepl("x", names(dat)) & !grepl("y", names(dat))
    }

    vars0 <- names(dat)[sel]

    if (any(ones)) {
      candidate <- candidate[, c(vars0, paste0(vars, 1))]
      names(candidate)[grep(1, names(candidate))] <- vars
    } else if (any(twos)) {
      candidate <- candidate[, c(vars0, paste0(vars, 2))]
      names(candidate)[grep(2, names(candidate))] <- vars
    }

    candidate
  }))
}

Squares <- function(nm = "Golden Square", label.coord = FALSE) {
  sq.nominal <- squareExits(nm)
  sq.latlong <- squareExits(nm, latlong = TRUE)
  sq <- merge(sq.nominal, sq.latlong[, c("id", "lon", "lat")],
    by = "id")

  if (nm == "Golden Square") {
    exits <- c("N", "W", "E", "S")
    start <- 1002L
  } else if (nm == "Soho Square") {
    exits <- c( "W", "S1", "S2",  "S3", "N", "E")
    start <- 1006L
  } else stop('nm must be "Golden Square" or "Soho Square"', call. = FALSE)

  sq$name <- paste0(nm, "-", exits)

  if (label.coord) {
    if (nm == "Golden Square") {
      sel <- sq$name %in% paste0(paste0(nm, "-"), c("N", "S"))
      case <- 1000L
    } else if (nm == "Soho Square") {
      sel <- sq$name %in% paste0(paste0(nm, "-"), c("N", "S2"))
      case <- 1001L
    }

    vars <- c("x", "y")
    NS <- sq[sel, vars]
    sel <- sq$name %in% paste0(paste0(nm, "-"), c("E", "W"))
    EW <- sq[sel, vars]
    coords.nominal <- squareCenter(NS, EW)

    if (nm == "Golden Square") {
      sel <- sq$name %in% paste0(paste0(nm, "-"), c("N", "S"))
    } else if (nm == "Soho Square") {
      sel <- sq$name %in% paste0(paste0(nm, "-"), c("N", "S2"))
    }

    vars <- c("lon", "lat")
    NS <- sq[sel, vars]
    sel <- sq$name %in% paste0(paste0(nm, "-"), c("E", "W"))
    EW <- sq[sel, vars]
    coords.latlong <- squareCenter(NS, EW, latlong = TRUE)

    out <- data.frame(case = case, coords.nominal, coords.latlong, name = nm)

  } else {
    coords <- data.frame(road.segment = sq$id,
                         x = sq$x,
                         y = sq$y,
                         x.lab = sq$x,
                         y.lab = sq$y,
                         lon = sq$lon,
                         lat = sq$lat,
                         lon.lab = sq$lon,
                         lat.lab = sq$lat,
                         name = sq$name)

    if (nm == "Golden Square") {
      ordered.exit <- c("-N", "-E", "-S", "-W")
    } else if (nm == "Soho Square") {
      ordered.exit <- c("-N", "-E", "-S1", "-S2", "-S3", "-W")
    }

    ord <- vapply(ordered.exit, function(x) grep(x, coords$name), integer(1L))
    coords <- coords[ord, ]
    case <- seq(start, start + length(exits) - 1)
    out <- data.frame(case, coords, row.names = NULL)
  }
  out
}

## Landmark Functions ##

argyllHouse <- function() {
  NW <- roadSegEndpt(seg.id = "116-2", endpt.sel = 2L)
  NE <- roadSegEndpt(seg.id = "144-1", endpt.sel = 2L)
  SW <- roadSegEndpt(seg.id = "161-1", endpt.sel = 2L)
  SE <- roadSegEndpt(seg.id = "161-1", endpt.sel = 1L)
  argyll <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x, SW$y)
  label.nominal <- data.frame(x.lab = argyll$x, y.lab = argyll$y)

  origin <- data.frame(lon = min(cholera::roads[, "lon"]),
                       lat = min(cholera::roads[, "lat"]))

  NW <- roadSegEndpt(seg.id = "116-2", endpt.sel = 2L, latlong = TRUE)
  NE <- roadSegEndpt(seg.id = "144-1", endpt.sel = 2L, latlong = TRUE)
  SW <- roadSegEndpt(seg.id = "161-1", endpt.sel = 2L, latlong = TRUE)
  SE <- roadSegEndpt(seg.id = "161-1", endpt.sel = 1L, latlong = TRUE)

  geo.cartesian <- lapply(list(NW, NE, SW, SE), function(coords) {
    x.proj <- c(coords$lon, origin$lat)
    y.proj <- c(origin$lon, coords$lat)
    m.lon <- geosphere::distGeo(y.proj, coords)
    m.lat <- geosphere::distGeo(x.proj, coords)
    data.frame(x = m.lon, y = m.lat)
  })

  names(geo.cartesian) <- c("NW", "NE", "SW", "SE")
  NW <- geo.cartesian$NW
  NE <- geo.cartesian$NE
  SW <- geo.cartesian$SW
  SE <- geo.cartesian$SE
  argyll <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x, SW$y)

  label.latlong <- meterLatLong(argyll)[,  c("lon", "lat")]

  seg.id <- "162-1"
  proj <- segmentTrigonometryAddress(seg.id = seg.id, delta = "neg")
  geo <- segmentTrigonometryAddress(seg.id = seg.id, delta = "neg",
    latlong = TRUE)
  data.frame(case = 1012L,
             road.segment = seg.id,
             proj,
             label.nominal,
             geo,
             lon.lab = label.latlong$lon,
             lat.lab = label.latlong$lat,
             name = "Argyll House",
             row.names = NULL)
}

cravenChapel <- function() {
  vars <- c("x", "y")
  seg.id <- "229-1" # Fouberts Place

  # nominal proj coordinates #

  sel <- cholera::road.segments$id == seg.id
  fouberts.pl <- cholera::road.segments[sel, ]

  fouberts <- rbind(stats::setNames(fouberts.pl[, paste0(vars, 1)], vars),
                    stats::setNames(fouberts.pl[, paste0(vars, 2)], vars))

  delta <- trignometricDelta(fouberts)
  left <- fouberts[which.min(fouberts$x), ] # West end
  x.proj <- left$x + delta$x
  y.proj <- left$y + delta$y

  # nominal label coordinates #

  ols <- stats::lm(y ~ x, data = fouberts)
  slope <- stats::coef(ols)[2]
  ortho.slope <- -1 / slope
  ortho.intercept <- y.proj - ortho.slope * x.proj

  theta <- ifelse(is.na(ortho.slope), pi / 2, atan(ortho.slope))
  # unitMeter(1, "yard") * 3 is appox. 177.2 ft
  # 0.5 nominal units approx 88 feet
  delta.x <- 0.5 * cos(theta)
  delta.y <- 0.5 * sin(theta)
  x.label <- x.proj + delta.x
  y.label <- y.proj + delta.y

  # latlong street address coordinates #

  proj.latlong <- segmentTrigonometryAddress("229-1", latlong = TRUE)

  # latlong label coordinates #

  rd.segs <- roadSegments(latlong = TRUE)
  vars <- c("lon", "lat")
  fouberts.pl <- rd.segs[rd.segs$id == seg.id, ]

  origin <- data.frame(lon = min(cholera::roads[, "lon"]),
                       lat = min(cholera::roads[, "lat"]))

  proj.cartesian <- projCartesian(proj.latlong, origin)
  coord.cartesian <- coordsCartesian(proj.latlong, origin)

  h <- stats::dist(rbind(proj.cartesian, coord.cartesian)) # 54 units
  delta.x <- h * cos(theta)
  delta.y <- h * sin(theta)
  proj.label <- data.frame(x = proj.cartesian$x + delta.x,
                           y = proj.cartesian$y + delta.y)

  label.latlong <- meterLatLong(proj.label)

  data.frame(case = 1013L,
             road.segment = seg.id,
             x = x.proj,
             y = y.proj,
             x.lab = x.label,
             y.lab = y.label,
             proj.latlong,
             lon.lab = label.latlong$lon,
             lat.lab = label.latlong$lat,
             name = "Craven Chapel",
             row.names = NULL)
}

lionBrewery <- function() {
  vars <- c("x", "y")
  seg.id <- "187-1"

  NW <- roadSegEndpt(seg.id = seg.id, endpt.sel = 1L)
  NE <- roadSegEndpt(seg.id = seg.id, endpt.sel = 2L)
  SW <- roadSegEndpt(seg.id = "225-1", endpt.sel = 2L)
  SE <- roadSegEndpt(seg.id = "225-1", endpt.sel = 1L)
  label.nominal <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x,
    SW$y)

  origin <- data.frame(lon = min(cholera::roads[, "lon"]),
                       lat = min(cholera::roads[, "lat"]))

  NW <- roadSegEndpt(seg.id = seg.id, endpt.sel = 1L, latlong = TRUE)
  NE <- roadSegEndpt(seg.id = seg.id, endpt.sel = 2L, latlong = TRUE)
  SW <- roadSegEndpt(seg.id = "225-1", endpt.sel = 2L, latlong = TRUE)
  SE <- roadSegEndpt(seg.id = "225-1", endpt.sel = 1L, latlong = TRUE)

  geo.cartesian <- lapply(list(NW, NE, SW, SE), function(coords) {
    x.proj <- c(coords$lon, origin$lat)
    y.proj <- c(origin$lon, coords$lat)
    m.lon <- geosphere::distGeo(y.proj, coords)
    m.lat <- geosphere::distGeo(x.proj, coords)
    data.frame(x = m.lon, y = m.lat)
  })

  names(geo.cartesian) <- c("NW", "NE", "SW", "SE")
  NW <- geo.cartesian$NW
  NE <- geo.cartesian$NE
  SW <- geo.cartesian$SW
  SE <- geo.cartesian$SE
  lion <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x, SW$y)

  label.latlong <- meterLatLong(lion)[, c("lon", "lat")]

  proj.nominal <- segmentTrigonometryAddress(seg.id = seg.id, delta = "pos")
  proj.latlong <- segmentTrigonometryAddress(seg.id = seg.id, delta = "pos",
    latlong = TRUE)

  data.frame(case = 1014L,
             road.segment = seg.id,
             proj.nominal,
             x.lab = label.nominal$x,
             y.lab = label.nominal$y,
             proj.latlong,
             lon.lab = label.latlong$lon,
             lat.lab = label.latlong$lat,
             name = "Lion Brewery",
             row.names = NULL)
}

magistratesCourt <- function() {
  vars <- c('x', "y")
  seg.id <- "151-1"

  proj.nominal <- segmentTrigonometryAddress(seg.id = seg.id, factor = 3L,
    delta = "pos")
  proj.latlong <- segmentTrigonometryAddress(seg.id = seg.id, factor = 3L,
    delta = "pos", latlong = TRUE)

  ## xy label ##

  # Great Marlborough Street #
  seg <- cholera::road.segments[cholera::road.segments$id == seg.id, ]
  gt.marlb <- rbind(stats::setNames(seg[, paste0(vars, 1)], vars),
                    stats::setNames(seg[, paste0(vars, 2)], vars))

  ## Great Marlborough Street label coordinate ##
  ortho.slope <- -1 / roadTheta(gt.marlb)
  ortho.intercept <- proj.nominal$y - ortho.slope * proj.nominal$x

  # Marlbrough Mews - parallel road, on same block, north of Great Marlborough #
  seg <- cholera::road.segments[cholera::road.segments$id == "116-2", ]
  mews <- rbind(stats::setNames(seg[, paste0(vars, 1)], vars),
                stats::setNames(seg[, paste0(vars, 2)], vars))

  ols.mews <- stats::lm(y ~ x, data = mews)

  # orthogonal point of intersection from Magistrates Court on Marlborough Mews
  ortho.x <- (ortho.intercept - stats::coef(ols.mews)["(Intercept)"]) /
             (stats::coef(ols.mews)["x"] - ortho.slope)

  ortho.y <- stats::coef(ols.mews)["x"] * ortho.x +
             stats::coef(ols.mews)["(Intercept)"]

  ortho.data <- rbind(data.frame(x = ortho.x, y = ortho.y),
                      data.frame(x = proj.nominal$x, y = proj.nominal$y))

  delta <- trignometricDelta(ortho.data)
  x.lab <- proj.nominal$x - delta$x
  y.lab <- proj.nominal$y - delta$y

  ## latlong label ##

  vars <- c("lon", "lat")
  rd.segs <- roadSegments(latlong = TRUE)

  seg <- rd.segs[rd.segs$id == "151-1", ]
  gt.marlb <- rbind(stats::setNames(seg[, paste0(vars, 1)], vars),
                    stats::setNames(seg[, paste0(vars, 2)], vars))

  # Marlbrough Mews - parallel road, on same block, north of Great Marlborough #
  seg <- rd.segs[rd.segs$id == "116-2", ]
  mews <- rbind(stats::setNames(seg[, paste0(vars, 1)], vars),
                stats::setNames(seg[, paste0(vars, 2)], vars))

  origin <- data.frame(lon = min(cholera::roads[, "lon"]),
                       lat = min(cholera::roads[, "lat"]))

  cartesian <- lapply(list(gt.marlb, mews), function(coords) {
    do.call(rbind, lapply(1:2, function(i) {
      x.proj <- c(coords[i, "lon"], origin$lat)
      y.proj <- c(origin$lon, coords[i, "lat"])
      m.lon <- geosphere::distGeo(y.proj, coords[i, ])
      m.lat <- geosphere::distGeo(x.proj, coords[i, ])
      data.frame(x = m.lon, y = m.lat)
    }))
  })

  names(cartesian) <- c("gt.marlb", "mews")

  proj.cartesian <- projCartesian(proj.latlong, origin)

  ## Great Marlborough Street latlong label coordinate ##
  ortho.slope <- -1 / roadTheta(cartesian$gt.marlb)
  ortho.intercept <- proj.cartesian$y - ortho.slope * proj.cartesian$x
  ols.mews <- stats::lm(y ~ x, data = cartesian$mews)

  # orthogonal point of intersection from  Marlborough Mews to Magistrates Court
  ortho.x <- (ortho.intercept - stats::coef(ols.mews)["(Intercept)"]) /
             (stats::coef(ols.mews)["x"] - ortho.slope)

  ortho.y <- stats::coef(ols.mews)["x"] * ortho.x +
             stats::coef(ols.mews)["(Intercept)"]

  ortho.data <- rbind(data.frame(x = ortho.x, y = ortho.y),
                      data.frame(x = proj.cartesian$x, y = proj.cartesian$y))

  delta <- trignometricDelta(ortho.data)
  label.cartesian <- data.frame(x = proj.cartesian$x - delta$x,
                                y = proj.cartesian$y - delta$y)

  label.latlong <- meterLatLong(label.cartesian)

  data.frame(case = 1015L,
             road.segment = seg.id,
             proj.nominal,
             x.lab = x.lab,
             y.lab = y.lab,
             proj.latlong,
             lon.lab = label.latlong$lon,
             lat.lab = label.latlong$lat,
             name = "Magistrates Court",
             row.names = NULL)
}

karlMarx <- function() {
  seg.id <- "174-1" # Dean Street
  proj.nominal <- segmentTrigonometryAddress(seg.id = seg.id, factor = 2L)
  proj.latlong <- segmentTrigonometryAddress(seg.id = seg.id, delta = "pos",
    latlong = TRUE)
  data.frame(case = 1016L,
             road.segment = seg.id,
             proj.nominal,
             x.lab = proj.nominal$x,
             y.lab = proj.nominal$y,
             proj.latlong,
             lon.lab = proj.latlong$lon,
             lat.lab = proj.latlong$lat,
             name = "Karl Marx",
             row.names = NULL)
}

modelLodgingHouses <- function() {
  ## nominal label ##
  NW <- roadSegEndpt(seg.id = "225-1", endpt.sel = 2L)
  NE <- roadSegEndpt(seg.id = "225-1", endpt.sel = 1L)
  SW <- roadSegEndpt(seg.id = "259-1", endpt.sel = 2L)
  SE <- roadSegEndpt(seg.id = "259-1", endpt.sel = 1L)
  label.nominal <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x,
    SW$y)

  ## nominal address (proj): segment proportion to estimate street address ##
  vars <- c("x", "y")
  sel <- cholera::road.segments$id %in% c("245-1", "245-2")
  hopkins <- cholera::road.segments[sel, ]

  sel <- hopkins$id == "245-1"
  seg1 <- rbind(stats::setNames(hopkins[sel, paste0(vars, 1)], vars),
                stats::setNames(hopkins[sel, paste0(vars, 2)], vars))

  sel <- hopkins$id == "245-2"
  seg2 <- rbind(stats::setNames(hopkins[sel, paste0(vars, 1)], vars),
                stats::setNames(hopkins[sel, paste0(vars, 2)], vars))

  d1 <- stats::dist(seg1)
  d2 <- stats::dist(seg2)
  mid.point <- sum(d1, d2) / 2 # arbitrarily use mid-point of block as address

  seg.data <- seg1 # mid-point on "245-1"
  h <- d1 - mid.point
  theta <- roadTheta(seg.data)
  delta.x <- h * cos(theta)
  delta.y <- h * sin(theta)

  # southern point
  pt2 <- stats::setNames(hopkins[hopkins$id == "245-1", paste0(vars, 2)], vars)
  proj <- data.frame(x = pt2$x - delta.x, y = pt2$y - delta.y)

  ## latlong label ##
  origin <- data.frame(lon = min(cholera::roads[, "lon"]),
                       lat = min(cholera::roads[, "lat"]))

  NW <- roadSegEndpt(seg.id = "225-1", endpt.sel = 2L, latlong = TRUE)
  NE <- roadSegEndpt(seg.id = "225-1", endpt.sel = 1L, latlong = TRUE)
  SW <- roadSegEndpt(seg.id = "259-1", endpt.sel = 2L, latlong = TRUE)
  SE <- roadSegEndpt(seg.id = "259-1", endpt.sel = 1L, latlong = TRUE)

  geo.cartesian <- lapply(list(NW, NE, SW, SE), function(coords) {
    x.proj <- c(coords$lon, origin$lat)
    y.proj <- c(origin$lon, coords$lat)
    m.lon <- geosphere::distGeo(y.proj, coords)
    m.lat <- geosphere::distGeo(x.proj, coords)
    data.frame(x = m.lon, y = m.lat)
  })

  names(geo.cartesian) <- c("NW", "NE", "SW", "SE")
  NW <- geo.cartesian$NW
  NE <- geo.cartesian$NE
  SW <- geo.cartesian$SW
  SE <- geo.cartesian$SE

  pt <- segmentIntersection(NW$x, NW$y, SE$x, SE$y, NE$x, NE$y, SW$x, SW$y)

  vars <- c("lon", "lat")
  label.latlong <- meterLatLong(pt)[, vars]

  ## latlong address (proj) ##
  rd.segs <- roadSegments(latlong = TRUE)
  hopkins <- rd.segs[rd.segs$id %in% c("245-1", "245-2"), ]

  sel <- hopkins$id == "245-1"
  seg1 <- rbind(stats::setNames(hopkins[sel, paste0(vars, 1)], vars),
                stats::setNames(hopkins[sel, paste0(vars, 2)], vars))

  sel <- hopkins$id == "245-2"
  seg2 <- rbind(stats::setNames(hopkins[sel, paste0(vars, 1)], vars),
                stats::setNames(hopkins[sel, paste0(vars, 2)], vars))

  d1 <- geosphere::distGeo(seg1[1, ], seg1[2, ])
  d2 <- geosphere::distGeo(seg2[1, ], seg2[2, ])
  # mid.point <- sum(d1, d2) / 2 # arbitrarily use mid-point of block as address
  # proportion <- mid.point / d1

  geo.cartesian1 <- lapply(list(seg1[1, ], seg1[2, ]), function(coords) {
    x.proj <- c(coords$lon, origin$lat)
    y.proj <- c(origin$lon, coords$lat)
    m.lon <- geosphere::distGeo(y.proj, coords)
    m.lat <- geosphere::distGeo(x.proj, coords)
    data.frame(x = m.lon, y = m.lat)
  })

  geo.cartesian2 <- lapply(list(seg2[1, ], seg2[2, ]), function(coords) {
    x.proj <- c(coords$lon, origin$lat)
    y.proj <- c(origin$lon, coords$lat)
    m.lon <- geosphere::distGeo(y.proj, coords)
    m.lat <- geosphere::distGeo(x.proj, coords)
    data.frame(x = m.lon, y = m.lat)
  })

  geo.cartesian1 <- do.call(rbind, geo.cartesian1)
  geo.cartesian2 <- do.call(rbind, geo.cartesian2)
  seg.d <- sum(stats::dist(geo.cartesian1), stats::dist(geo.cartesian2))

  h <- seg.d / 2 # arbitrarily use mid-point of block as address
  theta <- roadTheta(geo.cartesian1) # mid-point on geo.cartesian1 (i.e., seg1)
  delta.x <- h * cos(theta)
  delta.y <- h * sin(theta)
  proj.geo.cartesian <- data.frame(x = geo.cartesian1[1, ]$x + delta.x,
                                   y = geo.cartesian1[1, ]$y + delta.y)

  proj.latlong <- meterLatLong(proj.geo.cartesian)

  data.frame(case = 1017L,
             road.segment = "245-1",
             proj,
             x.lab = label.nominal$x,
             y.lab = label.nominal$y,
             proj.latlong[, vars],
             lon.lab = label.latlong$lon,
             lat.lab = label.latlong$lat,
             name = "Model Lodging Houses",
             row.names = NULL)
}

pantheonBazaar <- function() {
  st.nm <- "Winsley Street" # "73-1"

  vars <- c("x", "y")
  sel <- cholera::road.segments$name == st.nm
  proj.nominal <- cholera::road.segments[sel, paste0(vars, 2)]
  names(proj.nominal) <- vars

  rd.segs <- roadSegments(latlong = TRUE)
  vars <- c("lon", "lat")
  proj.latlong <- rd.segs[rd.segs$name == st.nm, paste0(vars, 2)]
  names(proj.latlong) <- vars

  data.frame(case = 1018L,
             road.segment = "73-1",
             proj.nominal,
             x.lab = proj.nominal$x,
             y.lab = proj.nominal$y,
             proj.latlong,
             lon.lab = proj.latlong$lon,
             lat.lab = proj.latlong$lat,
             name = "The Pantheon",
             row.names = NULL)
}

stJamesWorkhouse <- function() {
  seg.id <- "148-1" # "St James Workhouse"
  workhouse.west <- roadSegEndpt(seg.id = "148-1", endpt.sel = 1L)
  marshall.east <- roadSegEndpt(seg.id = "201-1", endpt.sel = 2L)
  proj.seg <- rbind(workhouse.west, marshall.east)
  h <- stats::dist(proj.seg)
  theta <- roadTheta(proj.seg)
  delta.x <- (h / 2) * cos(theta)
  delta.y <- (h / 2) * sin(theta)
  x.lab <- marshall.east$x + delta.x
  y.lab <- marshall.east$y + delta.y

  origin <- data.frame(lon = min(cholera::roads[, "lon"]),
                       lat = min(cholera::roads[, "lat"]))

  workhouse.west.geo <- roadSegEndpt(seg.id = "148-1", endpt.sel = 1L,
    latlong = TRUE)
  marshall.east.geo <- roadSegEndpt(seg.id = "201-1", endpt.sel = 2L,
    latlong = TRUE)
  coords.lst <- list(workhouse.west.geo, marshall.east.geo)

  geo.cartesian <- do.call(rbind, lapply(coords.lst, function(coords) {
    x.proj <- c(coords$lon, origin$lat)
    y.proj <- c(origin$lon, coords$lat)
    m.lon <- geosphere::distGeo(y.proj, coords)
    m.lat <- geosphere::distGeo(x.proj, coords)
    data.frame(x = m.lon, y = m.lat)
  }))

  h <- stats::dist(geo.cartesian)
  theta <- roadTheta(geo.cartesian)
  delta.x <- (h / 2) * cos(theta)
  delta.y <- (h / 2) * sin(theta)

  geo.cartesian.lab <- data.frame(x = geo.cartesian[2, "x"] + delta.x,
                                  y = geo.cartesian[2, "y"] + delta.y)

  label.latlong <- meterLatLong(geo.cartesian.lab)

  data.frame(case = 1019L,
             road.segment = seg.id,
             workhouse.west,
             x.lab = x.lab,
             y.lab = y.lab,
             workhouse.west.geo,
             lon.lab = label.latlong$lon,
             lat.lab = label.latlong$lat,
             name = "St James Workhouse",
             row.names = NULL)
}

stLukesChurch <- function() {
  # Berwick Street: provisionally use "Tylers Court" endpt on segment "221-1"
  # dat <- data.frame(case = 1020L, road.segment = "222-1", x = 14.94156,
  #   y = 11.25313)

  seg.id <- "221-1"
  proj.nominal <- roadSegEndpt(seg.id = seg.id, endpt.sel = 1L)
  proj.latlong <- roadSegEndpt(seg.id = seg.id, endpt.sel = 1L,
    latlong = TRUE)

  # nominal xy label

  taylor.data <- roadSegmentData(seg.id = "221-1")
  hopkins.data <- roadSegmentData(seg.id = "245-2")

  taylor.ols <- stats::lm(y ~ x, data = taylor.data)
  hopkins.ols <- stats::lm(y ~ x, data = hopkins.data)

  xs <- stats::coef(taylor.ols)["x"] -
        stats::coef(hopkins.ols)["x"]

  bs <- stats::coef(hopkins.ols)["(Intercept)"] -
        stats::coef(taylor.ols)["(Intercept)"]

  x.proj <- bs / xs
  y.proj <- stats::coef(hopkins.ols)["x"] * x.proj +
            stats::coef(hopkins.ols)["(Intercept)"]

  transversal <- rbind(proj.nominal, data.frame(x = x.proj, y = y.proj))
  delta <- trignometricDelta(transversal)
  label.nominal <- data.frame(x.lab = x.proj + delta$x,
                              y.lab = y.proj + delta$y)

  # latlong label

  origin <- data.frame(lon = min(cholera::roads[, "lon"]),
                       lat = min(cholera::roads[, "lat"]))

  taylor.data <- roadSegmentData(seg.id = "221-1", latlong = TRUE)
  taylor.data <- segmentGeoCartesian(taylor.data, origin)

  hopkins.data <- roadSegmentData(seg.id = "245-2", latlong = TRUE)
  hopkins.data <- segmentGeoCartesian(hopkins.data, origin)

  taylor.ols <- stats::lm(y ~ x, data = taylor.data)
  hopkins.ols <- stats::lm(y ~ x, data = hopkins.data)

  xs <- stats::coef(taylor.ols)["x"] -
        stats::coef(hopkins.ols)["x"]

  bs <- stats::coef(hopkins.ols)["(Intercept)"] -
        stats::coef(taylor.ols)["(Intercept)"]

  x.proj <- bs / xs
  y.proj <- stats::coef(hopkins.ols)["x"] * x.proj +
            stats::coef(hopkins.ols)["(Intercept)"]

  transversal <- rbind(taylor.data[1, ], data.frame(x = x.proj, y = y.proj))
  delta <- trignometricDelta(transversal)
  geo.cartesian <- data.frame(x = x.proj + delta$x, y = y.proj + delta$y)

  label.latlong <- meterLatLong(geo.cartesian)

  vars <- c("lon", "lat")
  label.latlong <- stats::setNames(label.latlong[, vars],  paste0(vars, ".lab"))

  data.frame(case = 1020L,
             road.segment = seg.id,
             proj.nominal,
             label.nominal,
             proj.latlong,
             label.latlong,
             name = "St Luke's Church",
             row.names = NULL)
}

johnSnow <- function() {
  seg.id <- "508-1"
  proj.nominal <- segmentTrigonometryAddress(seg.id = seg.id)
  proj.latlong <- segmentTrigonometryAddress(seg.id = seg.id, latlong = TRUE)
  data.frame(case = 1021L,
             road.segment = seg.id,
             proj.nominal,
             x.lab = proj.nominal$x,
             y.lab = proj.nominal$y,
             proj.latlong,
             lon.lab = proj.latlong$lon,
             lat.lab = proj.latlong$lat,
             name = "John Snow",
             row.names = NULL)
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

coordsCartesian <- function(proj.latlong, origin) {
  coord.zero <- geosphere::destPoint(unlist(proj.latlong), b = 0, d = 54)
  coord.zero <- data.frame(coord.zero)
  projCartesian(coord.zero, origin)
}

pasteCoordsB <- function(dat, var1 = "x1", var2 = "y1") {
  vapply(seq_len(nrow(dat)), function(i) {
    paste(dat[i, c(var1, var2)], collapse = "_&_")
  }, character(1L))
}

projCartesian <- function(proj.latlong, origin) {
  x.proj <- c(proj.latlong$lon, origin$lat)
  y.proj <- c(origin$lon, proj.latlong$lat)
  m.lon <- geosphere::distGeo(y.proj, proj.latlong)
  m.lat <- geosphere::distGeo(x.proj, proj.latlong)
  data.frame(x = m.lon, y = m.lat)
}

projectLandmarkAddress <- function(dat, latlong = FALSE) {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")
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
  data.frame(x.proj = x.proj, y.proj = y.proj, row.names = NULL)
}

roadSegEndpt <- function(seg.id = "116-2", endpt.sel = 2L, latlong = FALSE) {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  if (latlong) {
    rd.segs <- roadSegments(latlong = TRUE)
    seg.data <- rd.segs[rd.segs$id == seg.id, paste0(vars, endpt.sel)]
  } else {
    sel <- cholera::road.segments$id == seg.id
    seg.data <- cholera::road.segments[sel, paste0(vars, endpt.sel)]
  }

  out <- stats::setNames(seg.data, vars)
  row.names(out) <- NULL
  out
}

roadSegmentData <- function(seg.id = "116-2", latlong = FALSE) {
  if (latlong) {
    vars <- c("lon", "lat")
    rd.segs <- roadSegments(latlong = TRUE)
  } else {
    vars <- c("x", "y")
    rd.segs <- cholera::road.segments
  }
  seg.data <- rd.segs[rd.segs$id == seg.id, ]
  out <- rbind(stats::setNames(seg.data[, paste0(vars, 1)], vars),
               stats::setNames(seg.data[, paste0(vars, 2)], vars))
  row.names(out) <- NULL
  out
}

roadTheta <- function(dat) {
  ols <- stats::lm(y ~ x, data = dat)
  slope <- stats::coef(ols)[2]
  ifelse(is.na(slope), pi / 2, atan(slope))
}

segmentGeoCartesian <- function(endpt.data, origin) {
  do.call(rbind, lapply(1:2, function(i) {
    x.proj <- c(endpt.data[i, ]$lon, origin$lat)
    y.proj <- c(origin$lon, endpt.data[i, ]$lat)
    m.lon <- geosphere::distGeo(y.proj, unlist(endpt.data[i, ]))
    m.lat <- geosphere::distGeo(x.proj, unlist(endpt.data[i, ]))
    data.frame(x = m.lon, y = m.lat)
  }))
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

segmentTrigonometryAddress <- function(seg.id = "174-1", factor = 2L,
  delta = "pos", latlong = FALSE) {

  vars <- c("x", "y")

  if (latlong) {
    varsB <- c("lon", "lat")
    ew <- varsB[1]
    ns <- varsB[2]
    origin <- data.frame(lon = min(cholera::roads[, ew]),
                         lat = min(cholera::roads[, ns]))

    rd.segs <- roadSegments(latlong = TRUE)
    seg <- rd.segs[rd.segs$id == seg.id, ]
    alpha <- stats::setNames(seg[, paste0(varsB, 1)], varsB)
    omega <- stats::setNames(seg[, paste0(varsB, 2)], varsB)
  } else {
    seg <- cholera::road.segments[cholera::road.segments$id == seg.id, ]
    alpha <- stats::setNames(seg[, paste0(vars, 1)], vars)
    omega <- stats::setNames(seg[, paste0(vars, 2)], vars)
  }

  seg.data <- rbind(alpha, omega)

  if (latlong) {
    geo.cartesian <- do.call(rbind, lapply(1:2, function(i) {
      tmp <- seg.data[i, ]
      x.proj <- c(tmp$lon, origin$lat)
      y.proj <- c(origin$lon, tmp$lat)
      m.lon <- geosphere::distGeo(y.proj, tmp)
      m.lat <- geosphere::distGeo(x.proj, tmp)
      data.frame(pt.id = i, x = m.lon, y = m.lat)
    }))
    seg.data <- geo.cartesian[, vars]
  }

  h <- c(stats::dist(seg.data)) / factor
  theta <- roadTheta(seg.data)
  delta.x <- h * cos(theta)
  delta.y <- h * sin(theta)

  if (delta == "pos") {
    out <- data.frame(x = seg.data[1, ]$x + delta.x,
                      y = seg.data[1, ]$y + delta.y, row.names = NULL)
  } else if (delta == "neg") {
    out <- data.frame(x = seg.data[1, ]$x - delta.x,
                      y = seg.data[1, ]$y - delta.y, row.names = NULL)
  }

  if (latlong) out <- meterLatLong(out)[, varsB]
  out
}

trignometricDelta <- function(dat, factor = 2L) {
  h <- c(stats::dist(dat))
  theta <- roadTheta(dat)
  delta.x <- (h / factor) * cos(theta)
  delta.y <- (h / factor) * sin(theta)
  data.frame(x = delta.x, y = delta.y, row.names = NULL)
}
