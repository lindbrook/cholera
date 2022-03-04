#' Compute orthogonal projection of pumps (prototype).
#'
#' Projection from fatality address to nearest road segment.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

latlongOrthoPump <- function(path, vestry = FALSE, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  rd <- latlongRoads(path)
  addr <- latlongAddress(path)
  pump <- latlongPumps(path, vestry = vestry)

  vars <- c("lon", "lat")
  pool <- rbind(rd[, vars], addr[, vars], pump[, vars])
  lon.mean <- mean(pool$lon)
  lon.sd <- stats::sd(pool$lon)
  lat.mean <- mean(pool$lat)
  lat.sd <- stats::sd(pool$lat)

  rd$lon <- std(rd$lon, lon.mean, lon.sd)
  rd$lat <- std(rd$lat, lat.mean, lat.sd)
  addr$lon <- std(addr$lon, lon.mean, lon.sd)
  addr$lat <- std(addr$lat, lat.mean, lat.sd)
  pump$lon <- std(pump$lon, lon.mean, lon.sd)
  pump$lat <- std(pump$lat, lat.mean, lat.sd)

  rd.segs <- lapply(unique(rd$street), function(i) {
    dat <- rd[rd$street == i, ]
    names(dat)[names(dat) %in% vars] <- paste0(vars, 1)
    seg.data <- dat[-1, paste0(vars, 1)]
    names(seg.data) <- paste0(vars, 2)
    dat <- cbind(dat[-nrow(dat), ], seg.data)
    dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
    dat
  })

  rd.segs <- do.call(rbind, rd.segs)

  if (vestry) {
    ortho.pump <- cholera::ortho.proj.pump.vestry
  } else {
    ortho.pump <- cholera::ortho.proj.pump
  }

  orthogonal.projection <- parallel::mclapply(pump$id, function(p) {
    pump.case <- pump[pump$id == p, vars]
    pump.seg <- ortho.pump[ortho.pump$pump.id == p, "road.segment"]

    seg.data <- rd.segs[rd.segs$id == pump.seg,
      c("lon1", "lat1", "lon2", "lat2")]

    seg.df <- data.frame(lon = c(seg.data$lon1, seg.data$lon2),
                         lat = c(seg.data$lat1, seg.data$lat2))

    ols <- stats::lm(lat ~ lon, data = seg.df)
    segment.slope <- stats::coef(ols)[2]
    segment.intercept <- stats::coef(ols)[1]

    if (segment.slope == 0) {
      lon.proj <- pump.case$lon
      lat.proj <- segment.intercept
    } else {
       orthogonal.slope <- -1 / segment.slope
       orthogonal.intercept <- pump.case$lat - orthogonal.slope * pump.case$lon
       lon.proj <- (orthogonal.intercept - segment.intercept) /
                   (segment.slope - orthogonal.slope)
       lat.proj <- segment.slope * lon.proj + segment.intercept
    }

    pump.data <- rbind(c(pump.case$lon, pump.case$lat), c(lon.proj, lat.proj))
    ortho.dist <- c(stats::dist(pump.data))
    data.frame(lon.proj, lat.proj, ortho.dist, stringsAsFactors = FALSE)
  }, mc.cores = cores)

  soln <- do.call(rbind, orthogonal.projection)
  row.names(soln) <- NULL
  soln$lon <- unstd(soln$lon, lon.mean, lon.sd)
  soln$lat <- unstd(soln$lat, lat.mean, lat.sd)
  soln <- data.frame(pump = pump$id, seg = ortho.pump$road.segment, soln)
  soln
}
