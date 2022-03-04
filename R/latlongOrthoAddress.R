#' Compute latitude and longitude for orthogonal case projection (address).
#'
#' @param path Character. e.g., "~/Documents/Data/".
#' @param vestry Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @export

latlongOrthoAddress <- function(path, vestry = FALSE, multi.core = TRUE) {
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

  sel <- cholera::ortho.proj$case %in% unique(cholera::anchor.case$anchor)
  obs.segs <- cholera::ortho.proj[sel, "road.segment"]
  idx <- seq_along(addr$anchor)

  orthogonal.projection <- parallel::mclapply(idx, function(i) {
    case <- addr[addr$anchor == addr$anchor[i], vars]
    seg.data <- rd.segs[rd.segs$id == obs.segs[i],
      c("lon1", "lat1", "lon2", "lat2")]
    seg.df <- data.frame(lon = c(seg.data$lon1, seg.data$lon2),
                         lat = c(seg.data$lat1, seg.data$lat2))

    ols <- stats::lm(lat ~ lon, data = seg.df)
    segment.slope <- stats::coef(ols)[2]
    segment.intercept <- stats::coef(ols)[1]

    if (segment.slope == 0) {
      lon.proj <- case$lon
      lat.proj <- segment.intercept
    } else {
      orthogonal.slope <- -1 / segment.slope
      orthogonal.intercept <- case$lat - orthogonal.slope * case$lon
      lon.proj <- (orthogonal.intercept - segment.intercept) /
                  (segment.slope - orthogonal.slope)
      lat.proj <- segment.slope * lon.proj + segment.intercept
    }

    case.data <- rbind(c(case$lon, case$lat), c(lon.proj, lat.proj))
    ortho.dist <- c(stats::dist(case.data))

    data.frame(lon.proj, lat.proj, ortho.dist, stringsAsFactors = FALSE)
  }, mc.cores = cores)

  soln <- do.call(rbind, orthogonal.projection)
  row.names(soln) <- NULL
  soln$lon <- unstd(soln$lon, lon.mean, lon.sd)
  soln$lat <- unstd(soln$lat, lat.mean, lat.sd)
  soln <- data.frame(case = addr$anchor, seg = obs.segs, soln)
  soln
}
