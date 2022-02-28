#' Compute orthogonal projection of pumps (prototype).
#'
#' Projection from fatality address to nearest road segment.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @param radius Numeric.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @export

latlongOrthoPump <- function(path, vestry = FALSE, radius = 0.001,
  multi.core = TRUE) {

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

  road.segments <- lapply(unique(rd$street), function(i) {
    dat <- rd[rd$street == i, ]
    names(dat)[names(dat) %in% vars] <- paste0(vars, 1)
    seg.data <- dat[-1, paste0(vars, 1)]
    names(seg.data) <- paste0(vars, 2)
    dat <- cbind(dat[-nrow(dat), ], seg.data)
    dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
    dat
  })

  road.segments <- do.call(rbind, road.segments)

  soln <- parallel::mclapply(pump$id, function(p) {
    case <- pump[pump$id == p, vars]
    case.st <-  pump[pump$id == p, "street"]
    case.id <- road.segments[road.segments$name == case.st, "id"]

    ortho.proj.test <- lapply(case.id, function(x) {
      seg.data <- road.segments[road.segments$id == x,
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

      # segment bisection/intersection test
      distB <- stats::dist(rbind(seg.df[1, ], c(lon.proj, lat.proj))) +
               stats::dist(rbind(seg.df[2, ], c(lon.proj, lat.proj)))

      bisect.test <- signif(stats::dist(seg.df)) == signif(distB)

      if (bisect.test) {
        ortho.dist <- c(stats::dist(rbind(c(case$lon, case$lat),
                                          c(lon.proj, lat.proj))))
        ortho.pts <- data.frame(lon.proj, lat.proj)
        data.frame(id = x, ortho.pts, ortho.dist, stringsAsFactors = FALSE)
      } else NA
    })

    out <- do.call(rbind, ortho.proj.test)
    out[which.min(out$ortho.dist), ]
  }, mc.cores = cores)

  soln <- do.call(rbind, soln)
  row.names(soln) <- NULL

  soln$lon <- unstd(soln$lon, lon.mean, lon.sd)
  soln$lat <- unstd(soln$lat, lat.mean, lat.sd)
  soln <- data.frame(pump = pump$id, soln)
  soln
}
