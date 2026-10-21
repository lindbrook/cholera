#' Compute normalized Cartesian coordiantes of regular (expected/simulated) case data.
#'
#' Compute normalized distance from origin to case and translate into
#` horizontal (East-West) and vertical (North-South) components.
#' @param latlong Logical. Use latlong regular cases.
#' @importFrom geosphere distGeo
#' @noRd

cartesianCoordinates <- function(latlong = FALSE) {
	if (latlong) {
	  dat <- cholera::latlong.regular.cases

		origin <- data.frame(lon = min(cholera::roads$lon),
	                       lat = min(cholera::roads$lat))

		h <- data.frame(lon = max(cholera::roads$lon),
                    lat = max(cholera::roads$lat))
		
		x.len <- geosphere::distGeo(c(h$lon, origin$lat), origin)
		y.len <- geosphere::distGeo(c(origin$lon, h$lat), origin)

		coords <- do.call(rbind, parallel::mclapply(dat$case, function(case) {
	    tmp <- dat[dat$case == case, c("lon", "lat")]
	    x.proj <- c(tmp$lon, origin$lat)
	    y.proj <- c(origin$lon, tmp$lat)
	    m.lon <- 100 * geosphere::distGeo(y.proj, tmp) / x.len
	    m.lat <- 100 * geosphere::distGeo(x.proj, tmp) / y.len
	    data.frame(id = case, x = m.lon, y = m.lat)
		}, mc.cores = parallel::detectCores()))
	
	} else {
	  dat <- cholera::regular.cases

		origin <- data.frame(x = min(cholera::roads$x),
												 y = min(cholera::roads$y))
		
		h <- data.frame(x = max(cholera::roads$x),
									  y = max(cholera::roads$y))
		
		D <- rbind(origin, h)
		x.len <- max(D$x) - min(D$x)
		y.len <- max(D$y) - min(D$y)
		
		coords <- do.call(rbind, parallel::mclapply(dat$case, function(case) {
	    tmp <- dat[dat$case == case, c("x", "y")]
	    x.proj <- 100 * (tmp$x - origin$x) / x.len
	    y.proj <- 100 * (tmp$y - origin$y) / y.len
	    data.frame(id = case, x = x.proj, y = y.proj)
	  }, mc.cores = parallel::detectCores()))
	}
	coords
}

# nom <- cholera:::cartesianCoordinates()
# geo <- cholera:::cartesianCoordinates(latlong = TRUE)

# ds <- parallel::mclapply(nom$id, function(id) {
#   nom.tmp <- nom[nom$id == id, c("x", "y")]
  
#   d <- vapply(geo$id, function(g.id) {
#     stats::dist(rbind(nom.tmp, geo[geo$id == g.id, c("x", "y")]))
#   }, numeric(1L))
  
#   sel <- which.min(d)
#   data.frame(nom.case = id, geo.case = geo$id[sel], d = d[sel])
# }, mc.cores = parallel::detectCores())

#      user    system   elapsed 
# 52829.411  1077.193  6631.613 

# nominal.latlong <- do.call(rbind, ds)
# usethis::use_data(nominal.latlong)
# usethis::use_data(nominal.latlong, overwrite = TRUE)
