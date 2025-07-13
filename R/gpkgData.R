#' Create and write GeoPackage (GPKG) of anchor fatalities (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @param append Logical. TRUE overwrites existing file.
#' @noRd

anchorsGPKG <- function(path, append = FALSE) {
  vars <- c("x", "y")
  dat <- cholera::fatalities.address[, vars]
  anchor_geom <- sf::st_as_sf(dat, coords = vars)
  anchor_attr <- cholera::fatalities.address[, c("anchor", "case.count")]
  anchor_sf <- sf::st_sf(anchor_attr, geometry = sf::st_as_sfc(anchor_geom))
  sf::write_sf(anchor_sf, paste0(path, "anchor.gpkg"), append = append)
}

#' Create and write GeoPackage (GPKG) of fatalities (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @param append Logical. TRUE overwrites existing file.
#' @noRd

fatalitiesGPKG <- function(path, append = FALSE) {
  vars <- c("x", "y")
  dat <- cholera::fatalities[, vars]
  fatality_geom <- sf::st_as_sf(dat, coords = vars)
  fatality_attr <- cholera::fatalities[, "case", drop = FALSE]
  fatality_sf <- sf::st_sf(fatality_attr, 
    geometry = sf::st_as_sfc(fatality_geom))
  sf::write_sf(fatality_sf, paste0(path, "fatality.gpkg"), append = append)
}

#' Create and write GeoPackage (GPKG) of pump data (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @param append Logical. TRUE overwrites existing file.
#' @noRd

pumpsGPKG <- function(path, vestry = FALSE, append = FALSE) {
  vars <- c("x", "y")
  if (vestry) {
    dat <- cholera::pumps.vestry[, vars]
    pump_attr <- cholera::pumps.vestry[, c("id", "street")]
    file.nm <- "pump.vestry.gpkg"
  } else {
    dat <- cholera::pumps[, vars]
    pump_attr <- cholera::pumps[, c("id", "street")]
    file.nm <- "pump.gpkg"
  }
  pump_geom <- sf::st_as_sf(dat, coords = vars)
  pump_sf <- sf::st_sf(pump_attr, geometry = sf::st_as_sfc(pump_geom))
  sf::write_sf(pump_sf, paste0(path, file.nm), append = append)
}

#' Create and write GeoPackage (GPKG) of road line segment network (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param append Logical. TRUE overwrites existing file.
#' @noRd
#' @note For georeferencing.

roadsPKG <- function(path, append = FALSE) {
  vars <- c("x1", "y1", "x2", "y2")
  seg.data <- lapply(seq_along(cholera::road.segments$id), function(i) {
    matrix(unlist(cholera::road.segments[i, vars]), ncol = 2, byrow = TRUE)
  })
  rds_geom <- lapply(seg.data, sf::st_linestring)
  rds_attr <- cholera::road.segments[, c("street", "id", "name")]
  rds_sf <- sf::st_sf(rds_attr, geometry = sf::st_sfc(rds_geom))
  sf::st_write(rds_sf, paste0(path, "roads.gpkg"), append = append)
}

#' @importFrom sf st_as_sf st_as_sfc st_sf write_sf