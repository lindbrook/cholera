#' Create and write GeoPackage (GPKG) of anchor fatalities (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

anchorsGPKG <- function(path, write.gpkg = TRUE) {
  vars <- c("x", "y")
  dat <- cholera::fatalities.anchor[, vars]
  anchor_geom <- sf::st_as_sf(dat, coords = vars)
  anchor_attr <- cholera::fatalities.anchor[, c("anchor", "case.count")]
  anchor_sf <- sf::st_sf(anchor_attr, geometry = sf::st_as_sfc(anchor_geom))
  if (write.gpkg) {
    sf::write_sf(anchor_sf, paste0(path, "anchor.gpkg"), append = FALSE)
  } else {
    anchor_sf
  }
}

#' Create and write GeoPackage (GPKG) of fatalities (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

fatalitiesGPKG <- function(path, write.gpkg = TRUE) {
  vars <- c("x", "y")
  dat <- cholera::fatalities[, vars]
  fatality_geom <- sf::st_as_sf(dat, coords = vars)
  fatality_attr <- cholera::fatalities[, "case", drop = FALSE]
  fatality_sf <- sf::st_sf(fatality_attr, 
    geometry = sf::st_as_sfc(fatality_geom))
  if (write.gpkg) {
    sf::write_sf(fatality_sf, paste0(path, "fatality.gpkg"), append = FALSE)  
  } else {
    fatality_sf
  }
}

#' Create and write GeoPackage (GPKG) of fatalities.unstacked (prototype).
#'
#' @param path File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

unstackedGPKG <- function(path, write.gpkg = TRUE) {
  vars <- c("x", "y")
  dat <- cholera::fatalities.unstacked[, vars]
  unstack_geom <- sf::st_as_sf(dat, coords = vars)
  unstack_attr <- cholera::fatalities.unstacked[, "case", drop = FALSE]
  unstack_sf <- sf::st_sf(unstack_attr, geometry = sf::st_as_sfc(unstack_geom))
  if (write.gpkg) {
    sf::write_sf(unstack_sf, paste0(path, "unstack.gpkg"), append = FALSE)
  } else {
    unstack_sf
  }
}

#' Create and write GeoPackage (GPKG) of pump data (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param vestry Logical.
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

pumpGPKG <- function(path, vestry = FALSE, write.gpkg = TRUE) {
  vars <- c("x", "y")
  if (vestry) {
    dat <- cholera::pumps.vestry[, vars]
    pump_attr <- cholera::pumps.vestry[, c("id", "street")]
    file.nm <- "pumpVestry.gpkg"
  } else {
    dat <- cholera::pumps[, vars]
    pump_attr <- cholera::pumps[, c("id", "street")]
    file.nm <- "pump.gpkg"
  }
  pump_geom <- sf::st_as_sf(dat, coords = vars)
  pump_sf <- sf::st_sf(pump_attr, geometry = sf::st_as_sfc(pump_geom))
  if (write.gpkg) {
    sf::write_sf(pump_sf, paste0(path, file.nm), append = FALSE)
  } else {
    pump_sf
  }
}

#' Create and write GeoPackage (GPKG) of roads data (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

roadGPKG <- function(path, write.gpkg = TRUE) {
  vars <- c("x", "y")
  dat <- cholera::roads[, vars]
  road_geom <- sf::st_as_sf(dat, coords = vars)
  road_attr <- cholera::roads[, c("id", "street", "n", "name")] 
  road_sf <- sf::st_sf(road_attr, geometry = sf::st_as_sfc(road_geom))
  if (write.gpkg) {
    sf::write_sf(road_sf, paste0(path, "road.gpkg"), append = FALSE)
  } else {
    road_sf
  }
}

#' Create and write GeoPackage (GPKG) of roads data (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

frameGPKG <- function(path, write.gpkg = TRUE) {
  vars <- c("x", "y")
  dat <- cholera::frame.data[, vars]
  frame_geom <- sf::st_as_sf(dat, coords = vars)
  frame_attr <- cholera::frame.data[, c("id", "street", "n", "name")] 
  frame_sf <- sf::st_sf(frame_attr, geometry = sf::st_as_sfc(frame_geom))
  if (write.gpkg) {
    sf::write_sf(frame_sf, paste0(path, "frame.gpkg"), append = FALSE)
  } else {
    frame_sf
  }
}

#' Create and write GeoPackage (GPKG) of road line segment network (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd
#' @note For georeferencing.

roadSegmentGPKG <- function(path, write.gpkg = TRUE) {
  vars <- c("x1", "y1", "x2", "y2")
  seg.data <- lapply(seq_along(cholera::road.segments$id), function(i) {
    matrix(unlist(cholera::road.segments[i, vars]), ncol = 2, byrow = TRUE)
  })
  rd.segs_geom <- lapply(seg.data, sf::st_linestring)
  rd.segs_attr <- cholera::road.segments[, c("street", "id", "name")]
  rd.segs_sf <- sf::st_sf(rd.segs_attr, geometry = sf::st_sfc(rd.segs_geom))
  if (write.gpkg) {
    sf::st_write(rd.segs_sf, paste0(path, "roadSegment.gpkg"), append = FALSE)
  } else {
    rd.segs_sf
  }
}

#' Create and write GeoPackage (GPKG) of map frame line segments (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

mapFrameSegmentGPKG <- function(path, write.gpkg = TRUE) {
  vars <- c("x1", "y1", "x2", "y2")
  seg.data <- lapply(seq_along(cholera::frame.segments$id), function(i) {
    matrix(unlist(cholera::frame.segments[i, vars]), ncol = 2, byrow = TRUE)
  })
  fr.segs_geom <- lapply(seg.data, sf::st_linestring)
  fr.segs_attr <- cholera::frame.segments[, c("street", "id", "name")]
  fr.segs_sf <- sf::st_sf(fr.segs_attr, geometry = sf::st_sfc(fr.segs_geom))
  if (write.gpkg) {
    sf::st_write(fr.segs_sf, paste0(path, "frameSegment.gpkg"), append = FALSE)
  } else {
    fr.segs_sf
  }
}

#' Create and write GeoPackage (GPKG) of plague pit data (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

plagueGPKG <- function(path, write.gpkg = TRUE) {
  vars <- c("x", "y")
  dat <- cholera::plague.pit[, vars]
  plague_geom <- sf::st_as_sf(dat, coords = vars)
  plague_attr <- cholera::plague.pit[, "id", drop = FALSE]
  plague_sf <- sf::st_sf(plague_attr, geometry = sf::st_as_sfc(plague_geom))
  if (write.gpkg) {
    sf::write_sf(plague_sf, paste0(path, "plague.gpkg"), append = FALSE)
  } else {
    plague_sf
  }
}

#' Create and write GeoPackage (GPKG) of plague pit segment data (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

plagueSegmentGPKG <- function(path, write.gpkg = TRUE) {
  vars <- c("x", "y")
  newvars <- c(paste0(vars, 1), paste0(vars, 2))
  pit <- cholera::plague.pit
  
  seg.data <- do.call(rbind, lapply(seq_len(nrow(pit) - 1), function(i) {
    stats::setNames(cbind(pit[i, vars], pit[i + 1, vars]), newvars) 
  }))
  
  seg.data <- lapply(seq_along(seg.data$x1), function(i) {
    matrix(unlist(seg.data[i, newvars]), ncol = 2, byrow = TRUE)
  })
  
  pit.segs_geom <- lapply(seg.data, sf::st_linestring)
  pit.segs_attr <- data.frame(id = seq_along(seg.data))
  pit.segs_sf <- sf::st_sf(pit.segs_attr, geometry = sf::st_sfc(pit.segs_geom))
  if (write.gpkg) {
    sf::write_sf(pit.segs_sf, paste0(path, "plagueSegment.gpkg"), 
      append = FALSE)
  } else {
    pit.segs_sf
  }
}

#' Create and write GeoPackage (GPKG) of landmark data (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param label.coords Logical. Compute label coordinates.
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

landmarkGPKG <- function(path, label.coords = FALSE, write.gpkg = TRUE) {
  if (label.coords) vars <- c("x.lab", "y.lab")
  else vars <- c("x", "y")
  dat <- cholera::landmarks[, vars]
  landmark_geom <- sf::st_as_sf(dat, coords = vars)
  landmark_attr <- cholera::landmarks[, c("case", "road.segment", "name")]
  landmark_sf <- sf::st_sf(landmark_attr, 
    geometry = sf::st_as_sfc(landmark_geom))
  
  if (write.gpkg) {
    if (label.coords) {
      sf::write_sf(landmark_sf, paste0(path, "landmarkLabel.gpkg"),
        append = FALSE)
    } else {
      sf::write_sf(landmark_sf, paste0(path, "landmark.gpkg"), append = FALSE)
    }
  } else {
    landmark_sf
  }
}

#' Create and write GeoPackage (GPKG) of landmark squares data (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE saves GeoPackage to disk. FALSE returns simple features object.
#' @noRd

landmarkSquareGPKG <- function(path, write.gpkg = TRUE) {
  vars <- c("x", "y")
  dat <- cholera::landmark.squares[, vars]
  landmarkSquare_geom <- sf::st_as_sf(dat, coords = vars)
  landmarkSquare_attr <- cholera::landmark.squares[, c("case", "name")]
  landmarkSquare_sf <- sf::st_sf(landmarkSquare_attr, 
    geometry = sf::st_as_sfc(landmarkSquare_geom))
  if (write.gpkg) {
    sf::write_sf(landmarkSquare_sf, paste0(path, "landmarkSquare.gpkg"), 
      append = FALSE)
  } else {
    landmarkSquare_sf
  }
}

#' Extract Longitude and Latitude from Selected Georeferenced GeoPackage Object.
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param dataset Character. Name of 'cholera' dataset.
#' @noRd

latlongCoordinatesGPKG <- function(path, dataset = "fatalities") {
  if (dataset == "fatalities") {
    dat <- "fatality_modified.gpkg"
    nom.data <- cholera::fatalities
  } else if (dataset == "fatalities.anchor") {
    dat <- "anchor_modified.gpkg"
    nom.data <- cholera::fatalities.anchor
  } else if (dataset == "fatalities.unstacked") {
    dat <- "unstack_modified.gpkg"
    nom.data <- cholera::fatalities.unstacked
  } else if (dataset == "pumps") {
    dat <- "pump_modified.gpkg"
    nom.data <- cholera::pumps
  } else if (dataset == "pumps.vestry") {
    dat <- "pumpVestry_modified.gpkg"
    nom.data <- cholera::pumps.vestry
  } else if (dataset == "roads") {
    dat <- "road_modified.gpkg"
    nom.data <- cholera::roads
  } else if (dataset == "frame.data") {
    dat <- "frame_modified.gpkg"
    nom.data <- cholera::frame.data
  } else if (dataset == "frame.segments") {
    dat <- "frameSegment_modified.gpkg"
    nom.data <- cholera::frame.segments
  } else if (dataset == "road.segments") {
    dat <- "roadSegment_modified.gpkg"
    nom.data <- cholera::road.segments
  } else if (dataset == "plague.pit") {
    dat <- "plague_modified.gpkg"
    nom.data <- cholera::plague.pit
  } else if (dataset == "plague.pit.segments") {
    dat <- "plagueSegment_modified.gpkg"
    nom.data <- cholera::plague.pit.segments
  } else if (dataset == "landmarks") {
    dat <- "landmark_modified.gpkg"
    label.dat <- "landmarkLabel_modified.gpkg"
    geo.label.data <- sf::st_read(paste0(path, label.dat), quiet = TRUE)
    nom.data <- cholera::landmarks
  } else if (dataset == "landmark.squares") {
    dat <- "landmarkSquare_modified.gpkg"
    nom.data <- cholera::landmark.squares
  } else {
    stop('Invalid dataset. Check spelling.', call. = FALSE)
  }

  geo.data <- sf::st_read(paste0(path, dat), quiet = TRUE)

  linestring.segs <- c("frame.segments", "plague.pit.segments", "road.segments")
  
  if (dataset == "landmarks") {
    vars <- c("lon", "lat")
    varsB <- c(vars, paste0(vars, ".lab"))
    if (all(varsB %in% names(nom.data))) {
      nom.data <- nom.data[, !names(nom.data) %in% varsB]
    }
    coords <- data.frame(sf::st_coordinates(geo.data))
    labs <- data.frame(sf::st_coordinates(geo.label.data))
    names(coords) <- vars
    names(labs) <-  varsB[3:4]
    geo.data <- cbind(coords, labs)
  } else if (!dataset %in% linestring.segs) {
    vars <- c("lon", "lat")
    if (all(vars %in% names(nom.data))) {
      nom.data <- nom.data[, !names(nom.data) %in% vars]
    }
    geo.data <- stats::setNames(data.frame(sf::st_coordinates(geo.data)), vars)
  } else if (dataset %in% linestring.segs) {
    vars <- c("lon1", "lat1", "lon2", "lat2")
    if (all(vars %in% names(nom.data))) {
      nom.data <- nom.data[, !names(nom.data) %in% vars]
    }
    st <- data.frame(sf::st_coordinates(geo.data))
    geo.data <- do.call(rbind, lapply(unique(st$L1), function(id) {
      tmp <- st[st$L1 == id, ]
      tmp <- cbind(tmp[1, c("X", "Y")], tmp[2, c("X", "Y")])
      names(tmp) <- vars
      tmp
    }))
  }
  
  if (!dataset %in% c("landmarks", "landmark.squares")) {
    out <- cbind(nom.data, geo.data)
  } else {
    out <- cbind(nom.data[, names(nom.data) != "name"], 
                 geo.data,  
                 name = nom.data[, names(nom.data) == "name"])
  }
  out
}

#' Create and Write GeoPackage Versions of All Nominal cholera Data.
#'
#` Runs all *GPKG() data translator functtions.
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param write.gpkg Logical. TRUE writes GeoPackage to disk. FALSE returns simple features object.
#' @noRd

writeGPKG  <- function(path, write.gpkg = TRUE) {
  list(fatalities.anchor = anchorsGPKG(path, write.gpkg = write.gpkg), 
       fatalities = fatalitiesGPKG(path, write.gpkg = write.gpkg),
       fatalities.unstacked = unstackedGPKG(path, write.gpkg = write.gpkg),
       pumps = pumpGPKG(path, write.gpkg = write.gpkg),
       pumps.vestry = pumpGPKG(path, vestry = TRUE, write.gpkg = write.gpkg),
       roads = roadGPKG(path, write.gpkg = write.gpkg),
       frame.data = frameGPKG(path, write.gpkg = write.gpkg),
       road.segments = roadSegmentGPKG(path, write.gpkg = write.gpkg),
       frame.segments = mapFrameSegmentGPKG(path, write.gpkg = write.gpkg),
       plague.pit = plagueGPKG(path, write.gpkg = write.gpkg),
       plague.pit.segments = plagueSegmentGPKG(path, write.gpkg = write.gpkg),
       landmarks = landmarkGPKG(path, write.gpkg = write.gpkg),
       landmark.labels = landmarkGPKG(path, label.coords = TRUE, 
          write.gpkg = write.gpkg),
       landmark.squares = landmarkSquareGPKG(path, write.gpkg = write.gpkg))
}

#' Extract Longitude and Latitude from All Georeferenced GeoPackage Objects.
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @noRd

latlongGPKG  <- function(path) {
  data.sets <- c("fatalities", "fatalities.anchor", "fatalities.unstacked", 
    "pumps", "pumps.vestry", "roads", "frame.data", "frame.segments", 
    "road.segments", "frame.segments", "plague.pit", "plague.pit.segments",
    "landmarks", "landmark.squares")
  out <- lapply(data.sets, latlongCoordinatesGPKG, path = path)
  names(out) <- data.sets
  out
}

#' @importFrom sf st_as_sf st_as_sfc st_coordinates st_read st_sf write_sf
