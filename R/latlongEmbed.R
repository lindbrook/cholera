#' Embed anchors and pumps into road segments (prototype).
#'
#' @param vestry Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @noRd

latlongEmbed <- function(vestry = FALSE, multi.core = TRUE) {
  cores <- multiCore(multi.core)

  ortho.addr <- cholera::latlong.ortho.addr

  if (vestry) ortho.pump <- cholera::latlong.ortho.pump.vestry 
  else ortho.pump <- cholera::latlong.ortho.pump
  names(ortho.pump)[names(ortho.pump) == "pump.id"] <- "pump"
  
  road.data <- roadSegments(latlong = TRUE)

  obs.segs <- unique(c(ortho.addr$road.segment, ortho.pump$road.segment))
  no_embeds <- road.data[!road.data$id %in% obs.segs, ]

  vars <- c("lon", "lat")
  vars2 <- c(vars, "case", "pump")

  embeds <- lapply(obs.segs, function(s) {
    rd.tmp <- road.data[road.data$id == s, ]
    addr.tmp <- ortho.addr[ortho.addr$road.segment == s, ]
    pump.tmp <- ortho.pump[ortho.pump$road.segment == s, ]
    endpts <- data.frame(lon = unlist(rd.tmp[, paste0(vars[1], 1:2)]),
                         lat = unlist(rd.tmp[, paste0(vars[2], 1:2)]),
                         case = 0,
                         pump = 0,
                         row.names = NULL)

    if (nrow(addr.tmp) > 0) addr.embed <- addr.tmp[, c(vars, "case")]
    if (nrow(pump.tmp) > 0) pump.embed <- pump.tmp[, c(vars, "pump")]

    if (nrow(addr.tmp) > 0 & nrow(pump.tmp) > 0) {
      addr.embed$pump <- 0
      pump.embed$case <- 0
      pump.embed <- pump.embed[, vars2]
      embed.data <- rbind(endpts, addr.embed, pump.embed)
    } else if (nrow(addr.tmp) == 0 & nrow(pump.tmp) > 0) {
      pump.embed$case <- 0
      pump.embed <- pump.embed[, vars2]
      embed.data <- rbind(endpts, pump.embed)
    } else if (nrow(addr.tmp) > 0 & nrow(pump.tmp) == 0) {
      addr.embed$pump <- 0
      embed.data <- rbind(endpts, addr.embed)
    }

    out <- embed.data[order(embed.data$lon, embed.data$lat), ]
    tmp <- out[, vars]
    tmp <- cbind(tmp[-nrow(tmp), ], tmp[-1, ])
    coord.nms <- paste0(names(tmp), c(rep(1, 2), rep(2, 2)))
    names(tmp) <- coord.nms
    tmp <- cbind(tmp, rd.tmp[, c("street", "id", "name")])
    edges <- tmp[, c("street", "id", "name", coord.nms)]
    list(edges = edges, nodes = out)
  })

  edges <- do.call(rbind, lapply(embeds, function(x) x$edges))
  edges <- rbind(edges, no_embeds[, names(edges)])
  edges <- edges[order(edges$street), ]
  nodes <- do.call(rbind, lapply(embeds, function(x) x$nodes))
  list(edges = edges, nodes = nodes)
}
