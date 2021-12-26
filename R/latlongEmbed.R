#' Embed anchors and pumps into road segments (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @export

latlongEmbed <- function(path, vestry = FALSE) {
  vars <- c("lon", "lat")
  road.data <- latlongRoadSegments(path)
  ortho.addr <- latlongOrthoProj(path)
  ortho.pump <- latlongOrthoProjPumps(path, vestry = vestry)

  obs.segs <- unique(c(ortho.addr$road.segment, ortho.pump$road.segment))
  no_embeds <- road.data[!road.data$id %in% obs.segs, ]
  no_embeds$distance <- NULL

  embeds <- lapply(obs.segs, function(s) {
    rd.tmp <- road.data[road.data$id == s, ]
    addr.tmp <- ortho.addr[ortho.addr$road.segment == s, ]
    pump.tmp <- ortho.pump[ortho.pump$road.segment == s, ]
    endpts <- data.frame(lon = unlist(rd.tmp[, paste0(vars[1], 1:2)]),
                         lat = unlist(rd.tmp[, paste0(vars[2], 1:2)]),
                         case = 0,
                         pump = 0,
                         row.names = NULL)

    if (nrow(addr.tmp) > 0) {
      var.old <- c("lon.proj", "lat.proj", "case")
      var.new <- c(vars, "case")
      addr.embed <- stats::setNames(addr.tmp[, var.old], var.new)
    }

    if (nrow(pump.tmp) > 0) {
      var.old <- c("lon.proj", "lat.proj", "pump")
      var.new <- c(vars, "pump")
      pump.embed <- stats::setNames(pump.tmp[, var.old], var.new)
    }

    if (nrow(addr.tmp) > 0 & nrow(pump.tmp) > 0) {
      addr.embed$pump <- 0
      pump.embed$case <- 0
      pump.embed <- pump.embed[, c("lon", "lat", "case", "pump")]
      embed.data <- rbind(endpts, addr.embed, pump.embed)
    } else if (nrow(addr.tmp) == 0 & nrow(pump.tmp) > 0) {
      pump.embed$case <- 0
      pump.embed <- pump.embed[, c("lon", "lat", "case", "pump")]
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
    tmp[, c("street", "id", "name", coord.nms)]
  })

  embeds <- do.call(rbind, embeds)
  out <- rbind(embeds, no_embeds)
  out[order(out$street), ]
}
