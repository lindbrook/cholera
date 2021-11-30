#' Partition string-of-pearl fatality address graphs.
#'
#' Separate even and odd vertices to avoid point overlap.
#' @param g Object. 'igraph' list of graphs.
#' @return An R data frame.
#' @export

partitionStringGraph <- function(g) {
  edge.list.ordered <- lapply(g, function(x) {
    edg.lst <- igraph::as_edgelist(x)
    edg.lst <- data.frame(v1 = as.numeric(edg.lst[, 1]),
                          v2 = as.numeric(edg.lst[, 2]))

    v.table <- table(unlist(edg.lst))
    endpt <- as.numeric(names(v.table[v.table == 1]))[1]

    row.id <- vector("integer", nrow(edg.lst))
    link.id <- vector("integer", nrow(edg.lst))

    for (i in seq_along(row.id)) {
      if (i == 1) {
        endpt.id <- vapply(seq_along(edg.lst$v1), function(i) {
          any(endpt %in% edg.lst[i, ])
        }, logical(1L))

        sel <- which(endpt.id)
        row.id[1] <- sel
        alpha <- edg.lst[sel, ]
        link.id[1] <- alpha[alpha != endpt]

      } else {
        id <- row.id[row.id != 0]
        id <- id[length(id)]
        link <- link.id[link.id != 0]
        link <- link[length(link)]

        candidate <- which(vapply(seq_along(edg.lst$v1), function(i) {
          any(link %in% edg.lst[i, ])
        }, logical(1L)))

        new.id <- setdiff(candidate, id)
        row.id[i] <- new.id
        tmp <- edg.lst[new.id, ]
        link.id[i] <- tmp[tmp != link]
      }
    }
    list(edge.list = edg.lst[row.id, ], vertices = c(endpt, link.id))
  })

  names(edge.list.ordered) <- names(g)

  lapply(edge.list.ordered, function(x) {
    dat <- x$vertices
    data.frame(v1 = dat[seq_along(dat) %% 2 != 0],
               v2 = dat[seq_along(dat) %% 2 == 0])
  })
}
