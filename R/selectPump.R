#' Select desired pump neighborhoods.
#'
#' Allows negative selection.
#' @param pump.data Object. Pump data.
#' @param pump.select Character of Numeric. Vector of numeric pump IDs or pump names (street names) to define a pump neighborhood. Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @noRd

selectPump <- function(pump.data, pump.select = NULL, vestry = FALSE) {
  if (!is.null(pump.select)) {
    if (is.numeric(pump.select)) {
      if (any(!abs(pump.select) %in% pump.data$id)) {
        stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ',
          nrow(pump.data), ".", call. = FALSE)
      }
    }

    destination.chk <- vapply(pump.select, function(x) {
      if (is.numeric(x)) {
        name.chk <- FALSE
        number.chk <- abs(x) %in% pump.data$id
      } else if (is.character(x)) {
        tmp <- caseAndSpace(x)
        name.chk <- tmp %in% pump.data$street
        if (suppressWarnings(!is.na(as.integer(tmp)))) {
          number.chk <- abs(as.integer(tmp)) %in% pump.data$id
        } else {
          number.chk <- FALSE
        }
      }
      name.chk | number.chk
    }, logical(1L))

    if (all(!destination.chk)) {
      msg1 <- "No valid pumps. Check numerical ID or spelling in 'pumps' or"
      msg2 <- " 'pumps.vestry'."
      stop(msg1, msg2, call. = FALSE)

    } else if (all(destination.chk)) {
      if (is.numeric(pump.select)) {
        if (all(pump.select > 0)) {
          out <- pump.data$id[pump.data$id %in% pump.select]
        } else if (all(pump.select < 0)) {
          out <- setdiff(pump.data$id, abs(pump.select))
        } else {
          stop("'pump.select must either be all positive or negative.",
            call. = FALSE)
        }
      } else if (is.character(pump.select)) {
        audit <- lapply(pump.select, function(x) {
          if (suppressWarnings(!is.na(as.integer(x)))) {
            number.chk <- as.integer(x) %in% pump.data$id
            name.chk <- FALSE
          } else if (is.character(x)) {
            tmp <- caseAndSpace(x)
            name.chk <- tmp %in% pump.data$street
            number.chk <- FALSE
          }
          list(name.chk = name.chk, number.chk = number.chk)
        })

        num.sel <- vapply(audit, function(x) x$number.chk, logical(1L))
        pump.num <- as.integer(pump.select[num.sel])

        if (any(pump.num > 0) & any(pump.num < 0)) {
          stop("'pump.select' must either be all positive or negative.",
            call. = FALSE)
        }

        chr.sel <- vapply(audit, function(x) x$name.chk, logical(1L))

        if (any(chr.sel)) {
          pump.chr <- vapply(pump.select[chr.sel], caseAndSpace, character(1L))
          sel <- pump.data$street %in% pump.chr
          out <- sort(c(pump.num, pump.data[sel, ]$id))
        } else {
          out <- sort(pump.num)
        }
      }

    } else if (any(!destination.chk)) {
      message("Note invalid/misspelled pump(s): ",
        paste(pump.select[!destination.chk], collapse = ", "))

      if (is.character(pump.select)) {
        audit <- lapply(pump.select, function(x) {
         if (suppressWarnings(!is.na(as.integer(x)))) {
           number.chk <- abs(as.integer(x)) %in% pump.data$id
           name.chk <- FALSE
         } else if (is.character(x)) {
           tmp <- caseAndSpace(x)
           name.chk <- tmp %in% pump.data$street
           number.chk <- FALSE
         }
         list(name.chk = name.chk, number.chk = number.chk)
        })

        num.sel <- vapply(audit, function(x) x$number.chk, logical(1L))
        pump.num <- as.integer(pump.select[num.sel])

        if (length(pump.num) > 0) {
          if (any(pump.num > 0) & any(pump.num < 0)) {
            stop('pump.select must either be strictly positive or negative.',
              call. = FALSE)
          } else if (all(pump.num < 0)) {
            pump.num <- setdiff(pump.data$id, abs(pump.num))
          }
        }

        chr.sel <- vapply(audit, function(x) x$name.chk, logical(1L))

        if (any(chr.sel)) {
          pump.chr <- vapply(pump.select[chr.sel], caseAndSpace, character(1L))
          sel <- pump.data$street %in% pump.chr
          out <- sort(c(pump.num, pump.data[sel, ]$id))
        } else {
          out <- sort(pump.num)
        }

      } else if (is.numeric(pump.select)) {
        pump.vec <- pump.select[destination.chk]
        if (all(pump.vec > 0)) {
          out <- sort(pump.data[pump.data$id %in% pump.vec, ]$id)
        } else if (all(pump.select < 0)) {
          out <- sort(pump.data[!pump.data$id %in% abs(pump.vec), ]$id)
        }
      }
    }

  } else {
    out <- pump.data$id
  }

  unique(out)
}
