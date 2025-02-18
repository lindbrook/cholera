longTitle <- function(long.title, type, pmp, path.data, orig, land, x) {
  if (long.title) {
    if (type == "case-pump") {
      p.nm <- pmp[pmp$id == path.data$destination, ]$street
      if (orig < 1000L | orig > max(land$case)) {
        if (x$location == "nominal") {
          alpha <- paste("Case", orig)
        } else if (x$location %in% c("anchor", "orthogonal")) {
          alpha <- paste0("Anchor ", orig, " (Case ", x$orig, ")")
        }
        omega <- paste(p.nm, "Pump", paste0("(#", path.data$destination, ")"))
      } else if (orig >= 1000L & orig <= max(land$case)) {
        c.nm <- land[land$case == orig, ]$name
        alpha <- paste(c.nm, paste0("(#", orig, ")"))
        omega <- paste(p.nm, "Pump", paste0("(#", path.data$destination, ")"))
      }
    } else if (type == "cases") {
      if (orig >= 1000L & path.data$destination >= 1000L) {
        c.orig.nm <- land[land$case == orig, ]$name
        c.dest.nm <- land[land$case == path.data$destination, ]$name
        alpha <- paste(c.orig.nm, paste0("(#", orig, ")"))
        omega <- paste(c.dest.nm, paste0("(#", path.data$destination, ")"))
      } else if (orig < 1000L & path.data$destination >= 1000L) {
        c.dest.nm <- land[land$case == path.data$destination, ]$name
        alpha <- paste("Case", orig)
        omega <- paste(c.dest.nm, paste0("(#", path.data$destination, ")"))
      } else if (orig >= 1000L & path.data$destination < 1000L) {
        c.orig.nm <- land[land$case == orig, ]$name
        alpha <- paste(c.orig.nm, paste0("(#", orig, ")"))
        omega <- paste("to Case", path.data$destination)
      } else {
        alpha <- paste("Case", orig)
        omega <- paste("Case", path.data$destination)
      }
    } else if (type == "pumps") {
      orig.nm <- pmp[pmp$id == path.data$origin, ]$street
      dest.nm <- pmp[pmp$id == path.data$destination, ]$street
      alpha <- paste(orig.nm, paste0("(p", path.data$origin, ")"))
      omega <- paste(dest.nm, paste0("(p", path.data$destination, ")"))
    }
    title(main = paste(alpha, "to", omega))
  } else {
    if (type == "case-pump") {
      title(main = paste("Case", orig, "to Pump", path.data$destination))
    } else if (type == "cases") {
      title(main = paste("Case", orig, "to Case", path.data$destination))
    } else if (type == "pumps") {
      title(main = paste("Pump", orig, "to Pump", path.data$destination))
    }
  }
}

mapDataRange <- function(dat, land, path.data, vars, ew, ns) {
  if (any(path.data$origin >= 1000L &
          path.data$origin <= max(land$case))) {
    land.orig <- land[land$case %in% path.data$origin, ]
    if (grepl("Square", land.orig$name)) {
      sq.nm <- unlist(strsplit(path.data$origin.nm, "-"))[1]
      sel <- grepl(sq.nm, cholera::landmarks$name)
      label.orig <- cholera::landmarks[sel, vars]
    } else {
      label.orig <- land.orig[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
      names(label.orig) <- vars
    }
  }

  if (any(path.data$destination >= 1000L &
          path.data$origin <= max(land$case))) {
    land.dest <- land[land$case %in% path.data$destination, ]
    if (grepl("Square", land.dest$name)) {
      sq.nm <- unlist(strsplit(path.data$destination.nm, "-"))[1]
      sel <- grepl(sq.nm, cholera::landmarks$name)
      label.dest <- cholera::landmarks[sel, vars]
    } else {
      label.dest <- land.dest[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
      names(label.dest) <- vars
    }
  }

  if (exists("label.orig") & exists("label.dest")) {
    rbind(dat[, vars], label.orig, label.dest)
  } else if (exists("label.orig") & !exists("label.dest")) {
    rbind(dat[, vars], label.orig)
  } else if (!exists("label.orig") & exists("label.dest")) {
    rbind(dat[, vars], label.dest)
  } else {
    dat[, vars]
  }
}

validateCase <- function(x, case.set, include.landmarks) {
  if (case.set == "observed") {
    case.id <- cholera::fatalities$case
    case.nm <- paste(case.id)
    case.msg <- paste0("Case IDs range from 1 to ", max(case.id), ".")

    if (include.landmarks) {
      vars.lndmrk <- c("case", "name")
      lndmrk.sq <- cholera::landmark.squares[, vars.lndmrk]
      lndmrk.etc <- cholera::landmarks[, vars.lndmrk]
      lndmrk <- rbind(lndmrk.sq, lndmrk.etc)

      lndmrk.msg1 <- "Landmark IDs range from "
      lndmrk.msg2 <- paste0(min(lndmrk$case), ":", max(lndmrk$case), ".")
      lndmrk.msg <- paste0(lndmrk.msg1, lndmrk.msg2)

      case.id <- c(case.id, lndmrk$case)
      case.nm <- c(case.nm, lndmrk$name)
    }

    if (is.null(x)) {
        out <- case.id
        out.nm <- case.nm
    } else if (is.numeric(x)) {
      if (any(!x %in% case.id)) {
        if (!include.landmarks) {
          stop('For case.set = "observed", ', case.msg, call. = FALSE)
        } else {
          stop('For case.set = "observed" and include.landmarks = TRUE, ',
            case.msg, "\n", lndmrk.msg, call. = FALSE)
        }
      }
      if (all(abs(x) %in% case.id)) {
        if (all(x > 0)) {
          sel <- case.id %in% x
        } else if (all(x < 0)) {
          sel <- !case.id %in% abs(x)
        } else stop("destination should be strictly positive or negative.",
            call. = FALSE)

        out <- case.id[sel]
        out.nm <- case.nm[sel]

      } else if (any(!abs(x) %in% case.id)) {
        dropped <- paste(x[!x %in% case.id], collapse = ", ")

        if (exists("lndmrk.msg")) {
          message("Note: ", case.msg, " Invalid IDs (", dropped, ") dropped. ",
            lndmrk.msg)
        } else {
          message("Note: ", case.msg, " Invalid IDs (", dropped, ") dropped.")
        }

        if (all(x > 0)) {
          sel <- case.id %in% x
        } else if (all(x < 0)) {
          sel <- !case.id %in% abs(x)
        } else stop("destination should be strictly positive or negative.",
            call. = FALSE)

        out <- case.id[sel]
        out.nm <- case.nm[sel]

      } else if (all(!abs(x) %in% case.id)) stop(case.msg, .call = FALSE)

    } else if (is.character(x)) {
      x <- vapply(x, caseAndSpace, character(1L))

      # Square exits
      if (any(grepl("-", x))) {
        nm.string <- unlist(strsplit(x, "-"))
        ptB <- toupper(nm.string[2])
        x <- paste0(nm.string[1], "-", ptB)
      }

      if (all(!x %in% case.nm)) {
        ptA <- paste0("Case names are 1:", max(cholera::fatalities$case), ". ")
        ptB <- "Landmarks names in landmark.squaresB or landmarksB. "
        ptC <- 'Or type = "cases"?'
        stop(ptA, ptB, ptC, call. = FALSE)
      } else if (any(!x %in% case.nm)) {
        x0 <- x
        x.ok <- x[x %in% case.nm]

        sq.candidate <- x[!x %in% case.nm]
        dash.chk <- vapply(sq.candidate, function(x) grepl("-", x), logical(1L))
        sq.candidate <- sq.candidate[dash.chk]
        sq.candidate <- vapply(sq.candidate, squarePostfix, character(1L))
        sq.candidate <- sq.candidate[sq.candidate %in% case.nm]

        if (length(x.ok) != 0 & length(sq.candidate) != 0) {
          x <- c(x.ok, sq.candidate)
        } else if (length(x.ok) == 0 & length(sq.candidate) != 0) {
          x <- sq.candidate
        } else if (length(x.ok) != 0 & length(sq.candidate) == 0) {
          x <- x.ok
        }

        dropped <- paste(setdiff(x0, x), collapse = ", ")
        message("Invalid IDs (", dropped, ") dropped.")
        sel <- case.nm %in% x
        out <- case.id[sel]
        out.nm <- case.nm[sel]
      } else if (all(x %in% case.nm)) {
        sel <- case.nm %in% x
        out <- case.id[sel]
        out.nm <- case.nm[sel]
      }
    }

  } else if (case.set == "expected") {
    case.id <- cholera::sim.ortho.proj$case # equiv. to latlong.sim.ortho.proj
    case.nm <- paste(case.id)
    case.msg <- paste0("Expected case IDs range from ", min(case.id), " to ", 
      max(case.id), ".")

    if (include.landmarks) {
      vars.lndmrk <- c("case", "name")
      lndmrk.sq <- cholera::landmark.squares[, vars.lndmrk]
      lndmrk.etc <- cholera::landmarks[, vars.lndmrk]
      lndmrk <- rbind(lndmrk.sq, lndmrk.etc)

      lndmrk.msg1 <- "Landmark IDs range from "
      lndmrk.msg2 <- paste0(min(lndmrk$case), ":", max(lndmrk$case), ".")
      lndmrk.msg <- paste0(lndmrk.msg1, lndmrk.msg2)

      case.id <- c(case.id, lndmrk$case)
      case.nm <- c(case.nm, lndmrk$name)
    }

    if (is.null(x)) {
      out <- case.id
      out.nm <- case.nm
    } else {
      x <- ifelse(!is.numeric(x), as.numeric(x), x)

      if (all(!x %in% case.id)) {
        if (any(!x %in% case.id)) {
          if (!include.landmarks) {
            stop('For case.set = "expected", ', case.msg, call. = FALSE)
          } else {
            stop('For case.set = "expected" and include.landmarks = TRUE, ',
              case.msg, "\n", lndmrk.msg, call. = FALSE)
          }
        }
      } else if (any(!x %in% case.id)) {
        dropped <- paste(x[!x %in% case.id], collapse = ", ")
        message("Note: ", case.msg, " Invalid IDs (", dropped, ") dropped.")
        out <- x[x %in% case.id]
      } else {
        out <- x
      }
      out.nm <- paste(out)
    }
  }
  list(out = out, out.nm = out.nm)
}

squarePostfix <- function(string) {
  land.parts <- unlist(strsplit(string, " "))
  dash.chk <- grepl("-", land.parts) # check for Square postfix
  if (any(dash.chk)) {
    dash <- land.parts[dash.chk]
    no_dash <- land.parts[!dash.chk]
    postfix.id <- unlist(gregexpr("-", dash)) + 1
    postfix <- substr(dash, postfix.id, nchar(dash))
    substr(dash, postfix.id, postfix.id) <- toupper(postfix)
    paste(no_dash, dash)
  } else string
}

validatePump <- function(x, pmp, vestry) {
  if (is.null(x)) {
    out <- pmp$id
    out.nm <- pmp$street
  } else if (is.numeric(x)) {
    if (all(!abs(x) %in% pmp$id)) {
      stop("For vestry = ", vestry, ", pump IDs range from 1 to ", nrow(pmp),
        "." , call. = FALSE)
    } else if (all(abs(x) %in% pmp$id)) {
      if (all(x > 0)) {
        out <- x
        out.nm <- pmp[pmp$id %in% x, ]$street
      } else if (all(x < 0)) {
        sel <- !pmp$id %in% abs(x)
        out <- pmp$id[sel]
        out.nm <- pmp[sel, "street"]
      } else stop("pump IDs should be all positive or all negative.",
          call. = FALSE)
    } else if (any(abs(x) %in% pmp$id)) {
      message("Note: for vestry = ", vestry, ", abs(pump ID) range from 1 to ",
        nrow(pmp), ".")
      if (all(x > 0)) {
        out <- x[x %in% pmp$id]
        out.nm <- pmp[pmp$id %in% x, ]$street
      } else if (all(x < 0)) {
        sel <- !pmp$id %in% abs(x)
        out <- pmp$id[sel]
        out.nm <- pmp[sel, "street"]
      } else stop("pump IDs should be all positive or all negative.",
          call. = FALSE)
    }
  } else if (is.character(x)) {
    x <- caseAndSpace(x)
    ptA <- 'Pump (street) name not found (vestry = TRUE?). '
    ptB <- 'For landmarks, use type = "cases".'

    if (all(!x %in% pmp$street)) {
      stop(ptA, ptB, call. = FALSE)
    } else if (any(!x %in% pmp$street)) {
      message(ptA, ptB, call. = FALSE)
      x <- x[x %in% pmp$street]
      out.nm <- pmp[pmp$id %in% x, ]$street
    } else {
      out <- pmp[pmp$street %in% x, ]$id
      out.nm <- x
    }
  }

  list(out = out, out.nm = out.nm)
}

sqCases <- function(sq = "Golden", variable = "case") {
  if (!sq %in% c("Golden", "Soho")) sq <- wordCase(sq)
  if (!sq %in% c("Golden", "Soho")) stop('sq must be "Golden" or "Soho".')
  if (!variable %in% c("case", "name")) {
    stop('variable must be "case" or "name".' )
  }
  sel.A <- grep(sq, cholera::landmark.squares$name)
  sel.B <- grep(sq, cholera::landmarks$name)
  a <- cholera::landmark.squares[sel.A, variable]
  b <- cholera::landmarks[sel.B, variable]
  c(a, b)
}
