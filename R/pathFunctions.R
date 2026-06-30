longTitle <- function(long.title, type, pmp, data.summary, orig, land, x) {
  if (long.title) {
    if (type == "case-pump") {
      p.nm <- pmp[pmp$id == data.summary$destination, ]$street
      if (orig < 1000L | orig > max(land$case)) {
        if (x$location == "nominal") {
          alpha <- paste("Case", orig)
        } else if (x$location %in% c("anchor", "orthogonal")) {
          alpha <- paste0("Anchor ", orig, " (Case ", x$orig, ")")
        }
        omega <- paste(p.nm, "Pump", paste0("(#", data.summary$destination,
          ")"))
      } else if (orig >= 1000L & orig <= max(land$case)) {
        c.nm <- land[land$case == orig, ]$name
        alpha <- paste(c.nm, paste0("(#", orig, ")"))
        omega <- paste(p.nm, "Pump",
          paste0("(#", data.summary$destination, ")"))
      }
    } else if (type == "cases") {
      if (orig >= 1000L & data.summary$destination >= 1000L) {
        c.orig.nm <- land[land$case == orig, ]$name
        c.dest.nm <- land[land$case == data.summary$destination, ]$name
        alpha <- paste(c.orig.nm, paste0("(#", orig, ")"))
        omega <- paste(c.dest.nm, paste0("(#", data.summary$destination, ")"))
      } else if (orig < 1000L & data.summary$destination >= 1000L) {
        c.dest.nm <- land[land$case == data.summary$destination, ]$name
        alpha <- paste("Case", orig)
        omega <- paste(c.dest.nm, paste0("(#", data.summary$destination, ")"))
      } else if (orig >= 1000L & data.summary$destination < 1000L) {
        c.orig.nm <- land[land$case == orig, ]$name
        alpha <- paste(c.orig.nm, paste0("(#", orig, ")"))
        omega <- paste("to Case", data.summary$destination)
      } else {
        alpha <- paste("Case", orig)
        omega <- paste("Case", data.summary$destination)
      }
    } else if (type == "pumps") {
      orig.nm <- pmp[pmp$id == data.summary$origin, ]$street
      dest.nm <- pmp[pmp$id == data.summary$destination, ]$street
      alpha <- paste(orig.nm, paste0("(p", data.summary$origin, ")"))
      omega <- paste(dest.nm, paste0("(p", data.summary$destination, ")"))
    }
    title(main = paste(alpha, "to", omega))
  } else {
    if (type == "case-pump") {
      title(main = paste("Case", orig, "to Pump", data.summary$destination))
    } else if (type == "cases") {
      title(main = paste("Case", orig, "to Case", data.summary$destination))
    } else if (type == "pumps") {
      title(main = paste("Pump", orig, "to Pump", data.summary$destination))
    }
  }
}

mapDataRange <- function(path.data, land, data.summary, vars, ew, ns) {
  if (any(data.summary$origin >= 1000L &
          data.summary$origin <= max(land$case))) {
    land.orig <- land[land$case %in% data.summary$origin, ]
    if (grepl("Square", land.orig$name)) {
      sq.nm <- unlist(strsplit(data.summary$origin.nm, "-"))[1]
      sel <- grepl(sq.nm, cholera::landmarks$name)
      label.orig <- cholera::landmarks[sel, vars]
    } else {
      label.orig <- land.orig[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
      names(label.orig) <- vars
    }
  }

  if (any(data.summary$destination >= 1000L &
          data.summary$destination <= max(land$case))) {
    land.dest <- land[land$case %in% data.summary$destination, ]
    if (grepl("Square", land.dest$name)) {
      sq.nm <- unlist(strsplit(data.summary$destination.nm, "-"))[1]
      sel <- grepl(sq.nm, cholera::landmarks$name)
      label.dest <- cholera::landmarks[sel, vars]
    } else {
      label.dest <- land.dest[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
      names(label.dest) <- vars
    }
  }

  if (exists("label.orig") & exists("label.dest")) {
    rbind(path.data[, vars], label.orig, label.dest)
  } else if (exists("label.orig") & !exists("label.dest")) {
    rbind(path.data[, vars], label.orig)
  } else if (!exists("label.orig") & exists("label.dest")) {
    rbind(path.data[, vars], label.dest)
  } else {
    path.data[, vars]
  }
}

validateCase <- function(x, case.set = "observed") {
  obs.case.id <- cholera::fatalities$case
  obs.case.nm <- paste(obs.case.id)
  
  obs.case.msg <- paste0("Observed case IDs range from 1 to ",
    max(obs.case.id), ".")
  
  obs.case.msg.chr <- c(paste0('Observed case IDs range from "1" to ',
    paste0('\"', max(obs.case.id), '\"')), ".")

  exp.case.id <- cholera::sim.ortho.proj$case
  exp.case.nm <- paste(exp.case.id)
  
  exp.case.msg <- paste0("Expected case IDs range from ", min(exp.case.id),
    " to ", max(exp.case.id), ".")  
  exp.case.msg.chr <- paste0("Expected case IDs range from ", '\"',
    min(exp.case.id), '\"', " to ", '\"', max(exp.case.id), '\"', ".")

  vars.lndmrk <- c("case", "name")
  lndmrk.sq <- cholera::landmark.squares[, vars.lndmrk]
  lndmrk.data <- cholera::landmarks[, vars.lndmrk]
  lndmrk <- rbind(lndmrk.sq, lndmrk.data)
  
  lndmrk.msg1 <- "Landmark IDs range from "
  lndmrk.msg2 <- paste0(min(lndmrk$case), " to ", max(lndmrk$case), ".")
  lndmrk.msg <- paste0(lndmrk.msg1, lndmrk.msg2)

  lndmrk.msg2.chr <- paste0('\"', min(lndmrk$case), '\"', " to ",
    '\"', max(lndmrk$case), '\"', ".")
  lndmrk.msg.chr <- paste0(lndmrk.msg1, lndmrk.msg2.chr)
  
  if (case.set == "observed") {
    case.id <- c(obs.case.id, lndmrk$case)
    case.nm <- c(obs.case.nm, paste(lndmrk$case), lndmrk$name)
  } else if (case.set == "expected") {
    case.id <- c(lndmrk$case, exp.case.id)
    case.nm <- c(paste(lndmrk$case), lndmrk$name, exp.case.nm)
  }
  
  if (is.null(x)) {
      out <- case.id
      out.nm <- case.nm
  
  } else if (is.numeric(x)) {
    if (all(!abs(x) %in% case.id)) {
      stop("For numeric IDs (e.g., 1 or 5:9):", "\n", obs.case.msg, "\n",
        lndmrk.msg, "\n", exp.case.msg, call. = FALSE)
    } else if (all(abs(x) %in% case.id)) {
      if (all(x > 0)) {
        sel <- case.id %in% x
      } else if (all(x < 0)) {
        sel <- !case.id %in% abs(x)
      } else {
        stop("destination should strictly positive or negative.", call. = FALSE)
      }
      
      out <- case.id[sel]
      out.nm <- case.nm[case.nm %in% paste(x)]

    } else if (any(!abs(x) %in% case.id)) {
      dropped <- paste(x[!x %in% case.id], collapse = ", ")
      
      message(obs.case.msg, "\n", lndmrk.msg, "\n", exp.case.msg, "\n",
        "Invalid IDs (", dropped, ") dropped. ")
    
      if (all(x > 0)) {
        sel <- case.id %in% x
      } else if (all(x < 0)) {
        sel <- !case.id %in% abs(x)
      } else stop("destination(s) should either be positive or negative.",
          call. = FALSE)

      out <- case.id[sel]
      out.nm <- case.nm[case.nm %in% paste(x)]
    }

  } else if (is.character(x)) {

    # identify numeric cases represented as characters
    lndmrk.nm <- is.na(suppressWarnings(as.numeric(x)))

    # word case for named landmarks
    if (any(lndmrk.nm)) {
      x[lndmrk.nm] <- vapply(x[lndmrk.nm], caseAndSpace, character(1L))
    }
    
    if (any(grepl("-",  x[lndmrk.nm]))) {
      st.exit <- grepl("-",  x[lndmrk.nm])
      nm.string <- unlist(strsplit(x[lndmrk.nm][st.exit], "-"))
      sel <- !seq_along(nm.string) %% 2
      postfix <- toupper(nm.string[sel])
      x[lndmrk.nm][st.exit] <- paste0(nm.string[!sel], "-", postfix)
    }
    
    if (all(!x %in% case.nm)) {
      land.ptB <- "Landmarks names in 'landmark.squares' or 'landmarks'."
      stop('For character IDs (e.g., "1"):', "\n", obs.case.msg.chr, "\n",
        lndmrk.msg.chr, "\n", land.ptB, "\n", exp.case.msg.chr, call. = FALSE)
    
    } else if (any(!x %in% case.nm)) {
      sel <- x %in% case.nm
      dropped <- paste(x[!sel], collapse = ", ")
      message("Invalid IDs (", dropped, ") dropped.")
      
      out <- vector(mode = "logical", length = length(x[sel]))
      num.nm <- is.na(suppressWarnings(as.numeric(x[sel])))
      out[num.nm] <- lndmrk[lndmrk$name %in% x[sel], "case"]
      out[!num.nm] <- x[sel][!num.nm]

      out.nm <- vector(mode = "logical", length = length(x[sel]))
      num.nm <- is.na(suppressWarnings(as.numeric(x[sel])))
      out.nm[num.nm] <- x[sel][num.nm]
      out.nm[!num.nm] <- x[sel][!num.nm]
    
    } else if (all(x %in% case.nm)) {
      if (all(x %in% lndmrk$name)) {
        sel <- lndmrk$name %in% x[x %in% lndmrk$name]
        out <- lndmrk[sel, "case"]
        out.nm <- lndmrk[sel, "name"]
      } else if (any(x %in% lndmrk$name)) {
        lnd.test <- x %in% lndmrk$name
        
        num.id <- as.integer(x[!lnd.test])
        names(num.id) <- x[!lnd.test]
        
        lnd.id <- lndmrk[lndmrk$name %in% x[lnd.test], "case"]
        names(lnd.id) <- lndmrk[lndmrk$name %in% x[lnd.test], "name"]
        
        out <- c(num.id, lnd.id)[x]
        out.nm <- x
        names(out) <- NULL
        names(out.nm) <- NULL
        
      } else if (all(!x %in% lndmrk$name)) {
        out <- case.id[case.id %in% as.numeric(x)]
        out.nm <- case.nm[case.nm %in% x]
      }
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
