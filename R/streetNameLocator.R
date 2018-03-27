#' Locate road by name.
#'
#' Highlight a road and its cases. See the list of road names in \code{vignette}("road.names").
#' @param road.name Character vector. Note that \code{streetNameLocator}() tries to correct for case and to remove extra spaces.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param cases Character. Plot cases: NULL, "anchors" or "all".
#' @param add.title Logical. Include title.
#' @param add.pump Logical. Include nearby pumps.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param highlight Logical. Highlight selected road.
#' @param unit Character. Unit of measurement: "meter" or "yard". NULL returns the map's native scale.
#' @return A base R graphics plot.
#' @seealso \code{\link{roads}}, \code{\link{road.segments}}, \code{\link{streetNumberLocator}}, \code{vignette("road.names")}
#' @import graphics
#' @export
#' @examples
#' streetNameLocator("Oxford Street")
#' streetNameLocator("oxford street")
#' streetNameLocator("Cambridge Street", zoom = TRUE)
#' streetNameLocator("Cambridge Street", zoom = TRUE, radius = 0)

streetNameLocator <- function(road.name, zoom = FALSE, radius = 0.1,
  cases = "anchors", add.title = TRUE, add.pump = TRUE, vestry = FALSE,
  highlight = TRUE, unit = "meter") {

  real.road.names <- unique(cholera::roads$name)

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  if (is.null(cases) == FALSE) {
    if (cases %in% c("anchors", "all") == FALSE)
      stop('If specified, "cases" must either be "anchors" or "all".')
  }

  if (is.character(road.name) == FALSE) {
    stop("Road name must be a character string.")
  } else if (road.name %in% real.road.names == FALSE) {
    case.name <- caseAndSpace(road.name)
    if (case.name %in% real.road.names == FALSE) {
      error.msg <- paste("Invalid road name.",
        'Check spelling or see list of road names in vignette("road.names").')
      stop(error.msg)
    } else name <- case.name
  } else name <- road.name

  selected.road <- cholera::roads[cholera::roads$name == name, "street"]
  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)

  rng <- lapply(cholera::roads[cholera::roads$name == name, c("x", "y")],
      range)
  x.rng <- c(min(rng$x) - radius, max(rng$x) + radius)
  y.rng <- c(min(rng$y) - radius, max(rng$y) + radius)

  if (zoom == FALSE) {
    plot(cholera::fatalities[, c("x", "y")], xlim = range(cholera::roads$x),
      ylim = range(cholera::roads$y), pch = 15, cex = 0.5, col = "gray",
      asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))

    if (add.pump) {
      if (vestry) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 17, cex = 1,
          col = "blue")
        text(cholera::pumps.vestry[, c("x", "y")],
          label = paste0("p", cholera::pumps.vestry$id), pos = 1)
      } else {
        points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
        text(cholera::pumps[, c("x", "y")],
          label = paste0("p", cholera::pumps$id), pos = 1)
      }
    }

    if (highlight) {
      invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
        lwd = 3))
    }
  } else {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = NA, asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))

    if (is.null(cases) == FALSE) {
      id <- cholera::road.segments[cholera::road.segments$name == name, "id"]
      seg.ortho <- cholera::ortho.proj[cholera::ortho.proj$road.segment %in%
        id, ]
      seg.anchors <- cholera::fatalities.address$anchor.case %in% seg.ortho$case
      seg.cases <- cholera::fatalities$case %in% seg.ortho$case

      if (cases == "all") {
        text(cholera::fatalities[!seg.cases, c("x", "y")],
          labels = cholera::fatalities$case[!seg.cases], cex = 0.5)
        if (any(seg.cases)) {
          if (highlight) {
            text(cholera::fatalities[seg.cases, c("x", "y")],
              labels = cholera::fatalities$case[seg.cases], cex = 0.5,
              col = "red")
          } else {
            text(cholera::fatalities[seg.cases, c("x", "y")],
              labels = cholera::fatalities$case[seg.cases], cex = 0.5)
          }

        }
      } else if (cases == "anchors") {
        text(cholera::fatalities.address[!seg.anchors, c("x", "y")],
          labels = cholera::fatalities.address$anchor.case[!seg.anchors],
          cex = 0.5)
        if (any(seg.anchors)) {
          if (highlight) {
            text(cholera::fatalities.address[seg.anchors, c("x", "y")],
              labels = cholera::fatalities.address$anchor.case[seg.anchors],
              cex = 0.5, col = "red")
          } else {
            text(cholera::fatalities.address[seg.anchors, c("x", "y")],
              labels = cholera::fatalities.address$anchor.case[seg.anchors],
              cex = 0.5)
          }
        }
      }
    }

    if (add.pump) {
      if (vestry) {
        points(cholera::pumps.vestry[, c("x", "y")], pch = 17, cex = 1,
          col = "blue")
        text(cholera::pumps.vestry[, c("x", "y")],
          label = paste0("p", cholera::pumps.vestry$id), pos = 1)
      } else {
        points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
        text(cholera::pumps[, c("x", "y")],
          label = paste0("p", cholera::pumps$id), pos = 1)
      }
    }

    if (highlight) {
      invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
        lwd = 3))
    }
  }

  street.length <- cholera::streetLength(name, unit)

  if (is.null(unit)) {
    subtitle <- paste(round(street.length, 2), "units")
  } else if (unit == "meter") {
    subtitle <- paste(round(street.length, 2), "meters")
  } else if (unit == "yard") {
    subtitle <- paste(round(street.length, 2), "yards")
  }

  if (add.title) title(main = name, sub = subtitle)
}

wordCase <- function(x) {
  # faster than tools::toTitleCase(), bytecode?
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

caseAndSpace <- function(name) {
  valid.names <- unique(cholera::roads$name)
  name.parts <- unlist(strsplit(name, " "))
  extra.spaces <- vapply(name.parts, nchar, integer(1L))

  if (any(extra.spaces == 0)) {
    name.parts <- name.parts[extra.spaces != 0]
    road.name.string <- paste(name.parts, collapse = " ")
  } else {
    road.name.string <- paste(name.parts, collapse = " ")
  }

  if (road.name.string %in% valid.names) {
    string.out <- road.name.string
  } else {
    lo.case <- tolower(road.name.string)

    road.name.string <- unlist(strsplit(lo.case, " "))
    vec.length <- seq_along(road.name.string)
    word.case <- wordCase(road.name.string)
    string.out <- paste0(word.case, collapse = " ")
  }

  if (string.out %in% valid.names) {
    return(string.out)
  } else {

    # ------- tests ------- #

    # George Court (I) #
    test.multiple.name <- vapply(road.name.string, function(x) {
      grepl("(", x, fixed = TRUE)
    }, logical(1L))

    # Macclesfield Street/Gerrard Street #
    test.two.roads <- vapply(road.name.string, function(x) {
      grepl("/", x, fixed = TRUE)
    }, logical(1L))

    # Adam and Eve Court" #
    test.and <- "and" %in% road.name.string

    # Unknown-A1 #
    test.unknown <- grepl("unknown", road.name.string)

    # ------- road strings ------- #

    if (any(test.multiple.name)) {
      multi.name <- vapply(road.name.string, function(x) {
        grepl("(", x, fixed = TRUE)
      }, logical(1L))

      others.position <- which(multi.name == FALSE)
      multi.position <- which(multi.name)
      word.case <- wordCase(road.name.string[others.position])
      multi.case <- toupper(road.name.string[multi.position])
      string.out <- paste(c(word.case, multi.case), collapse = " ")

    } else if (any(test.two.roads)) {
      slash.position <- grep("/", road.name.string)

      if (road.name.string[test.two.roads] == "/") {
        # isolated "/": "Princes Street / Hanover Square"
        slash.names <- c(slash.position - 1, slash.position, slash.position + 1)
        word.case <- wordCase(road.name.string)
        pre <- word.case[vec.length < min(slash.names)]
        delimited <- paste(word.case[slash.names], collapse = "")
        post.select <- vec.length > max(slash.names)

        if (any(post.select)) {
          # "Princes Street/Hanover Square"
          post <- word.case[post.select]
          string.out <- paste(pre, delimited, post, collapse = " ")
        } else {
          # "Richmond Buildings/Mews"
          string.out <- paste(pre, delimited, collapse = " ")
        }

      } else {
        # "Princes Street /Hanover Square", "Princes Street/ Hanover Square"
        word.case <- wordCase(road.name.string)
        pre <- word.case[vec.length < slash.position]
        delimited <- tools::toTitleCase(word.case[slash.position])
        post.select <- vec.length > slash.position

        if (any(post.select)) {
          post <- word.case[post.select]

          if (length(pre) == 1 & sum(post.select) == 2) {
            string.out <- paste(pre, paste0(delimited, post[1]), post[2],
              collapse = " ")
          } else if (length(pre) == 2 & sum(post.select) == 1) {
            string.out <- paste(pre[1], paste0(pre[2], delimited), post,
              collapse = " ")
          } else if (length(pre) == 1 & sum(post.select) == 1) {
            string.out <- paste(pre, paste0(delimited, post), collapse = " ")
          }
        } else {
          string.out <- paste(pre[1], paste0(pre[2], delimited), collapse = " ")
        }
      }

    } else if (test.and) {
      others.position <- which(road.name.string %in% "and" == FALSE)
      and.position <- which(road.name.string == "and")
      word.case <- wordCase(road.name.string[others.position])
      string.out <- paste0(c(word.case[others.position < and.position],
        tolower(road.name.string[and.position]),
        word.case[others.position > and.position]), collapse = " ")

    } else if (any(test.unknown)) {
      dash.position <- grep("-", road.name.string)

      if (road.name.string[dash.position] == "-") {
        # isolated "-": "Unknown - C"
        name.parts <- unlist(strsplit(road.name.string, "-"))
        word.case <- wordCase(name.parts)
        string.out <- paste0(word.case[1], "-", word.case[3])
      } else {
        # "Unknown- C", "Unknown -C"
        word.case <- tools::toTitleCase(road.name.string)
        string.out <- paste(word.case, collapse = "")
      }
    }
  }
  string.out
}
