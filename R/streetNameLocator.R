#' Locate road by name.
#'
#' Highlight a road and its cases. See the list of road names in \code{vignette}("road.names").
#' @param road.name Character vector. Note that \code{streetNameLocator}() tries to correct for case and to remove extra spaces.
#' @param zoom Logical or Numeric. A numeric value >= 0 controls the degree of zoom. The default is FALSE, which is equivalent to zero.
#' @param cases Character. Plot cases: \code{NULL}, "address" or "fatality".
#' @param token Character. "id" or "point".
#' @param add.title Logical. Include title.
#' @param add.subtitle Logical. Include subtitle with road information.
#' @param add.pump Logical. Include nearby pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param highlight Logical. Highlight selected road and its cases.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @return A base R graphics plot.
#' @import graphics
#' @export
#' @examples
#' streetNameLocator("Oxford Street")
#' streetNameLocator("oxford street")
#' streetNameLocator("Cambridge Street", zoom = TRUE)
#' streetNameLocator("Cambridge Street", zoom = 0.5)

streetNameLocator <- function(road.name = "Broad Street", zoom = FALSE,
  cases = "address", token = "id", add.title = TRUE, add.subtitle = TRUE,
  add.pump = TRUE, vestry = FALSE, highlight = TRUE, distance.unit = "meter",
  time.unit = "minute", walking.speed = 5) {

  real.road.names <- unique(cholera::roads$name)
  vars <- c("x", "y")

  if (is.character(road.name) == FALSE) {
    stop("Road name must be a character string.", call. = FALSE)
  } else if (road.name %in% real.road.names == FALSE) {
    case.name <- caseAndSpace(road.name)
    if (case.name %in% real.road.names == FALSE) {
      txt1 <- "Invalid road name. Check spelling or"
      txt2 <- 'see list of road names in vignette("roads").'
      error.msg <- paste(txt1, txt2)
      stop(error.msg, call. = FALSE)
    } else name <- case.name
  } else name <- road.name

  if (is.null(cases) == FALSE) {
    if (cases %in% c("address", "fatality") == FALSE) {
      stop('If specified, cases must either be "address" or "fatality".',
        call. = FALSE)
    }
  }

  if (token %in% c("id", "point") == FALSE) {
    stop('token must be "id", or "point".', call. = FALSE)
  }

  if (distance.unit %in% c("meter", "yard", "native") == FALSE) {
    stop('distance.unit must be "meter", "yard" or "native".', call. = FALSE)
  }

  if (time.unit %in% c("minute", "hour", "second") == FALSE) {
    stop('time.unit must be "hour", "minute" or "second".', call. = FALSE)
  }

  selected.road <- cholera::roads[cholera::roads$name == name, "street"]
  roads.list <- split(cholera::roads[, vars], cholera::roads$street)

  rng <- lapply(cholera::roads[cholera::roads$name == name, vars], range)

  if (is.logical(zoom)) {
    if (zoom) {
      radius <- 0.1
      x.rng <- c(min(rng$x) - radius, max(rng$x) + radius)
      y.rng <- c(min(rng$y) - radius, max(rng$y) + radius)
    } else {
      x.rng <- range(cholera::roads$x)
      y.rng <- range(cholera::roads$y)
    }
  } else if (is.numeric(zoom)) {
    if (zoom >= 0) {
      x.rng <- c(min(rng$x) - zoom, max(rng$x) + zoom)
      y.rng <- c(min(rng$y) - zoom, max(rng$y) + zoom)
    } else stop("If numeric, zoom must be >= 0.", call. = FALSE)
  } else stop("zoom must either be logical or numeric.", call. = FALSE)

  plot(cholera::fatalities[, vars], xlim = x.rng, ylim = y.rng,
    pch = NA, asp = 1)
  invisible(lapply(roads.list, lines, col = "gray"))

  if ((is.logical(zoom) & zoom == TRUE) | is.numeric(zoom)) {
    if (is.null(cases) == FALSE) {
      id <- cholera::road.segments[cholera::road.segments$name == name, "id"]
      seg.ortho <- cholera::ortho.proj[cholera::ortho.proj$road.segment %in%
        id, ]
      seg.anchors <- cholera::fatalities.address$anchor %in% seg.ortho$case
      seg.cases <- cholera::fatalities$case %in% seg.ortho$case

      if (token == "id") {
        if (cases == "fatality") {
          text(cholera::fatalities[!seg.cases, vars],
            labels = cholera::fatalities$case[!seg.cases], cex = 0.5)
          if (any(seg.cases)) {
            if (highlight) {
              text(cholera::fatalities[seg.cases, vars],
                labels = cholera::fatalities$case[seg.cases], cex = 0.5,
                col = "red")
            } else {
              text(cholera::fatalities[seg.cases, vars],
                labels = cholera::fatalities$case[seg.cases], cex = 0.5)
            }
          }
        } else if (cases == "address") {
          text(cholera::fatalities.address[!seg.anchors, vars],
            labels = cholera::fatalities.address$anchor[!seg.anchors],
            cex = 0.5)
          if (any(seg.anchors)) {
            if (highlight) {
              text(cholera::fatalities.address[seg.anchors, vars],
                labels = cholera::fatalities.address$anchor[seg.anchors],
                cex = 0.5, col = "red")
            } else {
              text(cholera::fatalities.address[seg.anchors, vars],
                labels = cholera::fatalities.address$anchor[seg.anchors],
                cex = 0.5)
            }
          }
        }

      } else if (token == "point") {
        if (cases == "fatality") {
          points(cholera::fatalities[!seg.cases, vars], pch = 15, cex = 0.5)
          if (any(seg.cases)) {
            if (highlight) {
              points(cholera::fatalities[seg.cases, vars], pch = 15, cex = 0.5,
                col = "red")
            } else {
              points(cholera::fatalities[seg.cases, vars], pch = 15, cex = 0.5)
            }
          }
        } else if (cases == "address") {
          points(cholera::fatalities.address[!seg.anchors, vars], pch = 15,
            cex = 0.5)
          if (any(seg.anchors)) {
            if (highlight) {
              points(cholera::fatalities.address[seg.anchors, vars], pch = 15,
                cex = 0.5, col = "red")
            } else {
              points(cholera::fatalities.address[seg.anchors, vars], pch = 15,
                cex = 0.5)
            }
          }
        }
      }
    }
  }

  if (add.pump) {
    if (vestry) {
      points(cholera::pumps.vestry[, vars], pch = 17, cex = 1, col = "blue")
      text(cholera::pumps.vestry[, vars],
        label = paste0("p", cholera::pumps.vestry$id), pos = 1)
    } else {
      points(cholera::pumps[, vars], pch = 17, cex = 1, col = "blue")
      text(cholera::pumps[, vars], label = paste0("p", cholera::pumps$id),
        pos = 1)
    }
  }

  if (highlight) {
    invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
      lwd = 3))
  }

  if (add.title) title(main = name)

  if (add.subtitle) {
    street.length <- streetLength(name, distance.unit)
    est.time <- distanceTime(street.length, distance.unit = distance.unit,
      time.unit = time.unit, walking.speed = walking.speed)

    if (time.unit == "hour") {
      nominal.time <- paste(round(est.time, 1), "hr")
    } else if (time.unit == "minute") {
      nominal.time <- paste(round(est.time, 1), "min")
    } else if (time.unit == "second") {
      nominal.time <- paste(round(est.time, 1), "sec")
    }

    if (distance.unit == "native") {
      subtitle <- paste(round(street.length, 1), "units;", nominal.time)
    } else if (distance.unit == "meter") {
      subtitle <- paste(round(street.length, 1), "m;", nominal.time)
    } else if (distance.unit == "yard") {
      subtitle <- paste(round(street.length, 1), "yd;", nominal.time)
    }

    title(sub = paste(subtitle, "@", walking.speed, "km/hr"))
  }
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
