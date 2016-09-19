#' Plot road by name.
#'
#' Plots John Snow's map of the 1854 London cholera outbreak and highlights the
#' selected road segment. See the list of road names in \code{vignette}("road.names").
#' @param road.name Character vector. To match names in the data set, \code{streetNameViewer}() corrects for case and removes extraneous spaces.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom. Default value is 1. For values <= 5, the anchor case number is plotted rather than a point for each case.
#' @return A base R graphics plot.
#' @seealso \code{roads}
#'
#' \code{vignette}("road.names")
#'
#' \code{\link[cholera]{streetNumberViewer}}
#' @import graphics
#' @export
#' @examples
#' streetNameViewer("Oxford Street")
#' streetNameViewer("oxford street")
#' streetNameViewer("Cambridge Street", zoom = TRUE)
#' streetNameViewer("Cambridge Street", zoom = TRUE, radius = 0)

streetNameViewer <- function(road.name, zoom = FALSE, radius = 1) {
  real.road.names <- unique(cholera::roads$name)

  if (is.character(road.name) == FALSE) {
    stop("Road name must be a character string.")
  } else if (road.name %in% real.road.names == FALSE) {
    case.name <- caseAndSpace(road.name)
    if (case.name %in% real.road.names == FALSE) {
      error.msg <- paste("Invalid road name.",
        "Check spelling or see list of road names in vignette('road.names').")
      stop(error.msg)
    } else name <- case.name
  } else name <- road.name

  selected.road <- cholera::roads[cholera::roads$name == name, "street"]
  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)

  rng <- lapply(cholera::roads[cholera::roads$name == name, c("x", "y")],
      range)
  x.rng <- c(min(rng$x) - radius, max(rng$x) + radius)
  y.rng <- c(min(rng$y) - radius, max(rng$y) + radius)

  if (!zoom) {
    plot(cholera::fatalities[, c("x", "y")], xlim = range(cholera::roads$x),
      ylim = range(cholera::roads$y), pch = 15, cex = 0.5, col = "gray",
      asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = cholera::pumps$pump.id,
      col = "blue", pos = 1)
    invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
      lwd = 3))
    title(main = name)

  } else if (zoom & radius <= 5) {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = NA, asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    text(cholera::fatalities.address[, c("x", "y")], labels =
      cholera::fatalities.address$anchor.case, cex = 0.5)
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = cholera::pumps$pump.id,
      pos = 1)
    selected.road <- cholera::roads[cholera::roads$name == name, "street"]
    invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
      lwd = 3))
    title(main = name)

  } else {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = 15, cex = 0.5, col = "gray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = cholera::pumps$pump.id,
      pos = 1)
    selected.road <- cholera::roads[cholera::roads$name == name, "street"]
    invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
      lwd = 3))
    title(main = name)
  }
}

wordCase <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

wordCaseB <- function(x) {
  paste0(toupper(substr(x, 2, 2)), tolower(substr(x, 3, nchar(x))))
}

caseAndSpace <- function(name) {
  road.name.string <- unlist(strsplit(name, " "))
  extra.spaces <- vapply(road.name.string, nchar, integer(1L))

  if (any(extra.spaces == 0)) {
    road.name.string <- road.name.string[extra.spaces != 0]
  }

  spaced.name <- paste(road.name.string, collapse = " ")

  if (spaced.name %in% unique(cholera::roads$name)) {
    string.out <- spaced.name
  } else {
    lo.case <- tolower(road.name.string)
    vec.length <- seq_along(lo.case)

    # e.g., "George Court (I)"
    multi.name <- vapply(lo.case, function(x) {
      substr(x, 1, 1) == "("
    }, logical(1L))

    # e.g., "Macclesfield Street/Gerrard Street"
    two.roads <- vapply(lo.case, function(x) {
      grepl("/", x)
    }, logical(1L))

    # e.g., "Unknown-A1"
    unknown.road <- unlist(strsplit(lo.case, "-"))

    # e.g., "Adam and Eve Court"
    if ("and" %in% lo.case) {
      others.position <- which(lo.case %in% "and" == FALSE)
      and.position <- which(lo.case == "and")
      word.case <- lapply(road.name.string[others.position], wordCase)

      string.out <- paste0(c(word.case[others.position < and.position],
                             tolower(lo.case[and.position]),
                             word.case[others.position > and.position]),
                           collapse = " ")

    # e.g., "George Court (I)"
    } else if (any(multi.name)) {
      others.position <- which(multi.name == FALSE)
      multi.position <- which(multi.name)
      word.case <- lapply(lo.case[vec.length < multi.position], wordCase)
      multi.case <- lapply(lo.case[vec.length >= multi.position], toupper)

      string.out <- paste(c(paste(word.case, collapse = " "),
                            paste(multi.case, collapse = "")),
                          collapse = " ")

    # e.g., "Macclesfield Street/Gerrard Street", "Smiths Court/Yard"
    } else if (any(two.roads)) {
      slash.position <- grep("/", lo.case)

      # isolated "/"
      if (lo.case[slash.position] == "/") {
        pre <- lo.case[vec.length < slash.position]
        post <- lo.case[vec.length > slash.position]
        pre <- lapply(pre, wordCase)
        post <- lapply(post, wordCase)

        string.out <- paste0(paste(pre, collapse = " "), "/",
                             paste(post, collapse = " "))

      # leading "/"
      } else if (substr(lo.case[slash.position], 1, 1) == "/") {
        pre <- lo.case[vec.length < slash.position]
        delimited <- lo.case[vec.length == slash.position]
        post <- lo.case[vec.length > slash.position]
        pre <- lapply(pre, wordCase)
        delimited <- lapply(delimited, wordCaseB)
        post <- lapply(post, wordCase)

        if (length(post) == 0) {
          string.out <- paste0(paste(pre, collapse = " "),
                               paste(paste0("/", delimited)))

        } else {
          string.out <- paste0(paste(pre, collapse = " "),
                               paste(paste0("/", delimited),
                                     paste(post, collapse = " ")))
        }

      # trailing "/"
      } else if (substr(lo.case[slash.position],
                        nchar(lo.case[slash.position]),
                        nchar(lo.case[slash.position])) == "/") {

        word.case <- lapply(lo.case, wordCase)
        first <- paste(word.case[vec.length < slash.position],
                       word.case[slash.position], collapse = " ")
        last <- paste(word.case[vec.length > slash.position], collapse = " ")
        string.out <- paste0(first, last)

      # "/" in middle
      } else if (length(unlist(strsplit(lo.case[slash.position], "/"))) > 1) {
        # vec.length <- seq_along(lo.case)
        pre <- lo.case[vec.length < slash.position]
        delimited <- lo.case[vec.length == slash.position]
        post <- lo.case[vec.length > slash.position]

        if (length(post) == 0) {
          word.case.delimited <- wordCase(unlist(strsplit(delimited, "/")))
          string.out <- paste(wordCase(pre),
                              paste(word.case.delimited, collapse = "/"))

        } else {
          word.case.delimited <- wordCase(unlist(strsplit(delimited, "/")))
          string.out <- paste(wordCase(pre),
                              paste(word.case.delimited, collapse = "/"),
                              wordCase(post))
        }
      }

    # e.g., "Unknown-A1"
    } else if ("unknown" %in% unknown.road) {
      if (any(vapply(unknown.road, nchar, integer(1L)) == 0)) {
        not.empty <- vapply(unknown.road, nchar, integer(1L))
        unknown.road <- unknown.road[not.empty > 0]
        word.case <- lapply(unknown.road, wordCase)
        string.out <- paste0(word.case, collapse = "-")
      } else {
        word.case <- lapply(unknown.road, wordCase)
        string.out <- paste0(word.case, collapse = "-")
      }

    } else {
      word.case <- lapply(road.name.string, wordCase)
      string.out <- paste0(word.case, collapse = " ")
    }
    string.out
  }
  string.out
}

#' Plot road by name.
#'
#' Diagnostic plot with appropriate defaults.
#' @param road.name Character vector. To match names in the data set, \code{streetNameViewer}() corrects for case and removes extraneous spaces.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom. Default value is 1. For values <= 5, the anchor case number is plotted rather than a point for each case.
#' @return A base R graphics plot.
#' @export

streetViewer <- function(road.name, zoom = TRUE, radius = 1) {
  real.road.names <- unique(cholera::roads$name)

  if (is.character(road.name) == FALSE) {
    stop("Road name must be a character string.")
  } else if (road.name %in% real.road.names == FALSE) {
    case.name <- caseAndSpace(road.name)
    if (case.name %in% real.road.names == FALSE) {
      error.msg <- paste("Invalid road name.",
        "Check spelling or see list of road names in vignette('road.names').")
      stop(error.msg)
    } else name <- case.name
  } else name <- road.name

  # selected.road <- cholera::roads[cholera::roads$name == name, "street"]
  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)

  rng <- lapply(cholera::roads[cholera::roads$name == name, c("x", "y")],
      range)
  x.rng <- c(min(rng$x) - radius, max(rng$x) + radius)
  y.rng <- c(min(rng$y) - radius, max(rng$y) + radius)

  if (!zoom) {
    plot(cholera::fatalities[, c("x", "y")], xlim = range(cholera::roads$x),
      ylim = range(cholera::roads$y), pch = 15, cex = 0.5, col = "gray",
      asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = cholera::pumps$pump.id,
      col = "blue", pos = 1)
    # invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
    #   lwd = 3))
    title(main = name)

  } else if (zoom & radius <= 5) {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = NA, asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    text(cholera::fatalities.address[, c("x", "y")], labels =
      cholera::fatalities.address$anchor.case, cex = 0.5)
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = cholera::pumps$pump.id,
      pos = 1)
    # selected.road <- cholera::roads[cholera::roads$name == name, "street"]
    # invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
    #   lwd = 3))
    title(main = name)

  } else {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = 15, cex = 0.5, col = "gray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = cholera::pumps$pump.id,
      pos = 1)
    # selected.road <- cholera::roads[cholera::roads$name == name, "street"]
    # invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
    #   lwd = 3))
    title(main = name)
  }
}
