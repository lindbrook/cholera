#' Locate road by name.
#'
#' Plots John Snow's map of the 1854 London cholera outbreak and highlights the
#' selected road. See the list of road names in \code{vignette}("road.names").
#' @param road.name Character vector. Note that \code{streetNameLocator}() tries to correct for case and to remove extra spaces.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom. For "radius" <= 5, the anchor case number is plotted.
#' @return A base R graphics plot.
#' @seealso \code{\link{roads}}, \code{\link{road.segments}}, \code{\link{streetNumberLocator}}, \code{vignette("road.names")}
#' @import graphics
#' @export
#' @examples
#' streetNameLocator("Oxford Street")
#' streetNameLocator("oxford street")
#' streetNameLocator("Cambridge Street", zoom = TRUE)
#' streetNameLocator("Cambridge Street", zoom = TRUE, radius = 0)

streetNameLocator <- function(road.name, zoom = FALSE, radius = 1) {
  real.road.names <- unique(cholera::roads$name)

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
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = paste0("p", cholera::pumps$id),
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
    text(cholera::pumps[, c("x", "y")], label = paste0("p", cholera::pumps$id),
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
    text(cholera::pumps[, c("x", "y")], label = paste0("p", cholera::pumps$id),
      pos = 1)
    selected.road <- cholera::roads[cholera::roads$name == name, "street"]
    invisible(lapply(roads.list[paste(selected.road)], lines, col = "red",
      lwd = 3))
    title(main = name)
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
