index0 <- function(x) {
  stats::setNames(data.frame(t(utils::combn(length(x), 2))), c("v1", "v2"))
}

quadrantCoordinates <- function(dat, h, theta) {
  delta <- dat[2, ] - dat[1, ]

  # Pseudo-Quadrant I
  if (all(delta > 0)) {
    post.x <- dat[2, "x"] - abs(h * cos(theta))
    post.y <- dat[2, "y"] - abs(h * sin(theta))

  # Pseudo-Quadrant II
  } else if (delta[1] < 0 & delta[2] > 0) {
    post.x <- dat[2, "x"] + abs(h * cos(theta))
    post.y <- dat[2, "y"] - abs(h * sin(theta))

  # Pseudo-Quadrant III
  } else if (all(delta < 0)) {
    post.x <- dat[2, "x"] + abs(h * cos(theta))
    post.y <- dat[2, "y"] + abs(h * sin(theta))

  # Pseudo-Quadrant IV
  } else if (delta[1] > 0 & delta[2] < 0) {
    post.x <- dat[2, "x"] - abs(h * cos(theta))
    post.y <- dat[2, "y"] + abs(h * sin(theta))

  # I:IV
  } else if (delta[1] > 0 & delta[2] == 0) {
    post.x <- dat[2, "x"] - abs(h * cos(theta))
    post.y <- dat[2, "y"]

  # I:II
  } else if (delta[1] == 0 & delta[2] > 0) {
    post.x <- dat[2, "x"]
    post.y <- dat[2, "y"] - abs(h * sin(theta))

  # II:III
  } else if (delta[1] < 0 & delta[2] == 0) {
    post.x <- dat[2, "x"] + abs(h * cos(theta))
    post.y <- dat[2, "y"]

  # III:IV
  } else if (delta[1] == 0 & delta[2] < 0) {
    post.x <- dat[2, "x"]
    post.y <- dat[2, "y"] + abs(h * sin(theta))
  }

  data.frame(x = post.x, y = post.y)
}

#' Abbreviate street name postfix.
#'
#' Court -> Ct, Street -> St, Place -> Pl. For pumps.
#' @noRd

shortPostfix <- function(pump.select) {
  ct.id <- grep("Court", pump.select)
  st.id <- grep("Street", pump.select)
  pl.id <- grep("Place", pump.select)

  if (length(ct.id != 0)) {
    lst <- strsplit(pump.select[ct.id], " ")
    for (i in seq_along(lst)) {
      omega <- length(unlist(lst[[i]]))
      lst[[i]][omega] <- "Ct"
    }
    pump.select[ct.id] <- vapply(lst, function(x) {
      paste(unlist(x), collapse = " ")
    }, character(1L))
  } 
  
  if (length(st.id != 0)) {
    lst <- strsplit(pump.select[st.id], " ")
    for (i in seq_along(lst)) {
      omega <- length(lst[[i]])
      lst[[i]][omega] <- "St"
    }
    pump.select[st.id] <- vapply(lst, function(x) {
      paste(unlist(x), collapse = " ")
    }, character(1L))
  }  
  
  if (length(pl.id != 0)) {
    lst <- strsplit(pump.select[pl.id], " ")
    for (i in seq_along(lst)) {
      omega <- length(lst[[i]])
      lst[[i]][omega] <- "Pl"
    }
    pump.select[pl.id] <- vapply(lst, function(x) {
      paste(unlist(x), collapse = " ")
    }, character(1L))
  }
  pump.select
}
