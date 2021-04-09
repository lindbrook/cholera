#' Poverty and Born in London.
#'
#' Gareth Stedman Jones, p. 132.
#' @export

povertyLondon <- function() {
  # London registration districts: proportion of poverty (Booth) and proportion
  # born in London (1881 Census).

  district <- c("Holborn", "St. George's-in-the-East", "Bethnal Green",
    "St. Saviour's", "St. Olave's", "Shoreditch", "Whitechapel", "Stepney",
    "Greenwich", "Poplar", "Westminster", "City", "Islington", "St. Pancras",
    "Camberwell", "Wandsworth", "Marylebone", "St. Giles", "Mile End",
    "Lambeth", "Woolwich", "Fulham", "Kensington", "Chelsea", "Strand",
    "Hackney", "Paddington", "St. George's, Hanover Sq.", "Lewisham",
    "Hampstead")

  poverty <- c(48.9, 48.9, 44.6, 43.4, 42.2, 40.2, 39.2, 38.0, 36.8, 36.5,
    35.0, 31.5, 31.2, 30.4, 28.6, 27.4, 27.4, 26.7, 26.1, 26.1, 24.7, 24.7,
    24.7, 24.5, 23.9, 23.1, 21.7, 21.6, 18.1, 13.5)

  london.born <- c(70.8, 71.38, 83.57, 69.80, 72.60, 76.27, 64.17, 71.55,
    65.5, 66.47, 56.8, 60.5, 63.0, 61.0, 66.5, 58.4, 50.8, 62.8, 73.65, 62.9,
    55.5, 61.1, 43.8, 56.1, 57.0, 67.3, 47.2, 49.5, 54.6, 50.8)

  data.frame(district = district, poverty = poverty, london.born = london.born)
}
