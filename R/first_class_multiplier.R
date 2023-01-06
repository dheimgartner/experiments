remove_unit <- function(x) {
  number_unit <- strsplit(x, split = " ")
  numbers <- purrr::map(number_unit, function(x) {
    as.numeric(x[1])
  })
  numbers <- unlist(numbers)
  units <- purrr::map(number_unit, function(x) {
    x[2]
  })
  units <- unlist(units)
  list(numbers = numbers, units = units)
}



fcm <- function(x, multiplier) {
  nu <- remove_unit(x)
  n_multiplied <- nu$numbers * multiplier
  x_multiplied <- paste(n_multiplied, nu$units)
  cast_na <- purrr::map(x_multiplied, function(x) {
    ifelse(x == "NA NA", NA_character_, x)
  })
  unlist(cast_na)
}



# multiplier <- 1.7 (pretty accurate for)
first_class_multiplier
