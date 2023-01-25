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


paste_unit_number <- function(nu) {
    nu <- paste(nu$numbers, nu$units)
    cast_na <- purrr::map(nu, function(x) {
        ifelse(x == "NA NA", NA_character_, x)})
    unlist(cast_na)
}


manipulate_with_unit <- function(x, func, ...) {
    nu <- remove_unit(x)
    nu$numbers <- func(nu$numbers, ...)
    experiments::paste_unit_number(nu)
}
