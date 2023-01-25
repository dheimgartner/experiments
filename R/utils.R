#' Adds units from labels
#'
#' `labelr::labels$set needs to be called first` such that the function can
#' retrieve the values. Adding the units is most likely the last step!
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
add_units <- function(df) {
  units <-
    labelr::labels$get() %>%
    purrr::map2(.x = ., .y = names(.), function(l, n) {
      flag_unit <- "unit" %in% names(l)
      flag_filter <- "filter" %in% names(l)
      l[, "key"] <- n
      if (!flag_unit) l$unit <- ""
      if (!flag_filter) l$filter <- "long"
      l
    }) %>%
    purrr::reduce(rbind) %>%
    tidyr::drop_na() %>%
    dplyr::select(key, unit) %>%
    dplyr::distinct()

  nm <- names(df)

  all_cols <-
    purrr::map(nm, function(x) {
      flag <- x %in% units$key
      col <- df[[x]]
      if(flag) {
        unit <- units[units$key == x, ][["unit"]]
        col <- stringr::str_c(col, unit, sep = " ")  ## paste does not remove NA
      }
      col
    })

  names(all_cols) <- nm

  df <- as.data.frame(all_cols)
  df
}



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
