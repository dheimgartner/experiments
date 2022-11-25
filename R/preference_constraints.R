#' Apply preference constraints
#'
#' Since we use archetypes, we do not really need to apply preference constraints within
#' a mobility tool. The constraints applied here are mainly with regard to MTO combination
#' and shutting certain attributes off (e.g. battery capacity if car is not electric).
#'
#' @param full_factorial from `generate_candidate_set`
#' @param sub substitution for not available (defaults to 999)
#'
#' @return reduced factorial `data.frame` with `sub` if availability == 0 or
#' if attribute should not be shown for another reason (see above).
#' @export
preference_constraints <- function(full_factorial, sub = 999) {
  ## apply constraints
  ## at least one mt has to be available
  condition <- (
    full_factorial$ca_available == 0 &
      full_factorial$cs_available == 0 &
      full_factorial$eb_available == 0 &
      full_factorial$pt_available == 0
  )

  df <- full_factorial[!condition, ]



  ## if mt_available == 0 set other mt attrs to sub
  mt_not_available <- function(df, mt) {
    avail <- paste(mt, "available", sep = "_")
    df <-
      df %>%
      dplyr::mutate(dplyr::across(
        tidyselect::starts_with(mt) & !dplyr::contains("available"),
        ~ ifelse(.data[[avail]] == 0, sub, .x)
      )) %>%
      dplyr::distinct()

    return(df)
  }



  df <-
    df %>%
    mt_not_available("ca") %>%
    mt_not_available("cs") %>%
    mt_not_available("eb") %>%
    mt_not_available("pt")


  ## if ca_fuel != "electric" (2) then ca_reach == sub
  df <-
    df %>%
    dplyr::mutate(ca_reach = ifelse(ca_fuel != 2, sub, ca_reach)) %>%
    dplyr::distinct()


  ## if pt_type != "modulabo" (2) then pt_zones == sub & pt_commute == sub
  df <-
    df %>%
    dplyr::mutate(
      pt_zones = ifelse(pt_type != 2, sub, pt_zones),
      pt_commute = ifelse(pt_type != 2, sub, pt_commute)
    ) %>%
    dplyr::distinct()


  ## if pt_type != "halbtax" (1) then pt_variable_cost == sub
  df <-
    df %>%
    dplyr::mutate(
      pt_variable_cost = ifelse(pt_type != 1, sub, pt_variable_cost)
    ) %>%
    dplyr::distinct()


  ## set available == 0 to sub
  df <-
    df %>%
    dplyr::mutate(dplyr::across(tidyselect::contains("available"),
                                ~ ifelse(.x == 0, sub, .x)))

  return(df)
}
