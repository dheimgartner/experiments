generate_generic_archetypes <- function(return_cars = TRUE, ...) {
  gga <- reticulate::import("tcsscraper.experiments.generate_generic_archetypes")
  ga <- gga$generate_generic_archetypes(...)
  print(gga)
  return(ga)
}



gen_archs <- function(path) {
  utils <- python_import_module("utils")

  cars <- utils$load_pickle(path)

  ru <- reticulate::import("tcsscraper.experiments.generate_generic_archetypes")
  remove_unit <- ru$remove_unit

  df <- tibblify::tibblify(purrr::flatten(cars))
  df <-
    df %>%
    dplyr::mutate(vehicle_type = specs$Fahrzeugklasse,
                  fuel_type = specs$Treibstoffart)

  df <-
    df %>%
    dplyr::group_by(vehicle_type, fuel_type) %>%
    mutate(fix_cost = remove_unit(costs[["Fixe Kosten"]]),
           variable_cost = remove_unit(costs[["Variable Kosten"]]),
           cost_per_km = remove_unit(costs[["Kilometerkosten"]]),
           reach = ifelse(!is.na(specs[["Elektrische Reichweite"]]),
                          specs[["Elektrische Reichweite"]],
                          NA_character_),
           reach = ) %>%
    summarise(fix_cost = mean())
}
