generate_generic_archetypes <- function(return_cars = TRUE, ...) {
  gga <- reticulate::import("tcsscraper.experiments.generate_generic_archetypes")
  ga <- gga$generate_generic_archetypes(return_cars = return_cars, ...)
  print(gga)
  return(ga)
}



# TODO:
# 1. check mail thomas
# -----
# => vehicle_type (5):
# Small car (e.g. Fiat 500 or Volkswagen Polo)
# Medium to large car (e.g. Skoda Octavia or BMW 3 Series)
# SUV (e.g. BMW X3 or Volkswagen Tiguan)
# Minivan or van (e.g. Volkswagen T6)
# Luxury car or sports coupé (e.g. Mercedes-Benz E-Class, BMW 7 Series or Porsche 911)
# -----
# => fuel_type (5):
# Gasoline
# Diesel
# Hybrid (gasoline/diesel + electric)
# Plug-in hybrid (gasoline/diesel + electric)
# Electric
# (Other)
# -----
# 2. prep labels and proceed with replace effect codes
# 3. also adjust gen_archs to consider remapping

gen_archs <- function(cars) {

  ru <- reticulate::import("tcsscraper.experiments.generate_generic_archetypes")
  remove_unit <- ru$remove_unit

  df <- tibblify::tibblify(purrr::flatten(cars))
  df <-
    df %>%
    dplyr::mutate(vehicle_type = specs$Fahrzeugklasse,
                  fuel_type = specs$Treibstoffart)

  df <-
    df %>%
    mutate(fix_cost =
             map(costs[["Fixe Kosten"]],
                 ~ ifelse(is.na(.x), NA_real_, as.numeric(remove_unit(.x)))) %>%
             unlist(),
           variable_cost =
             map(costs[["Variable Kosten"]],
                 ~ ifelse(is.na(.x), NA_real_, as.numeric(remove_unit(.x)))) %>%
             unlist(),
           cost_per_km =
             map(costs[["Kilometerkosten"]],
                 ~ ifelse(is.na(.x), NA_real_, as.numeric(remove_unit(.x)))) %>%
             unlist(),
           reach =
             map(specs[["Elektrische Reichweite"]],
                 ~ ifelse(is.na(.x), NA_character_, .x)) %>%
             unlist(),
           reach =
             strsplit(reach, split = " / "),
           reach =
             map(reach,
                 ~ as.numeric(.x[2])) %>%
             unlist())

  df <-
    df %>%
    group_by(vehicle_type, fuel_type) %>%
    summarise(fix_cost = mean(fix_cost, na.rm = TRUE),
              variable_cost = mean(variable_cost, na.rm = TRUE),
              cost_per_km = mean(cost_per_km, na.rm = TRUE),
              reach = mean(reach, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(vehicle_type != "",
           !is.na(vehicle_type))

  df

}

