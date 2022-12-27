## code to prepare `design_labelled` dataset goes here

library(tidyverse)
library(jsonlite)

devtools::load_all()

design_labelled <- list()

df <-
  experiments::replace_effect_codes(experiments::design$final,
                                    add_units = TRUE)

df <-
  df %>%
  group_by(block) %>%
  mutate(cs = dplyr::row_number()) %>%
  ungroup() %>%
  select(block, cs, everything())

df <-
  df %>%
  select(-contains("_available"))

json <- jsonlite::toJSON(df, na = "string", pretty = TRUE)

jsonlite::write_json(json, "./data/design_labelled.json")

design_labelled$df <- df
design_labelled$json <- json

usethis::use_data(design_labelled, overwrite = TRUE)
