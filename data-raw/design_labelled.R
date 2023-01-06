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
  experiments::first_class_multiplier(multiplier = 1.7)

df <-
  df %>%
  group_by(block) %>%
  mutate(cs = dplyr::row_number()) %>%
  ungroup() %>%
  select(block, cs, everything())

df <-
  df %>%
  select(-contains("_available"))

write.table(df, "./data/design_labelled.csv", sep = ";", row.names = FALSE)

json <- jsonlite::toJSON(df, na = "string", pretty = TRUE)

jsonlite::write_json(json, "./data/design_labelled.json")

design_labelled$df <- df
design_labelled$json <- json

usethis::use_data(design_labelled, overwrite = TRUE)
