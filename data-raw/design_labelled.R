## code to prepare `design_labelled` dataset goes here

library(tidyverse)
library(jsonlite)

devtools::load_all()

rm(list = ls())



design_labelled <- list()



create_choice_cards <- function(lang = c("en", "de")) {
  lang <- match.arg(lang)

  df <-
    experiments::replace_effect_codes(experiments::design$final,
                                      add_units = TRUE,
                                      lang = lang
    )

  to_multi <-
    c(
      "A_pt_fix_cost",
      "A_pt_variable_cost",
      "B_pt_fix_cost",
      "B_pt_variable_cost"
    )

  df <-
    df %>%
    mutate(
      across(
        all_of(to_multi),
        function(x) experiments::first_class_multiplier(x, multiplier = 1.7),
        .names = "{.col}_m"
      ),
      # No first class half fare fix cost
      A_pt_fix_cost =
        ifelse(A_pt_class == "first" & A_pt_type != "HT",
               A_pt_fix_cost_m,
               A_pt_fix_cost
        ),
      B_pt_fix_cost =
        ifelse(B_pt_class == "first" & B_pt_type != "HT",
               B_pt_fix_cost_m,
               B_pt_fix_cost
        ),
      # Variable cost only for HT
      A_pt_variable_cost =
        ifelse(A_pt_class == "first" & A_pt_type == "HT",
               A_pt_variable_cost_m,
               A_pt_variable_cost
        ),
      B_pt_variable_cost =
        ifelse(B_pt_class == "first" & B_pt_type == "HT",
               B_pt_variable_cost_m,
               B_pt_variable_cost
        )
    ) %>%
    select(!ends_with("_m"))

  df <-
    df %>%
    mutate(across(contains("_pt_fix_cost"), function(x) {
      manipulate_with_unit(x, round, digits = 0)
    }))

  df <-
    df %>%
    group_by(block) %>%
    mutate(cs = dplyr::row_number()) %>%
    ungroup() %>%
    select(block, cs, everything())

  df <-
    df %>%
    select(-contains("_available"))
}



df_de <- create_choice_cards("de")
df_de$lang <- "de"
df_en <- create_choice_cards("en")
df_en$lang <- "en"

df <-
  rbind(df_en, df_de) %>%
  select(block, cs, lang, everything())



write.table(df, "./data/design_labelled.csv", sep = ";", row.names = FALSE)

json <- jsonlite::toJSON(df, na = "string", pretty = TRUE)

jsonlite::write_json(json, "./data/design_labelled.json")

design_labelled$df <- df
design_labelled$json <- json

usethis::use_data(design_labelled, overwrite = TRUE)
