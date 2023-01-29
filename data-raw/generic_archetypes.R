## code to prepare `generic_archetypes` dataset goes here

devtools::load_all()

activate_venv()

generic_archetypes <-
  mtosp::gen_archs(mtosp::cars$df) %>%
  select(-variable_cost)

usethis::use_data(generic_archetypes, overwrite = TRUE)
