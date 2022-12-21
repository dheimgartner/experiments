## code to prepare `generic_archetypes` dataset goes here

devtools::load_all()

activate_venv()

generic_archetypes <- experiments::gen_archs(experiments::cars$df)

usethis::use_data(generic_archetypes, overwrite = TRUE)
