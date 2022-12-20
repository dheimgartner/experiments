## code to prepare `generic_archetypes` dataset goes here

devtools::load_all()

#setup_venv()

reticulate::use_virtualenv("experiments")

## http://www.news-service.admin.ch/NSBSubscriber/message/attachments/1588.pdf
generic_archetpyes <- generate_generic_archetypes(km = 13500, path_save = "./tmp/cars")

usethis::use_data(generic_archetypes, overwrite = TRUE)
