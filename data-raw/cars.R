## code to prepare `cars` dataset goes here

devtools::load_all()

experiments::activate_venv()

## http://www.news-service.admin.ch/NSBSubscriber/message/attachments/1588.pdf
cars <-
  experiments::generate_generic_archetypes(return_cars = TRUE,
                                           km = 13500,
                                           path_save = "./tmp/cars")

usethis::use_data(cars, overwrite = TRUE)
