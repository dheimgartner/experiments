## code to prepare `cars` dataset goes here

devtools::load_all()

mtosp::activate_venv()

## http://www.news-service.admin.ch/NSBSubscriber/message/attachments/1588.pdf
cars <-
  mtosp::generate_generic_archetypes(return_cars = TRUE,
                                           km = 13500,
                                           path_save = "./tmp/cars")

df <- tibblify::tibblify(purrr::flatten(cars))

## ""
df <- df[df$specs$Fahrzeugklasse != "", ]

cars <- list()

cars$list <- cars
cars$df <- df

usethis::use_data(cars, overwrite = TRUE)
