## code to prepare `labels` dataset goes here

devtools::load_all()

labels_en <- mtosp::prep_labels("en")

usethis::use_data(labels_en, overwrite = TRUE)
