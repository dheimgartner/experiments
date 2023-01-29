## code to prepare `labels_de` dataset goes here

devtools::load_all()

labels_de <- mtosp::prep_labels("de")

usethis::use_data(labels_de, overwrite = TRUE)
