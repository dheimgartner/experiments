## code to prepare `labels` dataset goes here

library(tidyverse)

path <- "./data-raw/labels.xlsx"
sheets <- readxl::excel_sheets(path)

labels <-
  sheets %>%
  map(function(s) {
    readxl::read_xlsx(path, sheet = s)
  })

names(labels) <- sheets

nn <- sheets[!(sheets %in% c("vehicle_type", "fuel_type"))]
bl <- labels[nn]
bundles <- c("A", "B")

labels_ <- labels[!(names(labels) %in% nn)]

bundle_labels <-
  bundles %>%
  map(function(b) {
    nn <- names(bl)
    nn <- paste(b, nn, sep = "_")
    names(bl) <- nn
    bl
  })

A_bl <- bundle_labels[[1]]
B_bl <- bundle_labels[[2]]

labels <-
  labels_ %>%
  append(A_bl) %>%
  append(B_bl)

usethis::use_data(labels, overwrite = TRUE)
