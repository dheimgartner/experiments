## code to prepare `candidate_set` dataset goes here

library(tidyverse)

devtools::load_all()

candidate_set <- experiments::generate_candidate_set()

xlsx::write.xlsx2(candidate_set, file = "./data/candidate_set.xlsx", row.names = FALSE)

usethis::use_data(candidate_set, overwrite = TRUE)
