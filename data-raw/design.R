## code to prepare `design` dataset goes here

devtools::load_all()

## nTrials == number of total unique choice tasks
## nTrials / 8 == number of blocks

b <- generate_candidate_set(keep = 1000)
b <- dplyr::distinct(b)
design <- generate_design(b, nTrials = 240)


usethis::use_data(design, overwrite = TRUE)
