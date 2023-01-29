## code to prepare `design` dataset goes here

devtools::load_all()

## nTrials == number of total unique choice tasks
## nTrials / 8 == number of blocks

b <- mtosp::candidate_set
b <- dplyr::distinct(b)
design <- generate_design(b, nTrials = 240)


usethis::use_data(design, overwrite = TRUE)
