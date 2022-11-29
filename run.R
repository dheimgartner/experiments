devtools::load_all()

run <- function() {
  b <- generate_candidate_set()
  b <- dplyr::distinct(b)
  ## nTrials == number of total unique chocie tasks
  ## nTrials / 8 == number of blocks
  design <- generate_design(b, nTrials = 240)

  usethis::use_data(design, overwrite = TRUE)
}
