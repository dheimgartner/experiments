#' Generates a candidate set for implicit partial profile
#'
#' @seealso ngene user manual
#'
#' @param keep random sample to keep (defines nrows of output; defaults to 1e3)
#'
#' @return ngene candidate set (conforms to examples in ngene user manual)
#' @export
generate_candidate_set <- function(keep = 1e3) {

  ## full factorial
  attributes <-
    list(
      ca_available = c(-1, 0),
      ca_type = c(0, 1, 2, 3, 4, 5),
      ca_fuel = c(0, 1, 2, 3),
      ca_reach = c(0, 1),
      ca_fix_cost = c(0, 1),
      ca_variable_cost = c(0, 1),
      cs_available = c(-1, 0),
      cs_membership_fee = c(0, 1),
      cs_time_tariff = c(0, 1),
      cs_km_tariff = c(0, 1),
      cs_distance = c(0, 1),
      eb_available = c(-1, 0),
      eb_type = c(0, 1),
      eb_cost = c(0, 1),
      pt_available = c(-1, 0),
      pt_type = c(0, 1, 2),
      pt_zones = c(0, 1),
      pt_commute = c(0, 1),
      pt_fix_cost = c(0, 1),
      pt_variable_cost = c(0, 1)
    )

  full <- as.data.frame(expand.grid(attributes))

  ## preference constraints
  reduced <-
    experiments::preference_constraints(full, sub = -1)  ## afterwards => +1

  ## candidate bundles
  A <- reduced[sample(1:nrow(reduced)), ]
  B <- reduced[sample(1:nrow(reduced)), ]

  combined <- cbind(A, B)

  ## prepare for ngene
  ngene <- combined[sample(1:keep), ]
  ngene <- ngene + 1

  tmp <- data.frame(resp = 1, s = 1:nrow(ngene))

  ngene <- cbind(tmp, ngene)

  ngene

}
