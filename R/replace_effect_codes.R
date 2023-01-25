#' Helper for conditional substitution
#'
#' If one attribute is conditional on other attribute: E.g. eb_motor | eb_type
#'
#' @param df
#' @param target the attribute to be substituted with `substitution`
#' @param conditional the attribute to condition on
#' @param compare if `conditional == compare` substitute with `substitution`
#' @param substitution values to substitute with
#'
#' @internal
#'
#' @return
#' @export
#'
#' @examples
if (F) {
    sub_codes_(design, "A_eb_motor", "A_eb_type", c(1, 2), c(250, 350))
}
sub_codes_ <- function(df, target, conditional, compare, substitution, NA_replace = NA) {
    d <- df[[conditional]]
    t <- df[[target]]

    sub <-
        purrr::map2(d, t, function(x, y) {
            if (is.na(as.character(x))) {
                flag <- 0
            } else {
                flag <- (x == compare)
            }
            if (sum(flag) == 0) {
                return(NA_replace)
            }
            ref <- substitution[flag]
            new <- ref * as.numeric(as.character(y)) ## factor...

            return(new)
        })

    df[target] <- unlist(sub)
    return(df)
}



#' Conditional substitution across bundles
#'
#' To avoid that sub_bundle_ needs to be spelled out for leaders.
#' Iterates over leaders and finds appropriate column.
#'
#' @seealso `?experiments:::sub_codes_`
#'
#' @return
#' @export
#'
#' @examples
if (F) {
    sub_codes(design, "eb_motor", "eb_type", c(1, 2), c(250, 350))
}
sub_codes <- function(df, target, conditional, compare, substitution, leader = c("A_", "B_")) {
    dfs <-
        purrr::map(leader, function(x) {
            tar <- paste0(x, target)
            dec <- paste0(x, conditional)
            df <-
                df %>%
                sub_codes_(tar, dec, compare, substitution)
            out <- df %>% dplyr::select(tidyselect::starts_with(x))
        })
    df <- purrr::reduce(dfs, cbind)
}



#' Replaces effect codes with values from archetypes.
#'
#' Should be called after `labelr::label_df` has been applied
#'
#' @param df labeled design
#' @param archetypes see `generate_generic_archetypes` and `gen_archs`
#'
#' @seealso `tcsscraper` python package (in particular sub-package `experiments`)
#'
#' @return
#' @export
sub_codes_from_archs <- function(df, archetypes) {
    ## select variables of interest
    dfc <-
        df %>%
        dplyr::select(contains("ca")) %>%
        dplyr::mutate(across(everything(), as.character),
            id = 1:nrow(.)
        ) %>%
        tidyr::pivot_longer(-id) %>%
        dplyr::mutate(
            bundle = substring(name, first = 1, last = 1),
            name = sub(pattern = "^[A-Z]_", replacement = "", name)
        ) %>%
        tidyr::pivot_wider(id_cols = c(id, bundle))

    tmp_label <- list(experiments::labels$A_ca_type, experiments::labels$A_ca_fuel)
    names(tmp_label) <- c("ca_type", "ca_fuel")

    labelr::labels$set(tmp_label)

    dfc$ca_type <- labelr::label_var(dfc$ca_type, variable = "ca_type", filter = "archs")
    dfc$ca_fuel <- labelr::label_var(dfc$ca_fuel, variable = "ca_fuel", filter = "archs")

    dfc <-
        dfc %>%
        dplyr::left_join(archetypes, by = c("ca_type", "ca_fuel")) %>%
        dplyr::mutate(
            across(
                c("ca_fix_cost", "ca_variable_cost", "ca_reach"),
                as.numeric
            ),
            ca_fix_cost = round(ca_fix_cost * fc / 12, digits = 0), # monthly
            ca_variable_cost = round(ca_variable_cost * cpkm, digits = 2),
            ca_reach = round(ca_reach * reach, digits = 0)
        ) %>%
        dplyr::select(-c(fc, cpkm, reach)) %>%
        dplyr::mutate(across(everything(), as.character)) %>%
        tidyr::pivot_longer(-c(id, bundle)) %>%
        dplyr::mutate(name = paste(bundle, name, sep = "_")) %>%
        dplyr::select(-bundle) %>%
        tidyr::pivot_wider(id_cols = id)

    nm <- names(dfc[, !(names(dfc) %in% "id")])
    df <-
        df %>%
        dplyr::select(-tidyselect::all_of(nm)) %>%
        cbind(dfc)

    ## order cols
    nm <- sort(names(df))
    df <-
        df %>%
        dplyr::select(all_of(nm), -id) %>%
        dplyr::select(block, everything())

    revert_label <- function(df, variable, filter = "revert") {
        labelr::label_var(df[[variable]], variable = variable, filter = filter)
    }

    df$A_ca_type <- revert_label(df, "A_ca_type")
    df$B_ca_type <- revert_label(df, "B_ca_type")
    df$A_ca_fuel <- revert_label(df, "A_ca_fuel")
    df$B_ca_fuel <- revert_label(df, "B_ca_fuel")

    df
}



#' Replace effect codes with human readable levels
#'
#' Master function that calls all the helpers
#'
#' @param design output from `experiments::generate_design()$final`
#' @param add_units units should be added for choice cards
#'
#' @return
#' @export
replace_effect_codes <- function(design, add_units = TRUE) {
    block <- design$block # gets lost in sub_codes

    labelr::labels$set(experiments::labels)

    design <-
        design %>%
        labelr::label_df()

    design <-
        design %>%
        sub_codes("eb_cost", "eb_type", c("Up to 25 km/h", "Up to 45 km/h"), c(50, 80))

    ## price of modulabo depends on zones included
    design <-
        design %>%
        dplyr::mutate(
            A_ma = ifelse(A_pt_type == "Modulabo", paste(A_pt_type, A_pt_zones, sep = "_"), as.character(A_pt_type)),
            B_ma = ifelse(B_pt_type == "Modulabo", paste(B_pt_type, B_pt_zones, sep = "_"), as.character(B_pt_type))
        )

    design <-
        design %>%
        sub_codes("pt_fix_cost", "ma", c("GA", "HT", "Modulabo_1-2 zones", "Modulabo_Whole fare network"), c(300, 15, 65, 185))

    design <-
        design %>%
        dplyr::select(-c(A_ma, B_ma))

    design <-
        design %>%
        sub_codes("pt_variable_cost", "pt_type", c("GA", "HT", "Modulabo"), c(NA_real_, 0.5, NA_real_))

    ## cast to character
    design <-
        design %>%
        dplyr::mutate(across(everything(), as.character))

    design$block <- block # add again

    archetypes <-
        experiments::generic_archetypes %>%
        dplyr::rename(
            ca_type = vehicle_type, ca_fuel = fuel_type, fc = fix_cost,
            cpkm = cost_per_km
        )

    design <- sub_codes_from_archs(df = design, archetypes = archetypes)

    if (add_units) {
        design <-
            design %>%
            add_units()

        design <-
            design %>%
            mutate(across(where(is.character), trimws))
    }

    design
}
