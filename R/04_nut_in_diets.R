################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# December 2021
# 4_nut_in_diet.R
#
# Script with functions to compute mean nutrient content of diets of each sp
################################################################################


############################# load data ########################################
# not needed: use function load_xl defined in script 00_ratio_abundances.R

############################# Data wrangling ###################################
# join compo table with prey groups to have prey groups corresponding to 
# prey species 
join_clean_compo_tib <- function(compo_tib, preygps_tib) {
  nutri_df <- compo_tib |>
    # change outlier value to quantile 99% values
    dplyr::mutate(Se = dplyr::case_when(Sp_prey == "Hyperoplus lanceolatus" ~ 2.95, 
                                        TRUE ~ Se)) |>
    dplyr::left_join(preygps_tib, key = c("Sp_prey", "Genus", 
                                    "Family", "Order", "Taxa"), 
                     keep = FALSE) |>
    # get rid of elements of no interest here, ie non essential ones 
    dplyr::select(-c(Pb, Ag, Cd))
  
  # add Prey_groups artificially :
  # - Gelatinous pelagic cephalopod with species of Muscular pelagic cephalopods
  # - Fish undetermined with all species of fish
  # - Cephalopod undetermined with all species of cephalopods 
  rbind(nutri_df,
        nutri_df |>
          dplyr::filter(Prey_group == "Muscular pelagic cephalopods") |>
          dplyr::mutate(Prey_group = "Gelatinous pelagic cephalopods"),
        nutri_df |>
          dplyr::filter(Taxa  == "Fish") |>
          dplyr::mutate(Prey_group = "Fish undetermined"),
        nutri_df |>
          dplyr::filter(Taxa == "Cephalopod") |>
          dplyr::mutate(Prey_group = "Cephalopod undetermined")
  )
  
  
}


######################## bootstrapping ########################################
# function to bootstrap the composition of each prey group
# i.e sample with replacement samples of each prey group 
# to simulate a larger dataset that we have, 
# here we artificially generate nsim sample per prey group
bootstrap_compo_pg <- function(compo_tib, nsim) {
  
  compo_tib |>
    dplyr::filter(!is.na(Prey_group)) |> # should be just one sample jellyfish
    dplyr::group_by(Prey_group) |>
    # bootstrap : sample nsim species from each prey group
    dplyr::slice_sample(n = nsim, replace = TRUE) |>
    # replace NA values for N concentration by the mean of associated prey group
    dplyr::mutate(N = dplyr::case_when(is.na(N) ~ mean(N, na.rm = TRUE),
                                       TRUE ~ N)) |>
    # and get rid of useless columns
    dplyr::select(-c(Sp_prey, Genus, Family, Order, Taxa, Habitat))
  
}



##################### compute nutrient content of diet ########################
compute_nut_in_diet <- function(diet_tib, compo_tib_boot) {
  
  diet_tib |>
    # 1 - join the nut concentration bootstrapped tables per prey_group to the %W of each prey
    dplyr::mutate(Nut_W = seq_along(Diet) |>
                    purrr::map(~ purrr::pluck(Diet, .) |>
                                 tidyr::pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'),
                                                     names_to = "Prey_group",
                                                     values_to = "W") |>
                                 # add column with %W in diet associated to each prey_group, for each pred (ie.line)
                                 dplyr::left_join(y = compo_tib_boot,
                                                  by = "Prey_group")
                    )) |>
    # 2 - compute W*elemental concentration
    dplyr::mutate(
      Nut_W = seq_along(Nut_W) |>
        purrr::map(~ purrr::pluck(Nut_W, .) |>
                     dplyr::mutate(NRJ = NRJ*(W/100), 
                                   N = N*(W/100),
                                   P = P*(W/100),
                                   Fe = Fe*(W/100),
                                   Se = Se*(W/100),
                                   Cu = Cu*(W/100),
                                   Zn = Zn*(W/100),
                                   Mn = Mn*(W/100),
                                   Co = Co*(W/100),
                                   As = As*(W/100)) |>
                     # change it to get one column per prey_group
                     # and one line, each cell containing a full bootstrap tibble 
                     # of elemental concentration, size nsim*nelements
                     dplyr::select(-c(W)) |>
                     tidyr::nest(Nut = c("NRJ":"Zn")) |>
                     tidyr::pivot_wider(names_from = Prey_group, 
                                        values_from = Nut)),
      # 3 - compute the mean concentration of diet by summing these values across prey_groups
      Nut_diet = seq_along(Nut_W) |>
        purrr::map(~ purrr::pluck(Nut_W, ., 1, 1) +
                     purrr::pluck(Nut_W, ., 2, 1) +
                     purrr::pluck(Nut_W, ., 3, 1) +
                     purrr::pluck(Nut_W, ., 4, 1) +
                     purrr::pluck(Nut_W, ., 5, 1) +
                     purrr::pluck(Nut_W, ., 6, 1) +
                     purrr::pluck(Nut_W, ., 7, 1) +
                     purrr::pluck(Nut_W, ., 8, 1) +
                     purrr::pluck(Nut_W, ., 9, 1) +
                     purrr::pluck(Nut_W, ., 10, 1) +
                     purrr::pluck(Nut_W, ., 11, 1) +
                     purrr::pluck(Nut_W, ., 12, 1) +
                     purrr::pluck(Nut_W, ., 13, 1) ), 
      # NRJ should be a separated column as it will be used to compute the daily ration
      NRJ_diet = seq_along(Nut_diet) |>
        purrr::map(~ as_tibble_col(purrr::pluck(Nut_diet, ., "NRJ")*1e3)), # from kJ per g to kJ per kg
      # delete it from Nut_diet tibbles 
      Nut_diet = seq_along(Nut_diet) |>
        purrr::map(~ purrr::pluck(Nut_diet, .) |>
                     select(-NRJ))
      ) |>
    # delete now unused (and very heavy!) column
    dplyr::select(-c(Nut_W))
  
}


