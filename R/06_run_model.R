################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# December 2021
# 06_run_model.R
#
# Script with function to run the model 
################################################################################

####################### PRELIMINARY FUNCTIONS ##################################

# to simulate abundance uncertainty
abundance <- function(abund_bar, abund_cv,
                      n_sim){
  sigma <- sqrt(log1p(abund_cv*abund_cv))
  mu <- log(abund_bar/sqrt(1+abund_cv*abund_cv))
  return(tibble::as_tibble_col(rlnorm(n_sim, mu, sigma))) 
} # lognormal distribution commonly used for species abundances 

# to compute the daily need of an individual (Kleiber equation) of a given species
kleiber <- function(beta, mass, n_sim, 
                    assimil_mean = NULL,  assimil_sd = 0.05,
                    dietQuality
) {
  # should the daily ration be computed?
  if(!is.null(assimil_mean) && !is.null(dietQuality)) {
    a <- truncnorm::rtruncnorm(n = n_sim, 
                               mean = assimil_mean, 
                               sd = assimil_sd, 
                               a = assimil_a, 
                               b = assimil_b) # assimilation 
    
    return(tibble::tibble(ADMR = beta * (293.1*mass^(3/4)),
                          A_rate = a,
                          Ration = beta * (293.1*mass^(3/4))/(a*dietQuality),
                          PercentBM = 293.1*beta/(a*dietQuality*mass^(1/4)))
    )
  }
  else { return(list(ADMR = beta * (293.1*mass^(3/4)))) }
}



############################# COMPUTATION ######################################

run_model <- function(input_tib, nsim) {
  input_tib |> 
    ###### SIMULATE UNCERTAINTY IN MASS, BETA, ABUNDANCE DATA, EXCRETION
    dplyr::mutate(
      Abund = seq_along(Abund) |> 
        purrr::map(~ abundance(Abund[[.]]$Abund,
                               Abund[[.]]$Abund_CV, 
                               nsim)), 
      Mass = seq_along(Mass) |>
        purrr::map(~ tibble::as_tibble_col(rnorm(n = nsim, 
                                                 mean = Mass[[.]]$Mass, 
                                                 sd = 0.2*Mass[[.]]$Mass))), 
      Beta = seq_along(Beta) |>
        purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = nsim, 
                                                                 mean = Beta[[.]]$Beta, 
                                                                 sd = 0.2*Beta[[.]]$Beta, 
                                                                 a = 1, 
                                                                 b = 5))), 
      Nut_excrete = seq_along(Beta) |> # nutrient excretion rate, for migratory species in summering grounds, release reduced for N, P and set to 0 for micronutrients
        purrr::map(~ tibble::tibble(N = dplyr::case_when(Code_sp %in% c("Bala_ede", 
                                                                        "Bala_phy") & Geo_area == "Pacific_Hawai" ~ runif(n = nsim,   
                                                                                                                          min = 0.2, 
                                                                                                                          max = 0.4),
                                                         TRUE ~ runif(n = nsim,  
                                                                      min = 0.7, 
                                                                      max = 0.9)),
                                    P = dplyr::case_when(Code_sp %in% c("Bala_ede", 
                                                                        "Bala_phy") & Geo_area == "Pacific_Hawai" ~ runif(n = nsim,   
                                                                                                                          min = 0.2, 
                                                                                                                          max = 0.4),
                                                         TRUE ~ runif(n = nsim,  
                                                                      min = 0.7, 
                                                                      max = 0.9)),
                                    As = dplyr::case_when(Code_sp %in% c("Bala_ede", 
                                                                         "Bala_phy") & Geo_area == "Pacific_Hawai" ~ runif(n = nsim,   
                                                                                                                           min = 0, 
                                                                                                                           max = 0),
                                                          TRUE ~ runif(n = nsim,  
                                                                       min = 0.7, 
                                                                       max = 0.9)),
                                    Co = dplyr::case_when(Code_sp %in% c("Bala_ede", 
                                                                         "Bala_phy") & Geo_area == "Pacific_Hawai" ~ runif(n = nsim,   
                                                                                                                           min = 0, 
                                                                                                                           max = 0),
                                                          TRUE ~ runif(n = nsim,  
                                                                       min = 0.7, 
                                                                       max = 0.9)),
                                    Cu = dplyr::case_when(Code_sp %in% c("Bala_ede", 
                                                                         "Bala_phy") & Geo_area == "Pacific_Hawai" ~ runif(n = nsim,   
                                                                                                                           min = 0, 
                                                                                                                           max = 0),
                                                          TRUE ~ runif(n = nsim,  
                                                                       min = 0.7, 
                                                                       max = 0.9)),
                                    Fe = dplyr::case_when(Code_sp %in% c("Bala_ede", 
                                                                         "Bala_phy") & Geo_area == "Pacific_Hawai" ~ runif(n = nsim,   
                                                                                                                           min = 0, 
                                                                                                                           max = 0),
                                                          TRUE ~ runif(n = nsim,  
                                                                       min = 0.7, 
                                                                       max = 0.9)),
                                    Mn = dplyr::case_when(Code_sp %in% c("Bala_ede", 
                                                                         "Bala_phy") & Geo_area == "Pacific_Hawai" ~ runif(n = nsim,   
                                                                                                                           min = 0, 
                                                                                                                           max = 0),
                                                          TRUE ~ runif(n = nsim,  
                                                                       min = 0.7, 
                                                                       max = 0.9)),
                                    Se = dplyr::case_when(Code_sp %in% c("Bala_ede", 
                                                                         "Bala_phy") & Geo_area == "Pacific_Hawai" ~ runif(n = nsim,   
                                                                                                                           min = 0, 
                                                                                                                           max = 0),
                                                          TRUE ~ runif(n = nsim,  
                                                                       min = 0.7, 
                                                                       max = 0.9)),
                                    Zn = dplyr::case_when(Code_sp %in% c("Bala_ede", 
                                                                         "Bala_phy") & Geo_area == "Pacific_Hawai" ~ runif(n = nsim,   
                                                                                                                           min = 0, 
                                                                                                                           max = 0),
                                                          TRUE ~ runif(n = nsim,  
                                                                       min = 0.7, 
                                                                       max = 0.9)))),
      Ndays = seq_along(Ndays) |>
        purrr::map(~ tibble::as_tibble_col(runif(min = Ndays[[.]]$Ndays_min, 
                                                 max = Ndays[[.]]$Ndays_max, 
                                                 n = nsim))),
      ############################ COMPUTE INDIVIDUAL NRJTIC DATA, NEEDS AND CONSUMPTION OF POP ######  
      Indi_data = seq_along(Mass) |>
        purrr::map(~ kleiber(beta = purrr::pluck(Beta, ., 1), 
                             mass = purrr::pluck(Mass, ., 1), 
                             n_sim = nsim, 
                             assimil_mean = 0.85, assimil_sd = 0.05, assimil_a = 0.8, assimil_b = 0.9,
                             dietQuality = purrr::pluck(NRJ_diet, ., 1))), 
      # Population consumption and needs
      conso_pop = seq_along(Abund) |> # Annual amount of prey consumed by the population in kg
        purrr::map(~ purrr::pluck(Abund, ., 1)*purrr::pluck(Ndays, ., 1)*purrr::pluck(Indi_data, ., "Ration")), 
      Needs_pop = seq_along(Abund) |> # Annual need of the population in kJ
        purrr::map(~ purrr::pluck(Abund, ., 1)*purrr::pluck(Ndays, ., 1)*purrr::pluck(Indi_data, ., "ADMR")), 
      # change % to proportion for diet data 
      Diet = seq_along(Diet) |>
        purrr::map(~ tidyr::uncount(
          purrr::pluck(Diet, .)/100, nsim)) # the unique line to get the same dimensions as vector to be multiplied by 
    ) |>
    ############ COMPUTE MODEL OUTPUTS : NUTRIENT EXCRETION AND CARBON STORAGE #########################
  dplyr::mutate(conso_diet = seq_along(conso_pop) |> # Population annual consumption of each prey group (kg)
                  purrr::map(~ purrr::pluck(conso_pop, .) * purrr::pluck(Diet, .)),
                conso_diet_ind = seq_along(Indi_data) |> # Species daily consumption of each prey group (kg)
                  purrr::map(~ (purrr::pluck(Indi_data, .)$Ration) * purrr::pluck(Diet, .)),
                ## NUTRIENT CONSUMPTION AND EXCRETION ! 
                conso_nut = seq_along(conso_pop) |> # Annual consumption of nutrient
                  purrr::map(~ (purrr::pluck(conso_pop, .) * purrr::pluck(Nut_diet, .))/1e9), # from mg to tonnes 
                conso_nut_ind = seq_along(Indi_data) |> # Individual daily consumption of nutrient in mg 
                  purrr::map(~ purrr::pluck(Indi_data, ., "Ration") * purrr::pluck(Nut_diet, .)), 
                excrete_nut = seq_along(conso_nut) |> # Annual excretion of nutrient 
                  purrr::map(~ purrr::pluck(conso_nut, .) * purrr::pluck(Nut_excrete, .)), 
                excrete_nut_ind = seq_along(Nut_excrete) |> # Individual daily excretion of nutrient (mg/day)
                  purrr::map(~ purrr::pluck(conso_nut_ind, .) * purrr::pluck(Nut_excrete, .)),
                ## STANDING CARBON BIOMASS
                ww_to_c = seq_along(Mass) |> # ww to carbon ratio ie in 1kg of ww there is app. 125g of C (12.5%)
                  purrr::map(~ tibble::as_tibble_col(runif(n = nsim, min = 0.10, max = 0.15))), 
                c_in_biomass = seq_along(Mass) |>
                  purrr::map(~ (purrr::pluck(Mass, ., 1)*purrr::pluck(Abund, ., 1)*purrr::pluck(ww_to_c, . , 1))/1e3)
  ) 
}

