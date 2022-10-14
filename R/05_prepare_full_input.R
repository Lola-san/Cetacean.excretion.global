################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# December 2021
# 05_prepare_full_input.R
#
# Script with function to create the full input table of the model 
# with population data, diet data and nutrient composition of diet data
################################################################################


# only one function: join table of population data with table of diet data
# and format 
prepare_input <- function(pop_tib, diet_nut_tib) {
  
  # We'll add a Nb of days column to take account of the time spent in an area on a year
  ndays <- 365 # for non-migratory species
  # for migratory species, we consider presence of between 4 and 8 months
  ndays_migr_min <- 120 
  ndays_migr_max <- 240
  
  
  pop_tib |>
    dplyr::left_join(diet_nut_tib, 
                     by = c("Code_sp", "Species", "Eco_area")) |>
    # account for the enhance cost of lunge feeding for lunge feeding baleen whales 
    # to obtain daily rations in line with findings of Savoca et al 2021
    dplyr::mutate(Beta = dplyr::case_when(Code_sp %in% c("Bala_mus", "Bala_ede", 
                                           "Bala_phy", "Bala_bor", 
                                           "Bala_acu", "Mega_nov") ~ Beta + 0.5, 
                            TRUE ~ Beta)) |>
    tidyr::nest(Mass = c(Mass), 
                Beta = c(Beta), 
                Abund = c(Abund, Abund_CV)) |>
    dplyr::mutate(
      Ndays_min = dplyr::case_when(Code_sp %in% c("Bala_mus", "Bala_ede", 
                                                  "Bala_phy", "Bala_bor", 
                                                  "Mega_nov") & !(Geo_area %in% c("Med", 
                                                                                  "GoMexico")) ~ ndays_migr_min, 
                                   TRUE ~ ndays), 
      Ndays_max = dplyr::case_when(Code_sp %in% c("Bala_mus", "Bala_ede", 
                                                  "Bala_phy", "Bala_bor", 
                                                  "Mega_nov") & !(Geo_area %in% c("Med",
                                                                                  "GoMexico")) ~ ndays_migr_max, 
                                   TRUE ~ ndays)) |>
    tidyr::nest(Ndays = c(Ndays_min, Ndays_max))
  
}

