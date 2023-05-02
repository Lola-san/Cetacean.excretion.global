################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# March 2022
# 08_base_functions_for_outputs.R
#
# Script with functions to generate all output figures and tables
# 
################################################################################

# base function to sum tibbles 
# compute the addition of each vector of values with uncertainties to get values for several areas/sp
sum_tibb <- function(list_of_tibb) {
  summed_tibb <- matrix(0, 
                        nrow = nrow(list_of_tibb[[1]]), 
                        ncol = ncol(list_of_tibb[[1]]))
  colnames(summed_tibb) <- colnames(list_of_tibb[[1]])
  
  for (j in seq_along(list_of_tibb)) {
    summed_tibb <- summed_tibb + list_of_tibb[[j]]
  }
  return(tibble::as_tibble(summed_tibb))
}

sum_vec <- function(list_of_vec) {
  summed_vec <- rep(0, length(list_of_vec[[1]]))
  
  for (j in seq_along(list_of_vec)) {
    summed_vec <- summed_vec + list_of_vec[[j]]
  }
  return(summed_vec)
}


format_names <- function(output_tib) {
  output_tib |>
    dplyr::mutate(Geo_area = dplyr::case_when(Geo_area == "GoAlaska" ~ "Gulf of Alaska",
                                              Geo_area == "GoMexico" ~ "Gulf of Mexico",
                                              Geo_area == "Med" ~ "Mediterranean Sea",
                                              Geo_area == "NAtlantic" ~ "Central North Atlantic",
                                              Geo_area == "NEAtlantic" ~ "Northeast Atlantic",
                                              Geo_area == "NWAtlantic" ~ "Northwest Atlantic",
                                              Geo_area == "Pacific_Calif_current" ~ "California current",
                                              Geo_area == "Pacific_FPoly" ~ "French Polynesia",
                                              Geo_area == "Pacific_Hawai" ~ "Hawaii",
                                              Geo_area == "Pacific_NCal" ~ "New Caledonia",
                                              Geo_area == "Pacific_WFu" ~ "Wallis & Futuna",
                                              Geo_area == "Indian" ~ "West Indian ocean",
                                              Geo_area == "Guyana" ~ "French Guyana",
                                              Geo_area == "Antilles" ~ "French Antilles",
                                              TRUE ~ Geo_area)) |>
    dplyr::mutate(Geo_area = factor(Geo_area, 
                                    levels = c("Central North Atlantic", "Northeast Atlantic", "Gulf of Alaska", 
                                               "Northwest Atlantic", "California current", 
                                               "Mediterranean Sea", "West Indian ocean", "Gulf of Mexico", "French Antilles", 
                                               "New Caledonia", "Hawaii",  
                                               "French Guyana", "Wallis & Futuna", "French Polynesia"))
    )
}