################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# March 2022
# 08_generate_outputs.R
#
# Script with functions to generate all output figures and tables
# 
################################################################################

############## a few base functions ##############
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
                                    levels = c("Northeast Atlantic", "Central North Atlantic", "Gulf of Alaska", 
                                               "Northwest Atlantic", "California current", 
                                               "Mediterranean Sea", "West Indian ocean", "Gulf of Mexico", "French Antilles", 
                                               "New Caledonia", "Hawaii",  
                                               "French Guyana", "Wallis & Futuna", "French Polynesia"))
    )
}


######################## functions to create tables ############################

################ ALL AREAS #######################

#'
#'
#'
#'
#'
# statistics of estimates of total excretion per area
# for all areas, all elements
# in tons/yr
create_full_stat_tab_tons_yr <- function(output_tib, 
                                         object_type, # either "output" or "file" 
                                         name_file
                                         ) {
  table <- output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Excretion = Excretion, #Excretion*1e3/Surf, # from tons to kg/km2
                  Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As"))) |>
    dplyr::group_by(Geo_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion))
  
  if (object_type == "file") {
    write.table(table, paste0("output/tables/", 
                                  name_file,
                                  ".txt"), sep = "\t")
  } else {
    table
  }
}


#'
#'
#'
#'
#'
# statistics of estimates of total excretion per area per surface unit
# for all areas, all elements
# in kg/km2/yr
create_full_stat_tab_kg_km2_yr <- function(output_tib,
                                           object_type, # either "output" or "file" 
                                           name_file) {
  table <- output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Excretion = Excretion*1e3/Surf, # from tons to kg/km2
                  Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As"))) |>
    dplyr::group_by(Geo_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion))
  if (object_type == "file") {
    write.table(table, paste0("output/tables/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    table
  }
}


#'
#'
#'
#'
#'
# statistical test significance of differences between areas
# for all areas, but only for N (could be done similarly for other, just very long)
# in kg/km2/yr
create_tab_stat_diff_tot_exc <- function(output_tib, element) {
  output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Excretion = Excretion/Surf,
                  Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")))  |> 
    dplyr::filter(Element == element) |>
    dplyr::select(Geo_area, Excretion) |>
    tidyr::pivot_wider(names_from = Geo_area, 
                       values_from = Excretion, 
                       values_fn = list) |>
    tidyr::unnest(cols = c(`French Antilles`, `Gulf of Alaska`, `French Guyana`, `Gulf of Mexico`, `West Indian ocean`, 
                           `Mediterranean Sea`, `Central North Atlantic`, `Northeast Atlantic`, 
                           `Northwest Atlantic`, `California current`, `French Polynesia`, `Hawaii`, 
                           `New Caledonia`, `Wallis & Futuna`)) |>
    dplyr::mutate(t_Alaska_NAtl = dplyr::case_when(`Gulf of Alaska` > `Central North Atlantic` ~ 1,
                                                   TRUE ~ 0),
                  t_Alaska_NEA = dplyr::case_when(`Gulf of Alaska` > `Northeast Atlantic` ~ 1,
                                                  TRUE ~ 0),
                  t_Alaska_NWA = dplyr::case_when(`Gulf of Alaska` > `Northwest Atlantic` ~ 1,
                                                  TRUE ~ 0),
                  t_Alaska_Paci_Calif_current = dplyr::case_when(`Gulf of Alaska` > `California current` ~ 1,
                                                                 TRUE ~ 0),
                  t_Alaska_Med = dplyr::case_when(`Gulf of Alaska` > `Mediterranean Sea` ~ 1,
                                                  TRUE ~ 0),
                  t_Alaska_Ind = dplyr::case_when(`Gulf of Alaska` > `West Indian ocean` ~ 1,
                                                  TRUE ~ 0),
                  t_Alaska_Mex = dplyr::case_when(`Gulf of Alaska` > `Gulf of Mexico` ~ 1,
                                                  TRUE ~ 0), 
                  t_Alaska_Ant = dplyr::case_when(`Gulf of Alaska` > `French Antilles` ~ 1,
                                                  TRUE ~ 0),
                  t_Alaska_Paci_NCal = dplyr::case_when(`Gulf of Alaska` > `New Caledonia` ~ 1,
                                                        TRUE ~ 0), 
                  t_Alaska_Paci_Hawai = dplyr::case_when(`Gulf of Alaska` > `Hawaii` ~ 1,
                                                         TRUE ~ 0),
                  t_Alaska_Guy = dplyr::case_when(`Gulf of Alaska` > `French Guyana` ~ 1,
                                                  TRUE ~ 0),
                  t_Alaska_Paci_WFu = dplyr::case_when(`Gulf of Alaska` > `Wallis & Futuna` ~ 1,
                                                       TRUE ~ 0),
                  t_Alaska_Paci_FPoly = dplyr::case_when(`Gulf of Alaska` > `French Polynesia` ~ 1,
                                                         TRUE ~ 0),
                  t_NAtl_NEA = dplyr::case_when(`Central North Atlantic` > `Northeast Atlantic` ~ 1,
                                                TRUE ~ 0),
                  t_NAtl_NWA = dplyr::case_when(`Central North Atlantic` > `Northwest Atlantic` ~ 1,
                                                TRUE ~ 0),
                  t_NAtl_Paci_Calif_current = dplyr::case_when(`Central North Atlantic` > `California current` ~ 1,
                                                               TRUE ~ 0),
                  t_NAtl_Med = dplyr::case_when(`Central North Atlantic` > `Mediterranean Sea` ~ 1,
                                                TRUE ~ 0),
                  t_NAtl_Ind = dplyr::case_when(`Central North Atlantic` > `West Indian ocean` ~ 1,
                                                TRUE ~ 0),
                  t_NAtl_Mex = dplyr::case_when(`Central North Atlantic` > `Gulf of Mexico` ~ 1,
                                                TRUE ~ 0),
                  t_NAtl_Paci_FPoly = dplyr::case_when(`Central North Atlantic` > `French Polynesia` ~ 1,
                                                       TRUE ~ 0),
                  t_NAtl_Ant = dplyr::case_when(`Central North Atlantic` > `French Antilles` ~ 1,
                                                TRUE ~ 0),
                  t_NAtl_Paci_NCal = dplyr::case_when(`Central North Atlantic` > `New Caledonia` ~ 1,
                                                      TRUE ~ 0),
                  t_NAtl_Paci_Hawai = dplyr::case_when(`Central North Atlantic` > `Hawaii` ~ 1,
                                                       TRUE ~ 0),
                  t_NAtl_Guy = dplyr::case_when(`Central North Atlantic` > `French Guyana` ~ 1,
                                                TRUE ~ 0),
                  t_NAtl_Paci_WFu = dplyr::case_when(`Central North Atlantic` > `Wallis & Futuna` ~ 1,
                                                     TRUE ~ 0),
                  t_NAtl_Paci_FPoly = dplyr::case_when(`Central North Atlantic` > `French Polynesia` ~ 1,
                                                       TRUE ~ 0),
                  t_NEA_NWA = dplyr::case_when(`Northeast Atlantic` > `Northwest Atlantic` ~ 1,
                                               TRUE ~ 0),
                  t_NEA_Paci_Calif_current = dplyr::case_when(`Northeast Atlantic` > `California current` ~ 1,
                                                              TRUE ~ 0),
                  t_NEA_Med = dplyr::case_when(`Northeast Atlantic` > `Mediterranean Sea` ~ 1,
                                               TRUE ~ 0),
                  t_NEA_Ind = dplyr::case_when(`Northeast Atlantic` > `West Indian ocean` ~ 1,
                                               TRUE ~ 0),
                  t_NEA_Mex = dplyr::case_when(`Northeast Atlantic` > `Gulf of Mexico` ~ 1,
                                               TRUE ~ 0),
                  t_NEA_Ant = dplyr::case_when(`Northeast Atlantic` > `French Antilles` ~ 1,
                                               TRUE ~ 0),
                  t_NEA_Paci_NCal = dplyr::case_when(`Northeast Atlantic` > `New Caledonia` ~ 1,
                                                     TRUE ~ 0),
                  t_NEA_Paci_Hawai = dplyr::case_when(`Northeast Atlantic` > `Hawaii` ~ 1,
                                                      TRUE ~ 0),
                  t_NEA_Guy = dplyr::case_when(`Northeast Atlantic` > `French Guyana` ~ 1,
                                               TRUE ~ 0),
                  t_NEA_Paci_WFu = dplyr::case_when(`Northeast Atlantic` > `Wallis & Futuna` ~ 1,
                                                    TRUE ~ 0),
                  t_NEA_Paci_FPoly = dplyr::case_when(`Northeast Atlantic` > `French Polynesia` ~ 1,
                                                      TRUE ~ 0),
                  t_NWA_Paci_Calif_current = dplyr::case_when(`Northwest Atlantic` > `California current` ~ 1,
                                                              TRUE ~ 0),
                  t_NWA_Med = dplyr::case_when(`Northwest Atlantic` > `Mediterranean Sea` ~ 1,
                                               TRUE ~ 0),
                  t_NWA_Ind = dplyr::case_when(`Northwest Atlantic` > `West Indian ocean` ~ 1,
                                               TRUE ~ 0),
                  t_NWA_Mex = dplyr::case_when(`Northwest Atlantic` > `Gulf of Mexico` ~ 1,
                                               TRUE ~ 0),
                  t_NWA_Ant = dplyr::case_when(`Northwest Atlantic` > `French Antilles` ~ 1,
                                               TRUE ~ 0),
                  t_NWA_Paci_NCal = dplyr::case_when(`Northwest Atlantic` > `New Caledonia` ~ 1,
                                                     TRUE ~ 0),
                  t_NWA_Paci_Hawai = dplyr::case_when(`Northwest Atlantic` > `Hawaii` ~ 1,
                                                      TRUE ~ 0),
                  t_NWA_Guy = dplyr::case_when(`Northwest Atlantic` > `French Guyana` ~ 1,
                                               TRUE ~ 0),
                  t_NWA_Paci_WFu = dplyr::case_when(`Northwest Atlantic` > `Wallis & Futuna` ~ 1,
                                                    TRUE ~ 0),
                  t_NWA_Paci_FPoly = dplyr::case_when(`Northwest Atlantic` > `French Polynesia` ~ 1,
                                                      TRUE ~ 0),
                  t_Paci_Calif_current_Med = dplyr::case_when(`California current` > `Mediterranean Sea` ~ 1,
                                                              TRUE ~ 0),
                  t_Paci_Calif_current_Ind = dplyr::case_when(`California current` > `West Indian ocean` ~ 1,
                                                              TRUE ~ 0),
                  t_Paci_Calif_current_Mex = dplyr::case_when(`California current` > `Gulf of Mexico` ~ 1,
                                                              TRUE ~ 0),
                  t_Paci_Calif_current_Ant = dplyr::case_when(`California current` > `French Antilles` ~ 1,
                                                              TRUE ~ 0),
                  t_Paci_Calif_current_Paci_NCal = dplyr::case_when(`California current` > `New Caledonia` ~ 1,
                                                                    TRUE ~ 0),
                  t_Paci_Calif_current_Paci_Hawai = dplyr::case_when(`California current` > `Hawaii` ~ 1,
                                                                     TRUE ~ 0),
                  t_Paci_Calif_current_Guy = dplyr::case_when(`California current` > `French Guyana` ~ 1,
                                                              TRUE ~ 0),
                  t_Paci_Calif_current_Paci_WFu = dplyr::case_when(`California current` > `Wallis & Futuna` ~ 1,
                                                                   TRUE ~ 0),
                  t_Paci_Calif_current_Paci_FPoly = dplyr::case_when(`California current` > `French Polynesia` ~ 1,
                                                                     TRUE ~ 0),
                  t_Med_Ind = dplyr::case_when(`Mediterranean Sea` > `West Indian ocean` ~ 1,
                                               TRUE ~ 0),
                  t_Med_Mex = dplyr::case_when(`Mediterranean Sea` > `Gulf of Mexico` ~ 1,
                                               TRUE ~ 0),
                  t_Med_Ant = dplyr::case_when(`Mediterranean Sea` > `French Antilles` ~ 1,
                                               TRUE ~ 0),
                  t_Med_Paci_NCal = dplyr::case_when(`Mediterranean Sea` > `New Caledonia` ~ 1,
                                                     TRUE ~ 0),
                  t_Med_Paci_Hawai = dplyr::case_when(`Mediterranean Sea` > `Hawaii` ~ 1,
                                                      TRUE ~ 0),
                  t_Med_Guy = dplyr::case_when(`Mediterranean Sea` > `French Guyana` ~ 1,
                                               TRUE ~ 0),
                  t_Med_Paci_WFu = dplyr::case_when(`Mediterranean Sea` > `Wallis & Futuna` ~ 1,
                                                    TRUE ~ 0),
                  t_Med_Paci_FPoly = dplyr::case_when(`Mediterranean Sea` > `French Polynesia` ~ 1,
                                                      TRUE ~ 0),
                  t_Ind_Mex = dplyr::case_when(`West Indian ocean` > `Gulf of Mexico` ~ 1,
                                               TRUE ~ 0),
                  t_Ind_Ant = dplyr::case_when(`West Indian ocean` > `French Antilles` ~ 1,
                                               TRUE ~ 0),
                  t_Ind_Paci_NCal = dplyr::case_when(`West Indian ocean` > `New Caledonia` ~ 1,
                                                     TRUE ~ 0),
                  t_Ind_Paci_Hawai = dplyr::case_when(`West Indian ocean` > `Hawaii` ~ 1,
                                                      TRUE ~ 0),
                  t_Ind_Guy = dplyr::case_when(`West Indian ocean` > `French Guyana` ~ 1,
                                               TRUE ~ 0),
                  t_Ind_Paci_WFu = dplyr::case_when(`West Indian ocean` > `Wallis & Futuna` ~ 1,
                                                    TRUE ~ 0),
                  t_Ind_Paci_FPoly = dplyr::case_when(`West Indian ocean` > `French Polynesia` ~ 1,
                                                      TRUE ~ 0),
                  t_Mex_Ant = dplyr::case_when(`Gulf of Mexico` > `French Antilles` ~ 1,
                                               TRUE ~ 0),
                  t_Mex_Paci_NCal = dplyr::case_when(`Gulf of Mexico` > `New Caledonia` ~ 1,
                                                     TRUE ~ 0),
                  t_Mex_Paci_Hawai = dplyr::case_when(`Gulf of Mexico` > `Hawaii` ~ 1,
                                                      TRUE ~ 0),
                  t_Mex_Guy = dplyr::case_when(`Gulf of Mexico` > `French Guyana` ~ 1,
                                               TRUE ~ 0),
                  t_Mex_Paci_WFu = dplyr::case_when(`Gulf of Mexico` > `Wallis & Futuna` ~ 1,
                                                    TRUE ~ 0),
                  t_Mex_Paci_FPoly = dplyr::case_when(`Gulf of Mexico` > `French Polynesia` ~ 1,
                                                      TRUE ~ 0),
                  t_Ant_Paci_NCal = dplyr::case_when(`French Antilles` > `New Caledonia` ~ 1,
                                                     TRUE ~ 0),
                  t_Ant_Paci_Hawai = dplyr::case_when(`French Antilles` > `Hawaii` ~ 1,
                                                      TRUE ~ 0),
                  t_Ant_Guy = dplyr::case_when(`French Antilles` > `French Guyana` ~ 1,
                                               TRUE ~ 0),
                  t_Ant_Paci_WFu = dplyr::case_when(`French Antilles` > `Wallis & Futuna` ~ 1,
                                                    TRUE ~ 0),
                  t_Ant_Paci_FPoly = dplyr::case_when(`French Antilles` > `French Polynesia` ~ 1,
                                                      TRUE ~ 0),
                  t_Paci_NCal_Paci_Hawai = dplyr::case_when(`New Caledonia` > `Hawaii` ~ 1,
                                                            TRUE ~ 0),
                  t_Paci_NCal_Guy = dplyr::case_when(`New Caledonia` > `French Guyana` ~ 1,
                                                     TRUE ~ 0),
                  t_Paci_NCal_Paci_WFu = dplyr::case_when(`New Caledonia` > `Wallis & Futuna` ~ 1,
                                                          TRUE ~ 0),
                  t_Paci_NCal_Paci_FPoly = dplyr::case_when(`New Caledonia` > `French Polynesia` ~ 1,
                                                            TRUE ~ 0),
                  t_Paci_Hawai_Guy = dplyr::case_when(`Hawaii` > `French Guyana` ~ 1,
                                                      TRUE ~ 0),
                  t_Paci_Hawai_Paci_WFu = dplyr::case_when(`Hawaii` > `Wallis & Futuna` ~ 1,
                                                           TRUE ~ 0),
                  t_Paci_Hawai_Paci_FPoly = dplyr::case_when(`Hawaii` > `French Polynesia` ~ 1,
                                                             TRUE ~ 0),
                  t_Guy_Paci_WFu = dplyr::case_when(`French Guyana` > `Wallis & Futuna` ~ 1,
                                                    TRUE ~ 0),
                  t_Guy_Paci_FPoly = dplyr::case_when(`French Guyana` > `French Polynesia` ~ 1,
                                                      TRUE ~ 0),
                  t_Paci_WFu_Paci_FPoly= dplyr::case_when( `Wallis & Futuna` > `French Polynesia` ~ 1,
                                                           TRUE ~ 0)
    ) |>
    dplyr::summarise(t_Alaska_NAtl = mean(t_Alaska_NAtl),
                     t_Alaska_NEA = mean(t_Alaska_NEA),
                     t_Alaska_NWA = mean(t_Alaska_NWA),
                     t_Alaska_Paci_Calif_current = mean(t_Alaska_Paci_Calif_current),
                     t_Alaska_Med = mean(t_Alaska_Med),
                     t_Alaska_Ind = mean(t_Alaska_Ind),
                     t_Alaska_Mex = mean(t_Alaska_Mex), 
                     t_Alaska_Ant = mean(t_Alaska_Ant), 
                     t_Alaska_Paci_NCal = mean(t_Alaska_Paci_NCal),
                     t_Alaska_Paci_Hawai = mean(t_Alaska_Paci_Hawai),
                     t_Alaska_Guy = mean(t_Alaska_Guy),
                     t_Alaska_Paci_WFu = mean(t_Alaska_Paci_WFu),
                     t_Alaska_Paci_FPoly = mean(t_Alaska_Paci_FPoly),
                     t_NAtl_NEA = mean(t_NAtl_NEA),
                     t_NAtl_NWA = mean(t_NAtl_NWA),
                     t_NAtl_Paci_Calif_current = mean(t_NAtl_Paci_Calif_current),
                     t_NAtl_Med = mean(t_NAtl_Med),
                     t_NAtl_Ind = mean(t_NAtl_Ind),
                     t_NAtl_Mex = mean(t_NAtl_Mex),
                     t_NAtl_Ant = mean(t_NAtl_Ant),
                     t_NAtl_Paci_NCal = mean(t_NAtl_Paci_NCal),
                     t_NAtl_Paci_Hawai = mean(t_NAtl_Paci_Hawai),
                     t_NAtl_Guy = mean(t_NAtl_Guy),
                     t_NAtl_Paci_WFu = mean(t_NAtl_Paci_WFu),
                     t_NAtl_Paci_FPoly = mean(t_NAtl_Paci_FPoly),
                     t_NEA_NWA = mean(t_NEA_NWA),
                     t_NEA_Paci_Calif_current = mean(t_NEA_Paci_Calif_current),
                     t_NEA_Med = mean(t_NEA_Med),
                     t_NEA_Ind = mean(t_NEA_Ind),
                     t_NEA_Mex = mean(t_NEA_Mex),
                     t_NEA_Ant = mean(t_NEA_Ant),
                     t_NEA_Paci_NCal = mean(t_NEA_Paci_NCal),
                     t_NEA_Paci_Hawai = mean(t_NEA_Paci_Hawai),
                     t_NEA_Guy = mean(t_NEA_Guy),
                     t_NEA_Paci_FPoly = mean(t_NEA_Paci_FPoly),
                     t_NEA_Paci_WFu = mean(t_NEA_Paci_WFu),
                     t_NWA_Paci_Calif_current = mean(t_NWA_Paci_Calif_current),
                     t_NWA_Med = mean(t_NWA_Med),
                     t_NWA_Ind = mean(t_NWA_Ind),
                     t_NWA_Mex= mean(t_NWA_Mex),
                     t_NWA_Ant= mean(t_NWA_Ant),
                     t_NWA_Paci_NCal = mean(t_NWA_Paci_NCal),
                     t_NWA_Paci_Hawai = mean(t_NWA_Paci_Hawai),
                     t_NWA_Guy = mean(t_NWA_Guy),
                     t_NWA_Paci_WFu = mean(t_NWA_Paci_WFu),
                     t_NWA_Paci_FPoly = mean(t_NWA_Paci_FPoly),
                     t_Paci_Calif_current_Med = mean(t_Paci_Calif_current_Med),
                     t_Paci_Calif_current_Ind = mean(t_Paci_Calif_current_Ind),
                     t_Paci_Calif_current_Mex = mean(t_Paci_Calif_current_Mex),
                     t_Paci_Calif_current_Ant = mean(t_Paci_Calif_current_Ant),
                     t_Paci_Calif_current_Paci_NCal = mean(t_Paci_Calif_current_Paci_NCal),
                     t_Paci_Calif_current_Paci_Hawai = mean(t_Paci_Calif_current_Paci_Hawai),
                     t_Paci_Calif_current_Guy = mean(t_Paci_Calif_current_Guy),
                     t_Paci_Calif_current_Paci_WFu = mean(t_Paci_Calif_current_Paci_WFu),
                     t_Paci_Calif_current_Paci_FPoly = mean(t_Paci_Calif_current_Paci_FPoly),
                     t_Med_Ind = mean(t_Med_Ind),
                     t_Med_Mex = mean(t_Med_Mex),
                     t_Med_Ant = mean(t_Med_Ant),
                     t_Med_Paci_NCal = mean(t_Med_Paci_NCal),
                     t_Med_Paci_Hawai = mean(t_Med_Paci_Hawai),
                     t_Med_Guy = mean(t_Med_Guy),
                     t_Med_Paci_WFu = mean(t_Med_Paci_WFu),
                     t_Med_Paci_FPoly = mean(t_Med_Paci_FPoly),
                     t_Ind_Mex = mean(t_Ind_Mex),
                     t_Ind_Ant = mean(t_Ind_Ant),
                     t_Ind_Paci_NCal = mean(t_Ind_Paci_NCal),
                     t_Ind_Paci_Hawai = mean(t_Ind_Paci_Hawai),
                     t_Ind_Guy = mean(t_Ind_Guy),
                     t_Ind_Paci_WFu = mean(t_Ind_Paci_WFu),
                     t_Ind_Paci_FPoly = mean(t_Ind_Paci_FPoly),
                     t_Mex_Ant = mean(t_Mex_Ant),
                     t_Mex_Paci_NCal = mean(t_Mex_Paci_NCal),
                     t_Mex_Paci_Hawai = mean(t_Mex_Paci_Hawai),
                     t_Mex_Guy = mean(t_Mex_Guy),
                     t_Mex_Paci_WFu = mean(t_Mex_Paci_WFu),
                     t_Mex_Paci_FPoly = mean(t_Mex_Paci_FPoly),
                     t_Ant_Paci_NCal = mean(t_Ant_Paci_NCal),
                     t_Ant_Paci_Hawai = mean(t_Ant_Paci_Hawai),
                     t_Ant_Guy = mean(t_Ant_Guy),
                     t_Ant_Paci_WFu = mean(t_Ant_Paci_WFu),
                     t_Ant_Paci_FPoly = mean(t_Ant_Paci_FPoly),
                     t_Paci_NCal_Paci_Hawai = mean(t_Paci_NCal_Paci_Hawai),
                     t_Paci_NCal_Guy = mean(t_Paci_NCal_Guy),
                     t_Paci_NCal_Paci_WFu = mean(t_Paci_NCal_Paci_WFu),
                     t_Paci_NCal_Paci_FPoly = mean(t_Paci_NCal_Paci_FPoly),
                     t_Paci_Hawai_Guy = mean(t_Paci_Hawai_Guy),
                     t_Paci_Hawai_Paci_WFu = mean(t_Paci_Hawai_Paci_WFu),
                     t_Paci_Hawai_Paci_FPoly = mean(t_Paci_Hawai_Paci_FPoly),
                     t_Guy_Paci_WFu = mean(t_Guy_Paci_WFu),
                     t_Guy_Paci_FPoly = mean(t_Guy_Paci_FPoly),
                     t_Paci_WFu_Paci_FPoly = mean(t_Paci_WFu_Paci_FPoly)) |>
    tidyr::pivot_longer(cols = c("t_Alaska_NAtl":"t_Paci_WFu_Paci_FPoly"),
                        names_to = "Test", 
                        values_to = "is_area1_superior_to_area2") |>
    dplyr::mutate(Area = dplyr::case_when(stringr::str_starts(Test, "t_Ant") ~ "French Antilles",
                                          stringr::str_starts(Test, "t_Alaska") ~ "Gulf of Alaska",
                                          stringr::str_starts(Test, "t_Guy") ~ "French Guyana",
                                          stringr::str_starts(Test, "t_Mex") ~ "Gulf of Mexico",
                                          stringr::str_starts(Test, "t_Ind") ~ "West Indian ocean",
                                          stringr::str_starts(Test, "t_Med") ~ "Mediterranean Sea",
                                          stringr::str_starts(Test, "t_NAtl") ~ "Central North Atlantic",
                                          stringr::str_starts(Test, "t_NEA") ~ "Northeast Atlantic",
                                          stringr::str_starts(Test, "t_NWA") ~ "Northwest Atlantic",
                                          stringr::str_starts(Test, "t_Paci_Calif_current") ~ "California current",
                                          stringr::str_starts(Test, "t_Paci_FPoly") ~ "French Polynesia",
                                          stringr::str_starts(Test, "t_Paci_Hawai") ~ "Hawaii",
                                          stringr::str_starts(Test, "t_Paci_NCal") ~ "New Caledonia",
                                          stringr::str_starts(Test, "t_Paci_WFu") ~ "Wallis & Futuna"), 
                  Area2 = dplyr::case_when(stringr::str_ends(Test, "_Ant") ~ "French Antilles",
                                           stringr::str_ends(Test, "_Alaska") ~ "Gulf of Alaska",
                                           stringr::str_ends(Test, "_Guy") ~ "French Guyana",
                                           stringr::str_ends(Test, "_Mex") ~ "Gulf of Mexico",
                                           stringr::str_ends(Test, "_Ind") ~ "West Indian ocean",
                                           stringr::str_ends(Test, "_Med") ~ "Mediterranean Sea",
                                           stringr::str_ends(Test, "_NAtl") ~ "Central North Atlantic",
                                           stringr::str_ends(Test, "_NEA") ~ "Northeast Atlantic",
                                           stringr::str_ends(Test, "_NWA") ~ "Northwest Atlantic",
                                           stringr::str_ends(Test, "_Paci_Calif_current") ~ "California current",
                                           stringr::str_ends(Test, "_Paci_FPoly") ~ "French Polynesia",
                                           stringr::str_ends(Test, "_Paci_Hawai") ~ "Hawaii",
                                           stringr::str_ends(Test, "_Paci_NCal") ~ "New Caledonia",
                                           stringr::str_ends(Test, "_Paci_WFu") ~ "Wallis & Futuna")) |>
    dplyr::select(c(is_area1_superior_to_area2, Area, Area2)) |>
    tidyr::pivot_wider(names_from = Area2, 
                       values_from = is_area1_superior_to_area2)
}



#'
#'
#'
#'
#'
# function to create table with total surface of areas
table_tot_surf <- function(output_tib) {
  output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarize(Surface = sum(unique(Surf_tot)))
}



#'
#'
#'
#'
#'
# function to create table with fold change ratio between areas
table_fold_change <- function(output_tib,
                              object_type, # either "output" or "file" 
                              name_file) {
  inter_table <- output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = Excretion*1e3/Surf # from tons to kg/km2
    )  |>
    dplyr::group_by(Geo_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    dplyr::filter(Element != "As")
  
  
  minimum_df <- inter_table |> 
    dplyr::group_by(Element) |>
    dplyr::summarize(min_all_mean = min(mean), 
                     max_all_mean = max(mean))
  
  # compute the fold-change ratio
  table <- inter_table |>
    dplyr::left_join(minimum_df, by = "Element", keep = FALSE) |>
    dplyr::mutate(fold = round(mean/min_all_mean) )
  
  if (object_type == "file") {
    write.table(table, paste0("output/tables/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    table
    }
}

################ AREA PER AREA ######################


#'
#'
#'
#'
#'
# statistics of estimates of total excretion per habitat
# for all areas, all elements
# in tons/yr
create_hab_stat_tab_tons_yr <- function(output_tib,
                                        object_type, # either "output" or "file" 
                                        name_file, 
                                        geo_area) {
  table <- output_tib |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::filter(Geo_area == geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Excretion = Excretion, #Excretion*1e3/Surf, # from tons to kg/km2
                  Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As"))) |>
    dplyr::group_by(Geo_area, Eco_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion))
  
  if (object_type == "file") {
    write.table(table, paste0("output/tables/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    table
  }
}


#'
#'
#'
#'
#'
# statistics of estimates of total excretion per area per surface unit
# for all areas, all elements
# in kg/km2/yr
create_hab_stat_tab_kg_km2_yr <- function(output_tib, 
                                          object_type, # either "output" or "file" 
                                          name_file, 
                                          geo_area) {
  table <- output_tib |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::filter(Geo_area == geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Excretion = Excretion*1e3/Surf, # from tons to kg/km2
                  Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As"))) |>
    dplyr::group_by(Geo_area, Eco_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion))
  
  if (object_type == "file") {
    write.table(table, paste0("output/tables/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    table
  }
}

#'
#'
#'
#'
#'
# function to compute statistical test on excretion per habitat in total
test_differences_hab <- function(output_tib, 
                                 object_type, # either "output" or "file" 
                                 name_file, 
                                 geo_area) {
  final_table <- tibble::tibble(Geo_area = NA, 
                                Element = NA, 
                                Area1 = NA,
                                Area2 = NA, 
                                ratio_area1_superior_to_area2 = NA)
  
  for (i in c("N", "P", "Fe", "Cu", "Mn", 
              "Se", "Zn", "Co")) {
    
    el_table <- output_tib |>
      dplyr::group_by(Geo_area, Eco_area) |>
      dplyr::filter(Geo_area == geo_area) |>
      dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                       sum = list(sum_tibb(excrete_nut))) |>
      tidyr::unnest(sum) |>
      tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                          names_to = "Element", 
                          values_to = "Excretion") |> 
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co", "As")), 
                    Excretion = Excretion*1e3/Surf, # from tons to kg
      )  |> 
      dplyr::filter(Element == i) |>
      dplyr::ungroup() |>
      dplyr::select(Eco_area, Excretion) |>
      tidyr::pivot_wider(names_from = Eco_area, 
                         values_from = Excretion, 
                         values_fn = list) |>
      tidyr::unnest(cols = c(shelf, oceanic)) |>
      dplyr::mutate(t_shelf_oceanic = dplyr::case_when(shelf > oceanic ~ 1,
                                                       TRUE ~ 0), 
                    t_oceanic_shelf= dplyr::case_when(oceanic > shelf ~ 1,
                                                      TRUE ~ 0),
      ) |>
      dplyr::summarise(t_shelf_oceanic = mean(t_shelf_oceanic), 
                       t_oceanic_shelf = mean(t_oceanic_shelf)) |>
      tidyr::pivot_longer(cols = c("t_shelf_oceanic":"t_oceanic_shelf"),
                          names_to = "Test", 
                          values_to = "ratio_area1_superior_to_area2") |>
      dplyr::mutate(Geo_area = geo_area, 
                    Element = i, 
                    Area1 = dplyr::case_when(stringr::str_starts(Test, "t_shelf") ~ "Neritic waters",
                                             stringr::str_starts(Test, "t_oceanic") ~ "Oceanic waters"), 
                    Area2 = dplyr::case_when(stringr::str_ends(Test, "_shelf") ~ "Neritic waters",
                                             stringr::str_ends(Test, "_oceanic") ~ "Oceanic waters")) |>
      dplyr::select(-Test) 
    
    final_table <- rbind(final_table, el_table)
    
    rm(el_table)
    
  }
  
  final_table <- final_table[-1,]
  
  if (object_type == "file") {
    write.table(final_table, paste0("output/tables/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    final_table
  }
  
}

#'
#'
#'
#'
#'
# function to compute the relative contribution of cetacean taxa in each area
taxa_contribution_total <- function(output_tib, 
                                    object_type, # either "output" or "file" 
                                    name_file, 
                                    geo_area # should be character string
){
  table <- output_tib |>
    dplyr::group_by(Geo_area, Eco_gp) |>
    dplyr::filter(Geo_area == geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |>
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = Excretion
    )  |>
    dplyr::group_by(Geo_area, Eco_gp, Element) |>
    dplyr::summarize(mean = mean(Excretion)) |>
    dplyr::left_join(output_tib |>
                       dplyr::filter(Geo_area == geo_area) |>
                       dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                                        sum = list(sum_tibb(excrete_nut))) |>
                       tidyr::unnest(sum) |>
                       tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                                           names_to = "Element", 
                                           values_to = "Excretion") |>
                       dplyr::mutate(Element = factor(Element, 
                                                      levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                                 "Se", "Zn", "Co", "As")), 
                                     Excretion = Excretion
                       ) |>
                       dplyr::group_by(Geo_area, Element) |>
                       dplyr::summarize(mean_total = mean(Excretion))) |>
    dplyr::mutate(ratio_contribution = mean/mean_total)
  if (object_type == "file") {
    write.table(table, paste0("output/tables/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    table
  }
  
  
}


#'
#'
#'
#'
#'
# function to compute the relative contribution of cetacean taxa in each area and habitat
taxa_contribution_hab <- function(output_tib, 
                                  geo_area # should be character string
){
  output_tib |>
    dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
    dplyr::filter(Geo_area == geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = Excretion
    )  |>
    dplyr::group_by(Geo_area, Eco_area, Eco_gp, Element) |>
    dplyr::summarize(mean = mean(Excretion)) |>
    dplyr::left_join(output_tib |> 
                       dplyr::group_by(Geo_area, Eco_area) |>
                       dplyr::filter(Geo_area == geo_area) |>
                       dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                                        sum = list(sum_tibb(excrete_nut))) |>
                       tidyr::unnest(sum) |>
                       tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                                           names_to = "Element", 
                                           values_to = "Excretion") |> 
                       dplyr::mutate(Element = factor(Element, 
                                                      levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                                 "Se", "Zn", "Co", "As")), 
                                     Excretion = Excretion
                       )  |>
                       dplyr::group_by(Geo_area, Eco_area, Element) |>
                       dplyr::summarize(mean_total = mean(Excretion))) |>
    dplyr::mutate(ratio_contribution = mean/mean_total)
  
}



#'
#'
#'
#'
#'
# function to compute statistical test on excretion per taxa in total
test_differences_taxa <- function(output_tib,
                                  object_type, # either "output" or "file" 
                                  name_file, 
                                  geo_area
) {
  # element is a character string indicating the element for which to conduct the test
  final_table <- tibble::tibble(Geo_area = NA, 
                                Element = NA, 
                                Group1 = NA,
                                Group2 = NA, 
                                ratio_group1_superior_to_group2 = NA)
  
  for (i in c("N", "P", "Fe", "Cu", "Mn", 
              "Se", "Zn", "Co")) {
    # areas with baleen whales
    if (geo_area %in% c("Central North Atlantic", "Northeast Atlantic", "Northwest Atlantic", "California current", 
                        "Hawaii", "Gulf of Mexico", "Mediterranean Sea", "Gulf of Alaska")) {
      el_table <- output_tib |>
        dplyr::group_by(Geo_area, Eco_gp) |>
        dplyr::filter(Geo_area == geo_area) |>
        dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                         sum = list(sum_tibb(excrete_nut))) |>
        tidyr::unnest(sum) |>
        tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                            names_to = "Element", 
                            values_to = "Excretion") |> 
        dplyr::mutate(Element = factor(Element, 
                                       levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                  "Se", "Zn", "Co", "As")), 
                      Excretion = Excretion/Surf
        )  |> 
        dplyr::filter(Element == i) |>
        dplyr::ungroup() |>
        dplyr::select(Eco_gp, Excretion) |>
        tidyr::pivot_wider(names_from = Eco_gp, 
                           values_from = Excretion, 
                           values_fn = list) |>
        tidyr::unnest(cols = c(`Baleen whales`, `Deep divers`, `Small delphinids`)) |>
        dplyr::mutate(t_baleen_deep = dplyr::case_when(`Baleen whales` > `Deep divers` ~ 1,
                                                       TRUE ~ 0), 
                      t_baleen_delphi = dplyr::case_when(`Baleen whales` > `Small delphinids` ~ 1,
                                                         TRUE ~ 0),
                      t_deep_delphi = dplyr::case_when(`Deep divers` > `Small delphinids` ~ 1,
                                                       TRUE ~ 0),
        ) |>
        dplyr::summarise(t_baleen_deep = mean(t_baleen_deep), 
                         t_baleen_delphi = mean(t_baleen_delphi),
                         t_deep_delphi = mean(t_deep_delphi)) |>
        tidyr::pivot_longer(cols = c("t_baleen_deep":"t_deep_delphi"),
                            names_to = "Test", 
                            values_to = "ratio_group1_superior_to_group2") |>
        dplyr::mutate(Geo_area = geo_area, 
                      Element = i, 
                      Group1 = dplyr::case_when(stringr::str_starts(Test, "t_baleen") ~ "Baleen whales",
                                                stringr::str_starts(Test, "t_deep") ~ "Deep divers"), 
                      Group2 = dplyr::case_when(stringr::str_ends(Test, "_deep") ~ "Deep divers",
                                                stringr::str_ends(Test, "_delphi") ~ "Small delphinids")) |>
        dplyr::select(-Test) 
      
      final_table <- rbind(final_table, el_table)
      
      rm(el_table)
      
    } else if (geo_area %in% c("French Antilles", "French Guyana", "West Indian ocean",               
                               "French Polynesia", "New Caledonia", "Wallis & Futuna")) {
      el_table <- output_tib |>
        dplyr::group_by(Geo_area, Eco_gp) |>
        dplyr::filter(Geo_area == geo_area) |>
        dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                         sum = list(sum_tibb(excrete_nut))) |>
        tidyr::unnest(sum) |>
        tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                            names_to = "Element", 
                            values_to = "Excretion") |> 
        dplyr::mutate(Element = factor(Element, 
                                       levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                  "Se", "Zn", "Co", "As")), 
                      Excretion = Excretion/Surf
        )  |> 
        dplyr::filter(Element == i) |>
        dplyr::ungroup() |>
        dplyr::select(Eco_gp, Excretion) |>
        tidyr::pivot_wider(names_from = Eco_gp, 
                           values_from = Excretion, 
                           values_fn = list) |>
        tidyr::unnest(cols = c(`Deep divers`, `Small delphinids`)) |>
        dplyr::mutate(t_deep_delphi = dplyr::case_when(`Deep divers` > `Small delphinids` ~ 1,
                                                       TRUE ~ 0),
        ) |>
        dplyr::summarise(t_deep_delphi = mean(t_deep_delphi)) |>
        tidyr::pivot_longer(cols = c("t_deep_delphi"),
                            names_to = "Test", 
                            values_to = "ratio_group1_superior_to_group2") |>
        dplyr::mutate(Geo_area = geo_area, 
                      Element = i, 
                      Group1 = "Deep divers", 
                      Group2 = "Small delphinids") |>
        dplyr::select(-Test) 
      
      final_table <- rbind(final_table, el_table)
      
      rm(el_table)
      
    }
  }
  
  final_table <- final_table[-1,]
  
  if (object_type == "file") {
    write.table(final_table, paste0("output/tables/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    final_table
  }
  
}


#'
#'
#'
#'
#'
# function to compute statistical test on excretion per taxa in total
test_differences_taxa_hab <- function(output_tib, 
                                      geo_area
) {
  # element is a character string indicating the element for which to conduct the test
  final_table <- tibble::tibble(Geo_area = NA, 
                                Eco_area = NA,
                                Element = NA, 
                                Group1 = NA,
                                Group2 = NA,
                                ratio_group1_superior_to_group2 = NA)
  
  for (i in c("N", "P", "Fe", "Cu", "Mn", 
              "Se", "Zn", "Co")) {
    # areas with baleen whales
    if (geo_area %in% c("Central North Atlantic", "Northeast Atlantic", "Northwest Atlantic", "California current", 
                        "Hawaii", "Gulf of Mexico", "Mediterranean Sea", "Gulf of Alaska")) {
      el_table <- output_tib |>
        dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
        dplyr::filter(Geo_area == geo_area) |>
        dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                         sum = list(sum_tibb(excrete_nut))) |>
        tidyr::unnest(sum) |>
        tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                            names_to = "Element", 
                            values_to = "Excretion") |> 
        dplyr::mutate(Element = factor(Element, 
                                       levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                  "Se", "Zn", "Co", "As")), 
                      Excretion = Excretion*1e3/Surf # from tons to kg/km2
        )  |> 
        dplyr::filter(Element == i) |>
        dplyr::ungroup() |>
        dplyr::group_by(Eco_area) |>
        dplyr::select(Eco_gp, Eco_area, Excretion) |>
        tidyr::pivot_wider(names_from = Eco_gp, 
                           values_from = Excretion, 
                           values_fn = list) |>
        tidyr::unnest(cols = c(`Baleen whales`, `Deep divers`, `Small delphinids`)) |>
        dplyr::mutate(t_baleen_deep = dplyr::case_when(`Baleen whales` > `Deep divers` ~ 1,
                                                       TRUE ~ 0), 
                      t_baleen_delphi = dplyr::case_when(`Baleen whales` > `Small delphinids` ~ 1,
                                                         TRUE ~ 0),
                      t_deep_delphi = dplyr::case_when(`Deep divers` > `Small delphinids` ~ 1,
                                                       TRUE ~ 0),
        ) |>
        dplyr::summarise(t_baleen_deep = mean(t_baleen_deep), 
                         t_baleen_delphi = mean(t_baleen_delphi),
                         t_deep_delphi = mean(t_deep_delphi)) |>
        tidyr::pivot_longer(cols = c("t_baleen_deep":"t_deep_delphi"),
                            names_to = "Test", 
                            values_to = "ratio_group1_superior_to_group2") |>
        dplyr::mutate(Geo_area = geo_area, 
                      Element = i, 
                      Group1 = dplyr::case_when(stringr::str_starts(Test, "t_baleen") ~ "Baleen whales",
                                                stringr::str_starts(Test, "t_deep") ~ "Deep divers"), 
                      Group2 = dplyr::case_when(stringr::str_ends(Test, "_deep") ~ "Deep divers",
                                                stringr::str_ends(Test, "_delphi") ~ "Small delphinids")) |>
        dplyr::select(-Test) 
      
      final_table <- rbind(final_table, el_table)
      
      rm(el_table)
      
    } else if (geo_area %in% c("French Antilles", "French Guyana", "West Indian ocean",               
                               "French Polynesia", "New Caledonia", "Wallis & Futuna")) {
      el_table <- output_tib |>
        dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
        dplyr::filter(Geo_area == geo_area) |>
        dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                         sum = list(sum_tibb(excrete_nut))) |>
        tidyr::unnest(sum) |>
        tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                            names_to = "Element", 
                            values_to = "Excretion") |> 
        dplyr::mutate(Element = factor(Element, 
                                       levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                  "Se", "Zn", "Co", "As")), 
                      Excretion = Excretion*1e3/Surf # from tons to kg/km2
        )  |> 
        dplyr::filter(Element == i) |>
        dplyr::ungroup() |>
        dplyr::group_by(Eco_area) |>
        dplyr::select(Eco_gp, Eco_area, Excretion) |>
        tidyr::pivot_wider(names_from = Eco_gp, 
                           values_from = Excretion, 
                           values_fn = list) |>
        tidyr::unnest(cols = c(`Deep divers`, `Small delphinids`)) |>
        dplyr::mutate(t_deep_delphi = dplyr::case_when(`Deep divers` > `Small delphinids` ~ 1,
                                                       TRUE ~ 0),
        ) |>
        dplyr::summarise(t_deep_delphi = mean(t_deep_delphi)) |>
        tidyr::pivot_longer(cols = c("t_deep_delphi"),
                            names_to = "Test", 
                            values_to = "ratio_group1_superior_to_group2") |>
        dplyr::mutate(Geo_area = geo_area, 
                      Element = i, 
                      Group1 = "Deep divers", 
                      Group2 = "Small delphinids") |>
        dplyr::select(-Test) 
      
      final_table <- rbind(final_table, el_table)
      
      rm(el_table)
      
    }
  }
  
  final_table <- final_table[-1,]
  
  return(final_table)
  
  
}



############### ALL SPECIES - ALL AREAS ################ relative composition of poop 

#'
#'
#'
#'
#'
# statistics of estimates of the relative composition of poop of taxa
create_stat_tab_compo_poop <- function(output_tib, 
                                       object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                       name_file # should be a character string,
) {
  
  profile_excretion <- output_tib |>
    dplyr::ungroup() |>
    dplyr::select(c(Eco_gp, Species, Indi_data, excrete_nut_ind, Mass)) 
  
  
  # select only one line per species (as there is many lines for all the places each species occurs)
  profile_excretion <- profile_excretion[c(1, 9, 13, 15, 19, 
                                           31, 32, 33, 40, 
                                           42, 51, 58, 73, 
                                           76, 80, 92, 96, 
                                           100, 106, 107, 108, 
                                           115, 129, 136, 146, 
                                           149, 152, 168, 176, 
                                           177, 179, 186, 190, 
                                           191, 199, 201, 208,
                                           225),]
  
  table <- profile_excretion |>
    dplyr::group_by(Eco_gp) |>
    dplyr::mutate(excrete_ind_perkg_food = seq_along(excrete_nut_ind) |>
                    purrr::map(~ purrr::pluck(excrete_nut_ind, .)/purrr::pluck(Indi_data, ., "Ration"))) |>
    dplyr::select(-c(Indi_data, excrete_nut_ind, Mass)) |>
    tidyr::unnest(excrete_ind_perkg_food) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn),
                        names_to = "Element",
                        values_to = "Excretion_ind") |>
    dplyr::mutate(Element = factor(Element,
                                   levels = c("N", "P", "Fe", "Cu", "Mn",
                                              "Se", "Zn", "Co", "As")))  |>
    dplyr::group_by(Eco_gp, Element) |>
    dplyr::summarize(min = min(Excretion_ind),
                     `2.5_quant` = quantile(Excretion_ind, probs = c(0.025)),
                     mean = mean(Excretion_ind),
                     median = median(Excretion_ind),
                     `97.5_quant` = quantile(Excretion_ind, probs = c(0.975)),
                     max = max(Excretion_ind))
  
  if (object_type == "file") {
    write.table(table, paste0("output/tables/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    table
  }
  
}


#'
#'
#'
#'
#'
# statistics of estimates of the relative composition of poop of taxa 
# with normalisation per element 

create_stat_tab_compo_poop_norm <- function(output_tib, 
                                            object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                            name_file # should be a character string
) {
  
  profile_excretion <- output_tib |>
    dplyr::ungroup() |>
    dplyr::select(c(Eco_gp, Species, Indi_data, excrete_nut_ind, Mass)) 
  
  
  # select only one line per species (as there is many lines for all the places each species occurs)
  profile_excretion <- profile_excretion[c(1, 9, 13, 15, 19, 
                                           31, 32, 33, 40, 
                                           42, 51, 58, 73, 
                                           76, 80, 92, 96, 
                                           100, 106, 107, 108, 
                                           115, 129, 136, 146, 
                                           149, 152, 168, 176, 
                                           177, 179, 186, 190, 
                                           191, 199, 201, 208,
                                           225),]
  
  table <- profile_excretion |>
    dplyr::group_by(Eco_gp) |>
    dplyr::mutate(excrete_ind_perkg_food = seq_along(excrete_nut_ind) |>
                    purrr::map(~ purrr::pluck(excrete_nut_ind, .)/purrr::pluck(Indi_data, ., "Ration"))) |>
    dplyr::select(-c(Indi_data, excrete_nut_ind, Mass)) |>
    tidyr::unnest(excrete_ind_perkg_food) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn),
                        names_to = "Element",
                        values_to = "Excretion_ind") |>
    dplyr::mutate(Element = factor(Element,
                                   levels = c("N", "P", "Fe", "Cu", "Mn",
                                              "Se", "Zn", "Co", "As")))  |>
    dplyr::group_by(Element) |>
    dplyr::mutate(Exc_norm = (Excretion_ind - min(Excretion_ind))/(max(Excretion_ind) - min(Excretion_ind))) |>
    dplyr::group_by(Eco_gp, Element) |>
    dplyr::summarize(min = min(Exc_norm),
                     `2.5_quant` = quantile(Exc_norm, probs = c(0.025)),
                     `10_quant` = quantile(Exc_norm, probs = c(0.1)),
                     mean = mean(Exc_norm),
                     median = median(Exc_norm),
                     `90_quant` = quantile(Exc_norm, probs = c(0.9)),
                     `97.5_quant` = quantile(Exc_norm, probs = c(0.975)),
                     max = max(Exc_norm))
  
  if (object_type == "file") {
    write.table(table, paste0("output/tables/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    table
  }
  
}


#'
#'
#'
#'
#'
# statistics of estimates of the relative composition of poop of taxa 
# with normalisation per element 

create_tab_full_compo_poop_norm <- function(output_tib) {
  
  profile_excretion <- output_tib |>
    dplyr::ungroup() |>
    dplyr::select(c(Eco_gp, Species, Indi_data, excrete_nut_ind, Mass)) 
  
  
  # select only one line per species (as there is many lines for all the places each species occurs)
  profile_excretion <- profile_excretion[c(1, 9, 13, 15, 19, 
                                           31, 32, 33, 40, 
                                           42, 51, 58, 73, 
                                           76, 80, 92, 96, 
                                           100, 106, 107, 108, 
                                           115, 129, 136, 146, 
                                           149, 152, 168, 176, 
                                           177, 179, 186, 190, 
                                           191, 199, 201, 208,
                                           225),]
  
  profile_excretion |>
    dplyr::group_by(Eco_gp) |>
    dplyr::mutate(excrete_ind_perkg_food = seq_along(excrete_nut_ind) |>
                    purrr::map(~ purrr::pluck(excrete_nut_ind, .)/purrr::pluck(Indi_data, ., "Ration"))) |>
    dplyr::select(-c(Indi_data, excrete_nut_ind, Mass)) |>
    tidyr::unnest(excrete_ind_perkg_food) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn),
                        names_to = "Element",
                        values_to = "Excretion_ind") |>
    dplyr::mutate(Element = factor(Element,
                                   levels = c("N", "P", "Fe", "Cu", "Mn",
                                              "Se", "Zn", "Co", "As")))  |>
    dplyr::group_by(Element) |>
    dplyr::mutate(Exc_norm = (Excretion_ind - min(Excretion_ind))/(max(Excretion_ind) - min(Excretion_ind)))
  
  
  
}





#'
#'
#'
#'
#'
# function to compute statistical test on the relative composition of poop of the three taxa
test_differences_compo_poop <- function(output_tib, 
                                        object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                        name_file # should be a character string 
) {
  
  final_table <- tibble::tibble(Element = NA, 
                                Group1 = NA,
                                Group2 = NA, 
                                ratio_group1_superior_to_group2 = NA)
  
  
  profile_excretion <- output_tib |>
    dplyr::ungroup() |>
    dplyr::select(c(Eco_gp, Species, Indi_data, excrete_nut_ind, Mass)) 
  
  
  # select only one line per species (as there is many lines for all the places each species occurs)
  profile_excretion <- profile_excretion[c(1, 9, 13, 15, 19, 
                                           31, 32, 33, 40, 
                                           42, 51, 58, 73, 
                                           76, 80, 92, 96, 
                                           100, 106, 107, 108, 
                                           115, 129, 136, 146, 
                                           149, 152, 168, 176, 
                                           177, 179, 186, 190, 
                                           191, 199, 201, 208,
                                           225),]
  
  
  Bw <- profile_excretion |>
    dplyr::group_by(Eco_gp) |>
    dplyr::mutate(excrete_ind_perkg_food = seq_along(excrete_nut_ind) |>
                    purrr::map(~ purrr::pluck(excrete_nut_ind, .)/purrr::pluck(Indi_data, ., "Ration"))) |>
    dplyr::select(-c(Indi_data, excrete_nut_ind, Mass)) |>
    tidyr::unnest(excrete_ind_perkg_food) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn),
                        names_to = "Element",
                        values_to = "Excretion_ind") |> 
    dplyr::filter(Eco_gp == "Baleen whales") |> 
    tidyr::pivot_wider(names_from = Species, 
                       values_from = Excretion_ind, 
                       values_fn = list) |>
    tidyr::unnest(c("Balaenoptera acutorostrata", "Balaenoptera borealis",     
                    "Balaenoptera edeni", "Balaenoptera musculus",     
                    "Balaenoptera physalus", "Megaptera novaeangliae")) |>
    dplyr::rowwise() |>
    dplyr::mutate(mean_gp = mean(`Balaenoptera acutorostrata`, `Balaenoptera borealis`,     
                                 `Balaenoptera edeni`, `Balaenoptera musculus`,     
                                 `Balaenoptera physalus`, `Megaptera novaeangliae`)) |> 
    dplyr::select(Eco_gp, Element, mean_gp)
  
  
  Dd <- profile_excretion |>
    dplyr::group_by(Eco_gp) |>
    dplyr::mutate(excrete_ind_perkg_food = seq_along(excrete_nut_ind) |>
                    purrr::map(~ purrr::pluck(excrete_nut_ind, .)/purrr::pluck(Indi_data, ., "Ration"))) |>
    dplyr::select(-c(Indi_data, excrete_nut_ind, Mass)) |>
    tidyr::unnest(excrete_ind_perkg_food) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn),
                        names_to = "Element",
                        values_to = "Excretion_ind") |> 
    dplyr::filter(Eco_gp == "Deep divers") |> 
    tidyr::pivot_wider(names_from = Species, 
                       values_from = Excretion_ind, 
                       values_fn = list) |>
    tidyr::unnest(c("Berardius bairdii",          "Feresa attenuata",         
                    "Globicephala macrorhynchus", "Globicephala melas",        
                    "Grampus griseus",            "Hyperoodon ampullatus",     
                    "Indopacetus pacificus",      "Kogia spp",                 
                    "Mesoplodon spp",             "Peponocephala electra",     
                    "Physeter macrocephalus",     "Pseudorca crassidens",      
                    "Ziphius cavirostris")) |>
    dplyr::rowwise() |>
    dplyr::mutate(mean_gp = mean(`Berardius bairdii`,          `Feresa attenuata`,         
                                 `Globicephala macrorhynchus`, `Globicephala melas`,        
                                 `Grampus griseus`,            `Hyperoodon ampullatus`,     
                                 `Indopacetus pacificus`,      `Kogia spp`,                 
                                 `Mesoplodon spp`,             `Peponocephala electra`,     
                                 `Physeter macrocephalus`,     `Pseudorca crassidens`,      
                                 `Ziphius cavirostris`)) |> 
    dplyr::select(Eco_gp, Element, mean_gp)
  
  Sd <- profile_excretion |>
    dplyr::group_by(Eco_gp) |>
    dplyr::mutate(excrete_ind_perkg_food = seq_along(excrete_nut_ind) |>
                    purrr::map(~ purrr::pluck(excrete_nut_ind, .)/purrr::pluck(Indi_data, ., "Ration"))) |>
    dplyr::select(-c(Indi_data, excrete_nut_ind, Mass)) |>
    tidyr::unnest(excrete_ind_perkg_food) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn),
                        names_to = "Element",
                        values_to = "Excretion_ind") |> 
    dplyr::filter(Eco_gp == "Small delphinids") |> 
    tidyr::pivot_wider(names_from = Species, 
                       values_from = Excretion_ind, 
                       values_fn = list) |>
    tidyr::unnest(c("Delphinus capensis",         "Delphinus delphis",         
                    "Lagenorhynchus acutus",      "Lagenorhynchus albirostris",
                    "Lagenodelphis hosei",        "Lagenorhynchus obliquidens",
                    "Lissodelphis borealis",      "Orcinus orca",              
                    "Phocoenoides dalli",         "Phocoena phocoena",         
                    "Sotalia guianensis",         "Sousa plumbea",             
                    "Stenella attenuata",        "Steno bredanensis",         
                    "Stenella clymene",           "Stenella coeruleoalba",     
                    "Stenella frontalis",         "Stenella longirostris",     
                    "Tursiops truncatus")) |>
    dplyr::rowwise() |>
    dplyr::mutate(mean_gp = mean(`Delphinus capensis`,         `Delphinus delphis`,         
                                 `Lagenorhynchus acutus`,      `Lagenorhynchus albirostris`,
                                 `Lagenodelphis hosei`,        `Lagenorhynchus obliquidens`,
                                 `Lissodelphis borealis`,      `Orcinus orca`,              
                                 `Phocoenoides dalli`,         `Phocoena phocoena`,         
                                 `Sotalia guianensis`,         `Sousa plumbea`,             
                                 `Stenella attenuata`,        `Steno bredanensis`,         
                                 `Stenella clymene`,           `Stenella coeruleoalba`,     
                                 `Stenella frontalis`,         `Stenella longirostris`,     
                                 `Tursiops truncatus`)) |> 
    dplyr::select(Eco_gp, Element, mean_gp)
  
  
  table_mean_col_all_gp <- rbind(Bw, Dd, Sd)
  
  
  for (i in c("N", "P", "Fe", "Cu", "Mn", 
              "Se", "Zn", "Co")) {
    
    el_table <- table_mean_col_all_gp |>
      tidyr::pivot_wider(names_from = Eco_gp, 
                         values_from = mean_gp, 
                         values_fn = list) |>
      dplyr::filter(Element == i) |>
      tidyr::unnest(cols = c(`Baleen whales`, `Deep divers`, `Small delphinids`)) |>
      dplyr::mutate(t_baleen_deep = dplyr::case_when(`Baleen whales` > `Deep divers` ~ 1,
                                                     TRUE ~ 0), 
                    t_baleen_delphi = dplyr::case_when(`Baleen whales` > `Small delphinids` ~ 1,
                                                       TRUE ~ 0),
                    t_deep_delphi = dplyr::case_when(`Deep divers` > `Small delphinids` ~ 1,
                                                     TRUE ~ 0),
      ) |>
      dplyr::summarise(t_baleen_deep = mean(t_baleen_deep), 
                       t_baleen_delphi = mean(t_baleen_delphi),
                       t_deep_delphi = mean(t_deep_delphi)) |>
      tidyr::pivot_longer(cols = c("t_baleen_deep":"t_deep_delphi"),
                          names_to = "Test", 
                          values_to = "ratio_group1_superior_to_group2") |>
      dplyr::mutate(Element = i, 
                    Group1 = dplyr::case_when(stringr::str_starts(Test, "t_baleen") ~ "Baleen whales",
                                              stringr::str_starts(Test, "t_deep") ~ "Deep divers"), 
                    Group2 = dplyr::case_when(stringr::str_ends(Test, "_deep") ~ "Deep divers",
                                              stringr::str_ends(Test, "_delphi") ~ "Small delphinids")) |>
      dplyr::select(-Test) 
    
    final_table <- rbind(final_table, el_table)
    
    rm(el_table)
    
  }
  
  final_table <- final_table[-1,]
  
  if (object_type == "file") {
    write.table(final_table, paste0("output/tables/", 
                                    name_file,
                                    ".txt"), sep = "\t")
  } else {
    final_table
  }
  
}





######################## functions to create figures #############################

################ ALL AREAS #######################

#'
#'
#'
#'
#'
# function to create graph with densities of excretion in all areas, one facet/element
fig_exc_all_areas_1_facet_element <- function(output_tib, 
                                              object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                              name_file # should be a character string
                                              ) {
  output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = Excretion*1e3/Surf # from tons to kg/km2
    )  |>
    dplyr::group_by(Geo_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    ggplot2::ggplot() +
    ggplot2::geom_errorbar(ggplot2::aes(x = Geo_area, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Geo_area), 
                           size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = Geo_area, y = mean, color = Geo_area)) +
    ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                  14, # nb of areas
                                                                  type = "continuous"), 
                                name = "Area") +
    ggplot2::facet_wrap(~Element, scales = "free") +
    #guides(color = FALSE) + 
    ggplot2::xlab("Area") +
    ggplot2::ylab("Excretion (in kg/km2/yr)") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.length.x = ggplot2::unit(0, "cm"),
                   #legend.title = element_blank()
    )
  
  if (object_type == "file") {
  ggplot2::ggsave(paste0("output/figures/", 
                         name_file, 
                         ".jpg"), scale = 1)
  } else {
    output_tib |>
      dplyr::group_by(Geo_area) |>
      dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                       sum = list(sum_tibb(excrete_nut))) |>
      tidyr::unnest(sum) |>
      tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                          names_to = "Element", 
                          values_to = "Excretion") |> 
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co", "As")), 
                    Excretion = Excretion*1e3/Surf # from tons to kg/km2
      )  |>
      dplyr::group_by(Geo_area, Element) |>
      dplyr::summarize(min = min(Excretion), 
                       `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                       mean = mean(Excretion), 
                       median = median(Excretion), 
                       `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                       max = max(Excretion)) |>
      ggplot2::ggplot() +
      ggplot2::geom_errorbar(ggplot2::aes(x = Geo_area, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Geo_area), 
                             size = 1) +
      ggplot2::geom_point(ggplot2::aes(x = Geo_area, y = mean, color = Geo_area)) +
      ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                    14, # nb of areas
                                                                    type = "continuous"), 
                                  name = "Area") +
      ggplot2::facet_wrap(~Element, scales = "free") +
      #guides(color = FALSE) + 
      ggplot2::xlab("Area") +
      ggplot2::ylab("Excretion (in kg/km2/yr)") +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.length.x = ggplot2::unit(0, "cm"),
                     #legend.title = element_blank()
      )
  }
}



#'
#'
#'
#'
#'
# function to create graph with densities of excretion in all areas, one facet/element
# but just with one taxa
fig_exc_all_areas_1_facet_element_taxa <- function(output_tib, 
                                                   taxa, # should be a character string 
                                                   object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                                   name_file # should be a character string
) {
  output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::filter(Eco_gp == taxa) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = Excretion*1e3/Surf # from tons to kg/km2
    )  |>
    dplyr::group_by(Geo_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    ggplot2::ggplot() +
    ggplot2::geom_errorbar(ggplot2::aes(x = Geo_area, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Geo_area), 
                           size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = Geo_area, y = mean, color = Geo_area)) +
    ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                  14, # nb of areas
                                                                  type = "continuous"), 
                                name = "Area") +
    ggplot2::facet_wrap(~Element, scales = "free") +
    #guides(color = FALSE) + 
    ggplot2::xlab("Area") +
    ggplot2::ylab("Excretion (in kg/km2/yr)") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.length.x = ggplot2::unit(0, "cm"),
                   #legend.title = element_blank()
    )
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/figures/", 
                           name_file, 
                           ".jpg"), scale = 1)
  } else {
    output_tib |>
      dplyr::group_by(Geo_area) |>
      dplyr::filter(Eco_gp == taxa) |>
      dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                       sum = list(sum_tibb(excrete_nut))) |>
      tidyr::unnest(sum) |>
      tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                          names_to = "Element", 
                          values_to = "Excretion") |> 
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co", "As")), 
                    Excretion = Excretion*1e3/Surf # from tons to kg/km2
      )  |>
      dplyr::group_by(Geo_area, Element) |>
      dplyr::summarize(min = min(Excretion), 
                       `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                       mean = mean(Excretion), 
                       median = median(Excretion), 
                       `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                       max = max(Excretion)) |>
      ggplot2::ggplot() +
      ggplot2::geom_errorbar(ggplot2::aes(x = Geo_area, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Geo_area), 
                             size = 1) +
      ggplot2::geom_point(ggplot2::aes(x = Geo_area, y = mean, color = Geo_area)) +
      ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                    14, # nb of areas
                                                                    type = "continuous"), 
                                  name = "Area") +
      ggplot2::facet_wrap(~Element, scales = "free") +
      #guides(color = FALSE) + 
      ggplot2::xlab("Area") +
      ggplot2::ylab("Excretion (in kg/km2/yr)") +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.length.x = ggplot2::unit(0, "cm"),
                     #legend.title = element_blank()
      )
  }
}


#'
#'
#'
#'
#'
# function to create graph with densities of excretion in all areas, all elements with log10 trans
fig_exc_all_areas_log10 <- function(output_tib, 
                                    object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                    name_file # should be a character string
                                    ) {
  output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = Excretion*1e3/Surf # from tons to kg/km2
    )  |>
    dplyr::group_by(Geo_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    ggplot2::ggplot() +
    ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Geo_area), 
                           size = 1, position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Geo_area), 
                        position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                  14, #nb of areas
                                                                  type = "continuous"), name = "Area") +
    ggplot2::scale_y_continuous(trans = "log10", 
                                breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
    ggplot2::xlab("Element") +
    ggplot2::ylab("Excretion (in kg/km2/yr)") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/figures/", 
                           name_file, 
                           ".jpg"), scale = 1)
  } else {
    output_tib |>
      dplyr::group_by(Geo_area) |>
      dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                       sum = list(sum_tibb(excrete_nut))) |>
      tidyr::unnest(sum) |>
      tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                          names_to = "Element", 
                          values_to = "Excretion") |> 
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co", "As")), 
                    Excretion = Excretion*1e3/Surf # from tons to kg/km2
      )  |>
      dplyr::group_by(Geo_area, Element) |>
      dplyr::summarize(min = min(Excretion), 
                       `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                       mean = mean(Excretion), 
                       median = median(Excretion), 
                       `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                       max = max(Excretion)) |>
      ggplot2::ggplot() +
      ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Geo_area), 
                             size = 1, position = ggplot2::position_dodge(width = 0.9)) +
      ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Geo_area), 
                          position = ggplot2::position_dodge(width = 0.9)) +
      ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                    14, #nb of areas
                                                                    type = "continuous"), name = "Area") +
      ggplot2::scale_y_continuous(trans = "log10", 
                                  breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
      ggplot2::xlab("Element") +
      ggplot2::ylab("Excretion (in kg/km2/yr)") +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }
}


#'
#'
#'
#'
#'
# function with graph of total N excretion vs total surface of the areas 
fig_exc_vs_tot_surf <- function(output_tib,
                                object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd 
                                name_file # should be a character string
                                ) {
  output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = Excretion*1e3/Surf, 
                  Geo_area = factor(Geo_area, 
                                    levels = c("Northeast Atlantic", "Central North Atlantic", "Gulf of Alaska",
                                               "Northwest Atlantic", "California current", 
                                               "Mediterranean Sea", "West Indian ocean", "Gulf of Mexico", "French Antilles", 
                                               "New Caledonia", "Hawaii",  
                                               "French Guyana", "Wallis & Futuna", "French Polynesia"))
    )  |>
    dplyr::filter(Element == "N") |>
    dplyr::group_by(Geo_area, Surf) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = Surf, y = mean, color = Geo_area)) +
    ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                  14, # nb of areas
                                                                  type = "continuous"), name = "Area") +
    ggplot2::xlab("Surface of area (km2)") +
    ggplot2::ylab("Mean N fertilization density (kg/yr/km2)") +
    ggplot2::theme()
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/figures/", 
                           name_file, 
                           ".jpg"), scale = 1)
  } else {
    output_tib |>
      dplyr::group_by(Geo_area) |>
      dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                       sum = list(sum_tibb(excrete_nut))) |>
      tidyr::unnest(sum) |>
      tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                          names_to = "Element", 
                          values_to = "Excretion") |> 
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co", "As")), 
                    Excretion = Excretion*1e3/Surf, 
                    Geo_area = factor(Geo_area, 
                                      levels = c("Northeast Atlantic", "Central North Atlantic", "Gulf of Alaska",
                                                 "Northwest Atlantic", "California current", 
                                                 "Mediterranean Sea", "West Indian ocean", "Gulf of Mexico", "French Antilles", 
                                                 "New Caledonia", "Hawaii",  
                                                 "French Guyana", "Wallis & Futuna", "French Polynesia"))
      )  |>
      dplyr::filter(Element == "N") |>
      dplyr::group_by(Geo_area, Surf) |>
      dplyr::summarize(min = min(Excretion), 
                       `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                       mean = mean(Excretion), 
                       median = median(Excretion), 
                       `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                       max = max(Excretion)) |>
      ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = Surf, y = mean, color = Geo_area)) +
      ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                    14, # nb of areas
                                                                    type = "continuous"), name = "Area") +
      ggplot2::xlab("Surface of area (km2)") +
      ggplot2::ylab("Mean N fertilization density (kg/yr/km2)") +
      ggplot2::theme()
  }
}


################## AREA PER AREA ######################

#'
#'
#'
#'
#'
# function with graph of total excretion of each cetacean taxa
fig_exc_taxa_log10 <- function(output_tib, 
                               geo_area, 
                               object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                               name_file # should be a character string
) {
  
  output_tib |>
    dplyr::group_by(Eco_gp, Geo_area) |>
    dplyr::filter(Geo_area == geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As"))) |> # from tons to kg
    dplyr::filter(Element != "As") |>
    dplyr::group_by(Eco_gp, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    ggplot2::ggplot() +
    ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_gp), 
                           size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp)) +
    ggplot2::scale_color_manual(values = c(`Baleen whales` = wesanderson::wes_palette("FantasticFox1", 3, 
                                                                                      type = "continuous")[1],
                                           `Deep divers` = wesanderson::wes_palette("FantasticFox1", 3, 
                                                                                    type = "continuous")[2],
                                           `Small delphinids` = wesanderson::wes_palette("FantasticFox1", 3, 
                                                                                         type = "continuous")[3])) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab("Excretion (in tons/yr)") +
    ggplot2::ggtitle(geo_area) +
    ggplot2::xlab("Element") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   strip.text.x = ggplot2::element_text(face = "bold", size = 12),
                   strip.text.y = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.x = ggplot2::element_text(size = 12), 
                   axis.text.y = ggplot2::element_text(size = 12), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 12), 
                   axis.title.y = ggplot2::element_text(face = "bold", size = 12), 
                   legend.title = ggplot2::element_blank()
    )
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/figures/", 
                           name_file, 
                           ".jpg"), scale = 1)
  } else {
    output_tib |>
      dplyr::group_by(Eco_gp, Geo_area) |>
      dplyr::filter(Geo_area == geo_area) |>
      dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                       sum = list(sum_tibb(excrete_nut))) |>
      tidyr::unnest(sum) |>
      tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                          names_to = "Element", 
                          values_to = "Excretion") |> 
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co", "As"))) |> # from tons to kg
      dplyr::filter(Element != "As") |>
      dplyr::group_by(Eco_gp, Element) |>
      dplyr::summarize(min = min(Excretion), 
                       `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                       mean = mean(Excretion), 
                       median = median(Excretion), 
                       `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                       max = max(Excretion)) |>
      ggplot2::ggplot() +
      ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_gp), 
                             size = 1) +
      ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp)) +
      ggplot2::scale_color_manual(values = c(`Baleen whales` = wesanderson::wes_palette("FantasticFox1", 3, 
                                                                                        type = "continuous")[1],
                                             `Deep divers` = wesanderson::wes_palette("FantasticFox1", 3, 
                                                                                      type = "continuous")[2],
                                             `Small delphinids` = wesanderson::wes_palette("FantasticFox1", 3, 
                                                                                           type = "continuous")[3])) +
      ggplot2::scale_y_continuous(trans = "log10") +
      ggplot2::ylab("Excretion (in tons/yr)") +
      ggplot2::ggtitle(geo_area) +
      ggplot2::xlab("Element") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     strip.text.x = ggplot2::element_text(face = "bold", size = 12),
                     strip.text.y = ggplot2::element_text(face = "bold", size = 12),
                     axis.text.x = ggplot2::element_text(size = 12), 
                     axis.text.y = ggplot2::element_text(size = 12), 
                     axis.title.x = ggplot2::element_text(face = "bold", size = 12), 
                     axis.title.y = ggplot2::element_text(face = "bold", size = 12), 
                     legend.title = ggplot2::element_blank()
      )
  }
  
}



#'
#'
#'
#'
#'
# function with graph of total excretion per habitat 
# either in absolute value or spatial densities
fig_exc_hab_log10 <- function(output_tib, 
                              geo_area, 
                              quantity, 
                              object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                              name_file # should be a character string
) {
  # quantity is a character string stating if output should a weight or a weight per surface unit 
  # either "weight" or "weight_per_km2"
  
  # adapt y title depending on calculation
  if (quantity == "weight") {
    
    # adapt axe title 
    y_title <- "Excretion (in tons/yr)"
    
  } else if (quantity == "weight_per_km2") {
    
    # adapt axe title 
    y_title <- "Excretion (in kg/yr/km2)"
  }
  
  output_tib |>
    dplyr::group_by(Eco_area, Geo_area) |>
    dplyr::filter(Geo_area == geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Eco_area = dplyr::case_when(Eco_area == "oceanic" ~ "Slope & oceanic waters", 
                                              Eco_area == "shelf" ~ "Neritic waters"),
                  Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = dplyr::case_when(quantity == "weight" ~ Excretion, 
                                               quantity == "weight_per_km2" ~ Excretion*1e3/Surf)) |> # from tons to kg/km2/yr
    dplyr::filter(Element != "As") |>
    dplyr::group_by(Eco_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    ggplot2::ggplot() +
    ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_area), 
                           size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_area)) +
    ggplot2::scale_color_manual(values = c(`Neritic waters` = wesanderson::wes_palette("Darjeeling1", 
                                                                                       6, 
                                                                                       type = "continuous")[3],
                                           `Slope & oceanic waters` = wesanderson::wes_palette("Darjeeling1", 
                                                                                               6, 
                                                                                               type = "continuous")[2])) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab("Excretion (in tons/yr)") +
    ggplot2::ggtitle(geo_area) +
    ggplot2::xlab("Element") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   strip.text.x = ggplot2::element_text(face = "bold", size = 12),
                   strip.text.y = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.x = ggplot2::element_text(size = 12), 
                   axis.text.y = ggplot2::element_text(size = 12), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 12), 
                   axis.title.y = ggplot2::element_text(face = "bold", size = 12), 
                   legend.title = ggplot2::element_blank()
    )
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/figures/", 
                           name_file, 
                           ".jpg"), scale = 1)
  } else {
    output_tib |>
      dplyr::group_by(Eco_area, Geo_area) |>
      dplyr::filter(Geo_area == geo_area) |>
      dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                       sum = list(sum_tibb(excrete_nut))) |>
      tidyr::unnest(sum) |>
      tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                          names_to = "Element", 
                          values_to = "Excretion") |> 
      dplyr::mutate(Eco_area = dplyr::case_when(Eco_area == "oceanic" ~ "Slope & oceanic waters", 
                                                Eco_area == "shelf" ~ "Neritic waters"),
                    Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co", "As")), 
                    Excretion = dplyr::case_when(quantity == "weight" ~ Excretion, 
                                                 quantity == "weight_per_km2" ~ Excretion*1e3/Surf)) |> # from tons to kg/km2/yr
      dplyr::filter(Element != "As") |>
      dplyr::group_by(Eco_area, Element) |>
      dplyr::summarize(min = min(Excretion), 
                       `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                       mean = mean(Excretion), 
                       median = median(Excretion), 
                       `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                       max = max(Excretion)) |>
      ggplot2::ggplot() +
      ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_area), 
                             size = 1) +
      ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_area)) +
      ggplot2::scale_color_manual(values = c(`Neritic waters` = wesanderson::wes_palette("Darjeeling1", 
                                                                                         6, 
                                                                                         type = "continuous")[3],
                                             `Slope & oceanic waters` = wesanderson::wes_palette("Darjeeling1", 
                                                                                                 6, 
                                                                                                 type = "continuous")[2])) +
      ggplot2::scale_y_continuous(trans = "log10") +
      ggplot2::ylab("Excretion (in tons/yr)") +
      ggplot2::ggtitle(geo_area) +
      ggplot2::xlab("Element") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     strip.text.x = ggplot2::element_text(face = "bold", size = 12),
                     strip.text.y = ggplot2::element_text(face = "bold", size = 12),
                     axis.text.x = ggplot2::element_text(size = 12), 
                     axis.text.y = ggplot2::element_text(size = 12), 
                     axis.title.x = ggplot2::element_text(face = "bold", size = 12), 
                     axis.title.y = ggplot2::element_text(face = "bold", size = 12), 
                     legend.title = ggplot2::element_blank()
      )
  }
  
  
}


#'
#'
#'
#'
#'
# function with graph of total excretion of each cetacean taxa per habitat 
# in spatial densities
fig_exc_hab_taxa_log10 <- function(output_tib, 
                                   geo_area, 
                                   object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                   name_file # should be a character string
) {
  # output_table is the complete table with all results of monte carlo simulations
  # geo_area is character string stating the area of interest 
  
  # here no interest in changing to densities of excretion as we are more interested in the relative
  # contribution of cetacean groups, that won t change
  
  output_tib |>
    dplyr::group_by(Eco_area, Eco_gp, Geo_area) |>
    dplyr::filter(Geo_area == geo_area) |>
    dplyr::summarize(Surf = sum(unique(Surf_tot)),
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Eco_area = dplyr::case_when(Eco_area == "oceanic" ~ "Slope & oceanic waters", 
                                              Eco_area == "shelf" ~ "Neritic waters"), 
                  Excretion = Excretion*1e3/Surf # from tons to kg per km2
    ) |>
    dplyr::filter(Element != "As") |>
    dplyr::group_by(Eco_area, Eco_gp, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    ggplot2::ggplot() +
    ggplot2::geom_errorbar(ggplot2::aes(x = Element, 
                                        ymin = `2.5_quant`, 
                                        ymax = `97.5_quant`, 
                                        color = Eco_gp), size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp)) +
    ggplot2::scale_color_manual(values = c(`Baleen whales` = wesanderson::wes_palette("FantasticFox1", 
                                                                                      3, 
                                                                                      type = "continuous")[1],
                                           `Deep divers` = wesanderson::wes_palette("FantasticFox1", 
                                                                                    3, 
                                                                                    type = "continuous")[2],
                                           `Small delphinids` = wesanderson::wes_palette("FantasticFox1", 
                                                                                         3, 
                                                                                         type = "continuous")[3])) +
    ggplot2::facet_wrap(~ Eco_area) +
    ggplot2::ylab("Excretion (in tons/yr)") +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ggtitle(geo_area) +
    ggplot2::xlab("") +
    ggplot2::ylab("Excretion (in tons/km2/yr)") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   strip.text.x = ggplot2::element_text(face = "bold", size = 12),
                   strip.text.y = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.x = ggplot2::element_text(size = 12), 
                   axis.text.y = ggplot2::element_text(size = 12), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 12), 
                   axis.title.y = ggplot2::element_text(face = "bold", size = 12), 
                   legend.title = ggplot2::element_blank()
    )
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/figures/", 
                           name_file, 
                           ".jpg"), scale = 1)
  } else {
    output_tib |>
      dplyr::group_by(Eco_area, Eco_gp, Geo_area) |>
      dplyr::filter(Geo_area == geo_area) |>
      dplyr::summarize(Surf = sum(unique(Surf_tot)),
                       sum = list(sum_tibb(excrete_nut))) |>
      tidyr::unnest(sum) |>
      tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                          names_to = "Element", 
                          values_to = "Excretion") |> 
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co", "As")), 
                    Eco_area = dplyr::case_when(Eco_area == "oceanic" ~ "Slope & oceanic waters", 
                                                Eco_area == "shelf" ~ "Neritic waters"), 
                    Excretion = Excretion*1e3/Surf # from tons to kg per km2
      ) |>
      dplyr::filter(Element != "As") |>
      dplyr::group_by(Eco_area, Eco_gp, Element) |>
      dplyr::summarize(min = min(Excretion), 
                       `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                       mean = mean(Excretion), 
                       median = median(Excretion), 
                       `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                       max = max(Excretion)) |>
      ggplot2::ggplot() +
      ggplot2::geom_errorbar(ggplot2::aes(x = Element, 
                                          ymin = `2.5_quant`, 
                                          ymax = `97.5_quant`, 
                                          color = Eco_gp), size = 1) +
      ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp)) +
      ggplot2::scale_color_manual(values = c(`Baleen whales` = wesanderson::wes_palette("FantasticFox1", 
                                                                                        3, 
                                                                                        type = "continuous")[1],
                                             `Deep divers` = wesanderson::wes_palette("FantasticFox1", 
                                                                                      3, 
                                                                                      type = "continuous")[2],
                                             `Small delphinids` = wesanderson::wes_palette("FantasticFox1", 
                                                                                           3, 
                                                                                           type = "continuous")[3])) +
      ggplot2::facet_wrap(~ Eco_area) +
      ggplot2::ylab("Excretion (in tons/yr)") +
      ggplot2::scale_y_continuous(trans = "log10") +
      ggplot2::ggtitle(geo_area) +
      ggplot2::xlab("") +
      ggplot2::ylab("Excretion (in tons/km2/yr)") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     strip.text.x = ggplot2::element_text(face = "bold", size = 12),
                     strip.text.y = ggplot2::element_text(face = "bold", size = 12),
                     axis.text.x = ggplot2::element_text(size = 12), 
                     axis.text.y = ggplot2::element_text(size = 12), 
                     axis.title.x = ggplot2::element_text(face = "bold", size = 12), 
                     axis.title.y = ggplot2::element_text(face = "bold", size = 12), 
                     legend.title = ggplot2::element_blank()
      )
  }
  
}