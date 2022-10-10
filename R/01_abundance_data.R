################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# December 2021
# 01_abundance_data.R
#
# Script with all functions to create the full tibble with all population 
# abundance data 
################################################################################
 

######################## preliminary functions #################################

# # function to verify totals of abundances, CVs, and densities with report
# # because we are not using the same blocks, we are computing subtotals 
# # this is a checking function, just to use directly in the console, 
# # does not generate output with target
# # it is indicated within functions where it should be called (# line)
# verify_totals <- function(df) {
#   # special case for Observe survey (Ireland)
#   # there are stratum with mixed neritic and oceanic waters
#   # we use the ratio of surfaces to separate abudances estimates between neritic and oceanic
#   rS1_N <- 30177/62052
#   rS1_O <- 31875/62052
#   rS2_N <- 35464/60167
#   rS2_O <- 24703/60167
#   rS3_N <- 80009/100482
#   rS3_O <- 20743/100482
#   
#   # compute our totals, to be compared with those from reports/papers
#   df |>
#     dplyr::group_by(Geo_area) |> 
#     dplyr::mutate(Abund = dplyr::case_when(Block == "S1_N" ~ round(rS1_N*Abund, 0), 
#                                            Block == "S1_O" ~ round(rS1_O*Abund, 0),
#                                            Block == "S2_N" ~ round(rS2_N*Abund, 0),
#                                            Block == "S2_O" ~ round(rS2_O*Abund, 0),
#                                            Block == "S3_N" ~ round(rS3_N*Abund, 0),
#                                            Block == "S3_O" ~ round(rS3_O*Abund, 0),
#                                            TRUE ~ Abund),
#                   Var_abund = dplyr::case_when(Block == "S1_N" ~ rS1_N*(Abund*CV*Abund*CV), 
#                                                Block == "S1_O" ~ rS1_O*(Abund*CV*Abund*CV),
#                                                Block == "S2_N" ~ rS2_N*(Abund*CV*Abund*CV),
#                                                Block == "S2_O" ~ rS2_O*(Abund*CV*Abund*CV),
#                                                Block == "S3_N" ~ rS3_N*(Abund*CV*Abund*CV),
#                                                Block == "S3_O" ~ rS3_O*(Abund*CV*Abund*CV),
#                                                TRUE ~ Abund*CV*Abund*CV),
#                   Survey = dplyr::case_when(Block %in% c("AA", "AB", "AC",
#                                                          "B", "C", "D", 
#                                                          "E", "F", "G", 
#                                                          "H", "I", "J", 
#                                                          "K", "L", "M", 
#                                                          "N", "O", "P", 
#                                                          "Q", "R", "S", 
#                                                          "T", "U", "V", 
#                                                          "W", "X", "Y",
#                                                          "Z", "P1", "SVG", 
#                                                          "TRD") ~ "SCANSIII_aerial", 
#                                             Block %in% c("8", "9", "11", 
#                                                          "12", "13") ~ "SCANSIII_ship", 
#                                             Block %in% c("S1_N", "S1_O", "S2_N", "S2_O", "S3_N", "S3_O", "S4",
#                                                          "S5", "S6", "S7", "S8") ~ "Observe",
#                                             Block %in% c("Atlantic", "Alboran", "SWMed", "NWMed", 
#                                                          "PelagosW", "PelagosE", 
#                                                          "Tyrrhenian", "SCMed", 
#                                                          "Adriatic", "Ionian", 
#                                                          "Aegean", "NEMed", "EMed") ~ "ASI", 
#                                             Block %in% c("FC", "FW",  
#                                                          "IE", "IC", "IG", "IP", 
#                                                          "IQ", "IR", "IW", 
#                                                          "SW", "CG", "X") ~ "t-NASS", 
#                                             Block == "GoMexico" ~ "goMexico mean 2017-2018", 
#                                             Block %in% c("GOM/BOF", "NWAtl_slope", "NWAtl_offshore") ~ "NWAtl2011",
#                                             Block %in% c("AUS_C", "AUS_P", "AUS_O", 
#                                                          "GAM_C", "GAM_P", "GAM_O",
#                                                          "SOC_C", "SOC_P", "SOC_O", 
#                                                          "TUN_P", "TUN_O", "TUS_P", "TUS_O",
#                                                          "MAR_C", "MAR_P", "MAR_O") ~ "REMMOA_Poly", 
#                                             Block %in% c("Hawai", "Hawai_Pelagic", "NWHI") ~ "Hawai2017", 
#                                             Block == "NCal" ~ "REMMOA_NCal", 
#                                             Block == "WFu" ~ "REMMOA_WFu", 
#                                             Block %in% c("Calif_current") ~ "California_curr2005-8", 
#                                             Block %in% c("CMGM_N", "CMGM_P", "CMGM_O",
#                                                          "JMN_N", "JMN_P", "JMN_O", 
#                                                          "EBM_P", "EBM_O", 
#                                                          "TM_N", "TM_P", "TM_O",
#                                                          "MAU_N", "MAU_P", "MAU_O",
#                                                          "REU_N", "REU_O",  
#                                                          "RM", "RM_N", "RM_P", "RM_O", # unstratified subarea of Mauritius and Reunion
#                                                          "SE_N", "SE_P", "SE_O") ~ "REMMOA_Indian", 
#                                             Block %in% c("ANT_P1", "ANT_P2", "ANT_P3", 
#                                                          "ANT_O1") ~ "REMMOA_ANT", 
#                                             Block %in% c("GUY_P5", "GUY_O5") ~ "REMMOA_GUY", 
#                                             Block %in% c("Inshore", "Offshore", "Seamount", 
#                                                          "Slope") ~ "GoAlaska"
#                   ), 
#                   Surf_tot = dplyr::case_when(Survey == "SCANSIII_aerial" ~ 1200575,
#                                               Survey == "SCANSIII_ship" ~  604393, 
#                                               Survey == "Observe" ~ 328596, # all Stratum except S5 that is in SCANS III
#                                               Survey == "ASI" ~ sum(Surf), 
#                                               Survey == "t-NASS" ~ 812602, 
#                                               Survey == "goMexico mean 2017-2018" ~ 380432, 
#                                               Survey == "NWAtl2011" ~ 451985, 
#                                               Survey == "REMMOA_Poly" ~ 7849 + 75131 + 186545 + # AUS
#                                                 4078 + 150930 + 185976 + # GAM
#                                                 19372 + 81650 + 174253 + # SOC
#                                                 148366 + 138368 + # TUN
#                                                 146201 + 151913 + # TUS
#                                                 24868 + 64731 + 243531, # MAR 
#                                               Survey == "Hawai2017" ~ 2447635, 
#                                               Survey == "REMMOA_NCal" ~ 542300,
#                                               Survey == "REMMOA_WFu" ~ 233600,
#                                               Survey == "California_curr2005-8" ~ 1141807,
#                                               Survey == "REMMOA_Indian" ~ 1417309,
#                                               Survey == "REMMOA_ANT" ~ 163010,
#                                               Survey == "REMMOA_GUY" ~ 110594,
#                                               Survey == "GoAlaska" ~ 22749 + 60051 + 45377 + 36776
#                   )) |> 
#     dplyr::group_by(Geo_area, Survey) |>
#     dplyr::summarise(Abund_tot = sum(Abund), 
#                      Dens_tot = Abund_tot/unique(Surf_tot),
#                      Var_tot = sum(Var_abund), 
#                      CV_tot = sqrt(Var_tot)/Abund_tot)
# }



# Build tibble relative to just one species
build_sp_tib <- function(original_tib, species, code_sp) {
  # species and code_sp are character strings 
  # species is the full latin name and code sp the two first letters
  
  # special case for Observe survey (Ireland)
  # there are stratum with mixed neritic and oceanic waters
  # we use the ratio of surfaces to separate abudances estimates between neritic and oceanic
  rS1_N <- 30177/62052
  rS1_O <- 31875/62052
  rS2_N <- 35464/60167
  rS2_O <- 24703/60167
  rS3_N <- 80009/100482
  rS3_O <- 20743/100482
  
  original_tib |> 
    dplyr::filter(!(Block %in% c("W", "X", "Y", "Z", "P1", "SVG", "TRD", # SCANS III
                                 "Atlantic", # MED
                                 "S5")), # Observe, stratum that is included in the block E of SCANS III
                  Abund != 0) |>
    dplyr::mutate(Abund = dplyr::case_when(Block == "S1_N" ~ round(rS1_N*Abund, 0), 
                                           Block == "S1_O" ~ round(rS1_O*Abund, 0),
                                           Block == "S2_N" ~ round(rS2_N*Abund, 0),
                                           Block == "S2_O" ~ round(rS2_O*Abund, 0),
                                           Block == "S3_N" ~ round(rS3_N*Abund, 0),
                                           Block == "S3_O" ~ round(rS3_O*Abund, 0),
                                           TRUE ~ Abund),
                  Var_abund = dplyr::case_when(Block == "S1_N" ~ rS1_N*(Abund*CV*Abund*CV), 
                                               Block == "S1_O" ~ rS1_O*(Abund*CV*Abund*CV),
                                               Block == "S2_N" ~ rS2_N*(Abund*CV*Abund*CV),
                                               Block == "S2_O" ~ rS2_O*(Abund*CV*Abund*CV),
                                               Block == "S3_N" ~ rS3_N*(Abund*CV*Abund*CV),
                                               Block == "S3_O" ~ rS3_O*(Abund*CV*Abund*CV),
                                               TRUE ~ Abund*CV*Abund*CV),
                  Eco_area = dplyr::case_when(Block %in% c("AA", "AB", "AC",
                                                           "B", "C","D", "E", 
                                                           "F","G", "H", "I",
                                                           "J", "K", "L", "M", 
                                                           "N", "O", "P", "Q", 
                                                           "R", "S", "T", "U", 
                                                           "V") ~ "shelf", # shelf NEAtlantic
                                              Block %in% c("8","9", "11", "12", "13") ~ "oceanic", # Oceanic NEAtlantic
                                              Block %in% c("NWMed", "SWMed","PelagosW", 
                                                           "PelagosE", "Tyrrhenian", "IonianC", "IonianE",
                                                           "NEMed", "EMed") ~ "oceanic", # oceanic Med
                                              Block %in% c("Alboran", "SCMed","Adriatic", 
                                                           "Aegean") ~ "shelf", # Shelf Med
                                              Block %in% c("S1_N", "S2_N", "S3_N",
                                                           "S4", "S6", "S7", "S8") ~ "shelf", # Shelf Observe
                                              Block %in% c("S1_O", "S2_O", "S3_O") ~ "oceanic", # Slope Observe
                                              Block %in% c("IR", "IE", "IC", "CG") ~ "shelf", # Shelf t-NASS
                                              Block %in% c("FC", "FW", "IG", "IP",
                                                           "IQ", "IW") ~ "oceanic", # Oceanic t-NASS
                                              Block == "GoMexico" ~ "oceanic", # goMexico mean 2017-2018 is full oceanic 
                                              Block == "GOM/BOF" ~"shelf", # shelf Atlantic Wcoast
                                              Block %in% c("NWAtl_slope", "NWAtl_offshore") ~ "oceanic", 
                                              Block %in% c("AUS_C", "AUS_P", "AUS_O", 
                                                           "GAM_C", "GAM_P", "GAM_O", 
                                                           "SOC_C", "SOC_P", "SOC_O", "TUN_P", "TUN_O", 
                                                           "TUS_P", "TUS_O", "MAR_C", "MAR_P", "MAR_O") ~ "oceanic", # French Polynesia: all slope/oceanic data
                                              Block %in% c("Hawai", "Hawai_Pelagic", "NWHI") ~ "oceanic", # Hawai2017 is very mainly oceanic 
                                              Block == "NCal" ~ "oceanic", # New Caledonia is all slope/oceanic
                                              Block == "WFu" ~ "oceanic", # Wallis & Futuna is all slope/oceanic
                                              Block %in% c("Calif_current") ~ "oceanic", # California current is mainly oceanic, although there is a significant shelf area it is not distinguished, # most coastal obs are in canyons
                                              Block %in% c("CMGM_N", "JMN_N", "TM_N", 
                                                           "MAU_N", "SE_N", "RM_N", "REU_N") ~ "shelf", # Indian ocean
                                              Block %in% c("CMGM_P", "CMGM_O",
                                                           "JMN_P", "JMN_O", 
                                                           "EBM_P", "EBM_O", 
                                                           "TM_P", "TM_O",
                                                           "MAU_P", "MAU_O",
                                                           "REU_O", 
                                                           "RM", "RM_P", "RM_O", # unstratified subarea of Mauritius and Reunion
                                                           "SE_P", "SE_O") ~ "oceanic", # Indian ocean
                                              Block %in% c("ANT_P1", "ANT_P2", "ANT_P3") ~ "shelf", # Antilles
                                              Block %in% c("ANT_O1") ~ "oceanic",
                                              Block %in% c("GUY_P5") ~ "shelf", # Guyana
                                              Block %in% c("GUY_O5") ~ "oceanic", 
                                              Block %in% c("Inshore") ~ "shelf", # GoAlaska
                                              Block %in% c("Offshore", "Seamount", 
                                                           "Slope") ~ "oceanic", # GoAlaska
                  ) 
    ) |> 
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::summarise(Abund_tot = sum(Abund), 
                     Var_tot = sum(Var_abund), 
                     CV_tot = sqrt(Var_tot)/Abund_tot) |>
    dplyr::mutate(Code_sp = code_sp, 
                  Species = species, 
                  Surf_tot = dplyr::case_when(Geo_area == "NEAtlantic" & Eco_area == "shelf" ~ 1084226 + # SCANS III
                                                63162 + 15766 + 17261 + 9707 + 30177 + 35464 + 80009, # Observe Ireland
                                              Geo_area == "NEAtlantic" & Eco_area == "oceanic" ~ 543235 + # SCANS III
                                                31875 + 24703 + 20743, # Observe Ireland
                                              Geo_area == "Med" & Eco_area == "shelf" ~ 507965,
                                              Geo_area == "Med" & Eco_area == "oceanic" ~ 1361063,
                                              Geo_area == "NAtlantic" & Eco_area == "shelf" ~ 743874,
                                              Geo_area == "NAtlantic" & Eco_area == "oceanic" ~ 2046837, 
                                              Geo_area == "GoMexico" & Eco_area == "oceanic" ~ 380432,
                                              Geo_area == "NWAtlantic" & Eco_area == "shelf" ~ 199656,
                                              Geo_area == "NWAtlantic" & Eco_area == "oceanic" ~ 252329, 
                                              Geo_area == "Pacific_FPoly" & Eco_area == "oceanic" ~ 7849 + 75131 + 186545 + # AUS_C + AUS_P + AUS_O
                                                4078 + 150930 + 185976 + # GAM_C + GAM_P + GAM_O
                                                19372 + 81650 + 174253 + # SOC_C + SOC_P + SOC_O
                                                148366 + 138368 + # TUN_P + TUN_O
                                                146201 + 151913 + # TUS_P + TUS_O
                                                24868 + 64731 + 243531, # MAR_C + MAR_P + MAR_O
                                              Geo_area == "Pacific_Hawai" & Eco_area == "oceanic" ~ 2447635, 
                                              Geo_area == "Pacific_NCal" & Eco_area == "oceanic" ~ 542300,
                                              Geo_area == "Pacific_WFu" & Eco_area == "oceanic" ~ 233600, 
                                              Geo_area == "Pacific_Calif_current" & Eco_area == "oceanic" ~ 1141807, 
                                              Geo_area == "Indian" & Eco_area == "shelf" ~ 94114,
                                              Geo_area == "Indian" & Eco_area == "oceanic" ~ 1307862, 
                                              Geo_area == "Antilles" & Eco_area == "shelf" ~ 17476 + 36006 + 52687,
                                              Geo_area == "Antilles" & Eco_area == "oceanic" ~ 56841,
                                              Geo_area == "Guyana" & Eco_area == "shelf" ~ 61465,
                                              Geo_area == "Guyana" & Eco_area == "oceanic" ~ 49129,
                                              Geo_area == "GoAlaska" & Eco_area == "shelf" ~ 22749,
                                              Geo_area == "GoAlaska" & Eco_area == "oceanic" ~ 60051 + 45377 + 36776
                  )) |>
    dplyr::select(Code_sp, Species, Geo_area, Eco_area, Surf_tot, Abund_tot, CV_tot) |>
    dplyr::rename(Abund = Abund_tot, 
                  Abund_CV = CV_tot)
}



#################################### ABUNDANCE DATA ###########################################
############################## build original tibbles for each species ########################

## Balaenoptera acutorostrata - Minke whale
data_from_data_Bala_acu <- function() {
  
  original_df_Bala_acu <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ####################################### NEAtlantic - SCANS III 
                                          "NEAtlantic", "AC", 35180, 164, 0.0047, 1.137,
                                          "NEAtlantic", "B", 118471, 289, 0.0024, 0.837,
                                          "NEAtlantic", "C", 81297, 186, 0.0023, 1.119,
                                          "NEAtlantic", "D", 48590, 543, 0.0112, 0.755,
                                          "NEAtlantic", "E", 34870, 603, 0.0173, 0.618,
                                          "NEAtlantic", "G", 15122, 410, 0.0271, 0.700,
                                          "NEAtlantic", "H", 18634, 149, 0.0080, 1.072, 
                                          "NEAtlantic", "I", 13979, 285, 0.0204, 0.790,
                                          "NEAtlantic", "J", 35099, 647, 0.0184, 1.040, 
                                          "NEAtlantic", "K", 32505, 295, 0.0091, 0.805, 
                                          "NEAtlantic", "N", 69386, 1392, 0.0201, 0.504, 
                                          "NEAtlantic", "O", 60198, 603, 0.0100, 0.621, 
                                          "NEAtlantic", "P", 63655, 610, 0.0096, 0.657,
                                          "NEAtlantic", "Q", 49746, 348, 0.0070, 0.761,
                                          "NEAtlantic", "R", 64464, 2498, 0.0387, 0.614, 
                                          "NEAtlantic", "S", 40383, 383, 0.0095, 0.749, 
                                          "NEAtlantic", "T", 65417, 2068, 0.0316, 0.805, 
                                          "NEAtlantic", "U", 60046, 895, 0.0149, 0.848,
                                          "NEAtlantic", "V", 38306, 440, 0.0115, 1.144,
                                          "NEAtlantic", "X", 19496, 122, 0.0062, 1.092, 
                                          "NEAtlantic", "Y", 18779, 171, 0.0091, 1.101,
                                          "NEAtlantic", "8", 159669, 1657, 0.0104, 0.549, 
                                          ####################################### NEAtlantic - Observe (Season 3)
                                          "NEAtlantic", "S1_N", 62052, 751, 0.012, 0.705,
                                          "NEAtlantic", "S1_O", 62052, 751, 0.012, 0.705,
                                          "NEAtlantic", "S3_N", 100482, 930, 0.009, 0.6403,
                                          "NEAtlantic", "S3_O", 100482, 930, 0.009, 0.6403,
                                          "NEAtlantic", "S4", 63162, 760, 0.012, 0.6332,
                                          "NEAtlantic", "S5", 11010, 180, 0.016, 1.0613,
                                          "NEAtlantic", "S7", 17261, 1714, 0.102, 0.6915,
                                          "NEAtlantic", "S8", 9707, 2242, 0.236, 0.6614, 
                                          ######################################### t-NASS
                                          "NAtlantic", "CG", 46347, 2726, 0.0588, 0.52,
                                          "NAtlantic", "FC", 84816, 12926, 0.152, 0.64, 
                                          "NAtlantic", "FW", 170629, 5072, 0.0297, 0.43,
                                          "NAtlantic", "IC", 85700, 12710, 0.148, 0.53,
                                          "NAtlantic", "IE", 71325, 5655, 0.0793, 0.73,
                                          "NAtlantic", "IR", 52594, 1207, 0.023, 0.82,
                                          "NAtlantic", "IW", 92929, 2218, 0.0239, 0.53,
                                          ######################################### NWAtl2011
                                          "NWAtlantic", "GOM/BOF", 199656, 2538, NA, 0.87,
                                          "NWAtlantic", "NWAtl_slope", 54376, 53, NA, 0.73, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 438, 0.00018, 1.05, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 478, NA, 1.36 
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Bala_acu)
  
  original_df_Bala_acu
  
}


## Balaenoptera borealis - Sei whale
data_from_data_Bala_bor <- function() {
  original_df_Bala_bor <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### t-NASS
                                          "NAtlantic", "FW", 176905, 453, 0.00212, 0.72,
                                          "NAtlantic", "IG", 93953, 438, 0.00387, 0.85,
                                          "NAtlantic", "IP", 139248, 2601, 0.0155, 0.72,
                                          "NAtlantic", "IW", 37905, 275, 0.00603, 0.74,
                                          ######################################### NWAtl2011
                                          "NWAtlantic", "GOM/BOF", 199656, 145, NA, 1,
                                          "NWAtlantic", "NWAtl_slope", 54376, 212, NA, 0.54, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 126, NA, 0.53 
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Bala_bor)
  
  original_df_Bala_bor
}


## Balaenoptera edeni - Bryde's whale
data_from_data_Bala_ede <- function() {
  original_df_Bala_ede <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################## GoMexico 2018
                                          "GoMexico", "GoMexico", 380432, 51, NA, 0.503, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 139, 0.00006, 0.72, # Bryde's sure
                                          "Pacific_Hawai", "Hawai", 2447635, 157, 0.00006, 0.71 # Bryde's or Sei but only Bryde's confirmed observation
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Bala_ede)
  
  original_df_Bala_ede
}


## Balaenoptera musculus - Blue whale
data_from_data_Bala_mus <- function() {
  original_df_Bala_mus <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### t-NASS
                                          "NAtlantic", "FW", 176905, 273, 0.00128, 0.95,
                                          "NAtlantic", "IG", 93953, 656, 0.00579, 0.56,
                                          "NAtlantic", "IP", 139248, 271, 0.00161, 0.96,
                                          "NAtlantic", "IR", 108550, 1535, 0.0117, 0.58,
                                          "NAtlantic", "IW", 37905, 266, 0.00581, 0.51, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 565, NA, 0.18, 
                                          ########################################## Gulf of Alaska
                                          "GoAlaska", "Seamount", 45377, 63, 0.0014, 0.76
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Bala_mus)
  
  original_df_Bala_mus
}


## Balaenoptera physalus  - Fin whale
data_from_data_Bala_phy <- function() {
  original_df_Bala_phy <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################################## NEAtlantic
                                          "NEAtlantic", "8", 159669, 820, 0.0051, 0.493, 
                                          "NEAtlantic", "9", 144352, 10600, 0.0734, 0.290, 
                                          "NEAtlantic", "11", 68759, 2052, 0.0298, 0.220, 
                                          "NEAtlantic", "12", 111115, 10245, 0.0922, 0.212, 
                                          "NEAtlantic", "13", 59340, 3575, 0.0602, 0.215, 
                                          ####################################### NEAtlantic - Observe (Season 3)
                                          "NEAtlantic", "S2_N", 60167, 48, 0.001, 0.9951,
                                          "NEAtlantic", "S2_O", 60167, 48, 0.001, 0.9951,
                                          "NEAtlantic", "S3_N", 100482, 47, 0.000, 1.0074,
                                          "NEAtlantic", "S3_O", 100482, 47, 0.000, 1.0074,
                                          ############################################## MED
                                          "Med", "Atlantic", 33720, 175, 0.0052, 0.7632,
                                          "Med", "Alboran", 28071, 55, 0.0020, 1.0729,
                                          "Med", "SWMed", 279415, 163, 0.0007, 0.6419,
                                          "Med", "NWMed", 134760, 911, 0.0078, 0.3442,
                                          "Med", "PelagosW", 56756, 254, 0.0045, 0.3948,
                                          "Med", "PelagosE", 31076, 0, 0, 0,
                                          "Med", "Tyrrhenian", 231298, 181, 0.0008, 0.8727,
                                          "Med","SCMed", 152961, 10, 0.0001, 1.0079,
                                          "Med", "Adriatic", 135783, 0, 0, 0,
                                          "Med", "IonianC", 185926, 0, 0, 0,
                                          "Med", "IonianE", 172477, 0, 0, 0,
                                          "Med", "Aegean", 191150, 0, 0, 0,
                                          "Med", "NEMed", 161669, 0, 0, 0,
                                          "Med", "EMed", 107687, 0, 0, 0,
                                          ######################################### t-NASS
                                          "NAtlantic", "FC", 77857, 5014, 0.056, 0.75, 
                                          "NAtlantic", "FW", 176905, 9643, 0.0474, 0.39,
                                          "NAtlantic", "IE", 108052, 394, 0.00317, 0.72,
                                          "NAtlantic", "IG", 93953, 8887, 0.0822, 0.16,
                                          "NAtlantic", "IP", 139248, 4138, 0.0258, 0.54,
                                          "NAtlantic", "IQ", 70131, 1324, 0.0164, 0.39,
                                          "NAtlantic", "IR", 108550, 3298, 0.0264, 0.35,
                                          "NAtlantic", "IW", 37905, 4075, 0.0934, 0.19,
                                          ######################################### NWAtl2011
                                          "NWAtlantic", "GOM/BOF", 199656, 417, NA, 0.56,
                                          "NWAtlantic", "NWAtl_slope", 54376, 1178, NA, 0.4, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 203, 0.00008, 0.99, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 3044, NA, 0.18, 
                                          ########################################## Gulf of Alaska
                                          "GoAlaska", "Inshore", 22749, 1556, 0.068, 0.48,
                                          "GoAlaska", "Offshore", 60051, 971, 0.016, 0.23,
                                          "GoAlaska", "Seamount", 45377, 145, 0.003, 0.37, 
                                          "GoAlaska", "Slope", 36776, 496, 0.013, 0.2 
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Bala_phy)
  
  original_df_Bala_phy
  
}


## Feresa attenuata - pygmy killer whales 
data_from_data_Fere_att <- function() {
  original_df_Fere_att <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 613, NA, 1.15,
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 10328, 0.00422, 0.75
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Fere_att)
  
  original_df_Fere_att
  
}


## Globicephala macrorhynchus - short-finned pilot whales 
data_from_data_Glob_mac <- function() {
  original_df_Glob_mac <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ######################################### goMexico mean 2017-2018 - Glob_mac
                                          "GoMexico", "GoMexico", 380432, 1321, 0.0038, 0.43, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 7956, 0.00325, 0.59, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 760, NA, 0.64, 
                                          # ########################################## Japan
                                          # "Japan", "B_J", 44095*3.4342900120544, 1525, NA, 0.79, # (18891 + 44095 + 182656 + 213731 + 120949)*3.4342900120544
                                          # "Japan", "C_J", 182656*3.4342900120544, 6839, NA, 1.29,
                                          # "Japan", "D_J", 213731*3.4342900120544, 11243, NA, 0.55,
                                          # "Japan", "E_J", 120949*3.4342900120544, 11788, NA, 1.34
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Glob_mac)
  
  original_df_Glob_mac
  
}


## Globicephala melas - Long finned pilot whales
data_from_data_Glob_mel <- function() {
  original_df_Glob_mel <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ################################################## NEAtlantic SCANS III
                                          "NEAtlantic", "AA", 12015, 77, 0.0064, 1.093,
                                          "NEAtlantic", "AC", 35180, 1942, 0.0552, 1.224,
                                          "NEAtlantic", "B", 118471, 1365, 0.0115, 0.574,
                                          "NEAtlantic", "J", 35099, 87, 0.0025, 1.182,
                                          "NEAtlantic", "K", 32505, 1745, 0.0537, 1.059,
                                          "NEAtlantic", "8", 159669, 12662, 0.0793, 0.484, 
                                          "NEAtlantic", "9", 144352, 3125, 0.0216, 0.770, 
                                          "NEAtlantic", "11", 68759, 173, 0.0025, 1.111, 
                                          "NEAtlantic", "12", 111115, 3196, 0.0288, 0.718, 
                                          "NEAtlantic", "13", 59340, 4377, 0.0738, 0.638, 
                                          ####################################### NEAtlantic - Observe (Season 3)
                                          "NEAtlantic", "S1_N", 62052, 4334, 0.07, 0.5686,
                                          "NEAtlantic", "S1_O", 62052, 4334, 0.07, 0.5686,
                                          "NEAtlantic", "S2_N", 60167, 1617, 0.027, 0.7146,
                                          "NEAtlantic", "S2_O", 60167, 1617, 0.027, 0.7146,
                                          "NEAtlantic", "S3_N", 100482, 1462, 0.015, 0.6508,
                                          "NEAtlantic", "S3_O", 100482, 1462, 0.015, 0.6508,
                                          ############################################## MED 
                                          "Med", "Atlantic", 33720, 439, 0.0130, 0.7542,
                                          "Med", "Alboran", 28071, 198, 0.0071, 1.0132, 
                                          "Med", "SWMed", 279415, 2510, 0.0090, 0.6264,
                                          "Med", "NWMed", 134760, 1900, 0.0141, 0.8613,
                                          "Med", "PelagosW", 56756, 226, 0.0040, 1.0101,
                                          "Med", "PelagosE", 31076, 267, 0.0086, 1.0031,
                                          "Med", "Tyrrhenian", 231298, 0, 0, 0,
                                          "Med","SCMed", 152961, 0, 0, 0,
                                          "Med", "Adriatic", 135783, 0, 0, 0,
                                          "Med", "IonianC", 185926, 0, 0, 0,
                                          "Med", "IonianE", 172477, 0, 0, 0,
                                          "Med", "Aegean", 191150, 0, 0, 0,
                                          "Med", "NEMed", 161669, 0, 0, 0,
                                          "Med", "EMed", 107687, 0, 0, 0,
                                          ######################################### t-NASS 
                                          "NAtlantic", "FC", 77857, 24427, 0.233, 0.55, 
                                          "NAtlantic", "FW", 176905, 26177, 0.111, 0.56,
                                          "NAtlantic", "IE", 108052, 42528, 0.311, 0.6,
                                          "NAtlantic", "IG", 93953, 21327, 0.177, 0.46,
                                          "NAtlantic", "IP", 139248, 116543, 0.695, 0.84,
                                          "NAtlantic", "IR", 108550, 60428, 0.443, 0.42,
                                          "NAtlantic", "IW", 37905, 52718, 1.2, 0.41,
                                          ######################################### NWAtl2011 - Globispp --> Glob_mel
                                          "NWAtlantic", "NWAtl_slope", 54376, 9483, NA, 0.65, 
                                          "NWAtlantic", "NWAtl_offshore", 197953, 2382, NA, 0.59,
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Glob_mel)
  
  original_df_Glob_mel
  
}


## Grampus griseus - Risso's dolphin 
data_from_data_Gram_gri <- function() {
  original_df_Gram_gri <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################################## NEAtlantic - SCANS III 
                                          "NEAtlantic", "AA", 12015, 575, 0.0478, 1.033,
                                          "NEAtlantic", "AB", 26668, 640, 0.0240, 0.617,
                                          "NEAtlantic", "AC", 35180, 237, 0.0067, 1.034,
                                          "NEAtlantic", "B", 118471, 799, 0.0067, 0.982,
                                          "NEAtlantic", "E", 34870, 1090, 0.0313, 0.686, 
                                          "NEAtlantic", "H", 18634, 538, 0.0289, 0.954,  
                                          "NEAtlantic", "J", 35099, 6750, 0.1923, 0.802, 
                                          "NEAtlantic", "K", 32505, 440, 0.0135, 0.763,
                                          "NEAtlantic", "9", 144352, 2515, 0.0174, 0.818, 
                                          ####################################### NEAtlantic - Observe (Season 3 - S5 excluded as in SCANS III)
                                          "NEAtlantic", "S1_N", 62052, 746, 0.012, 0.7405,
                                          "NEAtlantic", "S1_O", 62052, 746, 0.012, 0.7405,
                                          "NEAtlantic", "S2_N", 60167, 100, 0.0017, 1.0191,
                                          "NEAtlantic", "S2_O", 60167, 100, 0.0017, 1.0191,
                                          "NEAtlantic", "S4", 63162, 809, 0.0128, 0.9475,
                                          "NEAtlantic", "S6", 15766, 426, 0.027, 0.7485,
                                          "NEAtlantic", "S8", 9707, 549, 0.0565, 0.5091, 
                                          ############################################## MED
                                          "Med", "Atlantic", 33720, 942, 0.0279, 0.9888,
                                          "Med", "Alboran", 28071, 595, 0.0212, 0.7655,  
                                          "Med", "SWMed", 279415, 15682, 0.0561, 0.4141,
                                          "Med", "NWMed", 134760, 2058, 0.0153, 0.5524,
                                          "Med", "PelagosW", 56756, 223, 0.0039, 0.8565, 
                                          "Med", "PelagosE", 31076, 0, 0, 0, 
                                          "Med", "Tyrrhenian", 231298, 0, 0, 0,
                                          "Med","SCMed", 152961, 0, 0, 0, 
                                          "Med", "Adriatic", 135783, 1467, 0.0108, 0.7054, 
                                          "Med", "IonianC", 185926, 1617, 0.0087, 0.5902,
                                          "Med", "IonianE", 172477, 2323, 0.0135, 0.9480, 
                                          "Med", "Aegean", 191150, 1101, 0.0058, 0.5946, 
                                          "Med", "NEMed", 161669, 0, 0, 0,
                                          "Med", "EMed", 107687, 0, 0, 0, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 1973, NA, 0.456,
                                          ######################################### NWAtl2011
                                          "NWAtlantic", "NWAtl_slope", 54376, 4521, NA, 0.48, 
                                          "NWAtlantic", "NWAtl_offshore", 197953, 10676, NA, 0.72, 
                                          ######################################### French Poly 
                                          "Pacific_FPoly", "GAM_P", 150930, 429, 0.00285, 1.03,
                                          "Pacific_FPoly", "GAM_O", 185976, 863, 0.00464, 0.74,
                                          "Pacific_FPoly", "SOC_P", 81650, 230, 0.00282, 1.02,  
                                          "Pacific_FPoly", "SOC_O", 174253, 1304, 0.00748, 0.56, 
                                          "Pacific_FPoly", "TUN_O", 138368, 1296, 0.00936, 0.56,
                                          "Pacific_FPoly", "TUS_P", 146201, 1127, 0.00771, 0.62, 
                                          "Pacific_FPoly", "TUS_O", 151913, 1035, 0.00682, 0.63, 
                                          "Pacific_FPoly", "MAR_C", 24868, 365, 0.01468, 0.75, 
                                          "Pacific_FPoly", "MAR_P", 64731, 252, 0.00389, 1.02, 
                                          "Pacific_FPoly", "MAR_O", 243531, 1178, 0.00484, 0.63, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 6245, 0.00255, 0.5, 
                                          ######################################### New Caledonia 
                                          "Pacific_NCal", "NCal", 542300, 7005, 0.004, 0.51,  
                                          ######################################### Wallis & Futuna (N and not Ncorrigee)
                                          "Pacific_WFu", "WFu", 233600, 1188, 0.005, 0.5, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 6272, NA, 0.3, 
                                          ######################################### Indian
                                          "Indian", "CMGM_P", 87560, 112, 0.00608, 1.02,
                                          "Indian", "CMGM_O", 171931, 186, 0.00218, 1.02, 
                                          "Indian", "JMN_P", 27420, 566, 2.063, 0.49,
                                          "Indian", "JMN_O", 74032, 2541, 0.03432, 0.39,
                                          "Indian", "EBM_P", 36127, 1042, 0.03097, 0.44,
                                          "Indian", "EBM_O", 119099, 2099, 0.01762, 0.44,
                                          "Indian", "RM_P", 74632, 252, 0.00270, 1.43, 
                                          "Indian", "RM_O", 306861, 302, 0.00098, 1.02, 
                                          "Indian", "SE_P", 97713, 7644, 0.07822, 0.53,
                                          "Indian", "SE_O", 167277, 2372, 0.01418, 0.82
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Gram_gri)
  
  original_df_Gram_gri
  
}


## Kogia species (K. sima and K. breviceps) - Dwarf and Pygmy sperm whale
data_from_data_Kogi_spp <- function() { 
  original_df_Kogi_spp <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 336, NA, 0.346,
                                          ######################################### NWAtl2011 # For dwarf
                                          "NWAtlantic", "NWAtl_offshore", 197953, 1042, NA, 0.65,
                                          ######################################### NWAtl2011 # for pygmy
                                          "NWAtlantic", "NWAtl_slope", 54376, 63, NA, 0.61, 
                                          "NWAtlantic", "NWAtl_offshore", 197953, 678, NA, 0.43, 
                                          ######################################### French Poly 
                                          "Pacific_FPoly", "SOC_C", 19372, 41, 0.002, 1.02, 
                                          "Pacific_FPoly", "TUN_P", 148366, 119, 0.001, 0.74, 
                                          "Pacific_FPoly", "TUN_O", 138368, 129, 0.001, 0.74,
                                          "Pacific_FPoly", "MAR_C", 24868, 73, 0.003, 0.75, 
                                          "Pacific_FPoly", "MAR_P", 64731, 201, 0.003, 0.55, 
                                          "Pacific_FPoly", "MAR_O", 243531, 863, 0.004, 0.4, 
                                          ######################################### Hawai2017 
                                          "Pacific_Hawai", "Hawai", 2447635, 42083, 0.01719, 0.64, # for pygmy
                                          "Pacific_Hawai", "Hawai", 2447635, 53421, 0.02183, 0.63, # und. Kogia
                                          ######################################### New Caledonia 
                                          "Pacific_NCal", "NCal", 542300, 1017, 0.002, 0.43,  
                                          ######################################### Wallis & Futuna 
                                          "Pacific_WFu", "WFu", 233600, 297, 0.001, 0.49, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 579, NA, 1.12, 
                                          ######################################### Indian # for kogia there is only one est per subarea but all obs were either in slope or oceanic area, so we put everything in O
                                          "Indian", "CMGM_O", 259491, 197, 0.00072, 0.64,
                                          "Indian", "JMN_O", 101452, 78, 0.00063, 0.84,
                                          "Indian", "EBM_O", 155226, 109, 0.00072, 1.1,
                                          "Indian", "TM_O", 145210, 36, 0.00023, 1.05, 
                                          "Indian", "RM_O", 381493, 275, 0.00068, 82.61,
                                          "Indian", "SE_O", 264990, 723, 0.00246, 0.43,
                                          ######################################### Antilles-Guyana
                                          "Antilles", "ANT_P1", 17476, 38, 0.002, 1,
                                          "Antilles", "ANT_P2", 36006, 252, 0.007, 0.58,
                                          "Antilles", "ANT_P3", 52687, 37, 0.001, 1.05,
                                          "Antilles", "ANT_O1", 56841, 286, 0.005, 0.59,
                                          "Guyana", "GUY_P5", 61465, 43, 0.001, 0.72,
                                          "Guyana", "GUY_O5", 49129, 275, 0.006, 0.64
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Kogi_spp)
  
  original_df_Kogi_spp
  
}


## Lagenorhynchus acutus - Atlantic white-sided dolphin
data_from_data_Lage_acu <- function() { 
  original_df_Lage_acu <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################## NEAtlantic  - SCANS III 
                                          "NEAtlantic", "R", 64464, 644, 0.0100, 0.994, 
                                          "NEAtlantic", "T", 65417, 1366, 0.0209, 0.984, 
                                          "NEAtlantic", "U", 60046, 177, 0.0029, 0.989,
                                          "NEAtlantic", "8", 159669, 13322, 0.0834, 0.826,
                                          ####################################### NEAtlantic - Observe (Season 1&3 - S5 excluded as in SCANS III)
                                          # only model based estimates were available 
                                          "NEAtlantic", "S1_N", 62052, 1505, 0.025, 0.531,
                                          "NEAtlantic", "S1_O", 62052, 1505, 0.025, 0.531,
                                          "NEAtlantic", "S2_N", 60167, 60, 0.001, 1.001,
                                          "NEAtlantic", "S2_O", 60167, 60, 0.001, 1.001,
                                          "NEAtlantic", "S3_N", 100482, 357, 0.003, 0.996,
                                          "NEAtlantic", "S3_O", 100482, 357, 0.003, 0.996,
                                          ############################ t-NASS
                                          "NAtlantic", "FC", 77857, 5463, 0.0215, 0.68, 
                                          "NAtlantic", "FW", 176905, 86053, 0.149, 0.83,
                                          "NAtlantic", "IG", 93953, 6267, 0.0204, 0.88,
                                          "NAtlantic", "IP", 139248, 33239, 0.0732, 1.07,
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "GOM/BOF", 199656, 48819, NA, 0.61
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Lage_acu)
  
  original_df_Lage_acu
  
}

## Lagenorhynchus albirostris - White-beaked dolphin
data_from_data_Lage_alb <- function() { 
  original_df_Lage_alb<- tibble::tribble(~ Geo_area, ~ Block, ~ Surf,  ~ Abund, ~Dens, ~CV, 
                                         ################################## NEAtlantic  - SCANS III 
                                         "NEAtlantic", "H", 18634, 5881, 0.316, 0.627,
                                         "NEAtlantic", "J", 35099, 1871, 0.053, 0.909,
                                         "NEAtlantic", "K", 32505, 7055, 0.217, 0.529,
                                         "NEAtlantic", "O", 60198, 143, 0.002, 0.970,
                                         "NEAtlantic", "P", 63655, 1938, 0.030, 0.385, 
                                         "NEAtlantic", "P1", 23557, 72, 0.003, 0.877,  
                                         "NEAtlantic", "R", 64464, 15694, 0.243, 0.484, 
                                         "NEAtlantic", "S", 40383, 868, 0.021, 0.690,
                                         "NEAtlantic", "T", 65417, 2417, 0.037, 0.463,
                                         "NEAtlantic", "V", 38306, 261, 0.007, 0.983,
                                         "NEAtlantic", "X", 19496, 88, 0.005, 0.923, 
                                         ####################################### NEAtlantic - Observe (Season 3 - S5 excluded as in SCANS III)
                                         # only model based estimates were available 
                                         "NEAtlantic", "S1_N", 62052, 893, 0.015, 0.691,
                                         "NEAtlantic", "S1_O", 62052, 893, 0.015, 0.691,
                                         "NEAtlantic", "S2_N", 60167, 359, 0.002, 0.985,
                                         "NEAtlantic", "S2_O", 60167, 359, 0.002, 0.985,
                                         "NEAtlantic", "S7", 17261, 1996, 0.118, 0.646,
                                         ############################ t-NASS
                                         "NAtlantic", "IE", 108052, 13046, 0.037, 1.26,
                                         "NAtlantic", "IG", 93953, 15216, 0.0497, 0.96,
                                         "NAtlantic", "IP", 139248, 27721, 0.061, 1.11,
                                         "NAtlantic", "IQ", 70131, 18317, 0.081, 0.8,
                                         "NAtlantic", "IR", 108550, 72102, 0.204, 0.68,
                                         "NAtlantic", "IW", 37905, 12599, 0.102, 0.9)
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Lage_alb)
  # there is a difference of CV for t-NASS
  
  original_df_Lage_alb
  
}

## Lagenodelphis hosei - Fraser's dolphin
data_from_data_Lage_hos <- function() { 
  original_df_Lage_hos <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 213, NA, 1.028, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 40960, 0.01673, 0.7
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Lage_hos)
  
  original_df_Lage_hos
  
}


## Lagenorhynchus obliquidens - Pacific white-sided dolphin
data_from_data_Lage_obl <- function() { 
  original_df_Lage_obl <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 26930, NA, 0.28
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Lage_obl)
  
  original_df_Lage_obl
  
}


## Lissodelphis borealis - Northern right whale dolphin
data_from_data_Liss_bor <- function() { 
  original_df_Liss_bor <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 8334, NA, 0.4
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Liss_bor)
  
  original_df_Liss_bor
  
}


## Megaptera novaeangliae - Humpback whale
data_from_data_Mega_nov <- function() { 
  original_df_Mega_nov <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ######################################### t-NASS
                                          "NAtlantic", "FC", 77857, 571, 0.00487, 0.72, 
                                          "NAtlantic", "FW", 176905, 1059, 0.00435, 0.42,
                                          "NAtlantic", "IE", 108052, 966, 0.00456, 0.52,
                                          "NAtlantic", "IG", 93953, 1111, 0.00913, 0.53,
                                          "NAtlantic", "IR", 108550, 6051, 0.0375, 0.5,
                                          "NAtlantic", "IW", 37905, 110, 0.00185, 0.78,
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "GOM/BOF", 199656, 129, NA, 0.41,
                                          "NWAtlantic", "NWAtl_slope", 54376, 206, NA, 0.55, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 1389, NA, 0.21,
                                          ########################################## Gulf of Alaska
                                          "GoAlaska", "Inshore", 22749, 2107, 0.093, 0.74,
                                          "GoAlaska", "Offshore", 60051, 65, 0.001, 0.85,
                                          "GoAlaska", "Seamount", 45377, 37, 0.001, 0.59, 
                                          "GoAlaska", "Slope", 36776, 6, 0.0000, 1.01
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Mega_nov)
  
  original_df_Mega_nov
  
}


## Orcinus orca - Killer whale 
data_from_data_Orci_orc <- function() { 
  original_df_Orci_orc <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 267, 0.00122, 0.749, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 161, 0.00007, 1.06, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 691, NA, 0.49,
                                          ########################################## Gulf of Alaska
                                          "GoAlaska", "Inshore", 22749, 107, 0.005, 0.5,
                                          "GoAlaska", "Seamount", 45377, 107, 0.002, 0.77, 
                                          "GoAlaska", "Slope", 36776, 685, 0.019, 0.92
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Orci_orc)
  
  original_df_Orci_orc
  
}

## Peponocephala electra - melon-headed whale 
data_from_data_Pepo_ele <- function() { 
  original_df_Pepo_ele <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 1749, NA, 0.683, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 40647, 0.01661, 0.74, 
                                          ######################################### New Caledonia : small globi -> 100% Pepo_elec
                                          "Pacific_NCal", "NCal", 542300, 10741, 0.014, 0.79,  
                                          ######################################### Wallis & Futuna : small globi -> 100% Pepo_elec
                                          "Pacific_WFu", "WFu", 233600, 10555, 0.04, 0.65,
                                          ######################################### Indian
                                          "Indian", "CMGM_N", 24237, 2335, 0.1273, 0.73,
                                          "Indian", "CMGM_P", 87560, 43103, 0.50494, 0.42,
                                          "Indian", "CMGM_O", 171931, 52161, 0.30338, 0.51,
                                          "Indian", "JMN_P", 27420, 14312, 0.52208, 0.67,
                                          "Indian", "JMN_O", 74032, 10047, 0.13572, 0.62,
                                          "Indian", "EBM_P", 36127, 4090, 0.1215, 1.01,
                                          "Indian", "EBM_O", 119099, 5490, 0.04610, 1.01,
                                          "Indian", "TM_P", 20992, 5637, 0.27391, 0.55, 
                                          "Indian", "TM_O", 124218, 8694, 0.06999, 0.77, 
                                          "Indian", "RM_P", 74632, 2228, 0.02385, 1.00, 
                                          "Indian", "RM_O", 306861, 3303, 0.01076, 1.02, 
                                          "Indian", "SE_P", 97713, 6031, 0.06172, 1.00,
                                          "Indian", "SE_O", 167277, 10294, 0.06154, 0.76
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Pepo_ele)
  
  original_df_Pepo_ele
  
}


## Phocoenoides dalli - Dall's porpoise
data_from_data_Phoc_dal <- function() {
  original_df_Phoc_dal <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 42000, NA, 0.33,
                                          ########################################## Gulf of Alaska
                                          "GoAlaska", "Inshore", 22749, 4961, 0.2180, 0.39,
                                          "GoAlaska", "Offshore", 60051, 2192, 0.037, 0.42,
                                          "GoAlaska", "Seamount", 45377, 1076, 0.0240, 0.45, 
                                          "GoAlaska", "Slope", 36776, 7194, 0.1960, 0.48
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Phoc_dal)
  
  original_df_Phoc_dal
  
}


## Phocoena phocoena - Harbour porpoise
data_from_data_Phoc_pho <- function() { 
  original_df_Phoc_pho <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ############################################ NEAtlantic
                                          "NEAtlantic", "AA", 12015, 0, 0, 0,
                                          "NEAtlantic", "AB", 26668, 2715, 0.102, 0.308,
                                          "NEAtlantic", "AC", 35180, 183, 0.005, 1.020,
                                          "NEAtlantic", "B", 118471, 3374, 0.028, 0.586,
                                          "NEAtlantic", "C", 81297, 17323, 0.213, 0.303,
                                          "NEAtlantic", "D", 48590, 5734, 0.118, 0.489,
                                          "NEAtlantic", "E", 34870, 8320, 0.239, 0.282, 
                                          "NEAtlantic", "F", 12322, 1056, 0.086, 0.383,
                                          "NEAtlantic", "G", 15122, 5087, 0.336, 0.428, 
                                          "NEAtlantic", "H", 18634, 1682, 0.09, 0.741, 
                                          "NEAtlantic", "I", 13979, 5556, 0.397, 0.347, 
                                          "NEAtlantic", "J", 35099, 2045, 0.058, 0.716, 
                                          "NEAtlantic", "K", 32505, 9999, 0.308, 0.273,
                                          "NEAtlantic", "L", 31404, 19064, 0.607, 0.383,
                                          "NEAtlantic", "M", 56469, 15655, 0.277, 0.342, 
                                          "NEAtlantic", "N", 69386, 58066, 0.837, 0.257,
                                          "NEAtlantic", "O", 60198, 53485, 0.888, 0.209,
                                          "NEAtlantic", "P", 63655, 52406, 0.823, 0.315, 
                                          "NEAtlantic", "P1", 23557, 25367, 1.077, 0.302, 
                                          "NEAtlantic", "Q", 49746, 16569, 0.333, 0.347, 
                                          "NEAtlantic", "R", 64464, 38646, 0.599, 0.287, 
                                          "NEAtlantic", "S", 40383, 6147, 0.152, 0.279, 
                                          "NEAtlantic", "T", 65417, 26309, 0.402, 0.295, 
                                          "NEAtlantic", "U", 60046, 19269, 0.321, 0.298,
                                          "NEAtlantic", "V", 38306, 5240, 0.137, 0.367, 
                                          "NEAtlantic", "W", 49778, 8978, 0.180, 0.568,
                                          "NEAtlantic", "X", 19496, 6713, 0.344, 0.305, 
                                          "NEAtlantic", "Y", 18779, 4006, 0.213, 0.400,
                                          "NEAtlantic", "Z", 11228, 4556, 0.406, 0.275,
                                          "NEAtlantic", "SVG", 714, 423, 0.593, 0.386, 
                                          "NEAtlantic", "TRD", 966, 273, 0.282, 0.476,
                                          ####################################### NEAtlantic - Observe (Season 3)
                                          "NEAtlantic", "S1_N", 62052, 3219, 0.053, 0.558,
                                          "NEAtlantic", "S1_O", 62052, 3219, 0.053, 0.558,
                                          "NEAtlantic", "S3_N", 100482, 3320, 0.032, 0.525,
                                          "NEAtlantic", "S3_O", 100482, 3320, 0.032, 0.525,
                                          "NEAtlantic", "S4", 63162, 14196, 0.227, 0.372,
                                          "NEAtlantic", "S5", 11010, 11624, 0.295, 0.282,
                                          "NEAtlantic", "S6", 15766, 3300, 0.212, 0.357,
                                          "NEAtlantic", "S7", 17261, 624, 0.037, 0.789,
                                          "NEAtlantic", "S8", 9707, 1977, 0.208, 0.626, 
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "GOM/BOF", 199656, 79883, NA, 0.32
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Phoc_pho)
  
  original_df_Phoc_pho
  
}


## Physeter macrocephalus - Sperm whale
data_from_data_Phys_mac <- function() { 
  original_df_Phys_mac <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################################## NEAtlantic
                                          "NEAtlantic", "8", 159669, 9599, 0.0601, 0.471, 
                                          "NEAtlantic", "9", 144352, 1427, 0.0099, 0.546, 
                                          "NEAtlantic", "11", 68759, 777, 0.0113, 1.083, 
                                          "NEAtlantic", "12", 111115, 4167, 0.0375, 0.740, 
                                          "NEAtlantic", "13", 59340, 1298, 0.0219, 0.640, 
                                          ############################################## MED
                                          "Med", "Atlantic", 33720, 0, 0, 0,
                                          "Med", "Alboran", 28071, 0, 0, 0,
                                          "Med", "SWMed", 279415, 416, 0.0015, 0.8240,
                                          "Med", "NWMed", 134760, 0, 0, 0,
                                          "Med", "PelagosW", 56756, 0, 0, 0,
                                          "Med", "PelagosE", 31076, 0, 0, 0,
                                          "Med", "Tyrrhenian", 231298, 63, 0.0003, 1.0596,
                                          "Med","SCMed", 152961, 0, 0, 0,
                                          "Med", "Adriatic", 135783, 0, 0, 0,
                                          "Med", "IonianC", 185926, 272, 0.0015, 1.0609,
                                          "Med", "IonianE", 172477, 64, 0.0004, 1.0544,
                                          "Med", "Aegean", 191150, 472, 0.0025, 1.0425,
                                          "Med", "NEMed", 161669, 195, 0.0012, 1.0577,
                                          "Med", "EMed", 107687, 0, 0, 0,
                                          ######################################### t-NASS
                                          "NAtlantic", "FC", 77857, 4992, 0.0174, 0.72, 
                                          "NAtlantic", "FW", 176905, 16204, 0.0249, 0.71,
                                          "NAtlantic", "IE", 108052, 213, 0.0015, 0.54,
                                          "NAtlantic", "IG", 93953, 399, 0.00323, 0.6,
                                          "NAtlantic", "IP", 139248, 721, 0.00394, 0.77,
                                          "NAtlantic", "IR", 108550, 372, 0.0026, 0.51,
                                          "NAtlantic", "IW", 37905, 265, 0.00532, 0.48, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 1180, NA, 0.219,
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "GOM/BOF", 199656, 287, NA, 1.4,
                                          "NWAtlantic", "NWAtl_slope", 54376, 161, NA, 0.46,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 1145, NA, 0.38, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 5095, 0.00208, 0.56, 
                                          ######################################### New Caledonia 
                                          "Pacific_NCal", "NCal", 542300, 700, 0.001, 0.6, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 971, NA, 0.31, 
                                          ######################################### Indian
                                          "Indian", "CMGM_P", 87560, 104, 0.0122, 0.76,
                                          "Indian", "JMN_O", 74032, 35, 0.00048, 1.04,
                                          "Indian", "EBM_O", 119099, 73, 0.00061, 0.76,
                                          "Indian", "TM_P", 20992, 60, 0.00292, 0.68, 
                                          "Indian", "TM_O", 124218, 77, 0.00062, 1.04, 
                                          "Indian", "RM_P", 74632, 31, 0.00033, 0.72, 
                                          "Indian", "RM_O", 306861, 42, 0.00014, 1.04, 
                                          "Indian", "SE_P", 97713, 241, 0.00247, 0.55,
                                          "Indian", "SE_O", 167277, 46, 0.00027, 1.04,
                                          ######################################### Antilles-Guyana
                                          "Antilles", "ANT_P1", 17476, 88, 0.005, 0.9,
                                          "Antilles", "ANT_P2", 36006, 23, 0.001, 1.07,
                                          "Antilles", "ANT_P3", 52687, 39, 0.001, 0.78,
                                          "Antilles", "ANT_O1", 56841, 58, 0.001, 0.63,
                                          "Guyana", "GUY_P5", 61465, 45, 0.001, 1.05,
                                          ########################################## Gulf of Alaska
                                          "GoAlaska", "Seamount", 45377, 12, 0.0000, 1.03, 
                                          "GoAlaska", "Slope", 36776, 117, 0.003, 0.46
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Phys_mac)
  
  original_df_Phys_mac
  
}


## Pseudorca crassidens - False killer whale 
data_from_data_Pseu_cra <- function() {
  original_df_Pseu_cra <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 494, NA, 0.787, 
                                          ######################################### Hawai2017 # from Bradford et al 2020 but reported in Baradford et al 2021
                                          "Pacific_Hawai", "Hawai_Pelagic", 2447635, 5106, 0.00209, 0.63,
                                          "Pacific_Hawai", "NWHI", 2447635, 477, 0.00106, 1.71
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Pseu_cra)
  
  original_df_Pseu_cra
}



## Sotalia guianensis - Sotalia dolphin
data_from_data_Sota_gui <- function() {
  original_df_Sota_gui <- tibble::tribble(~ Geo_area, ~ Block, ~Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### Antilles-Guyana
                                          "Guyana", "GUY_P5", 61465, 1764, 0.029, 0.53 
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Sota_gui)
  
  original_df_Sota_gui
  
}

## Stenella attenuata - Pantropical spotted dolphin
data_from_data_Sten_att <- function() {
  original_df_Sten_att <- tibble::tribble(~ Geo_area, ~ Block, ~Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 37195, NA, 0.244, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 39798, 0.01663, 0.51, 
                                          ######################################### REMMOA Wallis & Futuna : 
                                          #small delph are only Sten_att (no Sten_lon identified to species so no ratio)
                                          "Pacific_WFu", "WFu", 233600, 15283, 0.049, 0.28
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Sten_att)
  
  original_df_Sten_att
  
}

## Stenella clymene - Clymene dolphin
data_from_data_Sten_cly <- function() {
  original_df_Sten_cly <- tibble::tribble(~ Geo_area, ~ Block, ~Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 513, NA, 1.033
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Sten_cly)
  
  original_df_Sten_cly
  
}

## Stenella frontalis - Atlantic spotted dolphin
data_from_data_Sten_fro <- function() {
  original_df_Sten_fro <- tibble::tribble(~ Geo_area, ~ Block, ~Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 5577, NA, 0.414,
                                          ######################################### NWAtl2011
                                          "NWAtlantic", "NWAtl_slope", 54376, 1677, NA, 0.6,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 25121, NA, 0.7
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Sten_fro)
  
  original_df_Sten_fro
  
}


## Stenella longirostris - Spinner dolphin
data_from_data_Sten_lon <- function() {
  original_df_Sten_lon <- tibble::tribble(~ Geo_area, ~ Block, ~Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 2911, NA, 0.540
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Sten_lon)
  
  original_df_Sten_lon
  
}


## Steno bredanensis - Rough-toothed dolphin
data_from_data_Sten_bre <- function() {
  original_df_Sten_bre <- tibble::tribble(~ Geo_area, ~ Block, ~Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 76375, 0.0312, 0.41
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Sten_bre)
  
  original_df_Sten_bre
  
}

## Tursiops truncatus - Common bottlenose dolphin
data_from_data_Turs_tru <- function() {
  original_df_Turs_tru <- tibble::tribble(~ Geo_area, ~ Block, ~Surf, ~ Abund, ~Dens, ~CV, 
                                          ############################################### NEAtlantic
                                          "NEAtlantic", "AB", 26668, 735, 0.0275, 0.703,
                                          "NEAtlantic", "AC", 35180, 4210, 0.1197, 0.479,
                                          "NEAtlantic", "B", 118471, 6926, 0.0585, 0.382,
                                          "NEAtlantic", "D", 48590, 2938, 0.0605, 0.447,
                                          "NEAtlantic", "E", 34870, 288, 0.0082, 0.573, 
                                          "NEAtlantic", "G", 15122, 1824, 0.1206, 0.682,
                                          "NEAtlantic", "H", 18634, 59, 0.0032, 1.009,  
                                          "NEAtlantic", "P", 63655, 147, 0.0023, 0.995, 
                                          "NEAtlantic", "R", 64464, 1924, 0.0298, 0.861, 
                                          "NEAtlantic", "S", 40383, 151, 0.0037, 1.007,
                                          "NEAtlantic", "8", 159669, 1195, 0.0075, 0.634,
                                          "NEAtlantic", "9", 144352, 5928, 0.0411, 0.633, 
                                          "NEAtlantic", "12", 111115, 6029, 0.0543, 0.685, 
                                          "NEAtlantic", "13", 59340, 769, 0.0130, 1.056, 
                                          ####################################### NEAtlantic - Observe (Season 3 - S5 excluded as in SCANS III)
                                          "NEAtlantic", "S1_N", 62052, 10524, 0.17, 0.3899,
                                          "NEAtlantic", "S1_O", 62052, 10524, 0.17, 0.3899,
                                          "NEAtlantic", "S2_N", 60167, 9211, 0.153, 0.5508,
                                          "NEAtlantic", "S2_O", 60167, 9211, 0.153, 0.5508,
                                          "NEAtlantic", "S3_N", 100482, 29602, 0.295, 0.3072,
                                          "NEAtlantic", "S3_O", 100482, 29602, 0.295, 0.3072,
                                          "NEAtlantic", "S4", 63162, 5549, 0.088, 0.4772,
                                          "NEAtlantic", "S6", 15766, 2473, 0.157, 0.9741,
                                          "NEAtlantic", "S7", 17261, 18704, 1.084, 0.6245,
                                          "NEAtlantic", "S8", 9707, 11266, 1.161, 0.599, 
                                          ############################################## MED 
                                          "Med", "Atlantic", 33720, 3495, 0.1037, 0.6763,
                                          "Med", "Alboran", 28071, 9821, 0.3499, 0.4977,
                                          "Med", "SWMed", 279415, 590, 0.0021, 0.6054,
                                          "Med", "NWMed", 134760, 10615, 0.0788, 0.4090,
                                          "Med", "PelagosW", 56756, 988, 0.0174, 0.7797, 
                                          "Med", "PelagosE", 31076, 1217, 0.0391, 0.5987, 
                                          "Med", "Tyrrhenian", 231298, 4628, 0.02, 0.3830,
                                          "Med","SCMed", 152961, 8668, 0.0567, 0.5084, 
                                          "Med", "Adriatic", 135783, 10350, 0.0762, 0.2916, 
                                          "Med", "IonianC", 185926, 1311, 0.0071, 0.5928,
                                          "Med", "IonianE", 172477, 1898, 0.0110, 0.4795,
                                          "Med", "Aegean", 191150, 9017, 0.0472, 0.3728, 
                                          "Med", "NEMed", 161669, 661, 0.0041, 0.5690,
                                          "Med", "EMed", 107687, 75, 0.0007, 0.9934,
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 7462, NA, 0.313,
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "GOM/BOF", 199656, 814, NA, 0.52,
                                          "NWAtlantic", "NWAtl_slope", 54376, 13911, NA, 0.76,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 12041, NA, 0.39, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 1006, NA, 0.48
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Turs_tru)
  
  original_df_Turs_tru
  

}


######### cases involving ratio of species (mixed-species groups) ##############


############## long-beaked and short-beaked common dolphins and striped dolphins #############
build_sp_tib_Dd_Dc_Sc <- function(ratio_others_tib) {
  # ratio_others_tib is the tibble with ratio of observations for all surveys 
  # but REMMOAs 
  
  ####  1 - we start by building original df for each sp and for
  ########## the mixed species categories
  ## Delphinus delphis - short-beaked common dolphin
  original_df_Delp_del <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################################## NEAtlantic
                                          "NEAtlantic", "AA", 12015, 18458, 1.5362, 0.644,
                                          "NEAtlantic", "AB", 26668, 63243, 2.3715, 0.273,
                                          "NEAtlantic", "AC", 35180, 71082, 2.0205, 0.306,
                                          "NEAtlantic", "B", 118471, 92893, 0.7841, 0.269,
                                          "NEAtlantic", "D", 48590, 18187, 0.3743, 0.413, 
                                          "NEAtlantic", "J", 35099, 4679, 0.1333, 0.946,
                                          "NEAtlantic", "8", 159669, 10601, 0.066, 0.940, 
                                          "NEAtlantic", "9", 144352, 150208, 1.041, 0.718, 
                                          "NEAtlantic", "11", 68759, 34570, 0.503, 0.633, 
                                          "NEAtlantic", "12", 111115, 6432, 0.058, 0.543, 
                                          "NEAtlantic", "13", 59340, 3110, 0.052, 0.653, 
                                          ####################################### NEAtlantic - Observe (Season 3 - S5 excluded as in SCANS III) 
                                          # no confirmed sighting of Sscoe so all obs are considered Ddel + model-based estimates available only 
                                          "NEAtlantic", "S3_N", 100482, 26467, 0.257, 0.504,
                                          "NEAtlantic", "S3_O", 100482, 26467, 0.257, 0.504,
                                          "NEAtlantic", "S7", 17261, 5429, 0.322, 0.9055,
                                          "NEAtlantic", "S8", 9707, 1319, 0.139, 0.4547, 
                                          ############################################## MED
                                          "Med", "Atlantic", 33720, 35293, 1.0467, 0.5412,
                                          "Med", "Alboran", 28071, 25855, 0.9211, 0.6874,  
                                          "Med", "SWMed", 279415, 0, 0, 0,
                                          "Med", "NWMed", 134760, 0, 0, 0, 
                                          "Med", "PelagosW", 56756, 0, 0, 0, 
                                          "Med", "PelagosE", 31076, 0, 0, 0,
                                          "Med", "Tyrrhenian", 231298, 521, 0.0023, 1.0088,
                                          "Med","SCMed", 152961, 286, 0.0019, 0.8587,
                                          "Med", "Adriatic", 135783, 0, 0, 0, 
                                          "Med", "IonianC", 185926, 414, 0.0022, 0.9995,
                                          "Med", "IonianE", 172477, 0, 0, 0,
                                          "Med", "Aegean", 191150, 2759, 0.0144, 0.9089,
                                          "Med", "NEMed", 161669, 1230, 0.0076, 0.9862,
                                          "Med", "EMed", 107687, 0, 0, 0,
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "GOM/BOF", 199656, 2855, NA, 0.33,
                                          "NWAtlantic", "NWAtl_slope", 54376, 54507, NA, 0.3,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 9829, NA, 0.71,
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 411211, NA, 0.21
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Delp_del)
  
  # focus on blocks of interest and computations
  Abund_df_Delp_del <- build_sp_tib(original_df_Delp_del, "Delphinus delphis", "Delp_del")
  
  
  ## Delphinus capensis - long-beaked common dolphin
  # data from report 
  original_df_Delp_cap <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 27046, NA, 0.59
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Delp_cap)
  
  # focus on blocks of interest and computations
  Abund_df_Delp_cap <- build_sp_tib(original_df_Delp_cap, "Delphinus capensis", "Delp_cap")
  
  
  ## Stenella coeruleoalba  - striped dolphin
  # data from report 
  original_df_Sten_coe <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################################## NEAtlantic
                                          "NEAtlantic", "AB", 26668, 3039, 0.1140, 0.903,
                                          "NEAtlantic", "AC", 35180, 15581, 0.4429, 0.456,
                                          "NEAtlantic", "B", 118471, 228, 0.0019, 0.978,
                                          "NEAtlantic", "D", 48590, 262, 0.0054, 0.915,
                                          "NEAtlantic", "K", 32505, 142, 0.0044, 0.915,
                                          "NEAtlantic", "9", 144352, 164023, 1.136, 0.593, 
                                          "NEAtlantic", "11", 68759, 128559, 1.870, 0.408, 
                                          "NEAtlantic", "12", 111115, 76796, 0.691, 0.588, 
                                          "NEAtlantic", "13", 59340, 52823, 0.890, 0.565, 
                                          ############################################## MED
                                          "Med", "Atlantic", 33720, 6268, 0.1859, 0.9836,
                                          "Med", "Alboran", 28071, 37848, 1.3483, 0.4935,
                                          "Med", "SWMed", 279415, 164079, 0.5872, 0.2443,
                                          "Med", "NWMed", 134760, 86386, 0.6410, 0.2868,
                                          "Med", "PelagosW", 56756, 29154, 0.5137, 0.4049,
                                          "Med", "PelagosE", 31076, 394, 0.0127, 0.9877,
                                          "Med", "Tyrrhenian", 231298, 44367, 0.1918, 0.2339,
                                          "Med","SCMed", 152961, 199, 0.0013, 0.9848,
                                          "Med", "Adriatic", 135783, 10264, 0.0756, 0.5427,
                                          "Med", "IonianC", 185926, 21325, 0.1147, 0.3794,
                                          "Med", "IonianE", 172477, 16582, 0.0961, 0.6060,
                                          "Med", "Aegean", 191150, 8205, 0.0429, 0.5810,
                                          "Med", "NEMed", 161669, 1673, 0.0103, 1.0014,
                                          "Med", "EMed", 107687, 0, 0, 0, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 1817, NA, 0.558,
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "NWAtl_slope", 54376, 3822, NA, 0.28,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 43060, NA, 0.36, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 34271, 0.014, 0.32, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 10908, NA, 0.34
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Sten_coe)
  
  # focus on blocks of interest and computations
  Abund_df_Sten_coe <- build_sp_tib(original_df_Sten_coe, "Stenella coeruleoalba", "Sten_coe")
  
  
  ## Unidentified Sc or Dd
  # data from report 
  original_df_Dd_Dc_Sc <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~ CV, 
                                          ################################################## NEAtlantic MED Delp_del & Sten_coe
                                          "NEAtlantic", "AB", 26668, 6239, 0.2339, 0.773,
                                          "NEAtlantic", "AC", 35180, 5504, 0.1564, 0.835,
                                          "NEAtlantic", "B", 118471, 61741, 0.5211, 0.217,
                                          "NEAtlantic", "C", 81297, 1765, 0.0217, 0.819,
                                          "NEAtlantic", "D", 48590, 31800, 0.6545, 0.349, 
                                          "NEAtlantic", "I", 13979, 206, 0.0148, 1.016,
                                          "NEAtlantic", "9", 144352, 3377, 0.023, 0.665, 
                                          "NEAtlantic", "11", 68759, 31298, 0.455, 0.619,
                                          "NEAtlantic", "12", 111115, 28214, 0.254, 0.758, 
                                          "NEAtlantic", "13", 59340, 13414, 0.226, 0.403, 
                                          ############################################## MED Delp_del & Sten_coe
                                          "Med", "Atlantic", 33720, 6699, 0.1987, 0.6194,
                                          "Med", "Alboran", 28071, 69412, 2.4727, 0.6199,
                                          "Med", "SWMed", 279415, 81284, 0.2909, 0.4153,
                                          "Med", "NWMed", 134760, 29382, 0.2180, 0.3205, 
                                          "Med", "PelagosW", 56756, 6333, 0.1116, 0.4572, 
                                          "Med", "PelagosE", 31076, 342, 0.0110, 0.9860, 
                                          "Med", "Tyrrhenian", 231298, 5934, 0.0257, 0.6053, 
                                          "Med","SCMed", 152961, 0, 0, 0,
                                          "Med", "Adriatic", 135783, 0, 0, 0,
                                          "Med", "IonianC", 185926, 4349, 0.0234, 0.6199,
                                          "Med", "IonianE", 172477, 1029, 0.006, 0.6398,
                                          "Med", "Aegean", 191150, 8064, 0.0422, 0.4660,
                                          "Med", "NEMed", 161669, 0, 0, 0,
                                          "Med", "EMed", 107687, 0, 0, 0, 
                                          ######################################### California current mean 2005-2008 Delp_del & Delp_cap
                                          "Pacific_Calif_current", "Calif_current", 1141807, 6117, NA, 0.61
  ) 
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Dd_Dc_Sc)
  
  # focus on blocks of interest and computations
  Abund_df_Dd_Dc_Sc <- build_sp_tib(original_df_Dd_Dc_Sc, "Und Dd/Dc/Sc", "Und Dd/Dc/Sc") 
  
  
  ####  2 - concatenate everything with ratio of observation and 
  ########## compute total abundances 
  
  rbind(Abund_df_Sten_coe, Abund_df_Delp_del, Abund_df_Delp_cap) |>
    dplyr::left_join(Abund_df_Dd_Dc_Sc |>
                       dplyr::rename(Code_sp_und = Code_sp,
                                     Species_und = Species,
                                     Abund_und = Abund, 
                                     Abund_CV_und = Abund_CV, 
                                     Surf_und = Surf_tot), 
                     by = c("Geo_area", "Eco_area")) |>
    dplyr::select("Code_sp", "Species", "Geo_area", "Eco_area", "Surf_tot", 
                  "Abund", "Abund_CV", "Abund_und", "Abund_CV_und") |>
    dplyr::mutate(Abund_und = dplyr::case_when(is.na(Abund_und) ~ 0, 
                                               TRUE ~ Abund_und), 
                  Abund_CV_und = dplyr::case_when(is.na(Abund_CV_und) ~ 0, 
                                                  TRUE ~ Abund_CV_und)) |>
    dplyr::left_join(ratio_others_tib, by = c("Geo_area", "Eco_area", "Species")) |>
    dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                           TRUE ~ ratio),
                  Abund_tot = Abund + ratio*Abund_und, 
                  Var_tot = (Abund*Abund_CV*Abund*Abund_CV) + ratio*(Abund_und*Abund_CV_und*Abund_und*Abund_CV_und),
                  Abund_CV_tot = sqrt(Var_tot)/Abund_tot) |>
    dplyr::select(c("Code_sp", "Species", "Geo_area", "Eco_area", "Surf_tot", 
                    "Abund_tot", "Abund_CV_tot")) |>
    dplyr::rename(Abund = Abund_tot, 
                  Abund_CV = Abund_CV_tot)
  
}


############################### BEAKED WHALES #################################
build_sp_tib_BW <- function(ratio_REMMOAs_tib, ratio_others_tib) {
  ########## 1 - when there was on estimate for all beaked whales 
  # data from report 
  original_df_BW <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                    ################################################## NEAtlantic - SCANS III 
                                    "NEAtlantic", "AC", 35180, 581, 0.0165, 0.530,
                                    "NEAtlantic", "B", 118471, 101, 0.0009, 0.774,
                                    "NEAtlantic", "H", 18634, 100, 0.0054, 1.106,
                                    "NEAtlantic", "J", 35099, 325, 0.0092, 0.621,
                                    "NEAtlantic", "K", 32505, 211, 0.0065, 0.904, 
                                    "NEAtlantic", "U", 60046, 75, 0.0012, 1.040,
                                    "NEAtlantic", "V", 38306, 97, 0.0025, 1.020,
                                    "NEAtlantic", "8", 159669, 1530, 0.0096, 0.700, 
                                    "NEAtlantic", "9", 144352, 461, 0.0032, 0.915, 
                                    "NEAtlantic", "11", 68759, 408, 0.0059, 0.867, 
                                    "NEAtlantic", "12", 111115, 1156, 0.0104, 0.763, 
                                    "NEAtlantic", "13", 59340, 1755, 0.0296, 0.611, 
                                    ####################################### NEAtlantic - Observe (Season 3 - S5 excluded as in SCANS III)
                                    "NEAtlantic", "S1_N", 62052, 1579, 0.0254, 0.9230,
                                    "NEAtlantic", "S1_O", 62052, 1579, 0.0254, 0.9230,
                                    "NEAtlantic", "S2_N", 60167, 698, 0.0116, 0.5388,
                                    "NEAtlantic", "S2_O", 60167, 698, 0.0116, 0.5388,
                                    "NEAtlantic", "S3_N", 100482, 866, 0.0086, 0.4687,
                                    "NEAtlantic", "S3_O", 100482, 866, 0.0086, 0.4687,
                                    ############################################## MED
                                    "Med", "Atlantic", 33720, 498, 0.0148, 0.6598,
                                    "Med", "Alboran", 28071, 271, 0.096, 1.0196,
                                    "Med", "SWMed", 279415, 75, 0.0003, 1.0062,
                                    "Med", "NWMed", 134760, 944, 0.0070, 0.8433,
                                    "Med", "PelagosW", 56756, 0, 0, 0,
                                    "Med", "PelagosE", 31076, 0, 0, 0,
                                    "Med", "Tyrrhenian", 231298, 181, 0.0008, 0.7671,
                                    "Med","SCMed", 152961, 246, 0.0016, 0.7996,
                                    "Med", "Adriatic", 135783, 66, 0.0005, 1.0120,
                                    "Med", "IonianC", 185926, 258, 0.0014, 0.7292,
                                    "Med", "IonianE", 172477, 245, 0.0014, 0.7256,
                                    "Med", "Aegean", 191150, 193, 0.0010, 0.7679,
                                    "Med", "NEMed", 161669, 0, 0, 0,
                                    "Med", "EMed", 107687, 420, 0.0039, 0.9793,
                                    ######################################### French Poly 
                                    "Pacific_FPoly", "AUS_C", 11693, 68, 0.00579, 0.76,
                                    "Pacific_FPoly", "AUS_P", 75131, 25, 0.00291, 0.51,
                                    "Pacific_FPoly", "AUS_O", 186545, 20, 0.00165, 0.56,
                                    "Pacific_FPoly", "GAM_P", 150930, 30, 0.00518, 0.48,
                                    "Pacific_FPoly", "GAM_O", 185976, 5, 0.0007, 1.03,
                                    "Pacific_FPoly", "SOC_C", 19372, 20, 0.01287, 0.55,
                                    "Pacific_FPoly", "SOC_P", 81650, 5, 0.00085, 1.03,  
                                    "Pacific_FPoly", "SOC_O", 174253, 25, 0.00284, 0.51, 
                                    "Pacific_FPoly", "TUN_P", 148366, 25, 0.00306, 0.51,  
                                    "Pacific_FPoly", "TUN_O", 138368, 20, 0.00284, 0.56,
                                    "Pacific_FPoly", "TUS_P", 146201, 20, 0.00312, 0.54, 
                                    "Pacific_FPoly", "TUS_O", 151913, 5, 0.00069, 1.03, 
                                    "Pacific_FPoly", "MAR_C", 24868, 10, 0.00445, 0.75, 
                                    "Pacific_FPoly", "MAR_P", 64731, 15, 0.00354, 0.63, 
                                    "Pacific_FPoly", "MAR_O", 243531, 56, 0.005, 0.38, 
                                    ######################################### New Caledonia 
                                    "Pacific_NCal", "NCal", 542300, 2442, 0.0045, 0.28,  
                                    ######################################### Wallis & Futuna 
                                    "Pacific_WFu", "WFu", 233600, 468, 0.002, 0.43, 
                                    ######################################### Indian : for beaked whale only one estimation but only obs in slope and oceanic strata so considered all oceanic in the end
                                    "Indian", "CMGM_O", 259491, 1422, 0.00516, 0.43,
                                    "Indian", "JMN_O", 101452, 827, 0.00669, 0.38,
                                    "Indian", "EBM_O", 155226, 831, 0.00544, 0.53,
                                    "Indian", "TM_O", 124218, 553, 0.00361, 68.22, 
                                    "Indian", "RM", 404884, 659, 0.00162, 723.83,  
                                    "Indian", "SE_O", 264990, 766, 0.00261, 0.41, 
                                    ######################################### Antilles-Guyana
                                    "Antilles", "ANT_P2", 36006, 21, 0.001, 1.01,
                                    "Antilles", "ANT_P3", 52687, 58, 0.001, 0.57,
                                    "Antilles", "ANT_O1", 56841, 225, 0.004, 0.41,
                                    "Guyana", "GUY_P5", 61465, 43, 0.001, 0.72,
                                    "Guyana", "GUY_O5", 49129, 275, 0.006, 0.64
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_BW)
  # For French Polynesia there is nothing to check as there is no totals in the table of report...
  # except : we see there is a tiny tiny CV where it is very high individually... 
  
  # focus on blocks of interest and computations
  Abund_df_BW <- build_sp_tib(original_df_BW, "All beaked whales","BW")
  
  Abund_Zc_Ha_Meso_Indo_pac <- Abund_df_BW |>
    dplyr::left_join(rbind(ratio_others_tib, 
                           ratio_REMMOAs_tib |> 
                             dplyr::ungroup() |>
                             dplyr::select(-"Group_sp")) |>
                       dplyr::filter(Species %in% c("Hyperoodon ampullatus", 
                                                    "Ziphius cavirostris", 
                                                    "Mesoplodon spp", 
                                                    "Indopacetus pacificus")), by = c("Geo_area", "Eco_area")) |>
    dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                           TRUE ~ ratio), 
                  Code_sp = dplyr::case_when(Species.y == "Mesoplodon spp" ~ "Meso_spp", 
                                             Species.y == "Hyperoodon ampullatus" ~ "Hype_amp",
                                             Species.y == "Ziphius cavirostris" ~ "Ziph_cav", 
                                             Species.y == "Indopacetus pacificus" ~ "Indo_pac"),
                  Abund_sp = round(ratio*Abund, 0), 
                  Var_sp = ratio*(Abund*Abund_CV*Abund*Abund_CV),
                  Abund_CV_sp = sqrt(Var_sp)/Abund_sp) |>
    dplyr::select(c("Code_sp", "Species.y", "Geo_area", "Eco_area", "Surf_tot", 
                    "Abund_sp", "Var_sp", "Abund_CV_sp")) |>
    dplyr::rename(Species = Species.y, 
                  Abund = Abund_sp, 
                  Abund_CV = Abund_CV_sp) |> 
    dplyr::group_by(Code_sp, Species, Geo_area, Eco_area, Surf_tot) |>
    dplyr::summarise(Abund = sum(Abund), 
                     Var = sum(Var_sp), 
                     Abund_CV = sqrt(Var)/Abund) |> 
    dplyr::select(- Var) |>
    dplyr::filter(!is.na(Species), !(Abund == 0)) # Block where there was no observations
  
  # clean up 
  rm(original_df_BW, Abund_df_BW)
  
  
  ######### 2 - For when there were both abundance estimates for species and for all beaked whales
  # GoMexico, California Current, Hawai
  
  ## Mesoplodont species 
  # data from report 
  original_df_Meso_spp <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 20, NA, 0.977, # M. densirostris
                                          "GoMexico", "GoMexico", 380432, 98, NA, 0.464, # Meso_spp
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 1132, 0.00046, 0.99, # Mesoplodon densirostris
                                          "Pacific_Hawai", "Hawai", 2447635, 2923, 0.00119, 0.61, # Mesoplopon spp.
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 841, NA, 0.88, # Mesoplopon spp.
                                          "Pacific_Calif_current", "Calif_current", 1141807, 603, NA, 1.16 # Mesoplopon densirostris
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Meso_spp)
  
  # focus on blocks of interest and computations
  Abund_df_Meso_spp <- build_sp_tib(original_df_Meso_spp, "Mesoplodon spp", "Meso_spp")
  
  
  ## Ziphius cavirostris - Cuvier's beaked whale
  # data from report 
  original_df_Ziph_cav <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 18, NA, 0.749, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 4431, 0.00181, 0.41, # Ziphius cavirostris
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 2143, NA, 0.65 # Ziphius cavirostris
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Ziph_cav)
  
  # focus on blocks of interest and computations
  Abund_df_Ziph_cav <- build_sp_tib(original_df_Ziph_cav, "Ziphius cavirostris", "Ziph_cav")
  
  
  ## Indopacetus pacificus - Longman's beaked whale
  # data from report 
  original_df_Indo_pac <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 2550, 0.00104, 0.67, # Indopacetus pacificus
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Indo_pac)
  
  # focus on blocks of interest and computations
  Abund_df_Indo_pac <- build_sp_tib(original_df_Indo_pac, "Indopacetus pacificus", "Indo_pac")
  
  
  ## Berardius bairdii - Baird's beaked whale
  # data from report 
  original_df_Bera_bai <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 907, NA, 0.49, # Berardius bairdii
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Bera_bai)
  
  # focus on blocks of interest and computations
  Abund_df_Bera_bai <- build_sp_tib(original_df_Bera_bai, "Berardius bairdii", "Bera_bai")
  
  
  ## Unidentified Ziphiid
  # data from report
  original_df_Ziphiid <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~ CV,
                                         ######################################### goMexico mean 2017-2018
                                         "GoMexico", "GoMexico", 380432, 181, NA, 0.308,
                                         ######################################### Hawai2017
                                         "Pacific_Hawai", "Hawai", 2447635, 1826, 0.00075, 0.46, # Und. Ziphiid
                                         ######################################### California current mean 2005-2008
                                         "Pacific_Calif_current", "Calif_current", 1141807, 1505, NA, 0.63 # Und. Ziphiid
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Ziphiid)
  
  # focus on blocks of interest and computations
  Abund_df_Ziphiid <- build_sp_tib(original_df_Ziphiid, "Und Ziphiid", "Und Ziphiid")
  
  # concatenate everything with ratio of observation and compute total abundances 
  Abund_Meso_spp_Ziph_cav_Indo_pac_Bera_bai_final <- rbind(Abund_df_Meso_spp, Abund_df_Ziph_cav, 
                                                           Abund_df_Indo_pac, Abund_df_Bera_bai) |>
    dplyr::left_join(Abund_df_Ziphiid |>
                       dplyr::rename(Code_sp_und = Code_sp,
                                     Species_und = Species,
                                     Abund_und = Abund, 
                                     Abund_CV_und = Abund_CV, 
                                     Surf_und = Surf_tot), by = c("Geo_area", "Eco_area")) |>
    dplyr::select("Code_sp", "Species", "Geo_area", "Eco_area", "Surf_tot", 
                  "Abund", "Abund_CV", "Abund_und", "Abund_CV_und") |>
    dplyr::mutate(Abund_und = dplyr::case_when(is.na(Abund_und) ~ 0, 
                                               TRUE ~ Abund_und), 
                  Abund_CV_und = dplyr::case_when(is.na(Abund_CV_und) ~ 0, 
                                                  TRUE ~ Abund_CV_und)) |>
    dplyr::left_join(ratio_others_tib, by = c("Geo_area", "Eco_area", "Species")) |>
    dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                           TRUE ~ ratio),
                  Abund_tot = Abund + ratio*Abund_und, 
                  Var_tot = (Abund*Abund_CV*Abund*Abund_CV) + 
                    ratio*(Abund_und*Abund_CV_und*Abund_und*Abund_CV_und),
                  Abund_CV_tot = sqrt(Var_tot)/Abund_tot) |>
    dplyr::select(c("Code_sp", "Species", "Geo_area", "Eco_area", "Surf_tot", 
                    "Abund_tot", "Abund_CV_tot")) |>
    dplyr::rename(Abund = Abund_tot, 
                  Abund_CV = Abund_CV_tot)
  
  # clean up 
  rm(original_df_Meso_spp, Abund_df_Meso_spp,
     original_df_Ziph_cav, Abund_df_Ziph_cav,
     original_df_Indo_pac, Abund_df_Indo_pac,
     original_df_Bera_bai, Abund_df_Bera_bai,
     original_df_Ziphiid, Abund_df_Ziphiid)
  
  
  ######### 3 - For when abundance was already dispatched between species including the und. species
  
  ######### t-NASS NAtlantic ##############
  # Hyperoodon ampullatus - Northern Bottlenose whale
  original_df_Hype_amp <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################### t-NASS
                                          "NAtlantic", "FC", 77857, 11384, 0.135, 0.94, 
                                          "NAtlantic", "FW", 176905, 2522, 0.0131, 0.84,
                                          "NAtlantic", "IG", 93953, 1121, 0.011, 1.05,
                                          "NAtlantic", "IR", 108550, 2777, 0.0235, 0.9,
                                          "NAtlantic", "IW", 37905, 2170, 0.0527, 0.79,
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Hype_amp)
  
  # focus on blocks of interest and computations
  Abund_df_Hype_amp <- build_sp_tib(original_df_Hype_amp, "Hyperoodon ampullatus", "Hype_amp")
  
  
  
  ######### WAtlantic 2011 ##############
  # Ziphius cavirostris - Cuvier's beaked whale
  original_df_Ziph_cav <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "NWAtl_slope", 54376, 946, NA, 0.48,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 4016, NA, 0.44
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Ziph_cav)
  
  # focus on blocks of interest and computations
  Abund_df_Ziph_cav <- build_sp_tib(original_df_Ziph_cav, "Ziphius cavirostris", "Ziph_cav")
  
  
  # Mesoplodon spp (M. bidens & M. europaeus) - Sowerby's and Gervais' beaked whales
  original_df_Meso_spp <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### NWAtl2011 # Mbid 
                                          "NWAtlantic", "NWAtl_slope", 54376, 2007, NA, 0.99,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 1646, NA, 0.93, 
                                          ######################################### NWAtl2011 # Meur
                                          "NWAtlantic", "NWAtl_offshore", 197953, 1847, NA, 0.96
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Meso_spp)
  # here the cv is much lower than individual estimates.... :/
  
  # focus on blocks of interest and computations
  Abund_df_Meso_spp <- build_sp_tib(original_df_Meso_spp, "Mesoplodon spp", "Meso_spp")
  
  
  ##### concatenate everything 
  rbind(Abund_Zc_Ha_Meso_Indo_pac, # from #1
        Abund_Meso_spp_Ziph_cav_Indo_pac_Bera_bai_final, # from #2
        Abund_df_Hype_amp, Abund_df_Ziph_cav, Abund_df_Meso_spp # from #3
  )

}


##################################### REMMOA - Small delphinids ################
build_sp_tib_REMMOA_smalldel <- function(ratio_REMMOAs_tib) {
  # two species identified in French polynesia : Stenella longirostris and Stenella attenuata
  # idem in NCal and WFu (but considered as 100? Sten_att as no Sten_long identified to species), and Indian ocean
  
  original_df_small_delph <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                             ######################################### French Poly 
                                             "Pacific_FPoly", "SOC_C", 19372, 917, 0.04734, 0.65,  
                                             "Pacific_FPoly", "SOC_P", 81650, 684, 0.00838, 0.76, 
                                             "Pacific_FPoly", "TUN_P", 148366, 1335, 0.009, 0.65, 
                                             "Pacific_FPoly", "TUS_P", 146201, 1678, 0.01148, 0.65, 
                                             "Pacific_FPoly", "TUS_O", 151913, 514, 0.0338, 1.05, 
                                             "Pacific_FPoly", "MAR_C", 24868, 199, 0.00802, 1.05, 
                                             "Pacific_FPoly", "MAR_P", 64731, 826, 0.01276, 0.66, 
                                             "Pacific_FPoly", "MAR_O", 243531, 3004, 0.01233, 0.49, 
                                             ######################################### New Caledonia 
                                             "Pacific_NCal", "NCal", 542300, 19589, 0.027, 0.38, 
                                             ######################################### Indian
                                             "Indian", "CMGM_N", 24237, 3037, 0.1655, 0.59,
                                             "Indian", "CMGM_P", 87560, 30486, 0.3571, 0.34,
                                             "Indian", "CMGM_O", 171931, 9721, 0.565, 0.8,
                                             "Indian", "JMN_N", 24634, 5005, 0.2251, 0.57,
                                             "Indian", "JMN_P", 27420, 8488, 0.3096, 0.41,
                                             "Indian", "JMN_O", 74032, 10449, 0.1411, 0.44,
                                             "Indian", "EBM_P", 36127, 3393, 0.1008, 0.48,
                                             "Indian", "EBM_O", 119099, 561, 0.0047, 0.6,
                                             "Indian", "TM_O", 155045, 578, 0.0038, 0.71, # it's not stratified here but as TM is only 6.4% shelf in surface we consider it as oceanic in this case
                                             "Indian", "RM", 404884, 6115, 0.0150, 0.47,
                                             "Indian", "SE_N", 29158, 3795, 0.1313, 0.54,
                                             "Indian", "SE_P", 97713, 61155, 0.6259, 0.37,
                                             "Indian", "SE_O", 167277, 30997, 0.1853, 0.5,
                                             ######################################### Antilles-Guyana
                                             "Antilles", "ANT_P1", 17476, 999, 0.057, 1.2,
                                             "Antilles", "ANT_P2", 36006, 2224, 0.062, 0.81,
                                             "Antilles", "ANT_P3", 52687, 4435, 0.084, 0.88,
                                             "Antilles", "ANT_O1", 56841, 1139, 0.020, 1.01,
                                             "Guyana", "GUY_P5", 61465, 5811, 0.095, 0.42,
                                             "Guyana", "GUY_O5", 49129, 2840, 0.058, 0.6
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_small_delph) # not really anything to check here as no totals in the tables
  # but the CV is far lower than all CVs of individual abundance estimates
  
  
  # focus on blocks of interest and computations
  Abund_df_small_delph <- build_sp_tib(original_df_small_delph, "Small delphinids","smalldel")
  
  
  Abund_df_small_delph |>
    dplyr::left_join(ratio_REMMOAs_tib |>
                       dplyr::ungroup() |> 
                       dplyr::filter(Group_sp == "Small delphinids"), 
                     by = c("Geo_area", "Eco_area")) |>
    dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                           TRUE ~ ratio), 
                  Code_sp = dplyr::case_when(Species.y == "Stenella attenuata" ~ "Sten_att", 
                                             Species.y == "Stenella longirostris" ~ "Sten_lon"),
                  Abund_sp = round(ratio*Abund, 0), 
                  Var_sp = ratio*(Abund*Abund_CV*Abund*Abund_CV),
                  Abund_CV_sp = sqrt(Var_sp)/Abund_sp) |>
    dplyr::select(c("Code_sp", "Species.y", "Geo_area", "Eco_area", "Surf_tot", 
                    "Abund_sp", "Var_sp", "Abund_CV_sp")) |>
    dplyr::rename(Species = Species.y, 
                  Abund = Abund_sp, 
                  Abund_CV = Abund_CV_sp) |> 
    dplyr::group_by(Code_sp, Species, Geo_area, Eco_area, Surf_tot) |>
    dplyr::summarise(Abund = sum(Abund), 
                     Var = sum(Var_sp), 
                     Abund_CV = sqrt(Var)/Abund) |> 
    dplyr::select(- Var) |>
    dplyr::filter(!is.na(Species), !(Abund == 0)) # Block where there was no observations
  
  
}


##################################### REMMOA - Large delphinids ################
build_sp_tib_REMMOA_largedel <- function(ratio_REMMOAs_tib) {
  # three species identified in French polynesia :Tursiops truncatus, Steno bredanensis, Lagenodelphis hosei
  # For Indian ocean : add Sousa plumbea
  original_df_large_delph <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                             ######################################### French Poly 
                                             "Pacific_FPoly", "AUS_P", 75131, 283, 0.00377, 0.61,
                                             "Pacific_FPoly", "SOC_C", 19372, 808, 0.0417, 0.55,  
                                             "Pacific_FPoly", "SOC_P", 81650, 301, 0.0369, 0.73,  
                                             "Pacific_FPoly", "SOC_O", 174253, 214, 0.00123, 1.02, 
                                             "Pacific_FPoly", "TUN_P", 148366, 392, 0.00264, 0.73,  
                                             "Pacific_FPoly", "TUN_O", 138368, 1486, 0.01074, 0.46,
                                             "Pacific_FPoly", "TUS_P", 146201, 1232, 0.00842, 0.55, 
                                             "Pacific_FPoly", "TUS_O", 151913, 226, 0.00149, 1.02, 
                                             "Pacific_FPoly", "MAR_C", 24868, 120, 0.00481, 1.01, 
                                             "Pacific_FPoly", "MAR_P", 64731, 661, 0.01021, 0.53, 
                                             "Pacific_FPoly", "MAR_O", 243531, 3863, 0.01586, 0.35, 
                                             ######################################### New Caledonia 
                                             "Pacific_NCal", "NCal", 542300, 7367, 0.011, 0.33,  
                                             ######################################### Wallis & Futuna 
                                             "Pacific_WFu", "WFu", 233600, 4310, 0.014, 0.44, 
                                             ######################################### Indian
                                             "Indian", "CMGM_N", 24237, 1346, 0.0734, 0.3,
                                             "Indian", "CMGM_P", 87560, 2462, 0.0288, 0.45,
                                             "Indian", "CMGM_O", 171931, 7275, 0.0423, 0.35,
                                             "Indian", "JMN_N", 24634, 2799, 0.1259, 0.23,
                                             "Indian", "JMN_P", 27420, 4671, 0.1704, 0.24,
                                             "Indian", "JMN_O", 74032, 26323, 0.3556, 0.29,
                                             "Indian", "EBM_P", 36127, 5458, 0.1621, 0.3,
                                             "Indian", "EBM_O", 119099, 20958, 0.1760, 0.33,
                                             "Indian", "TM_N", 9835, 234, 0.0277, 0.46, 
                                             "Indian", "TM_P", 20992, 419, 0.0203, 0.61, 
                                             "Indian", "TM_O", 124218, 2968, 0.0239, 0.43, 
                                             "Indian", "MAU_N", 6250, 362, 0.0579, 0.51,  
                                             "Indian", "MAU_P", 74632, 3358, 0.0463, 0.3,  
                                             "Indian", "MAU_O", 137889, 2091, 0.0152, 0.52,  
                                             "Indian", "REU_N", 17141, 2161, 0.1038, 0.36, 
                                             "Indian", "REU_O", 168972, 3753, 0.0222, 0.4, 
                                             "Indian", "SE_N", 29158, 4891, 0.1692, 0.19,
                                             "Indian", "SE_P", 97713, 16457, 0.1684, 0.29,
                                             "Indian", "SE_O", 167277, 10352, 0.0619, 0.29,
                                             ######################################### Antilles-Guyana
                                             "Antilles", "ANT_P2", 36006, 2673, 0.074, 1.08,
                                             "Antilles", "ANT_P3", 52687, 994, 0.019, 0.73,
                                             "Antilles", "ANT_O1", 56841, 1515, 0.027, 0.66,
                                             "Guyana", "GUY_P5", 61465, 5546, 0.090, 0.46,
                                             "Guyana", "GUY_O5", 49129, 581, 0.012, 0.64 
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_large_delph) # not really anything to check here as no totals in the tables
  # IDEM but the CV is far lower than all CVs of individual abundance estimates
  
  
  # focus on blocks of interest and computations
  Abund_df_large_delph <- build_sp_tib(original_df_large_delph, "Large delphinids","largedel")
  
  
  Abund_df_large_delph |>
    dplyr::left_join(ratio_REMMOAs_tib |>
                       dplyr::ungroup() |>
                       dplyr::filter(Group_sp == "Large delphinids"), 
                     by = c("Geo_area", "Eco_area")) |>
    dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                           TRUE ~ ratio), 
                  Code_sp = dplyr::case_when(Species.y == "Lagenodelphis hosei" ~ "Lage_hos", 
                                             Species.y == "Steno bredanensis" ~ "Sten_bre", 
                                             Species.y == "Tursiops truncatus" ~ "Turs_tru", 
                                             Species.y == "Sousa plumbea" ~ "Sous_plu"),
                  Abund_sp = round(ratio*Abund, 0), 
                  Var_sp = ratio*(Abund*Abund_CV*Abund*Abund_CV),
                  Abund_CV_sp = sqrt(Var_sp)/Abund_sp) |>
    dplyr::select(c("Code_sp", "Species.y", "Geo_area", "Eco_area", "Surf_tot", 
                    "Abund_sp", "Var_sp", "Abund_CV_sp")) |>
    dplyr::rename(Species = Species.y, 
                  Abund = Abund_sp, 
                  Abund_CV = Abund_CV_sp) |> 
    dplyr::group_by(Code_sp, Species, Geo_area, Eco_area, Surf_tot) |>
    dplyr::summarise(Abund = sum(Abund), 
                     Var = sum(Var_sp), 
                     Abund_CV = sqrt(Var)/Abund) |> 
    dplyr::select(- Var) |>
    dplyr::filter(!is.na(Species), !(Abund == 0)) # Block where there was no observations
  
}


##################################### REMMOA - Small globicephalinids ################
build_sp_tib_REMMOA_smallglobi <- function(ratio_REMMOAs_tib) {
# 3 species identified in Antilles/Guyana: Grampus griseus, Peponocephala electra, Feresa attenuata
# but Feresa attenuata never identified to species, so we considered Pepo_ele/Fere_att was 100 % Pepo_ele
# NCal & Wallis & Futuna -> 100% Pepo_elec

original_df_small_globi <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                   ######################################### Antilles-Guyana
                                   "Antilles", "ANT_P1", 17476, 458, 0.026, 0.75,
                                   "Antilles", "ANT_O1", 56841, 90, 0.002, 1.06,
                                   "Guyana", "GUY_P5", 61465, 478, 0.008, 0.77,
                                   "Guyana", "GUY_O5", 49129, 189, 0.004, 0.77 
)

# check totals from report with verify_totals
#verify_total(original_df_small_globi) # not really anything to check here as no totals in the tables
# IDEM but the CV is far lower than all CVs of individual abundance estimates


# focus on blocks of interest and computations
Abund_df_small_globi <- build_sp_tib(original_df_small_globi, "Small globicephalinids","small globi")


Abund_df_small_globi |>
  dplyr::left_join(ratio_REMMOAs_tib |>
                     dplyr::ungroup() |>
                     dplyr::filter(Group_sp == "Small globicephalinids"), 
                   by = c("Geo_area", "Eco_area")) |>
  dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                  TRUE ~ ratio), 
                Code_sp = dplyr::case_when(Species.y == "Grampus griseus" ~ "Gram_gri", 
                                    Species.y == "Peponocephala electra" ~ "Pepo_ele"),
                Abund_sp = round(ratio*Abund, 0), 
                Var_sp = ratio*(Abund*Abund_CV*Abund*Abund_CV),
                Abund_CV_sp = sqrt(Var_sp)/Abund_sp) |>
  dplyr::select(c("Code_sp", "Species.y", "Geo_area", "Eco_area", "Surf_tot", 
                  "Abund_sp", "Var_sp", "Abund_CV_sp")) |>
  dplyr::rename(Species = Species.y, 
         Abund = Abund_sp, 
         Abund_CV = Abund_CV_sp) |> 
  dplyr::group_by(Code_sp, Species, Geo_area, Eco_area, Surf_tot) |>
  dplyr::summarise(Abund = sum(Abund), 
            Var = sum(Var_sp), 
            Abund_CV = sqrt(Var)/Abund) |> 
  dplyr::select(- Var) |>
  dplyr::filter(!is.na(Species), !(Abund == 0)) # Block where there was no observations

}


##################################### REMMOA - Large globicephalinids ################
build_sp_tib_REMMOA_largeglobi <- function(ratio_REMMOAs_tib) {
  # 3 species identified in French polynesia: Globicephala macrorhynchus, Pseudorca crassidens, Orcinus orca
  
  original_df_large_globi <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                             ######################################### French Poly 
                                             "Pacific_FPoly", "AUS_O", 186545, 358, 0.00192, 0.74,
                                             "Pacific_FPoly", "SOC_C", 19372, 145, 0.0075, 1.03,  
                                             "Pacific_FPoly", "SOC_P", 81650, 325, 0.00398, 0.74,  
                                             "Pacific_FPoly", "TUN_P", 148366, 846, 0.0057, 0.82,
                                             "Pacific_FPoly", "TUN_O", 138368, 917, 0.00663, 0.54,
                                             "Pacific_FPoly", "TUS_P", 146201, 266, 0.00182, 1.03, 
                                             "Pacific_FPoly", "TUS_O", 151913, 733, 0.00482, 0.61, 
                                             "Pacific_FPoly", "MAR_C", 24868, 419, 0.01683, 0.61, 
                                             "Pacific_FPoly", "MAR_P", 64731, 1156, 0.01786, 0.51, 
                                             "Pacific_FPoly", "MAR_O", 243531, 3002, 0.01233, 0.4, 
                                             ######################################### New Caledonia 
                                             "Pacific_NCal", "NCal", 542300, 10512, 0.014, 0.3,  
                                             ######################################### Wallis & Futuna 
                                             "Pacific_WFu", "WFu", 233600, 1086, 0.003, 0.56, 
                                             ######################################### Indian
                                             "Indian", "CMGM_P", 87560, 707, 0.00828, 0.68,
                                             "Indian", "CMGM_O", 171931, 300, 0.00174, 0.82,
                                             "Indian", "JMN_P", 27420, 1437, 0.05242, 0.57,
                                             "Indian", "JMN_O", 74032, 1435, 0.01938, 0.69,
                                             "Indian", "EBM_P", 36127, 1403, 0.04168, 0.69,
                                             "Indian", "EBM_O", 119099, 3139, 0.02636, 0.51,
                                             "Indian", "TM_P", 20992, 252, 0.01222, 0.66, 
                                             "Indian", "TM_O", 124218, 905, 0.00729, 0.51,  
                                             "Indian", "RM_P", 74632, 293, 0.00314, 0.94, 
                                             "Indian", "RM_O", 306861, 623, 0.00203, 1.44, 
                                             "Indian", "SE_P", 97713, 9861, 0.10092, 0.62,
                                             "Indian", "SE_O", 167277, 10358, 0.6192, 0.65,
                                             ######################################### Antilles-Guyana
                                             "Antilles", "ANT_P1", 17476, 731, 0.042, 1,
                                             "Antilles", "ANT_P2", 36006, 1649, 0.046, 0.87,
                                             "Antilles", "ANT_P3", 52687, 2414, 0.046, 0.83,
                                             "Antilles", "ANT_O1", 56841, 342, 0.006, 0.83,
                                             "Guyana", "GUY_O5", 49129, 726, 0.012, 0.79 
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_large_globi) # not really anything to check here as no totals in the tables
  # IDEM but the CV is far lower than all CVs of individual abundance estimates
  
  
  # focus on blocks of interest and computations
  Abund_df_large_globi <- build_sp_tib(original_df_large_globi, "Large globicephalinids","large globi")
  
  Abund_df_large_globi |>
    dplyr::left_join(ratio_REMMOAs_tib |>
                       dplyr::ungroup() |>
                       dplyr::filter(Group_sp == "Large globicephalinids"), by = c("Geo_area", "Eco_area")) |>
    dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                           TRUE ~ ratio), 
                  Code_sp = dplyr::case_when(Species.y == "Globicephala macrorhynchus" ~ "Glob_mac", 
                                             Species.y == "Orcinus orca" ~ "Orci_orc", 
                                             Species.y == "Pseudorca crassidens" ~ "Pseu_cra"),
                  Abund_sp = round(ratio*Abund, 0), 
                  Var_sp = ratio*(Abund*Abund_CV*Abund*Abund_CV),
                  Abund_CV_sp = sqrt(Var_sp)/Abund_sp) |>
    dplyr::select(c("Code_sp", "Species.y", "Geo_area", "Eco_area", "Surf_tot", 
                    "Abund_sp", "Var_sp", "Abund_CV_sp")) |>
    dplyr::rename(Species = Species.y, 
                  Abund = Abund_sp, 
                  Abund_CV = Abund_CV_sp) |> 
    dplyr::group_by(Code_sp, Species, Geo_area, Eco_area, Surf_tot) |>
    dplyr::summarise(Abund = sum(Abund), 
                     Var = sum(Var_sp), 
                     Abund_CV = sqrt(Var)/Abund) |> 
    dplyr::select(- Var) |>
    dplyr::filter(!is.na(Species), !(Abund == 0)) # Block where there was no observations
  
}


########################################################################################################################################
#################################### SUMMER Project specific function (with separation between West and East Mediterranean Sea) ########
########################################################################################################################################

# Build tibble relative to just one species
build_sp_tib_SUMMER <- function(original_tib, species, code_sp) {
  # species and code_sp are character strings 
  # species is the full latin name and code sp the two first letters
  
  # special case for Observe survey (Ireland)
  # there are stratum with mixed neritic and oceanic waters
  # we use the ratio of surfaces to separate abudances estimates between neritic and oceanic
  rS1_N <- 30177/62052
  rS1_O <- 31875/62052
  rS2_N <- 35464/60167
  rS2_O <- 24703/60167
  rS3_N <- 80009/100482
  rS3_O <- 20743/100482
  
  original_tib |> 
    dplyr::filter(!(Block %in% c("W", "X", "Y", "Z", "P1", "SVG", "TRD", # SCANS III
                                 "Atlantic", # MED
                                 "S5")), # Observe, stratum that is included in the block E of SCANS III
                  Abund != 0) |>
    dplyr::mutate(Abund = dplyr::case_when(Block == "S1_N" ~ round(rS1_N*Abund, 0), 
                                           Block == "S1_O" ~ round(rS1_O*Abund, 0),
                                           Block == "S2_N" ~ round(rS2_N*Abund, 0),
                                           Block == "S2_O" ~ round(rS2_O*Abund, 0),
                                           Block == "S3_N" ~ round(rS3_N*Abund, 0),
                                           Block == "S3_O" ~ round(rS3_O*Abund, 0),
                                           TRUE ~ Abund),
                  Var_abund = dplyr::case_when(Block == "S1_N" ~ rS1_N*(Abund*CV*Abund*CV), 
                                               Block == "S1_O" ~ rS1_O*(Abund*CV*Abund*CV),
                                               Block == "S2_N" ~ rS2_N*(Abund*CV*Abund*CV),
                                               Block == "S2_O" ~ rS2_O*(Abund*CV*Abund*CV),
                                               Block == "S3_N" ~ rS3_N*(Abund*CV*Abund*CV),
                                               Block == "S3_O" ~ rS3_O*(Abund*CV*Abund*CV),
                                               TRUE ~ Abund*CV*Abund*CV),
                  Eco_area = dplyr::case_when(Block %in% c("AA", "AB", "AC",
                                                           "B", "C","D", "E", 
                                                           "F","G", "H", "I",
                                                           "J", "K", "L", "M", 
                                                           "N", "O", "P", "Q", 
                                                           "R", "S", "T", "U", 
                                                           "V") ~ "shelf", # shelf NEAtlantic
                                              Block %in% c("8","9", "11", "12", "13") ~ "oceanic", # Oceanic NEAtlantic
                                              Block %in% c("NWMed", "SWMed","PelagosW", 
                                                           "PelagosE", "Tyrrhenian", "Ionian",
                                                           "NEMed", "EMed") ~ "oceanic", # oceanic Med
                                              Block %in% c("Alboran", "SCMed","Adriatic", 
                                                           "Aegean") ~ "shelf", # Shelf Med
                                              Block %in% c("S1_N", "S2_N", "S3_N",
                                                           "S4", "S6", "S7", "S8") ~ "shelf", # Shelf Observe
                                              Block %in% c("S1_O", "S2_O", "S3_O") ~ "oceanic", # Slope Observe
                                              Block %in% c("IR", "IE", "IC", "CG") ~ "shelf", # Shelf t-NASS
                                              Block %in% c("FC", "FW", "IG", "IP",
                                                           "IQ", "IW") ~ "oceanic", # Oceanic t-NASS
                                              Block == "GoMexico" ~ "oceanic", # goMexico mean 2017-2018 is full oceanic 
                                              Block == "GOM/BOF" ~"shelf", # shelf Atlantic Wcoast
                                              Block %in% c("NWAtl_slope", "NWAtl_offshore") ~ "oceanic", 
                                              Block %in% c("AUS_C", "AUS_P", "AUS_O", 
                                                           "GAM_C", "GAM_P", "GAM_O", 
                                                           "SOC_C", "SOC_P", "SOC_O", "TUN_P", "TUN_O", 
                                                           "TUS_P", "TUS_O", "MAR_C", "MAR_P", "MAR_O") ~ "oceanic", # French Polynesia: all slope/oceanic data
                                              Block %in% c("Hawai", "Hawai_Pelagic", "NWHI") ~ "oceanic", # Hawai2017 is very mainly oceanic 
                                              Block == "NCal" ~ "oceanic", # New Caledonia is all slope/oceanic
                                              Block == "WFu" ~ "oceanic", # Wallis & Futuna is all slope/oceanic
                                              Block %in% c("Calif_current") ~ "oceanic", # California current is mainly oceanic, although there is a significant shelf area it is not distinguished, # most coastal obs are in canyons
                                              Block %in% c("CMGM_N", "JMN_N", "TM_N", 
                                                           "MAU_N", "SE_N", "RM_N", "REU_N") ~ "shelf", # Indian ocean
                                              Block %in% c("CMGM_P", "CMGM_O",
                                                           "JMN_P", "JMN_O", 
                                                           "EBM_P", "EBM_O", 
                                                           "TM_P", "TM_O",
                                                           "MAU_P", "MAU_O",
                                                           "REU_O", 
                                                           "RM", "RM_P", "RM_O", # unstratified subarea of Mauritius and Reunion
                                                           "SE_P", "SE_O") ~ "oceanic", # Indian ocean
                                              Block %in% c("ANT_P1", "ANT_P2", "ANT_P3") ~ "shelf", # Antilles
                                              Block %in% c("ANT_O1") ~ "oceanic",
                                              Block %in% c("GUY_P5") ~ "shelf", # Guyana
                                              Block %in% c("GUY_O5") ~ "oceanic", 
                                              Block %in% c("Inshore") ~ "shelf", # GoAlaska
                                              Block %in% c("Offshore", "Seamount", 
                                                           "Slope") ~ "oceanic", # GoAlaska
                  ), 
                  Geo_area = dplyr::case_when(Block %in% c("Ionian", "NEMed", "EMed") ~ "Med_E", 
                                              Block %in% c("NWMed", "SWMed","PelagosW", 
                                                           "PelagosE", "Tyrrhenian") ~ "Med_W",
                                              TRUE ~ Geo_area)
    ) |> 
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::summarise(Abund_tot = sum(Abund), 
                     Var_tot = sum(Var_abund), 
                     CV_tot = sqrt(Var_tot)/Abund_tot) |>
    dplyr::mutate(Code_sp = code_sp, 
                  Species = species, 
                  Surf_tot = dplyr::case_when(Geo_area == "NEAtlantic" & Eco_area == "shelf" ~ 1084226 # SCANS III
                                              , # Observe Ireland
                                              Geo_area == "NEAtlantic" & Eco_area == "oceanic" ~ 543235 # SCANS III
                                              , # Observe Ireland
                                              Geo_area == "Med" & Eco_area == "shelf" ~ 507965,
                                              Geo_area == "Med_E" & Eco_area == "oceanic" ~ 358402+161669+107687,
                                              Geo_area == "Med_W" & Eco_area == "oceanic" ~ 279415+134760+56756+31076+231298,
                                              Geo_area == "NAtlantic" & Eco_area == "shelf" ~ 743874,
                                              Geo_area == "NAtlantic" & Eco_area == "oceanic" ~ 2046837, 
                                              Geo_area == "GoMexico" & Eco_area == "oceanic" ~ 380432,
                                              Geo_area == "NWAtlantic" & Eco_area == "shelf" ~ 199656,
                                              Geo_area == "NWAtlantic" & Eco_area == "oceanic" ~ 252329, 
                                              Geo_area == "Pacific_FPoly" & Eco_area == "oceanic" ~ 7849 + 75131 + 186545 + # AUS_C + AUS_P + AUS_O
                                                4078 + 150930 + 185976 + # GAM_C + GAM_P + GAM_O
                                                19372 + 81650 + 174253 + # SOC_C + SOC_P + SOC_O
                                                148366 + 138368 + # TUN_P + TUN_O
                                                146201 + 151913 + # TUS_P + TUS_O
                                                24868 + 64731 + 243531, # MAR_C + MAR_P + MAR_O
                                              Geo_area == "Pacific_Hawai" & Eco_area == "oceanic" ~ 2447635, 
                                              Geo_area == "Pacific_NCal" & Eco_area == "oceanic" ~ 542300,
                                              Geo_area == "Pacific_WFu" & Eco_area == "oceanic" ~ 233600, 
                                              Geo_area == "Pacific_Calif_current" & Eco_area == "oceanic" ~ 1141807, 
                                              Geo_area == "Indian" & Eco_area == "shelf" ~ 94114,
                                              Geo_area == "Indian" & Eco_area == "oceanic" ~ 1307862, 
                                              Geo_area == "Antilles" & Eco_area == "shelf" ~ 17476 + 36006 + 52687,
                                              Geo_area == "Antilles" & Eco_area == "oceanic" ~ 56841,
                                              Geo_area == "Guyana" & Eco_area == "shelf" ~ 61465,
                                              Geo_area == "Guyana" & Eco_area == "oceanic" ~ 49129,
                                              Geo_area == "GoAlaska" & Eco_area == "shelf" ~ 22749,
                                              Geo_area == "GoAlaska" & Eco_area == "oceanic" ~ 60051 + 45377 + 36776
                  )) |>
    dplyr::select(Code_sp, Species, Geo_area, Eco_area, Surf_tot, Abund_tot, CV_tot) |>
    dplyr::rename(Abund = Abund_tot, 
                  Abund_CV = CV_tot)
}


############## long-beaked and short-beaked common dolphins and striped dolphins #############
build_sp_tib_Dd_Dc_Sc_SUMMER <- function(ratio_others_tib) {
  # ratio_others_tib is the tibble with ratio of observations for all surveys 
  # but REMMOAs 
  
  ####  1 - we start by building original df for each sp and for
  ########## the mixed species categories
  ## Delphinus delphis - short-beaked common dolphin
  original_df_Delp_del <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################################## NEAtlantic
                                          "NEAtlantic", "AA", 12015, 18458, 1.5362, 0.644,
                                          "NEAtlantic", "AB", 26668, 63243, 2.3715, 0.273,
                                          "NEAtlantic", "AC", 35180, 71082, 2.0205, 0.306,
                                          "NEAtlantic", "B", 118471, 92893, 0.7841, 0.269,
                                          "NEAtlantic", "D", 48590, 18187, 0.3743, 0.413, 
                                          "NEAtlantic", "J", 35099, 4679, 0.1333, 0.946,
                                          "NEAtlantic", "8", 159669, 10601, 0.066, 0.940, 
                                          "NEAtlantic", "9", 144352, 150208, 1.041, 0.718, 
                                          "NEAtlantic", "11", 68759, 34570, 0.503, 0.633, 
                                          "NEAtlantic", "12", 111115, 6432, 0.058, 0.543, 
                                          "NEAtlantic", "13", 59340, 3110, 0.052, 0.653, 
                                          ####################################### NEAtlantic - Observe (Season 3 - S5 excluded as in SCANS III) 
                                          # no confirmed sighting of Sscoe so all obs are considered Ddel + model-based estimates available only 
                                          "NEAtlantic", "S3_N", 100482, 26467, 0.257, 0.504,
                                          "NEAtlantic", "S3_O", 100482, 26467, 0.257, 0.504,
                                          "NEAtlantic", "S7", 17261, 5429, 0.322, 0.9055,
                                          "NEAtlantic", "S8", 9707, 1319, 0.139, 0.4547, 
                                          ############################################## MED
                                          "Med", "Atlantic", 33720, 35293, 1.0467, 0.5412,
                                          "Med", "Alboran", 28071, 25855, 0.9211, 0.6874,  
                                          "Med", "SWMed", 279415, 0, 0, 0,
                                          "Med", "NWMed", 134760, 0, 0, 0, 
                                          "Med", "PelagosW", 56756, 0, 0, 0, 
                                          "Med", "PelagosE", 31076, 0, 0, 0,
                                          "Med", "Tyrrhenian", 231298, 521, 0.0023, 1.0088,
                                          "Med","SCMed", 152961, 286, 0.0019, 0.8587,
                                          "Med", "Adriatic", 135783, 0, 0, 0, 
                                          "Med", "Ionian", 358402, 404, 0.0011, 1.0030,
                                          "Med", "Aegean", 191150, 2759, 0.0144, 0.9089,
                                          "Med", "NEMed", 161669, 1230, 0.0076, 0.9862,
                                          "Med", "EMed", 107687, 0, 0, 0,
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "GOM/BOF", 199656, 2855, NA, 0.33,
                                          "NWAtlantic", "NWAtl_slope", 54376, 54507, NA, 0.3,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 9829, NA, 0.71,
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 411211, NA, 0.21
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Delp_del)
  
  # focus on blocks of interest and computations
  Abund_df_Delp_del <- build_sp_tib_SUMMER(original_df_Delp_del, "Delphinus delphis", "Delp_del")
  
  
  ## Delphinus capensis - long-beaked common dolphin
  # data from report 
  original_df_Delp_cap <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 27046, NA, 0.59
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Delp_cap)
  
  # focus on blocks of interest and computations
  Abund_df_Delp_cap <- build_sp_tib_SUMMER(original_df_Delp_cap, "Delphinus capensis", "Delp_cap")
  
  
  ## Stenella coeruleoalba  - striped dolphin
  # data from report 
  original_df_Sten_coe <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################################## NEAtlantic
                                          "NEAtlantic", "AB", 26668, 3039, 0.1140, 0.903,
                                          "NEAtlantic", "AC", 35180, 15581, 0.4429, 0.456,
                                          "NEAtlantic", "B", 118471, 228, 0.0019, 0.978,
                                          "NEAtlantic", "D", 48590, 262, 0.0054, 0.915,
                                          "NEAtlantic", "K", 32505, 142, 0.0044, 0.915,
                                          "NEAtlantic", "9", 144352, 164023, 1.136, 0.593, 
                                          "NEAtlantic", "11", 68759, 128559, 1.870, 0.408, 
                                          "NEAtlantic", "12", 111115, 76796, 0.691, 0.588, 
                                          "NEAtlantic", "13", 59340, 52823, 0.890, 0.565, 
                                          ############################################## MED
                                          "Med", "Atlantic", 33720, 6268, 0.1859, 0.9836,
                                          "Med", "Alboran", 28071, 37848, 1.3483, 0.4935,
                                          "Med", "SWMed", 279415, 164079, 0.5872, 0.2443,
                                          "Med", "NWMed", 134760, 86386, 0.6410, 0.2868,
                                          "Med", "PelagosW", 56756, 29154, 0.5137, 0.4049,
                                          "Med", "PelagosE", 31076, 394, 0.0127, 0.9877,
                                          "Med", "Tyrrhenian", 231298, 44367, 0.1918, 0.2339,
                                          "Med","SCMed", 152961, 199, 0.0013, 0.9848,
                                          "Med", "Adriatic", 135783, 10264, 0.0756, 0.5427,
                                          "Med", "Ionian", 358402, 37819, 0.1055, 0.3438,
                                          "Med", "Aegean", 191150, 8205, 0.0429, 0.5810,
                                          "Med", "NEMed", 161669, 1673, 0.0103, 1.0014,
                                          "Med", "EMed", 107687, 0, 0, 0, 
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 1817, NA, 0.558,
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "NWAtl_slope", 54376, 3822, NA, 0.28,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 43060, NA, 0.36, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 34271, 0.014, 0.32, 
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 10908, NA, 0.34
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Sten_coe)
  
  # focus on blocks of interest and computations
  Abund_df_Sten_coe <- build_sp_tib_SUMMER(original_df_Sten_coe, "Stenella coeruleoalba", "Sten_coe")
  
  
  ## Unidentified Sc or Dd
  # data from report 
  original_df_Dd_Dc_Sc <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~ CV, 
                                          ################################################## NEAtlantic MED Delp_del & Sten_coe
                                          "NEAtlantic", "AB", 26668, 6239, 0.2339, 0.773,
                                          "NEAtlantic", "AC", 35180, 5504, 0.1564, 0.835,
                                          "NEAtlantic", "B", 118471, 61741, 0.5211, 0.217,
                                          "NEAtlantic", "C", 81297, 1765, 0.0217, 0.819,
                                          "NEAtlantic", "D", 48590, 31800, 0.6545, 0.349, 
                                          "NEAtlantic", "I", 13979, 206, 0.0148, 1.016,
                                          "NEAtlantic", "9", 144352, 3377, 0.023, 0.665, 
                                          "NEAtlantic", "11", 68759, 31298, 0.455, 0.619,
                                          "NEAtlantic", "12", 111115, 28214, 0.254, 0.758, 
                                          "NEAtlantic", "13", 59340, 13414, 0.226, 0.403, 
                                          ############################################## MED Delp_del & Sten_coe
                                          "Med", "Atlantic", 33720, 6699, 0.1987, 0.6194,
                                          "Med", "Alboran", 28071, 69412, 2.4727, 0.6199,
                                          "Med", "SWMed", 279415, 81284, 0.2909, 0.4153,
                                          "Med", "NWMed", 134760, 29382, 0.2180, 0.3205, 
                                          "Med", "PelagosW", 56756, 6333, 0.1116, 0.4572, 
                                          "Med", "PelagosE", 31076, 342, 0.0110, 0.9860, 
                                          "Med", "Tyrrhenian", 231298, 5934, 0.0257, 0.6053, 
                                          "Med","SCMed", 152961, 0, 0, 0,
                                          "Med", "Adriatic", 135783, 0, 0, 0,
                                          "Med", "Ionian", 358402, 5296, 0.0148, 0.5117,
                                          "Med", "Aegean", 191150, 8064, 0.0422, 0.4660,
                                          "Med", "NEMed", 161669, 0, 0, 0,
                                          "Med", "EMed", 107687, 0, 0, 0, 
                                          ######################################### California current mean 2005-2008 Delp_del & Delp_cap
                                          "Pacific_Calif_current", "Calif_current", 1141807, 6117, NA, 0.61
  ) 
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Dd_Dc_Sc)
  
  # focus on blocks of interest and computations
  Abund_df_Dd_Dc_Sc <- build_sp_tib_SUMMER(original_df_Dd_Dc_Sc, "Und Dd/Dc/Sc", "Und Dd/Dc/Sc") 
  
  
  ####  2 - concatenate everything with ratio of observation and 
  ########## compute total abundances 
  
  rbind(Abund_df_Sten_coe, Abund_df_Delp_del, Abund_df_Delp_cap) |>
    dplyr::left_join(Abund_df_Dd_Dc_Sc |>
                       dplyr::rename(Code_sp_und = Code_sp,
                                     Species_und = Species,
                                     Abund_und = Abund, 
                                     Abund_CV_und = Abund_CV, 
                                     Surf_und = Surf_tot), 
                     by = c("Geo_area", "Eco_area")) |>
    dplyr::select("Code_sp", "Species", "Geo_area", "Eco_area", "Surf_tot", 
                  "Abund", "Abund_CV", "Abund_und", "Abund_CV_und") |>
    dplyr::mutate(Abund_und = dplyr::case_when(is.na(Abund_und) ~ 0, 
                                               TRUE ~ Abund_und), 
                  Abund_CV_und = dplyr::case_when(is.na(Abund_CV_und) ~ 0, 
                                                  TRUE ~ Abund_CV_und)) |>
    dplyr::left_join(ratio_others_tib, by = c("Geo_area", "Eco_area", "Species")) |>
    dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                           TRUE ~ ratio),
                  Abund_tot = Abund + ratio*Abund_und, 
                  Var_tot = (Abund*Abund_CV*Abund*Abund_CV) + ratio*(Abund_und*Abund_CV_und*Abund_und*Abund_CV_und),
                  Abund_CV_tot = sqrt(Var_tot)/Abund_tot) |>
    dplyr::select(c("Code_sp", "Species", "Geo_area", "Eco_area", "Surf_tot", 
                    "Abund_tot", "Abund_CV_tot")) |>
    dplyr::rename(Abund = Abund_tot, 
                  Abund_CV = Abund_CV_tot)
  
}


############################### BEAKED WHALES #################################
build_sp_tib_BW_SUMMER <- function(ratio_REMMOAs_tib, ratio_others_tib) {
  ########## 1 - when there was on estimate for all beaked whales 
  # data from report 
  original_df_BW <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                    ################################################## NEAtlantic - SCANS III 
                                    "NEAtlantic", "AC", 35180, 581, 0.0165, 0.530,
                                    "NEAtlantic", "B", 118471, 101, 0.0009, 0.774,
                                    "NEAtlantic", "H", 18634, 100, 0.0054, 1.106,
                                    "NEAtlantic", "J", 35099, 325, 0.0092, 0.621,
                                    "NEAtlantic", "K", 32505, 211, 0.0065, 0.904, 
                                    "NEAtlantic", "U", 60046, 75, 0.0012, 1.040,
                                    "NEAtlantic", "V", 38306, 97, 0.0025, 1.020,
                                    "NEAtlantic", "8", 159669, 1530, 0.0096, 0.700, 
                                    "NEAtlantic", "9", 144352, 461, 0.0032, 0.915, 
                                    "NEAtlantic", "11", 68759, 408, 0.0059, 0.867, 
                                    "NEAtlantic", "12", 111115, 1156, 0.0104, 0.763, 
                                    "NEAtlantic", "13", 59340, 1755, 0.0296, 0.611, 
                                    ####################################### NEAtlantic - Observe (Season 3 - S5 excluded as in SCANS III)
                                    "NEAtlantic", "S1_N", 62052, 1579, 0.0254, 0.9230,
                                    "NEAtlantic", "S1_O", 62052, 1579, 0.0254, 0.9230,
                                    "NEAtlantic", "S2_N", 60167, 698, 0.0116, 0.5388,
                                    "NEAtlantic", "S2_O", 60167, 698, 0.0116, 0.5388,
                                    "NEAtlantic", "S3_N", 100482, 866, 0.0086, 0.4687,
                                    "NEAtlantic", "S3_O", 100482, 866, 0.0086, 0.4687,
                                    ############################################## MED
                                    "Med", "Atlantic", 33720, 498, 0.0148, 0.6598,
                                    "Med", "Alboran", 28071, 271, 0.096, 1.0196,
                                    "Med", "SWMed", 279415, 75, 0.0003, 1.0062,
                                    "Med", "NWMed", 134760, 944, 0.0070, 0.8433,
                                    "Med", "PelagosW", 56756, 0, 0, 0,
                                    "Med", "PelagosE", 31076, 0, 0, 0,
                                    "Med", "Tyrrhenian", 231298, 181, 0.0008, 0.7671,
                                    "Med","SCMed", 152961, 246, 0.0016, 0.7996,
                                    "Med", "Adriatic", 135783, 66, 0.0005, 1.0120,
                                    "Med", "Ionian", 358402, 504, 0.0014, 0.5266,
                                    "Med", "Aegean", 191150, 193, 0.0010, 0.7679,
                                    "Med", "NEMed", 161669, 0, 0, 0,
                                    "Med", "EMed", 107687, 420, 0.0039, 0.9793,
                                    ######################################### French Poly 
                                    "Pacific_FPoly", "AUS_C", 11693, 68, 0.00579, 0.76,
                                    "Pacific_FPoly", "AUS_P", 75131, 25, 0.00291, 0.51,
                                    "Pacific_FPoly", "AUS_O", 186545, 20, 0.00165, 0.56,
                                    "Pacific_FPoly", "GAM_P", 150930, 30, 0.00518, 0.48,
                                    "Pacific_FPoly", "GAM_O", 185976, 5, 0.0007, 1.03,
                                    "Pacific_FPoly", "SOC_C", 19372, 20, 0.01287, 0.55,
                                    "Pacific_FPoly", "SOC_P", 81650, 5, 0.00085, 1.03,  
                                    "Pacific_FPoly", "SOC_O", 174253, 25, 0.00284, 0.51, 
                                    "Pacific_FPoly", "TUN_P", 148366, 25, 0.00306, 0.51,  
                                    "Pacific_FPoly", "TUN_O", 138368, 20, 0.00284, 0.56,
                                    "Pacific_FPoly", "TUS_P", 146201, 20, 0.00312, 0.54, 
                                    "Pacific_FPoly", "TUS_O", 151913, 5, 0.00069, 1.03, 
                                    "Pacific_FPoly", "MAR_C", 24868, 10, 0.00445, 0.75, 
                                    "Pacific_FPoly", "MAR_P", 64731, 15, 0.00354, 0.63, 
                                    "Pacific_FPoly", "MAR_O", 243531, 56, 0.005, 0.38, 
                                    ######################################### New Caledonia 
                                    "Pacific_NCal", "NCal", 542300, 2442, 0.0045, 0.28,  
                                    ######################################### Wallis & Futuna 
                                    "Pacific_WFu", "WFu", 233600, 468, 0.002, 0.43, 
                                    ######################################### Indian : for beaked whale only one estimation but only obs in slope and oceanic strata so considered all oceanic in the end
                                    "Indian", "CMGM_O", 259491, 1422, 0.00516, 0.43,
                                    "Indian", "JMN_O", 101452, 827, 0.00669, 0.38,
                                    "Indian", "EBM_O", 155226, 831, 0.00544, 0.53,
                                    "Indian", "TM_O", 124218, 553, 0.00361, 68.22, 
                                    "Indian", "RM", 404884, 659, 0.00162, 723.83,  
                                    "Indian", "SE_O", 264990, 766, 0.00261, 0.41, 
                                    ######################################### Antilles-Guyana
                                    "Antilles", "ANT_P2", 36006, 21, 0.001, 1.01,
                                    "Antilles", "ANT_P3", 52687, 58, 0.001, 0.57,
                                    "Antilles", "ANT_O1", 56841, 225, 0.004, 0.41,
                                    "Guyana", "GUY_P5", 61465, 43, 0.001, 0.72,
                                    "Guyana", "GUY_O5", 49129, 275, 0.006, 0.64
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_BW)
  # For French Polynesia there is nothing to check as there is no totals in the table of report...
  # except : we see there is a tiny tiny CV where it is very high individually... 
  
  # focus on blocks of interest and computations
  Abund_df_BW <- build_sp_tib_SUMMER(original_df_BW, "All beaked whales","BW")
  
  Abund_Zc_Ha_Meso_Indo_pac <- Abund_df_BW |>
    dplyr::left_join(rbind(ratio_others_tib, 
                           ratio_REMMOAs_tib |> 
                             dplyr::ungroup() |>
                             dplyr::select(-"Group_sp")) |>
                       dplyr::filter(Species %in% c("Hyperoodon ampullatus", 
                                                    "Ziphius cavirostris", 
                                                    "Mesoplodon spp", 
                                                    "Indopacetus pacificus")), by = c("Geo_area", "Eco_area")) |>
    dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                           TRUE ~ ratio), 
                  Code_sp = dplyr::case_when(Species.y == "Mesoplodon spp" ~ "Meso_spp", 
                                             Species.y == "Hyperoodon ampullatus" ~ "Hype_amp",
                                             Species.y == "Ziphius cavirostris" ~ "Ziph_cav", 
                                             Species.y == "Indopacetus pacificus" ~ "Indo_pac"),
                  Abund_sp = round(ratio*Abund, 0), 
                  Var_sp = ratio*(Abund*Abund_CV*Abund*Abund_CV),
                  Abund_CV_sp = sqrt(Var_sp)/Abund_sp) |>
    dplyr::select(c("Code_sp", "Species.y", "Geo_area", "Eco_area", "Surf_tot", 
                    "Abund_sp", "Var_sp", "Abund_CV_sp")) |>
    dplyr::rename(Species = Species.y, 
                  Abund = Abund_sp, 
                  Abund_CV = Abund_CV_sp) |> 
    dplyr::group_by(Code_sp, Species, Geo_area, Eco_area, Surf_tot) |>
    dplyr::summarise(Abund = sum(Abund), 
                     Var = sum(Var_sp), 
                     Abund_CV = sqrt(Var)/Abund) |> 
    dplyr::select(- Var) |>
    dplyr::filter(!is.na(Species), !(Abund == 0)) # Block where there was no observations
  
  # clean up 
  rm(original_df_BW, Abund_df_BW)
  
  
  ######### 2 - For when there were both abundance estimates for species and for all beaked whales
  # GoMexico, California Current, Hawai
  
  ## Mesoplodont species 
  # data from report 
  original_df_Meso_spp <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 20, NA, 0.977, # M. densirostris
                                          "GoMexico", "GoMexico", 380432, 98, NA, 0.464, # Meso_spp
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 1132, 0.00046, 0.99, # Mesoplodon densirostris
                                          "Pacific_Hawai", "Hawai", 2447635, 2923, 0.00119, 0.61, # Mesoplopon spp.
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 841, NA, 0.88, # Mesoplopon spp.
                                          "Pacific_Calif_current", "Calif_current", 1141807, 603, NA, 1.16 # Mesoplopon densirostris
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Meso_spp)
  
  # focus on blocks of interest and computations
  Abund_df_Meso_spp <- build_sp_tib(original_df_Meso_spp, "Mesoplodon spp", "Meso_spp")
  
  
  ## Ziphius cavirostris - Cuvier's beaked whale
  # data from report 
  original_df_Ziph_cav <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ######################################### goMexico mean 2017-2018
                                          "GoMexico", "GoMexico", 380432, 18, NA, 0.749, 
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 4431, 0.00181, 0.41, # Ziphius cavirostris
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 2143, NA, 0.65 # Ziphius cavirostris
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Ziph_cav)
  
  # focus on blocks of interest and computations
  Abund_df_Ziph_cav <- build_sp_tib(original_df_Ziph_cav, "Ziphius cavirostris", "Ziph_cav")
  
  
  ## Indopacetus pacificus - Longman's beaked whale
  # data from report 
  original_df_Indo_pac <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ######################################### Hawai2017
                                          "Pacific_Hawai", "Hawai", 2447635, 2550, 0.00104, 0.67, # Indopacetus pacificus
  )
  
  # check totals from report with verify_totals
  #verify_totals(original_df_Indo_pac)
  
  # focus on blocks of interest and computations
  Abund_df_Indo_pac <- build_sp_tib(original_df_Indo_pac, "Indopacetus pacificus", "Indo_pac")
  
  
  ## Berardius bairdii - Baird's beaked whale
  # data from report 
  original_df_Bera_bai <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV,
                                          ######################################### California current mean 2005-2008
                                          "Pacific_Calif_current", "Calif_current", 1141807, 907, NA, 0.49, # Berardius bairdii
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Bera_bai)
  
  # focus on blocks of interest and computations
  Abund_df_Bera_bai <- build_sp_tib(original_df_Bera_bai, "Berardius bairdii", "Bera_bai")
  
  
  ## Unidentified Ziphiid
  # data from report
  original_df_Ziphiid <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~ CV,
                                         ######################################### goMexico mean 2017-2018
                                         "GoMexico", "GoMexico", 380432, 181, NA, 0.308,
                                         ######################################### Hawai2017
                                         "Pacific_Hawai", "Hawai", 2447635, 1826, 0.00075, 0.46, # Und. Ziphiid
                                         ######################################### California current mean 2005-2008
                                         "Pacific_Calif_current", "Calif_current", 1141807, 1505, NA, 0.63 # Und. Ziphiid
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Ziphiid)
  
  # focus on blocks of interest and computations
  Abund_df_Ziphiid <- build_sp_tib(original_df_Ziphiid, "Und Ziphiid", "Und Ziphiid")
  
  # concatenate everything with ratio of observation and compute total abundances 
  Abund_Meso_spp_Ziph_cav_Indo_pac_Bera_bai_final <- rbind(Abund_df_Meso_spp, Abund_df_Ziph_cav, 
                                                           Abund_df_Indo_pac, Abund_df_Bera_bai) |>
    dplyr::left_join(Abund_df_Ziphiid |>
                       dplyr::rename(Code_sp_und = Code_sp,
                                     Species_und = Species,
                                     Abund_und = Abund, 
                                     Abund_CV_und = Abund_CV, 
                                     Surf_und = Surf_tot), by = c("Geo_area", "Eco_area")) |>
    dplyr::select("Code_sp", "Species", "Geo_area", "Eco_area", "Surf_tot", 
                  "Abund", "Abund_CV", "Abund_und", "Abund_CV_und") |>
    dplyr::mutate(Abund_und = dplyr::case_when(is.na(Abund_und) ~ 0, 
                                               TRUE ~ Abund_und), 
                  Abund_CV_und = dplyr::case_when(is.na(Abund_CV_und) ~ 0, 
                                                  TRUE ~ Abund_CV_und)) |>
    dplyr::left_join(ratio_others_tib, by = c("Geo_area", "Eco_area", "Species")) |>
    dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                           TRUE ~ ratio),
                  Abund_tot = Abund + ratio*Abund_und, 
                  Var_tot = (Abund*Abund_CV*Abund*Abund_CV) + 
                    ratio*(Abund_und*Abund_CV_und*Abund_und*Abund_CV_und),
                  Abund_CV_tot = sqrt(Var_tot)/Abund_tot) |>
    dplyr::select(c("Code_sp", "Species", "Geo_area", "Eco_area", "Surf_tot", 
                    "Abund_tot", "Abund_CV_tot")) |>
    dplyr::rename(Abund = Abund_tot, 
                  Abund_CV = Abund_CV_tot)
  
  # clean up 
  rm(original_df_Meso_spp, Abund_df_Meso_spp,
     original_df_Ziph_cav, Abund_df_Ziph_cav,
     original_df_Indo_pac, Abund_df_Indo_pac,
     original_df_Bera_bai, Abund_df_Bera_bai,
     original_df_Ziphiid, Abund_df_Ziphiid)
  
  
  ######### 3 - For when abundance was already dispatched between species including the und. species
  
  ######### t-NASS NAtlantic ##############
  # Hyperoodon ampullatus - Northern Bottlenose whale
  original_df_Hype_amp <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ################################### t-NASS
                                          "NAtlantic", "FC", 77857, 11384, 0.135, 0.94, 
                                          "NAtlantic", "FW", 176905, 2522, 0.0131, 0.84,
                                          "NAtlantic", "IG", 93953, 1121, 0.011, 1.05,
                                          "NAtlantic", "IR", 108550, 2777, 0.0235, 0.9,
                                          "NAtlantic", "IW", 37905, 2170, 0.0527, 0.79,
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Hype_amp)
  
  # focus on blocks of interest and computations
  Abund_df_Hype_amp <- build_sp_tib(original_df_Hype_amp, "Hyperoodon ampullatus", "Hype_amp")
  
  
  
  ######### WAtlantic 2011 ##############
  # Ziphius cavirostris - Cuvier's beaked whale
  original_df_Ziph_cav <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### NWAtl2011 
                                          "NWAtlantic", "NWAtl_slope", 54376, 946, NA, 0.48,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 4016, NA, 0.44
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Ziph_cav)
  
  # focus on blocks of interest and computations
  Abund_df_Ziph_cav <- build_sp_tib(original_df_Ziph_cav, "Ziphius cavirostris", "Ziph_cav")
  
  
  # Mesoplodon spp (M. bidens & M. europaeus) - Sowerby's and Gervais' beaked whales
  original_df_Meso_spp <- tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                                          ######################################### NWAtl2011 # Mbid 
                                          "NWAtlantic", "NWAtl_slope", 54376, 2007, NA, 0.99,
                                          "NWAtlantic", "NWAtl_offshore", 197953, 1646, NA, 0.93, 
                                          ######################################### NWAtl2011 # Meur
                                          "NWAtlantic", "NWAtl_offshore", 197953, 1847, NA, 0.96
  )
  
  # check totals from report with verify_totals
  #verify_total(original_df_Meso_spp)
  # here the cv is much lower than individual estimates.... :/
  
  # focus on blocks of interest and computations
  Abund_df_Meso_spp <- build_sp_tib(original_df_Meso_spp, "Mesoplodon spp", "Meso_spp")
  
  
  ##### concatenate everything 
  rbind(Abund_Zc_Ha_Meso_Indo_pac, # from #1
        Abund_Meso_spp_Ziph_cav_Indo_pac_Bera_bai_final, # from #2
        Abund_df_Hype_amp, Abund_df_Ziph_cav, Abund_df_Meso_spp # from #3
  )
  
}
