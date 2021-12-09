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

# function to verify totals of abundances, CVs, and densities with report
# because we are not using the same blocks, we are computing subtotals 
# this is a checking function, just to use directly in the console, 
# does not generate output with target
# it is indicated within functions where it should be called (# line)
verify_totals <- function(df) {
  # special case for Observe survey (Ireland)
  # there are stratum with mixed neritic and oceanic waters
  # we use the ratio of surfaces to separate abudances estimates between neritic and oceanic
  rS1_N <- 30177/62052
  rS1_O <- 31875/62052
  rS2_N <- 35464/60167
  rS2_O <- 24703/60167
  rS3_N <- 80009/100482
  rS3_O <- 20743/100482
  
  # compute our totals, to be compared with those from reports/papers
  df |>
    dplyr::group_by(Geo_area) |> 
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
                  Survey = dplyr::case_when(Block %in% c("AA", "AB", "AC",
                                                         "B", "C", "D", 
                                                         "E", "F", "G", 
                                                         "H", "I", "J", 
                                                         "K", "L", "M", 
                                                         "N", "O", "P", 
                                                         "Q", "R", "S", 
                                                         "T", "U", "V", 
                                                         "W", "X", "Y",
                                                         "Z", "P1", "SVG", 
                                                         "TRD") ~ "SCANSIII_aerial", 
                                            Block %in% c("8", "9", "11", 
                                                         "12", "13") ~ "SCANSIII_ship", 
                                            Block %in% c("S1_N", "S1_O", "S2_N", "S2_O", "S3_N", "S3_O", "S4",
                                                         "S5", "S6", "S7", "S8") ~ "Observe",
                                            Block %in% c("Atlantic", "Alboran", "SWMed", "NWMed", 
                                                         "PelagosW", "PelagosE", 
                                                         "Tyrrhenian", "SCMed", 
                                                         "Adriatic", "Ionian", 
                                                         "Aegean", "NEMed", "EMed") ~ "ASI", 
                                            Block %in% c("FC", "FW",  
                                                         "IE", "IC", "IG", "IP", 
                                                         "IQ", "IR", "IW", 
                                                         "SW", "CG", "X") ~ "t-NASS", 
                                            Block == "GoMexico" ~ "goMexico mean 2017-2018", 
                                            Block %in% c("GOM/BOF", "NWAtl_slope", "NWAtl_offshore") ~ "NWAtl2011",
                                            Block %in% c("AUS_C", "AUS_P", "AUS_O", 
                                                         "GAM_C", "GAM_P", "GAM_O",
                                                         "SOC_C", "SOC_P", "SOC_O", 
                                                         "TUN_P", "TUN_O", "TUS_P", "TUS_O",
                                                         "MAR_C", "MAR_P", "MAR_O") ~ "REMMOA_Poly", 
                                            Block %in% c("Hawai", "Hawai_Pelagic", "NWHI") ~ "Hawai2017", 
                                            Block == "NCal" ~ "REMMOA_NCal", 
                                            Block == "WFu" ~ "REMMOA_WFu", 
                                            Block %in% c("Calif_current") ~ "California_curr2005-8", 
                                            Block %in% c("CMGM_N", "CMGM_P", "CMGM_O",
                                                         "JMN_N", "JMN_P", "JMN_O", 
                                                         "EBM_P", "EBM_O", 
                                                         "TM_N", "TM_P", "TM_O",
                                                         "MAU_N", "MAU_P", "MAU_O",
                                                         "REU_N", "REU_O",  
                                                         "RM", "RM_N", "RM_P", "RM_O", # unstratified subarea of Mauritius and Reunion
                                                         "SE_N", "SE_P", "SE_O") ~ "REMMOA_Indian", 
                                            Block %in% c("ANT_P1", "ANT_P2", "ANT_P3", 
                                                         "ANT_O1") ~ "REMMOA_ANT", 
                                            Block %in% c("GUY_P5", "GUY_O5") ~ "REMMOA_GUY", 
                                            Block %in% c("Inshore", "Offshore", "Seamount", 
                                                         "Slope") ~ "GoAlaska"
                  ), 
                  Surf_tot = dplyr::case_when(Survey == "SCANSIII_aerial" ~ 1200575,
                                              Survey == "SCANSIII_ship" ~  604393, 
                                              Survey == "Observe" ~ 328596, # all Stratum except S5 that is in SCANS III
                                              Survey == "ASI" ~ sum(Surf), 
                                              Survey == "t-NASS" ~ 812602, 
                                              Survey == "goMexico mean 2017-2018" ~ 380432, 
                                              Survey == "NWAtl2011" ~ 451985, 
                                              Survey == "REMMOA_Poly" ~ 7849 + 75131 + 186545 + # AUS
                                                4078 + 150930 + 185976 + # GAM
                                                19372 + 81650 + 174253 + # SOC
                                                148366 + 138368 + # TUN
                                                146201 + 151913 + # TUS
                                                24868 + 64731 + 243531, # MAR 
                                              Survey == "Hawai2017" ~ 2447635, 
                                              Survey == "REMMOA_NCal" ~ 542300,
                                              Survey == "REMMOA_WFu" ~ 233600,
                                              Survey == "California_curr2005-8" ~ 1141807,
                                              Survey == "REMMOA_Indian" ~ 1417309,
                                              Survey == "REMMOA_ANT" ~ 163010,
                                              Survey == "REMMOA_GUY" ~ 110594,
                                              Survey == "GoAlaska" ~ 22749 + 60051 + 45377 + 36776
                  )) |> 
    dplyr::group_by(Geo_area, Survey) |>
    dplyr::summarise(Abund_tot = sum(Abund), 
                     Dens_tot = Abund_tot/unique(Surf_tot),
                     Var_tot = sum(Var_abund), 
                     CV_tot = sqrt(Var_tot)/Abund_tot)
}



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
                  ) 
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
# data from report 
data_from_data_Bala_acu <- function() {
  
  # here is where verify_totals should be called
  #verify_totals(
  tibble::tribble(~ Geo_area, ~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
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
  #) # end parenthesis for verify totals
}