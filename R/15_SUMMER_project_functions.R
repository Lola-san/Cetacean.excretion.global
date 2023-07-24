################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# 15_SUMMER_project_functions.R
#
# Script with functions only related to the SUMMER project
# including creation of input data and creation of outputs
################################################################################

################################################################################
### INPUT ABUNDANCE DATA with separation between West and East Mediterranean Sea
################################################################################

# ratio abundance for un ID sp ASI with separation W/E
compute_ratio_ASI_SUMMER <- function(ASI_tib) {
  # ASI_tib is the tibble resulting from the call of load_ASI()
  # delete unwanted variables 
  ASI_Dd_Sc <- ASI_tib |>
    dplyr::select(region, survey, strate, taxon, group, family, species, speciesNam, speciesLat, podSize) |>
    dplyr::filter(speciesNam %in% c("Striped dolphin", "Striped dolphin / Common dolphin", "Common dolphin")) |>
    dplyr::filter(!is.na(strate), !(strate %in% c("01c", "01d"))) |> # delete lines where strate is NA
    dplyr::mutate(Eco_area = dplyr::case_when(strate %in% c("02", "21", "20", 
                                                            "16", "17", "23a", 
                                                            "23b", "22a") ~ "shelf",  # define Eco areas
                                              strate %in% c("03","04", "05", 
                                                            "06", "07", "08a", "08b",
                                                            "09", "10", "11", "12", 
                                                            "13", "14", "15", "18", 
                                                            "19", "22b", "24", "22c", 
                                                            "22d", "29a", "29b", 
                                                            "29c", "30", "31", "32") ~ "oceanic"), 
                  Geo_area = dplyr::case_when(strate %in% c("02", "21", "20", # shelf MedW
                                                            "03","04", "05", 
                                                            "06", "07", "08a", "08b",
                                                            "09", "10", "11", "12", 
                                                            "13", "14", "15" # oceanic MedW
                                                            ) ~ "Med_W",  # define Eco areas
                                              strate %in% c("16", "17", "23a", 
                                                            "23b", "22a", # shelf Med E
                                                            "18", "19", "22b", 
                                                            "24", "22c", "22d", 
                                                            "29a", "29b", "29c", 
                                                            "30", "31", "32" # oceanic Med E
                                                            ) ~ "Med_E")) |>
    dplyr::group_by(Geo_area, Eco_area, speciesNam) |>
    dplyr::filter(speciesNam != "Striped dolphin / Common dolphin") |>
    dplyr::mutate(speciesNam = dplyr::case_when(speciesNam == "Striped dolphin" ~ "Stenella coeruleoalba", 
                                                speciesNam == "Common dolphin" ~ "Delphinus delphis")) |>
    dplyr::summarise(nobs = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::mutate(sumnobs = sum(nobs),
                  ratio = nobs/(sum(nobs))) |>
    dplyr::rename(Species = speciesNam)
  
  ## same for beaked whales 
  
  ASI_BW <- ASI_tib |>
    dplyr::filter(speciesNam %in% c("Ziphiid sp. (Beaked whale)", "Cuvier's beaked whale", "Mesoplodont whales sp")) |>
    dplyr::filter(!is.na(strate), !(strate %in% c("01c", "01d"))) |> # delete lines where strate is NA
    dplyr::mutate(Eco_area = dplyr::case_when(strate %in% c("02", "21", "20", 
                                                            "16", "17", "23a", 
                                                            "23b", "22a", "22b") ~ "shelf",  # define Eco areas
                                              strate %in% c("03","04", "05", 
                                                            "06", "07", "08a", "08b",
                                                            "09", "10", "11", "12", 
                                                            "13", "14", "15", "18", 
                                                            "19", "22b", "24", "22c", 
                                                            "22d", "29a", "29b", 
                                                            "29c", "30", "31", "32") ~ "oceanic"),
                  Geo_area = dplyr::case_when(strate %in% c("02", "21", "20", # shelf MedW
                                                            "03","04", "05", 
                                                            "06", "07", "08a", "08b",
                                                            "09", "10", "11", "12", 
                                                            "13", "14", "15" # oceanic MedW
                  ) ~ "Med_W",  # define Eco areas
                  strate %in% c("16", "17", "23a", 
                                "23b", "22a", # shelf Med E
                                "18", "19", "22b", 
                                "24", "22c", "22d", 
                                "29a", "29b", "29c", 
                                "30", "31", "32" # oceanic Med E
                  ) ~ "Med_E")) |>
    dplyr::group_by(Geo_area, Eco_area, speciesNam) |>
    dplyr::filter(speciesNam != "Ziphiid sp. (Beaked whale)") |>
    dplyr::mutate(speciesNam = dplyr::case_when(speciesNam == "Cuvier's beaked whale" ~ "Ziphius cavirostris", 
                                                speciesNam == "Mesoplodont whales sp" ~ "Mesoplodon spp")) |>
    dplyr::summarise(nobs = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::mutate(sumnobs = sum(nobs),
                  ratio = nobs/(sum(nobs))) |>
    dplyr::rename(Species = speciesNam)
  
  rbind(ASI_Dd_Sc, ASI_BW)
}

#'
#'
#'
# bind ratio abundance for NEA (not changed, in 00_ratio_abundances.R), and 
# the new version for Med with separation E/W
bind_ratio_SUMMER <- function(ratio_NEA_tib, ratio_ASI_tib_SUMMER) {
  rbind(ratio_NEA_tib, ratio_ASI_tib_SUMMER)
}



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
    dplyr::filter(# keep only areas of interest for SUMMER
      Block %in% c("AA", "AB", "AC",
                   "B", "C","D", "E", 
                   "F","G", "H", "I",
                   "J", "K", "L", "M", 
                   "N", "O", "P", "Q", 
                   "R", "S", "T", "U", 
                   "V", # shelf SCANS III NEAtlantic
                   "8","9", "11", "12", "13", # Oceanic SCANS III NEAtlantic
                   "Alboran", "SCMed","Adriatic", 
                   "Aegean", # Shelf Med ASI
                   "NWMed", "SWMed","PelagosW", 
                   "PelagosE", "Tyrrhenian", "Ionian",
                   "NEMed", "EMed", # oceanic Med ASI
                   "S1_N", "S2_N", "S3_N",
                   "S4", "S6", "S7", "S8",  # Shelf Observe (Ireland)
                   "S1_O", "S2_O", "S3_O", # oceanic Observe (Ireland)
                   "IR", "IE", "IC", "CG", # Shelf t-NASS (Central N Atla)
                   "FC", "FW", "IG", "IP",
                   "IQ", "IW" # oceanic t-NASS (Central N Atla)
      ), 
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
                                                           "IQ", "IW") ~ "oceanic" # Oceanic t-NASS
                  )) |>
    dplyr::mutate(Geo_area = dplyr::case_when(Block %in% c("Ionian", "NEMed", "EMed", 
                                                           "Aegean", "Adriatic") ~ "Med_E", 
                                              Block %in% c("NWMed", "SWMed","PelagosW", 
                                                           "PelagosE", "Tyrrhenian", 
                                                           "Alboran", "SCMed") ~ "Med_W",
                                              TRUE ~ Geo_area)) |> 
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
                                              Geo_area == "Med_E" & Eco_area == "shelf" ~ 135783 + 191150,
                                              Geo_area == "Med_E" & Eco_area == "oceanic" ~ 358402+161669+107687,
                                              Geo_area == "Med_W" & Eco_area == "shelf" ~ 28071+152961,
                                              Geo_area == "Med_W" & Eco_area == "oceanic" ~ 279415+134760+56756+31076+231298,
                                              Geo_area == "NAtlantic" & Eco_area == "shelf" ~ 743874,
                                              Geo_area == "NAtlantic" & Eco_area == "oceanic" ~ 2046837
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
                                          "Med_W", "Atlantic", 33720, 35293, 1.0467, 0.5412,
                                          "Med_W", "Alboran", 28071, 25855, 0.9211, 0.6874,  
                                          "Med_W", "SWMed", 279415, 0, 0, 0,
                                          "Med_W", "NWMed", 134760, 0, 0, 0, 
                                          "Med_W", "PelagosW", 56756, 0, 0, 0, 
                                          "Med_W", "PelagosE", 31076, 0, 0, 0,
                                          "Med_W", "Tyrrhenian", 231298, 521, 0.0023, 1.0088,
                                          "Med_W","SCMed", 152961, 286, 0.0019, 0.8587,
                                          "Med_E", "Adriatic", 135783, 0, 0, 0, 
                                          "Med_E", "Ionian", 358402, 404, 0.0011, 1.0030,
                                          "Med_E", "Aegean", 191150, 2759, 0.0144, 0.9089,
                                          "Med_E", "NEMed", 161669, 1230, 0.0076, 0.9862,
                                          "Med_E", "EMed", 107687, 0, 0, 0
  )

  # focus on blocks of interest and computations
  Abund_df_Delp_del <- build_sp_tib_SUMMER(original_df_Delp_del, "Delphinus delphis", "Delp_del")
  
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
                                          "Med_W", "Atlantic", 33720, 6268, 0.1859, 0.9836,
                                          "Med_W", "Alboran", 28071, 37848, 1.3483, 0.4935,
                                          "Med_W", "SWMed", 279415, 164079, 0.5872, 0.2443,
                                          "Med_W", "NWMed", 134760, 86386, 0.6410, 0.2868,
                                          "Med_W", "PelagosW", 56756, 29154, 0.5137, 0.4049,
                                          "Med_W", "PelagosE", 31076, 394, 0.0127, 0.9877,
                                          "Med_W", "Tyrrhenian", 231298, 44367, 0.1918, 0.2339,
                                          "Med_W","SCMed", 152961, 199, 0.0013, 0.9848,
                                          "Med_E", "Adriatic", 135783, 10264, 0.0756, 0.5427,
                                          "Med_E", "Ionian", 358402, 37819, 0.1055, 0.3438,
                                          "Med_E", "Aegean", 191150, 8205, 0.0429, 0.5810,
                                          "Med_E", "NEMed", 161669, 1673, 0.0103, 1.0014,
                                          "Med_E", "EMed", 107687, 0, 0, 0
  )
  
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
                                          "Med_W", "Atlantic", 33720, 6699, 0.1987, 0.6194,
                                          "Med_W", "Alboran", 28071, 69412, 2.4727, 0.6199,
                                          "Med_W", "SWMed", 279415, 81284, 0.2909, 0.4153,
                                          "Med_W", "NWMed", 134760, 29382, 0.2180, 0.3205, 
                                          "Med_W", "PelagosW", 56756, 6333, 0.1116, 0.4572, 
                                          "Med_W", "PelagosE", 31076, 342, 0.0110, 0.9860, 
                                          "Med_W", "Tyrrhenian", 231298, 5934, 0.0257, 0.6053, 
                                          "Med_W","SCMed", 152961, 0, 0, 0,
                                          "Med_E", "Adriatic", 135783, 0, 0, 0,
                                          "Med_E", "Ionian", 358402, 5296, 0.0148, 0.5117,
                                          "Med_E", "Aegean", 191150, 8064, 0.0422, 0.4660,
                                          "Med_E", "NEMed", 161669, 0, 0, 0,
                                          "Med_E", "EMed", 107687, 0, 0, 0
  ) 
  
  # focus on blocks of interest and computations
  Abund_df_Dd_Dc_Sc <- build_sp_tib_SUMMER(original_df_Dd_Dc_Sc, "Und Dd/Dc/Sc", "Und Dd/Dc/Sc") 
  
  
  ####  2 - concatenate everything with ratio of observation and 
  ########## compute total abundances 
  
  rbind(Abund_df_Sten_coe, Abund_df_Delp_del) |>
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
build_sp_tib_BW_SUMMER <- function(ratio_others_tib) {
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
                                    "Med_W", "Atlantic", 33720, 498, 0.0148, 0.6598,
                                    "Med_W", "Alboran", 28071, 271, 0.096, 1.0196,
                                    "Med_W", "SWMed", 279415, 75, 0.0003, 1.0062,
                                    "Med_W", "NWMed", 134760, 944, 0.0070, 0.8433,
                                    "Med_W", "PelagosW", 56756, 0, 0, 0,
                                    "Med_W", "PelagosE", 31076, 0, 0, 0,
                                    "Med_W", "Tyrrhenian", 231298, 181, 0.0008, 0.7671,
                                    "Med_W","SCMed", 152961, 246, 0.0016, 0.7996,
                                    "Med_E", "Adriatic", 135783, 66, 0.0005, 1.0120,
                                    "Med_E", "Ionian", 358402, 504, 0.0014, 0.5266,
                                    "Med_E", "Aegean", 191150, 193, 0.0010, 0.7679,
                                    "Med_E", "NEMed", 161669, 0, 0, 0,
                                    "Med_E", "EMed", 107687, 420, 0.0039, 0.9793
  )

  # focus on blocks of interest and computations
  Abund_df_BW <- build_sp_tib_SUMMER(original_df_BW, "All beaked whales","BW")
  
  # add species details
  Abund_Zc_Ha_Meso_spp <- Abund_df_BW |>
    dplyr::left_join(ratio_others_tib |>
                       dplyr::filter(Species %in% c("Hyperoodon ampullatus", 
                                                    "Ziphius cavirostris", 
                                                    "Mesoplodon spp")), 
                     by = c("Geo_area", "Eco_area")) |>
    dplyr::mutate(ratio = dplyr::case_when(is.na(ratio) ~ 0, 
                                           TRUE ~ ratio), 
                  Code_sp = dplyr::case_when(Species.y == "Mesoplodon spp" ~ "Meso_spp", 
                                             Species.y == "Hyperoodon ampullatus" ~ "Hype_amp",
                                             Species.y == "Ziphius cavirostris" ~ "Ziph_cav"),
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
  
  ######### 2 - For when abundance was already dispatched between species 
  # including the und. species
  
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
  
  # focus on blocks of interest and computations
  Abund_df_Hype_amp <- build_sp_tib(original_df_Hype_amp, 
                                    "Hyperoodon ampullatus", "Hype_amp")
  
  ##### concatenate everything 
  rbind(Abund_Zc_Ha_Meso_spp, # from #1
        Abund_df_Hype_amp # from #2
  )
  
}


################################################################################
### OUTPUTS
################################################################################

format_names_SUMMER <- function(output_tib) {
  output_tib |>
    dplyr::mutate(Geo_area = factor(dplyr::case_when(Geo_area == "Med_E" ~ "Eastern Mediterranean Sea",
                                              Geo_area == "Med_W" ~ "Western Mediterranean Sea",
                                              Geo_area == "NAtlantic" ~ "Central North Atlantic",
                                              Geo_area == "NEAtlantic" ~ "Northeast Atlantic"), 
                                    levels = c("Central North Atlantic", 
                                               "Northeast Atlantic",
                                               "Western Mediterranean Sea", 
                                               "Eastern Mediterranean Sea"))) 
}



#'
#'
#'
#'
# stack barplot to show on the map, one per area
fig_SUMMER_taxa_contrib_stacked_barplot <- function(output_tib_NEA,
                                                    output_tib_CNA, 
                                                    output_tib_Med
                                                    # output_tib are area specific tables
                                                    # generated using taxa_contribution_total
                                                    # function
) {
  
  rbind(output_tib_NEA,
        output_tib_CNA, 
        output_tib_Med) |> 
    dplyr::filter(Element != "As") |>
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn",
                                              "Se", "Zn", "Co", "As")), 
                  ratio_contribution = ratio_contribution*100) |>
    ggplot2::ggplot(ggplot2::aes(x = Element, y = ratio_contribution, fill = Eco_gp)) +
    ggplot2::geom_col(position = "stack", 
                      width = 0.7) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 3) +
    ggplot2::xlab("") +
    ggplot2::ylab("Relative contribution (in %)") +
    ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                          `Deep divers` = "slategray3", 
                                          `Baleen whales` = "#cf7474ff"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::geom_text(ggplot2::aes(label = round(ratio_contribution, 0)), 
                       vjust = 1,
                       position = "stack",
                       hjust = 0.5,
                       colour = "black") +
    ggplot2::theme(legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 12), 
                   axis.title.y = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/figures/SUMMER_contrib_taxa.jpg"), 
                  scale = 1, 
                  width = 8, 
                  height = 4)
  
}


#'
#'
#'
#'
#'
# stack barplot to show on the map, one per area
fig_SUMMER_taxa_contrib_stacked_barplot_oceanic_only <- function(output_tib_hab_NEA,
                                                                 output_tib_hab_CNA, 
                                                                 output_tib_hab_Med
                                                                 # output_tib_hab are area specific tables
                                                                 # generated using taxa_contribution_hab
                                                                 # function
) {
  
  rbind(output_tib_hab_NEA,
        output_tib_hab_CNA, 
        output_tib_hab_Med) |> 
    dplyr::filter(Element != "As", 
                  Eco_area == "oceanic") |>
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn",
                                              "Se", "Zn", "Co", "As")), 
                  ratio_contribution = ratio_contribution*100) |>
    ggplot2::ggplot(ggplot2::aes(x = Element, y = ratio_contribution, fill = Eco_gp)) +
    ggplot2::geom_col(position = "stack", 
                      width = 0.7) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 3) +
    ggplot2::xlab("") +
    ggplot2::ylab("Relative contribution (in %)") +
    ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                          `Deep divers` = "slategray3", 
                                          `Baleen whales` = "#cf7474ff"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::geom_text(ggplot2::aes(label = round(ratio_contribution, 0)), 
                       vjust = 1,
                       position = "stack",
                       hjust = 0.5,
                       colour = "black") +
    ggplot2::theme(legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 11),
                   axis.text.y = ggplot2::element_text(size = 12), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_contrib_taxa_oceanic_only.jpg"), 
                  scale = 1, 
                  width = 10, 
                  height = 6)
  
}



# SUMMER table with annual consumption of each prey group per species
#'
#'
#'
#'
#'
#' 
SUMMER_table_conso_sp <- function(output_tib) {
  
  options(scipen = 999)
  
  table <- output_tib |>
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic") |>
    dplyr::ungroup() |>
    dplyr::select(Species, Eco_gp, Geo_area, conso_diet) |>
    tidyr::unnest(conso_diet) |>
    tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                        names_to = "Prey group",
                        values_to = "value") |>
    # change from kg/yr to t/yr
    dplyr::mutate(value = value*1e-3) |>
    dplyr::group_by(Geo_area, Eco_gp, Species, `Prey group`) |>
    dplyr::summarize(mean = round(mean(value), 0),
                     sd = round(sd(value), 0)) |>
    tidyr::pivot_wider(names_from = `Prey group`, 
                       values_from = c(mean, sd), 
                       names_sep = "_") |>
    dplyr::bind_rows(output_tib |>
                       dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                                     "Northeast Atlantic", 
                                                     "Eastern Mediterranean Sea", 
                                                     "Western Mediterranean Sea"), 
                                     Eco_area == "oceanic") |>
                       dplyr::ungroup() |>
                       dplyr::select(Geo_area, conso_diet) |>
                       tidyr::unnest(conso_diet) |>
                       tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                                           names_to = "Prey group",
                                           values_to = "value") |>
                       # change from kg/yr to t/yr
                       dplyr::mutate(value = value*1e-3) |>
                       dplyr::group_by(Geo_area, `Prey group`) |>
                       dplyr::summarize(mean = round(mean(value), 0),
                                        sd = round(sd(value), 0)) |>
                       dplyr::mutate(Species = "All species", 
                                     Eco_gp = "All species") |>
                       dplyr::select(Geo_area, Eco_gp, Species, `Prey group`, mean, sd) |>
                       tidyr::pivot_wider(names_from = `Prey group`, 
                                          values_from = c(mean, sd), 
                                          names_sep = "_") )  
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/SUMMER/SUMMER_conso_prey_group_oceanic.xlsx"))
  
  
}


# SUMMER table with annual needs in NRJ per species
#'
#'
#'
#'
#'
#' 
SUMMER_table_needs_sp <- function(output_tib) {
  
  options(scipen = 999)
  
  table <- output_tib |> # needs in kJ per species per yr in each area
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic") |>
    dplyr::ungroup() |>
    dplyr::select(Species, Eco_gp, Geo_area, Surf_tot, Needs_pop) |>
    tidyr::unnest(Needs_pop) |>
    dplyr::group_by(Geo_area, Surf_tot, Eco_gp, Species) |>
    dplyr::summarize(mean_kJ_per_yr = round(mean(Needs_pop), 0),
                     sd = round(sd(Needs_pop), 0)) |>
    dplyr::bind_rows(output_tib |> # needs in kJ per species per yr in each area
                       dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                                     "Northeast Atlantic", 
                                                     "Eastern Mediterranean Sea", 
                                                     "Western Mediterranean Sea"), 
                                     Eco_area == "oceanic") |>
                       dplyr::ungroup() |>
                       dplyr::select(Species, Eco_gp, Geo_area, Surf_tot, Needs_pop) |>
                       tidyr::unnest(Needs_pop) |>
                       dplyr::group_by(Geo_area, Surf_tot, Eco_gp) |>
                       dplyr::summarize(mean_kJ_per_yr = round(mean(Needs_pop), 0),
                                        sd = round(sd(Needs_pop), 0)) |> 
                       dplyr::mutate(Species = "All species") |>
                       dplyr::select(Geo_area, Eco_gp, Species, 
                                     mean_kJ_per_yr, sd)) 
    
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/SUMMER/SUMMER_needs_sp.xlsx"))
  
  
}


# SUMMER barplot with annual needs in tons per species
#'
#'
#'
#'
#'
#' 
SUMMER_barplot_needs_sp <- function(output_tib) {
  
  #options(scipen = 999)
  
  output_tib |> # needs in kJ per species per yr in each area
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic") |>
    dplyr::ungroup() |>
    dplyr::select(Species, Eco_gp, Geo_area, Needs_pop) |>
    tidyr::unnest(Needs_pop) |>
    dplyr::group_by(Geo_area, Species) |>
    dplyr::summarize(mean_kJ_per_yr = round(mean(Needs_pop), 0),
                     `10_quant` = round(quantile(Needs_pop, probs = c(0.10)), 
                                        0), 
                     `90_quant` = round(quantile(Needs_pop, probs = c(0.90)), 
                                        0)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Species, y = mean_kJ_per_yr,
                                   fill = Species),
                      stat = "identity", 
                      size = 2,
                      position = ggplot2::position_dodge(1), 
                      alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = Species, y = mean_kJ_per_yr, 
                                     color = Species), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Species, 
                                        ymin = `10_quant`,
                                        ymax = `90_quant`, 
                                        color = Species),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 2, scales = "free") +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual NRJ needs (in kJ/yr)") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::scale_color_manual(values = c("#274637FF", "#4C413FFF", "#5A6F80FF", 
                                           "#278B9AFF", "#E75B64FF", "#DE7862FF", 
                                           "#D8AF39FF", "#44A57CFF", "#E8C4A2FF", 
                                           "#14191FFF", "#1D2645FF", "#AE93BEFF",
                                           "#403369FF", "#58A449FF", "#B4DAE5FF", 
                                           "#F0D77BFF", "#4D6D93FF"), 
                                name = "") +
    ggplot2::scale_fill_manual(values = c("#274637FF", "#4C413FFF", "#5A6F80FF", 
                                          "#278B9AFF", "#E75B64FF", "#DE7862FF", 
                                          "#D8AF39FF", "#44A57CFF", "#E8C4A2FF", 
                                          "#14191FFF", "#1D2645FF", "#AE93BEFF",
                                          "#403369FF", "#58A449FF", "#B4DAE5FF", 
                                          "#F0D77BFF", "#4D6D93FF"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_needs_sp.jpg"), 
                  scale = 1, 
                  width = 11, 
                  height = 4)
  
  
}


# SUMMER barplot with annual needs in tons per group
#'
#'
#'
#'
#'
#' 
SUMMER_barplot_needs_gp <- function(output_tib) {
  
  #options(scipen = 999)
  
  output_tib |> # needs in kJ per species per yr in each area
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic") |>
    dplyr::ungroup() |>
    dplyr::select(Species, Eco_gp, Geo_area, Needs_pop) |>
    tidyr::unnest(Needs_pop) |>
    dplyr::group_by(Geo_area, Eco_gp) |>
    dplyr::summarize(mean_kJ_per_yr = round(mean(Needs_pop), 0),
                     `10_quant` = round(quantile(Needs_pop, probs = c(0.10)), 
                                        0), 
                     `90_quant` = round(quantile(Needs_pop, probs = c(0.90)), 
                                        0)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Eco_gp, y = mean_kJ_per_yr,
                                   fill = Eco_gp),
                      stat = "identity", 
                      size = 2,
                      position = ggplot2::position_dodge(1), 
                      alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = Eco_gp, y = mean_kJ_per_yr, 
                                     color = Eco_gp), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Eco_gp, 
                                        ymin = `10_quant`,
                                        ymax = `90_quant`, 
                                        color = Eco_gp),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 2, scales = "free") +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual NRJ needs (in kJ/yr)") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::scale_color_manual(values = c(`Small delphinids` = "#365579ff", 
                                           `Deep divers` = "slategray3", 
                                           `Baleen whales` = "#cf7474ff"), 
                                name = "") +
    ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                          `Deep divers` = "slategray3", 
                                          `Baleen whales` = "#cf7474ff"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_needs_gps.jpg"), 
                  scale = 1, 
                  width = 10, 
                  height = 4)
  
  
}


# SUMMER barplot with annual consumption of each prey group per species
#'
#'
#'
#'
#'
# all taxa, all prey groups
SUMMER_barplot_conso_gp <- function(output_tib) {
  
  options(scipen = 999)
  
  output_tib |>
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic") |>
    dplyr::ungroup() |>
    dplyr::select(Eco_gp, Geo_area, conso_diet) |>
    tidyr::unnest(conso_diet) |>
    tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                        names_to = "Prey group",
                        values_to = "value") |>
    # change from kg/yr to t/yr
    dplyr::mutate(value = value*1e-3, 
    ) |>
    dplyr::group_by(Geo_area, Eco_gp, `Prey group`) |>
    dplyr::summarize(mean = round(mean(value), 0),
                     sd = round(sd(value), 0), 
                     `2.5_quant` = round(quantile(value, probs = c(0.025)), 
                                         0), 
                     `97.5_quant` = round(quantile(value, probs = c(0.975)), 
                                          0)) |>
    # change 0 values to 1 so that it doesnot make error with log10 trans
    dplyr::mutate(mean = dplyr::case_when(mean == 0 ~ 1, 
                                          TRUE ~ mean), 
                  `2.5_quant` = dplyr::case_when(`2.5_quant` == 0 ~ 1, 
                                                 TRUE ~ `2.5_quant`), 
                  `97.5_quant` = dplyr::case_when(`97.5_quant` == 0 ~ 1, 
                                                  TRUE ~ `97.5_quant`)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = `Prey group`, y = mean,
                                   fill = Eco_gp),
                      stat = "identity", 
                      size = 2,
                      position = ggplot2::position_dodge(1), 
                      alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = `Prey group`, y = mean, 
                                     color = Eco_gp), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = `Prey group`, 
                                        ymin = `2.5_quant`,
                                        ymax = `97.5_quant`, 
                                        color = Eco_gp),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 2) +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual consumption (in t/yr)") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::scale_color_manual(values = c(`Small delphinids` = "#365579ff", 
                                           `Deep divers` = "slategray3", 
                                           `Baleen whales` = "#cf7474ff"), 
                                name = "") +
    ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                          `Deep divers` = "slategray3", 
                                          `Baleen whales` = "#cf7474ff"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_conso_all_taxa_and_prey_gps.jpg"), 
                  scale = 1, 
                  width = 11, 
                  height = 9)
  
  
}





# SUMMER barplot with annual consumption of each prey group per species
#'
#'
#'
#'
#'
# all taxa, all prey groups
SUMMER_barplot_conso_gp <- function(output_tib) {
  
  options(scipen = 999)
  
  output_tib |>
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic") |>
    dplyr::ungroup() |>
    dplyr::select(Eco_gp, Geo_area, conso_diet) |>
    tidyr::unnest(conso_diet) |>
    tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                        names_to = "Prey group",
                        values_to = "value") |>
    # change from kg/yr to t/yr
    dplyr::mutate(value = value*1e-3, 
                  ) |>
    dplyr::group_by(Geo_area, Eco_gp, `Prey group`) |>
    dplyr::summarize(mean = round(mean(value), 0),
                     sd = round(sd(value), 0), 
                     `2.5_quant` = round(quantile(value, probs = c(0.025)), 
                                         0), 
                     `97.5_quant` = round(quantile(value, probs = c(0.975)), 
                                          0)) |>
    # change 0 values to 1 so that it doesnot make error with log10 trans
    dplyr::mutate(mean = dplyr::case_when(mean == 0 ~ 1, 
                                          TRUE ~ mean), 
                  `2.5_quant` = dplyr::case_when(`2.5_quant` == 0 ~ 1, 
                                          TRUE ~ `2.5_quant`), 
                  `97.5_quant` = dplyr::case_when(`97.5_quant` == 0 ~ 1, 
                                          TRUE ~ `97.5_quant`)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = `Prey group`, y = mean,
                                 fill = Eco_gp),
                    stat = "identity", 
                    size = 2,
                    position = ggplot2::position_dodge(1), 
                    alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = `Prey group`, y = mean, 
                                     color = Eco_gp), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = `Prey group`, 
                                        ymin = `2.5_quant`,
                                        ymax = `97.5_quant`, 
                                        color = Eco_gp),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 2) +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual consumption (in t/yr)") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::scale_color_manual(values = c(`Small delphinids` = "#365579ff", 
                                          `Deep divers` = "slategray3", 
                                          `Baleen whales` = "#cf7474ff"), 
                               name = "") +
    ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                          `Deep divers` = "slategray3", 
                                          `Baleen whales` = "#cf7474ff"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_conso_all_taxa_and_prey_gps.jpg"), 
                  scale = 1, 
                  width = 11, 
                  height = 9)
  
  
}



# SUMMER barplot with annual consumption of each prey group per species
#'
#'
#'
#'
#'
# all taxa, lower the nb of prey groups
SUMMER_barplot_conso_gp_simplified <- function(output_tib) {
  
  options(scipen = 999)
  
  output_tib |>
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic") |>
    dplyr::ungroup() |>
    dplyr::select(Eco_gp, Geo_area, conso_diet) |>
    tidyr::unnest(conso_diet) |>
    tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                        names_to = "Prey group",
                        values_to = "value") |>
    # change from kg/yr to t/yr
    dplyr::mutate(value = value*1e-3, 
                  `Prey group` = factor(dplyr::case_when(
                    `Prey group` %in% c("Muscular pelagic cephalopods",
                                        "Gelatinous pelagic cephalopods") ~ "Pelagic cephalopods", 
                    `Prey group` == "Bottom cephalopods" ~ "Demersal cephalopods",
                    `Prey group` %in% c("Miscellanous benthodemersal fish",
                                        "Large demersal energy-lean fish", 
                                        "Large demersal energy-rich fish") ~ "Demersal fish",
                    `Prey group` %in% c("Small schooling energy-lean fish",
                                        "Small schooling energy-rich fish", 
                                        "Miscellanous pelagic fish") ~ "Pelagic fish",
                    `Prey group` %in% c("Fish undetermined",
                                        "Cephalopod undetermined") ~ "Other", 
                    TRUE ~ `Prey group`
                  ), 
                  levels = c("Other", "Crustaceans", 
                             "Zooplankton", "Pelagic fish", "Demersal fish", 
                             "Pelagic cephalopods", "Demersal cephalopods"
                             ))) |>
    dplyr::group_by(Geo_area, Eco_gp, `Prey group`) |>
    dplyr::summarize(mean = round(mean(value), 0),
                     sd = round(sd(value), 0), 
                     `2.5_quant` = round(quantile(value, probs = c(0.025)), 
                                         0), 
                     `97.5_quant` = round(quantile(value, probs = c(0.975)), 
                                          0)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = `Prey group`, y = mean,
                                   fill = Eco_gp),
                      stat = "identity", 
                      size = 2,
                      position = ggplot2::position_dodge(1), 
                      alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = `Prey group`, y = mean, 
                                     color = Eco_gp), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = `Prey group`, 
                                        ymin = `2.5_quant`,
                                        ymax = `97.5_quant`, 
                                        color = Eco_gp),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 2) +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual consumption (in t/yr)") +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c(`Small delphinids` = "#365579ff", 
                                           `Deep divers` = "slategray3", 
                                           `Baleen whales` = "#cf7474ff"), 
                                name = "") +
    ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                          `Deep divers` = "slategray3", 
                                          `Baleen whales` = "#cf7474ff"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_conso_all_taxa_few_prey_gps.jpg"), 
                  scale = 1, 
                  width = 11, 
                  height = 9)
  
  
}


# SUMMER barplot with annual consumption of each prey group per species
#'
#'
#'
#'
#'
# SD and DD only, lower the nb of prey groups
SUMMER_barplot_conso_gp_simplified_SD_DD <- function(output_tib) {
  
  #options(scipen = 999)
  
  output_tib |>
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic", 
                  Eco_gp != "Baleen whales") |>
    dplyr::ungroup() |>
    dplyr::select(Eco_gp, Geo_area, conso_diet) |>
    tidyr::unnest(conso_diet) |>
    tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                        names_to = "Prey group",
                        values_to = "value") |>
    # change from kg/yr to t/yr
    dplyr::mutate(value = value*1e-3, 
                  `Prey group` = factor(dplyr::case_when(
                    `Prey group` %in% c("Muscular pelagic cephalopods",
                                        "Gelatinous pelagic cephalopods") ~ "Pelagic cephalopods", 
                    `Prey group` == "Bottom cephalopods" ~ "Demersal cephalopods",
                    `Prey group` %in% c("Miscellanous benthodemersal fish",
                                        "Large demersal energy-lean fish", 
                                        "Large demersal energy-rich fish") ~ "Demersal fish",
                    `Prey group` %in% c("Small schooling energy-lean fish",
                                        "Small schooling energy-rich fish", 
                                        "Miscellanous pelagic fish") ~ "Pelagic fish",
                    `Prey group` %in% c("Fish undetermined",
                                        "Cephalopod undetermined") ~ "Other", 
                    TRUE ~ `Prey group`
                  ), 
                  levels = c("Other", "Crustaceans", 
                             "Zooplankton", "Pelagic fish", "Demersal fish", 
                             "Pelagic cephalopods", "Demersal cephalopods"
                  ))) |>
    dplyr::filter(!(`Prey group`  %in% c("Zooplankton", 
                                         "Other"))) |>
    dplyr::group_by(Geo_area, Eco_gp, `Prey group`) |>
    dplyr::summarize(mean = round(mean(value), 0),
                     sd = round(sd(value), 0), 
                     `10_quant` = round(quantile(value, probs = c(0.10)), 
                                         0), 
                     `90_quant` = round(quantile(value, probs = c(0.90)), 
                                          0)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = `Prey group`, y = mean,
                                   fill = Eco_gp),
                      stat = "identity", 
                      size = 2,
                      position = ggplot2::position_dodge(1), 
                      alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = `Prey group`, y = mean, 
                                     color = Eco_gp), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = `Prey group`, 
                                        ymin = `10_quant`,
                                        ymax = `90_quant`, 
                                        color = Eco_gp),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 2, 
                        scales = "free_x") +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual consumption (in t/yr)") +
    ggplot2::coord_flip() +
    #ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::scale_color_manual(values = c(`Small delphinids` = "#365579ff", 
                                           `Deep divers` = "slategray3"), 
                                name = "") +
    ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                          `Deep divers` = "slategray3"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_conso_SD_DD_few_prey_gps.jpg"), 
                  scale = 1, 
                  width = 12, 
                  height = 5)
  
  
}



# SUMMER barplot with annual consumption of each prey group per species
#'
#'
#'
#'
#'
# SD and DD only but per sp, only for Demersal fish and ceph and Pelagic
# fish and ceph
SUMMER_barplot_conso_sp_simplified_SD_DD <- function(output_tib) {
  
  #options(scipen = 999)
  
  output_tib |>
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic", 
                  Eco_gp != "Baleen whales") |>
    dplyr::ungroup() |>
    dplyr::select(Species, Geo_area, conso_diet) |>
    tidyr::unnest(conso_diet) |>
    tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                        names_to = "Prey group",
                        values_to = "value") |>
    # change from kg/yr to t/yr
    dplyr::mutate(value = value*1e-3, 
                  `Prey group` = factor(dplyr::case_when(
                    `Prey group` %in% c("Muscular pelagic cephalopods",
                                        "Gelatinous pelagic cephalopods") ~ "Pelagic cephalopods", 
                    `Prey group` == "Bottom cephalopods" ~ "Demersal cephalopods",
                    `Prey group` %in% c("Miscellanous benthodemersal fish",
                                        "Large demersal energy-lean fish", 
                                        "Large demersal energy-rich fish") ~ "Demersal fish",
                    `Prey group` %in% c("Small schooling energy-lean fish",
                                        "Small schooling energy-rich fish", 
                                        "Miscellanous pelagic fish") ~ "Pelagic fish",
                    `Prey group` %in% c("Fish undetermined",
                                        "Cephalopod undetermined") ~ "Other", 
                    TRUE ~ `Prey group`
                  ), 
                  levels = c("Other", "Crustaceans", 
                             "Zooplankton", "Pelagic fish", "Demersal fish", 
                             "Pelagic cephalopods", "Demersal cephalopods"
                  ))) |>
    dplyr::filter(`Prey group` %in% c("Pelagic cephalopods", 
                                      "Pelagic fish")) |>
    dplyr::group_by(Geo_area, Species, `Prey group`) |>
    dplyr::summarize(mean = round(mean(value), 0),
                     sd = round(sd(value), 0), 
                     `10_quant` = round(quantile(value, probs = c(0.10)), 
                                         0), 
                     `90_quant` = round(quantile(value, probs = c(0.90)), 
                                          0)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = `Prey group`, y = mean,
                                   fill = Species),
                      stat = "identity", 
                      size = 2,
                      position = ggplot2::position_dodge(1), 
                      alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = `Prey group`, y = mean, 
                                     color = Species), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = `Prey group`, 
                                        ymin = `10_quant`,
                                        ymax = `90_quant`, 
                                        color = Species),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 2, 
                        scales = "free_x") +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual consumption (in t/yr)") +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                           "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                           "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                           "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                           "#F0D77BFF"), 
                                name = "") +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_conso_SD_DD_sp.jpg"), 
                  scale = 1, 
                  width = 12, 
                  height = 5)
  
  
}





# SUMMER barplot with annual consumption of each prey group per species
#'
#'
#'
#'
#'
# BW only, lower the nb of prey groups
SUMMER_barplot_conso_gp_simplified_BW <- function(output_tib) {
  
  #options(scipen = 999)
  
  output_tib |>
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic",  
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic", 
                  Eco_gp == "Baleen whales") |>
    dplyr::ungroup() |>
    dplyr::select(Eco_gp, Geo_area, Species, conso_diet) |>
    tidyr::unnest(conso_diet) |>
    tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                        names_to = "Prey group",
                        values_to = "value") |>
    # change from kg/yr to t/yr
    dplyr::mutate(value = value*1e-3, 
                  `Prey group` = factor(dplyr::case_when(
                    `Prey group` %in% c("Muscular pelagic cephalopods",
                                        "Gelatinous pelagic cephalopods") ~ "Pelagic cephalopods", 
                    `Prey group` == "Bottom cephalopods" ~ "Demersal cephalopods",
                    `Prey group` %in% c("Miscellanous benthodemersal fish",
                                        "Large demersal energy-lean fish", 
                                        "Large demersal energy-rich fish") ~ "Demersal fish",
                    `Prey group` %in% c("Small schooling energy-lean fish",
                                        "Small schooling energy-rich fish", 
                                        "Miscellanous pelagic fish") ~ "Pelagic fish", 
                    `Prey group` == "Crustaceans" ~ "Other crustaceans",
                    TRUE ~ `Prey group`
                  ), 
                  levels = c("Zooplankton", "Demersal fish", 
                             "Pelagic fish",
                             "Fish undetermined",
                             "Pelagic cephalopods", "Demersal cephalopods",
                             "Cephalopod undetermined", 
                              "Other crustaceans"))) |>
    dplyr::group_by(Geo_area, Eco_gp, Species, `Prey group`) |>
    dplyr::summarize(mean = round(mean(value), 0),
                     sd = round(sd(value), 0), 
                     `10_quant` = round(quantile(value, probs = c(0.10)), 
                                         0), 
                     `90_quant` = round(quantile(value, probs = c(0.90)), 
                                          0)) |>
    dplyr::filter(`Prey group` %in% c("Zooplankton", 
                                      "Pelagic fish", 
                                      "Demersal fish")) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Geo_area, y = mean,
                                   fill = Species),
                      stat = "identity", 
                      size = 2,
                      position = ggplot2::position_dodge(1), 
                      alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = Geo_area, y = mean, 
                                     color = Species), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Geo_area, 
                                        ymin = `10_quant`,
                                        ymax = `90_quant`, 
                                        color = Species),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::facet_wrap(~ `Prey group`, ncol = 2, 
                        scales = "free_x") +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual consumption (in t/yr)") +
    ggplot2::coord_flip() +
   #ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::scale_color_manual(values = c("#D8AF39FF",
                                           "#58A449FF",
                                           "#AE93BEFF",
                                           "#B4DAE5FF",
                                           "#E75B64FF"), 
                                name = "") +
    ggplot2::scale_fill_manual(values = c("#D8AF39FF",
                                          "#58A449FF",
                                          "#AE93BEFF",
                                          "#B4DAE5FF",
                                          "#E75B64FF"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_conso_BW_few_prey_gps.jpg"), 
                  scale = 1, 
                  width = 12, 
                  height = 4)
  
  
}




# SUMMER barplot with annual consumption of each prey group per species
# in tons/yr/km2
#'
#'
#'
#'
#'
# SD and DD only, lower the nb of prey groups
SUMMER_barplot_conso_dens_gp_simplified_SD_DD <- function(output_tib) {
  
  options(scipen = 999)
  
  output_tib |>
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic", 
                  Eco_gp != "Baleen whales") |>
    dplyr::ungroup() |>
    dplyr::select(Eco_gp, Geo_area, Surf_tot, conso_diet) |>
    tidyr::unnest(conso_diet) |>
    tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                        names_to = "Prey group",
                        values_to = "value") |>
    # change from kg/yr to t/yr
    dplyr::mutate(value_tons = value*1e-3, 
                  value_dens = value/Surf_tot, 
                  `Prey group` = factor(dplyr::case_when(
                    `Prey group` %in% c("Muscular pelagic cephalopods",
                                        "Gelatinous pelagic cephalopods") ~ "Pelagic cephalopods", 
                    `Prey group` == "Bottom cephalopods" ~ "Demersal cephalopods",
                    `Prey group` %in% c("Miscellanous benthodemersal fish",
                                        "Large demersal energy-lean fish", 
                                        "Large demersal energy-rich fish") ~ "Demersal fish",
                    `Prey group` %in% c("Small schooling energy-lean fish",
                                        "Small schooling energy-rich fish", 
                                        "Miscellanous pelagic fish") ~ "Pelagic fish",
                    `Prey group` %in% c("Fish undetermined",
                                        "Cephalopod undetermined") ~ "Other", 
                    TRUE ~ `Prey group`
                  ), 
                  levels = c("Other", "Crustaceans", 
                             "Zooplankton", "Pelagic fish", "Demersal fish", 
                             "Pelagic cephalopods", "Demersal cephalopods"
                  ))) |>
    dplyr::filter(!(`Prey group`  %in% c("Zooplankton", 
                                         "Other"))) |>
    dplyr::group_by(Geo_area, Eco_gp, `Prey group`) |>
    dplyr::summarize(mean = round(mean(value_dens), 0),
                     sd = round(sd(value_dens), 0), 
                     `10_quant` = round(quantile(value_dens, probs = c(0.10)), 
                                         0), 
                     `90_quant` = round(quantile(value_dens, probs = c(0.90)), 
                                          0)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = `Prey group`, y = mean,
                                   fill = Eco_gp),
                      stat = "identity", 
                      size = 2,
                      position = ggplot2::position_dodge(1), 
                      alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = `Prey group`, y = mean, 
                                     color = Eco_gp), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = `Prey group`, 
                                        ymin = `10_quant`,
                                        ymax = `90_quant`, 
                                        color = Eco_gp),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 2, 
                        scales = "free_x") +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual consumption (in kg/yr/km2)") +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c(`Small delphinids` = "#365579ff", 
                                           `Deep divers` = "slategray3"), 
                                name = "") +
    ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                          `Deep divers` = "slategray3"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_conso_SD_DD_few_prey_gps_dens.jpg"), 
                  scale = 1,
                  width = 12, 
                  height = 5)
  
  
}



# SUMMER barplot with annual consumption of each prey group per species
# in tons/yr/km2
#'
#'
#'
#'
#'
# SD and DD only but per sp, only for Demersal fish and ceph and Pelagic
# fish and ceph
SUMMER_barplot_conso_dens_sp_simplified_SD_DD <- function(output_tib) {
  
  options(scipen = 999)
  
  output_tib |>
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic", 
                                  "Eastern Mediterranean Sea", 
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic", 
                  Eco_gp != "Baleen whales") |>
    dplyr::ungroup() |>
    dplyr::select(Species, Geo_area, Surf_tot, conso_diet) |>
    tidyr::unnest(conso_diet) |>
    tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                        names_to = "Prey group",
                        values_to = "value") |>
    # change from kg/yr to t/yr
    dplyr::mutate(value_tons = value*1e-3, 
                  value_dens = value/Surf_tot, 
                  `Prey group` = factor(dplyr::case_when(
                    `Prey group` %in% c("Muscular pelagic cephalopods",
                                        "Gelatinous pelagic cephalopods") ~ "Pelagic cephalopods", 
                    `Prey group` == "Bottom cephalopods" ~ "Demersal cephalopods",
                    `Prey group` %in% c("Miscellanous benthodemersal fish",
                                        "Large demersal energy-lean fish", 
                                        "Large demersal energy-rich fish") ~ "Demersal fish",
                    `Prey group` %in% c("Small schooling energy-lean fish",
                                        "Small schooling energy-rich fish", 
                                        "Miscellanous pelagic fish") ~ "Pelagic fish",
                    `Prey group` %in% c("Fish undetermined",
                                        "Cephalopod undetermined") ~ "Other", 
                    TRUE ~ `Prey group`
                  ), 
                  levels = c("Other", "Crustaceans", 
                             "Zooplankton", "Pelagic fish", "Demersal fish", 
                             "Pelagic cephalopods", "Demersal cephalopods"
                  ))) |>
    dplyr::filter(`Prey group` %in% c("Pelagic cephalopods", 
                                      "Pelagic fish")) |>
    dplyr::group_by(Geo_area, Species, `Prey group`) |>
    dplyr::summarize(mean = round(mean(value_dens), 0),
                     sd = round(sd(value_dens), 0), 
                     `10_quant` = round(quantile(value_dens, probs = c(0.10)), 
                                         0), 
                     `90_quant` = round(quantile(value_dens, probs = c(0.90)), 
                                          0)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = `Prey group`, y = mean,
                                   fill = Species),
                      stat = "identity", 
                      size = 2,
                      position = ggplot2::position_dodge(1), 
                      alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = `Prey group`, y = mean, 
                                     color = Species), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = `Prey group`, 
                                        ymin = `10_quant`,
                                        ymax = `90_quant`, 
                                        color = Species),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::facet_wrap(~ Geo_area, ncol = 2, 
                        scales = "free_x") +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual consumption (in kg/yr/km2)") +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                           "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                           "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                           "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                           "#F0D77BFF"), 
                                name = "") +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_conso_SD_DD_sp_dens.jpg"), 
                  scale = 1, 
                  width = 12, 
                  height = 5)
  
  
}





# SUMMER barplot with annual consumption of each prey group per species
# in tons/yr/km2
#'
#'
#'
#'
#'
# BW only, lower the nb of prey groups
SUMMER_barplot_conso_dens_gp_simplified_BW <- function(output_tib) {
  
  options(scipen = 999)
  
  output_tib |>
    dplyr::filter(Geo_area %in% c("Central North Atlantic", 
                                  "Northeast Atlantic",  
                                  "Western Mediterranean Sea"), 
                  Eco_area == "oceanic", 
                  Eco_gp == "Baleen whales") |>
    dplyr::ungroup() |>
    dplyr::select(Eco_gp, Geo_area, Species, Surf_tot, conso_diet) |>
    tidyr::unnest(conso_diet) |>
    tidyr::pivot_longer(cols = c(`Large demersal energy-lean fish`:Zooplankton), 
                        names_to = "Prey group",
                        values_to = "value") |>
    # change from kg/yr to t/yr
    dplyr::mutate(value_tons = value*1e-3, 
                  value_dens = value/Surf_tot, 
                  `Prey group` = factor(dplyr::case_when(
                    `Prey group` %in% c("Muscular pelagic cephalopods",
                                        "Gelatinous pelagic cephalopods") ~ "Pelagic cephalopods", 
                    `Prey group` == "Bottom cephalopods" ~ "Demersal cephalopods",
                    `Prey group` %in% c("Miscellanous benthodemersal fish",
                                        "Large demersal energy-lean fish", 
                                        "Large demersal energy-rich fish") ~ "Demersal fish",
                    `Prey group` %in% c("Small schooling energy-lean fish",
                                        "Small schooling energy-rich fish", 
                                        "Miscellanous pelagic fish") ~ "Pelagic fish", 
                    `Prey group` == "Crustaceans" ~ "Other crustaceans",
                    TRUE ~ `Prey group`
                  ), 
                  levels = c("Zooplankton", "Demersal fish", "Pelagic fish", 
                             "Fish undetermined",
                             "Pelagic cephalopods", "Demersal cephalopods",
                             "Cephalopod undetermined", 
                             "Other crustaceans"))) |>
    dplyr::group_by(Geo_area, Eco_gp, Species, `Prey group`) |>
    dplyr::summarize(mean = round(mean(value_dens), 0),
                     sd = round(sd(value_dens), 0), 
                     `10_quant` = round(quantile(value_dens, probs = c(0.10)), 
                                         0), 
                     `90_quant` = round(quantile(value_dens, probs = c(0.90)), 
                                          0)) |>
    dplyr::filter(`Prey group` %in% c("Zooplankton", 
                                      "Pelagic fish", 
                                      "Demersal fish"))  |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Geo_area, y = mean,
                                   fill = Species),
                      stat = "identity", 
                      size = 2,
                      position = ggplot2::position_dodge(1), 
                      alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = Geo_area, y = mean, 
                                     color = Species), 
                        position = ggplot2::position_dodge(1), 
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Geo_area, 
                                        ymin = `10_quant`,
                                        ymax = `90_quant`, 
                                        color = Species),
                           position = ggplot2::position_dodge(1), 
                           width = 0, size = 1) +
    ggplot2::xlab("") +
    ggplot2::ylab("Annual consumption (in kg/yr/km2)") +
    ggplot2::facet_wrap(~ `Prey group`, scales = "free_x", 
                        ncol = 2) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c("#D8AF39FF",
                                           "#58A449FF",
                                           "#AE93BEFF",
                                           "#B4DAE5FF",
                                           "#E75B64FF"), 
                                name = "") +
    ggplot2::scale_fill_manual(values = c("#D8AF39FF",
                                          "#58A449FF",
                                          "#AE93BEFF",
                                          "#B4DAE5FF",
                                          "#E75B64FF"), 
                               name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right", 
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_text(face = "bold", size = 13),
                   strip.text =  ggplot2::element_text(face = "bold", size = 13)) 
  
  ggplot2::ggsave(paste0("output/SUMMER/SUMMER_conso_BW_few_prey_gps_dens.jpg"), 
                  scale = 1, 
                  width = 10, 
                  height = 4)
  
  
}




################################################################################
### INPUT ABUNDANCE DATA with separation between West and East Mediterranean Sea
################################################################################

# Build tibble relative to just one species
build_sp_tib_SUMMER_1st_version <- function(original_tib, species, code_sp) {
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
build_sp_tib_Dd_Dc_Sc_SUMMER_1st_version <- function(ratio_others_tib) {
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
build_sp_tib_BW_SUMMER_1st_version <- function(ratio_REMMOAs_tib, ratio_others_tib) {
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