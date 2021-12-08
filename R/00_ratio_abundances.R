################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# December 2021
# 00_ratio_abundances.R
#
# Script with all functions to create the tibble with ratio data 
# for abundances of species 
################################################################################



########################### Survey data ########################################
########################## load or create data #################################

# load ASI sighting data 
load_ASI <- function(pathASI) {
  readxl::read_excel(pathASI)
}

# load SCANS III aerial sighting data
load_SCANSIII_air <- function(pathSCANSIII_aerial) {
  readxl::read_excel(pathSCANSIII_aerial)
}

# load SCANS III ship sighting data
load_SCANSIII_ship <- function(pathSCANSIII_ship) {
  readr::read_csv(pathSCANSIII_ship)
}

# load REMMOA Antilles sighting data
load_REMMOA_ANT <- function(pathREMMOA_ANT) {
  readr::read_csv(pathREMMOA_ANT)
}

# load REMMOA Indian ocean sighting data
load_REMMOA_Ind <- function(pathREMMOA_Ind) {
  readr::read_csv(pathREMMOA_Ind)
}

######################### compute ratios ########################################

# ASI
compute_ratio_ASI <- function(ASI_tib) {
  # delete unwanted variables 
  ASI_Dd_Sc <- ASI_tib |>
    dplyr::select(region, survey, strate, taxon, group, family, species, speciesNam, speciesLat, podSize) |>
    dplyr::filter(speciesNam %in% c("Striped dolphin", "Striped dolphin / Common dolphin", "Common dolphin")) |>
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
                  Geo_area = "Med") |>
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
                  Geo_area = "Med") |>
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
