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
############################# load data ########################################

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
  readxl::read_excel(pathSCANSIII_ship)
}

# load REMMOA Antilles & Guyana sighting data
load_REMMOA_ANTGUY <- function(pathREMMOA_ANTGUY) {
  readxl::read_csv(pathREMMOA_ANTGUY)
}

# load REMMOA Indian ocean sighting data
load_REMMOA_Ind <- function(pathREMMOA_Ind) {
  readr::read_csv(pathREMMOA_Ind)
}


######################### compute ratios #######################################

# ASI
compute_ratio_ASI <- function(ASI_tib) {
  # ASI_tib is the tibble resulting from the call of load_ASI()
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


# SCANS III: include aerial sightings, ship sightings and Observe (Ireland) sightings
compute_ratio_NEA <- function(SCANS_air_tib, SCANS_ship_tib) {
  # SCANS_air_tib is the tibble resulting from the call of load_SCANSIII_air()
  # SCANS_ship_tib is the tibble resulting from the call of load_SCANSIII_ship()
  
  # SCANS_air sighting tibble 
  SCANS_air_sightings <- rbind(
    # first Dd and Sc
    SCANS_air_tib |>
    dplyr::filter(species %in% c("ddel", "ddsc", "scoe"), !(block %in% c("W", "X", "Y", "Z", 
                                                                  "DKA", "P1", "SVG", "TRD"))) |> # delete unwanted blocks
    dplyr::mutate(Eco_area = "shelf", 
                  Geo_area = "NEAtlantic") |>
    dplyr::group_by(Geo_area, Eco_area, species) |>
    dplyr::filter(species != "ddsc") |>
    dplyr::mutate(species = dplyr::case_when(species == "scoe" ~ "Stenella coeruleoalba",   # format name of species 
                                      species == "ddel" ~ "Delphinus delphis")) |>
    dplyr::summarise(nobs = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::mutate(sumnobs = sum(nobs)) |>
    dplyr::rename(Species = species), 
    # then beaked whales 
    SCANS_air_tib |>
      dplyr::filter(species %in% c("hamp", "mbid", "zcav", "zisp"), !(block %in% c("W", "X", "Y", "Z", 
                                                                            "DKA", "P1", "SVG", "TRD"))) |>
      dplyr::mutate(Eco_area = "shelf", 
                    Geo_area = "NEAtlantic") |>
      dplyr::group_by(Geo_area, Eco_area, species) |>
      dplyr::filter(species != "zisp") |>
      dplyr::mutate(species = dplyr::case_when(species == "hamp" ~ "Hyperoodon ampullatus", 
                                        species == "mbid" ~ "Mesoplodon spp", 
                                        species == "zcav" ~ "Ziphius cavirostris")) |>
      dplyr::summarise(nobs = sum(n)) |>
      dplyr::ungroup() |>
      dplyr::group_by(Geo_area, Eco_area) |>
      dplyr::mutate(sumnobs = sum(nobs)) |>
      dplyr::rename(Species = species)
    )
  
  # SCANS ship sighting tibble
  SCANS_ship_sightings <- rbind(
    # first Dd and Sc 
    SCANS_ship_tib |>
    dplyr::filter(Species %in% c("Common dolphin", "Striped dolphin", "Unidentified common/striped")) |>
    dplyr::mutate(Eco_area = "oceanic", 
                  Geo_area = "NEAtlantic") |>
    dplyr::group_by(Geo_area, Eco_area, Species) |>
    dplyr::filter(Species != "Unidentified common/striped") |>
    dplyr::mutate(Species = dplyr::case_when(Species == "Striped dolphin" ~ "Stenella coeruleoalba", 
                                      Species == "Common dolphin" ~ "Delphinus delphis")) |>
    dplyr::summarise(nobs = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::mutate(sumnobs = sum(nobs)), 
    # then beaked whales 
    SCANS_ship_tib |>
      dplyr::filter(Species %in% c("Cuvier's", "Sowerby's", "Gervais'", "Mesoplodon spp", "Unid beaked")) |>
      dplyr::mutate(Eco_area = "oceanic", 
                    Geo_area = "NEAtlantic") |>
      dplyr::group_by(Geo_area, Eco_area, Species) |>
      dplyr::filter(Species != "Unid beaked") |>
      dplyr::mutate(Species = dplyr::case_when(Species == "Sowerby's" ~ "Mesoplodon spp", 
                                        Species == "Gervais'" ~ "Mesoplodon spp", 
                                        Species == "Cuvier's" ~ "Ziphius cavirostris", 
                                        TRUE ~ Species)) |>
      dplyr::summarise(nobs = sum(n)) |>
      dplyr::ungroup() |>
      dplyr::group_by(Geo_area, Eco_area) |>
      dplyr::mutate(sumnobs = sum(nobs))
  )
  
  # Observe sighting tibble (Season 3 summer 2016)
  Observe_sightings <- tibble::tribble(~ Geo_area, ~ Eco_area, ~ Species, ~ n,  
                             "NEAtlantic", "oceanic", "Mesoplodon spp", 8,
                             "NEAtlantic", "oceanic", "Ziphius cavirostris", 2) |>
    dplyr::group_by(Geo_area, Eco_area, Species) |>
    dplyr::summarise(nobs = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::mutate(sumnobs = sum(nobs))
  
  
  # bind and compute ratio
  rbind(SCANS_air_sightings, 
        SCANS_ship_sightings, 
        Observe_sightings) |>
    dplyr::group_by(Geo_area, Eco_area, Species) |>
    dplyr:: mutate(nobs = sum(nobs), 
                   sumnobs = sum(sumnobs), 
                   ratio = nobs/sumnobs) |>
    dplyr::distinct()
}


# REMMOA ANTGUY
compute_ratio_REMMOA_ANTGUY <- function(REMMOA_ANTGUY_tib) {
  # REMMOA_ANTGUY_tib is the tibble resulting from the call of load_REMMOA_ANTGUY()
  
  REMMOA_ANTGUY_tib |> 
    dplyr::mutate(Secteur = dplyr::case_when(Secteur == "IleNord" ~ "ANT", 
                                             TRUE ~ Secteur)) |>
    dplyr::filter(TAXON == "Marine mammal", !is.na(STRATE_TYP)) |>
    dplyr::filter(!(SPECIES_LA %in% c("Delphinidae sp.", "Small Cetacean", "Medium Cetacean",
                                      "Ziphiidae sp.", "Large delphininae", "Small delphininae", "Globicephala / Pseudorca",
                                      "Balaenopteridae sp.", "Megaptera novaeangliae", # no abundance estimate
                                      "Physeter macrocephalus", "Kogiidae sp.", "Sotalia guianensis" # separated abundance estimate
    ))) |>
    dplyr::mutate(SPECIES_LA = dplyr::case_when(SPECIES_LA == "Peponocephala / Feresa" ~ "Peponocephala electra", 
                                                SPECIES_LA == "Mesoplodon sp." ~ "Mesoplodon spp",
                                                TRUE ~ SPECIES_LA),
                  Group_sp = dplyr::case_when(SPECIES_LA %in% c("Stenella attenuata",  "Stenella longirostris") ~ "Small delphinids", 
                                              SPECIES_LA %in% c("Lagenodelphis hosei",  "Tursiops truncatus", 
                                                                "Steno bredanensis") ~ "Large delphinids",
                                              SPECIES_LA %in% c("Peponocephala electra", "Grampus griseus") ~ "Small globicephalinids",
                                              SPECIES_LA %in% c("Globicephala macrorhynchus", "Pseudorca crassidens") ~ "Large globicephalinids",
                                              SPECIES_LA %in% c("Ziphius cavirostris", "Mesoplodon spp") ~ "Beaked whales"),
                  Eco_area = dplyr::case_when(STRATE %in% c("P3", "P2", "P1", "P5") ~ "shelf", 
                                              STRATE %in% c("O1", "O5") ~ "oceanic"), 
                  Geo_area = dplyr::case_when(Secteur == "ANT" ~ "Antilles", 
                                              Secteur == "GUY" ~ "Guyana")
    ) |>
    dplyr::rename(Species = SPECIES_LA) |>
    dplyr::group_by(Geo_area, Eco_area, Group_sp, Species) |>
    dplyr::summarise(nobs = dplyr::n()) |> 
    dplyr::group_by(Geo_area, Eco_area, Group_sp) |> 
    dplyr::mutate(sumnobs = sum(nobs), 
                  ratio = nobs/sumnobs) |>
    dplyr::select(Geo_area, Eco_area, Group_sp, Species, nobs, sumnobs, ratio) |>
    dplyr::arrange(Geo_area, Eco_area, Group_sp, Species) 
}
