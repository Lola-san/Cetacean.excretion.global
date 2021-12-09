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

# load excel files (ASI, SCANSIII, REMMOA_ANTGUY) 
load_xl <- function(pathxl) {
  readxl::read_excel(pathxl)
}

# load  csv files (REMMOA Indian)
load_csv <- function(pathcsv) {
  readr::read_csv(pathcsv, show_col_types = FALSE)
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

# REMMOA Indian ocean 
compute_ratio_REMMOA_Ind <- function(REMMOA_Ind_tib) {
  # REMMOA_Ind_tib is the tibble resulting from the call of load_REMMOA_Ind()
  
  working_tib <- REMMOA_Ind_tib |> 
    dplyr::filter(taxon_eng == "Marine mammal") |>
    dplyr::select(subRegion, strateType, sighting, date_time, 
           nom_latin, group_eng, name_eng) |>
    # there is still off effort data (T for transit and C for circle)
    dplyr::filter(strateType %in% c("N", "P", "O")) |>
    # there are some regions with different names than for abundances 
    # JM -> JMN
    # NP = TM
    dplyr::mutate(subRegion = dplyr::case_when(subRegion == "NP" ~ "TM", 
                                 subRegion == "JM" ~ "JMN",
                                 TRUE ~ subRegion),
                  #create a block variable  
                  Block = paste(subRegion, strateType, sep = "_"))

  
  rbind(working_tib,
        # In reports/paper: REU and MAU are sometimes reunited in RM for abundance estimates, 
        # sometimes it is only RM and sometimes it's RM_N, RM_P, RM_O, 
        # so we'll duplicate lines of REU and MAU to have ratio for these different assemblages
        working_tib |>
          dplyr::filter(Block %in% c("REU_N", "REU_O",
                                     "MAU_N", "MAU_P", "MAU_O")) |>
          dplyr::mutate(Block = "RM"),
        working_tib |>
          dplyr::filter(Block %in% c("REU_N", "REU_O",
                                     "MAU_N", "MAU_P", "MAU_O")) |>
          dplyr::mutate(Block = dplyr::case_when(Block %in% c("REU_N",
                                                              "MAU_N") ~ "RM_N",
                                                 Block == "MAU_P" ~ "RM_P",
                                                 Block %in% c("REU_O",
                                                              "MAU_O") ~ "RM_O"))
  ) |>
    # names pf groups are different than for other REMMOAs
    dplyr::rename(Group_sp = group_eng,
                  Species = nom_latin) |>
    dplyr::filter(Group_sp %in% c("Large globicephalinae",
                                  "Large delphininae",
                                  "Small delphininae",
                                  "Mesoplodon",
                                  "Other beaked whale")) |>
    dplyr::mutate(Group_sp = dplyr::case_when(Group_sp == "Large globicephalinae" ~ "Large globicephalinids",
                                              Group_sp == "Large delphininae" ~ "Large delphinids",
                                              Group_sp == "Small delphininae" ~ "Small delphinids",
                                              Group_sp %in% c("Mesoplodon",
                                                              "Other beaked whale") ~ "Beaked whales")) |>
    dplyr::filter(!(Species %in% c("Grampus griseus", # Only obs of this sp for the associate group, so no need of ratio
                                   "Large delphininae sp.",
                                   "Small delphininae sp.",
                                   "Peponocephala / Feresa sp.", # Only confirmed obs of one sp for the associate group, so no need of ratio
                                   "Globicephala / Pseudorca sp.") # Only confirmed obs of one sp for the associate group, so no need of ratio
    )) |>
    # format name of species 
    dplyr::mutate(Species = dplyr::case_when(Species %in% c("Mesoplodon sp.",
                                                            "Mesoplodon densirostris") ~ "Mesoplodon spp",
                                             TRUE ~ Species)) |>
    # add Geo_area and Eco_area
    dplyr::mutate(Geo_area = "Indian",
                  Eco_area = dplyr::case_when(Block %in% c("CMGM_N", "JMN_N", "TM_N",
                                                           "MAU_N", "SE_N", "RM_N") ~ "shelf", # Indian ocean
                                              Block %in% c("CMGM_P", "CMGM_O",
                                                           "JMN_P", "JMN_O",
                                                           "EBM_P", "EBM_O",
                                                           "TM_P", "TM_O",
                                                           "MAU_P", "MAU_O",
                                                           "REU_P", "REU_O",
                                                           "RM", "RM_P", "RM_O", # unstratified subarea of MAU_ritius and Reunion
                                                           "SE_P", "SE_O") ~ "oceanic")) |>
    dplyr::group_by(Geo_area, Eco_area, Group_sp, Species) |>
    dplyr::summarise(nobs = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(Geo_area, Eco_area, Group_sp) |>
    # compute ratio
    dplyr::mutate(sumnobs = sum(nobs),
                  ratio = nobs/sumnobs) |>
    dplyr::arrange(Geo_area, Eco_area, Group_sp, Species)
}

# REMMOA French Polynesia
compute_ratio_REMMOA_FPol <- function() {
  # no file associated here, so tibb of sightings reproduced
  tibble::tribble(~ Geo_area, ~ Eco_area, ~ Species, ~ Sector, ~ Group_sp, ~ n,  
                                  "Pacific_FPoly", "oceanic", "Mesoplodon densirostris", "SOC", "Beaked whales", 2,
                                  "Pacific_FPoly", "oceanic", "Mesoplodon densirostris", "AUS", "Beaked whales", 5,
                                  "Pacific_FPoly", "oceanic", "Ziphius cavirostris", "SOC", "Beaked whales", 4,
                                  "Pacific_FPoly", "oceanic", "Ziphius cavirostris", "AUS", "Beaked whales", 6,
                                  "Pacific_FPoly", "oceanic", "Ziphius cavirostris", "TUN", "Beaked whales", 6,
                                  "Pacific_FPoly", "oceanic", "Ziphius cavirostris", "TUS", "Beaked whales", 2,
                                  "Pacific_FPoly", "oceanic", "Ziphius cavirostris", "GAM", "Beaked whales", 5,
                                  "Pacific_FPoly", "oceanic", "Ziphius cavirostris", "MAR", "Beaked whales", 5,
                                  "Pacific_FPoly", "oceanic", "Mesoplodon und.", "SOC", "Beaked whales", 2,
                                  "Pacific_FPoly", "oceanic", "Mesoplodon und.", "TUN", "Beaked whales", 1,
                                  "Pacific_FPoly", "oceanic", "Mesoplodon und.", "TUS", "Beaked whales", 2,
                                  "Pacific_FPoly", "oceanic", "Mesoplodon und.", "GAM", "Beaked whales", 1,
                                  "Pacific_FPoly", "oceanic", "Mesoplodon und.", "MAR", "Beaked whales", 4,
                                  "Pacific_FPoly", "oceanic", "Ziphiid und.", "SOC", "Beaked whales", 3,
                                  "Pacific_FPoly", "oceanic", "Ziphiid und.", "AUS", "Beaked whales", 4,
                                  "Pacific_FPoly", "oceanic", "Ziphiid und.", "TUN", "Beaked whales", 3,
                                  "Pacific_FPoly", "oceanic", "Ziphiid und.", "TUS", "Beaked whales", 1,
                                  "Pacific_FPoly", "oceanic", "Ziphiid und.", "GAM", "Beaked whales", 1,
                                  "Pacific_FPoly", "oceanic", "Ziphiid und.", "MAR", "Beaked whales", 8,
                                  "Pacific_FPoly", "oceanic", "Pseudorca crassidens", "MAR", "Large globicephalinids", 1,
                                  "Pacific_FPoly", "oceanic", "Globicephala / Pseudorca", "SOC", "Large globicephalinids", 3,
                                  "Pacific_FPoly", "oceanic", "Globicephala / Pseudorca", "AUS", "Large globicephalinids", 2,
                                  "Pacific_FPoly", "oceanic", "Globicephala / Pseudorca", "TUN", "Large globicephalinids", 3,
                                  "Pacific_FPoly", "oceanic", "Globicephala / Pseudorca", "TUS", "Large globicephalinids", 2,
                                  "Pacific_FPoly", "oceanic", "Globicephala / Pseudorca", "MAR", "Large globicephalinids", 7,
                                  "Pacific_FPoly", "oceanic", "Globicephala macrorhynchus", "SOC", "Large globicephalinids", 1,
                                  "Pacific_FPoly", "oceanic", "Globicephala macrorhynchus", "AUS", "Large globicephalinids", 1,
                                  "Pacific_FPoly", "oceanic", "Globicephala macrorhynchus", "TUN", "Large globicephalinids", 4,
                                  "Pacific_FPoly", "oceanic", "Globicephala macrorhynchus", "TUS", "Large globicephalinids", 2,
                                  "Pacific_FPoly", "oceanic", "Globicephala macrorhynchus", "MAR", "Large globicephalinids", 12,
                                  "Pacific_FPoly", "oceanic", "Orcinus orca", "TUN", "Large globicephalinids", 1,
                                  "Pacific_FPoly", "oceanic", "Orcinus orca", "MAR", "Large globicephalinids", 1,
                                  "Pacific_FPoly", "oceanic", "Tursiops truncatus", "SOC", "Large delphinids", 3,
                                  "Pacific_FPoly", "oceanic", "Tursiops truncatus", "TUN", "Large delphinids", 5,
                                  "Pacific_FPoly", "oceanic", "Tursiops truncatus", "TUS", "Large delphinids", 4,
                                  "Pacific_FPoly", "oceanic", "Tursiops truncatus", "GAM", "Large delphinids", 1,
                                  "Pacific_FPoly", "oceanic", "Tursiops truncatus", "MAR", "Large delphinids", 6,
                                  "Pacific_FPoly", "oceanic", "Lagenodelphis hosei", "GAM", "Large delphinids", 1,
                                  "Pacific_FPoly", "oceanic", "Steno bredanensis", "SOC", "Large delphinids", 3,
                                  "Pacific_FPoly", "oceanic", "Steno bredanensis", "TUS", "Large delphinids", 1,
                                  "Pacific_FPoly", "oceanic", "Steno bredanensis", "MAR", "Large delphinids", 5,
                                  "Pacific_FPoly", "oceanic", "Large delphinids und.", "SOC", "Large delphinids", 4,
                                  "Pacific_FPoly", "oceanic", "Large delphinids und.", "AUS", "Large delphinids", 3,
                                  "Pacific_FPoly", "oceanic", "Large delphinids und.", "TUN", "Large delphinids", 4,
                                  "Pacific_FPoly", "oceanic", "Large delphinids und.", "TUS", "Large delphinids", 2,
                                  "Pacific_FPoly", "oceanic", "Large delphinids und.", "MAR", "Large delphinids", 11,
                                  "Pacific_FPoly", "oceanic", "Stenella attenuata", "MAR", "Small delphinids", 1,
                                  "Pacific_FPoly", "oceanic", "Stenella longirostris", "SOC", "Small delphinids", 1,
                                  "Pacific_FPoly", "oceanic", "Stenella spp", "SOC", "Small delphinids", 4,
                                  "Pacific_FPoly", "oceanic", "Stenella spp", "TUN", "Small delphinids", 4,
                                  "Pacific_FPoly", "oceanic", "Stenella spp", "TUS", "Small delphinids", 4,
                                  "Pacific_FPoly", "oceanic", "Stenella spp", "GAM", "Small delphinids", 1,
                                  "Pacific_FPoly", "oceanic", "Stenella spp", "MAR", "Small delphinids", 15
                  ) |>
    dplyr::mutate(Species = dplyr::case_when(Species == "Mesoplodon und." ~ "Mesoplodon spp", 
                                      Species == "Mesoplodon densirostris" ~ "Mesoplodon spp", 
                                      TRUE ~ Species)) |>
    dplyr::filter(!(Species %in% c("Ziphiid und.", "Globicephala / Pseudorca", "Large delphinids und.", "Stenella spp"))) |>
    dplyr::group_by(Geo_area, Eco_area, Group_sp, Species) |>
    dplyr::summarise(nobs = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::group_by(Geo_area, Eco_area, Group_sp) |>
    dplyr::mutate(sumnobs = sum(nobs), 
                  ratio = nobs/sumnobs) |>
    dplyr::arrange(Geo_area, Eco_area, Group_sp, Species) 
}


# REMMOA New Caledonia
compute_ratio_REMMOA_NCal <- function() {
  # no file associated here, so tibb of sightings reproduced
  tibble::tribble(~ Geo_area, ~ Eco_area, ~ Species, ~ Group_sp, ~ nobs,  
                  "Pacific_NCal", "oceanic", "Indopacetus pacificus", "Beaked whales", 7,
                  "Pacific_NCal", "oceanic", "Ziphius cavirostris", "Beaked whales", 16,
                  "Pacific_NCal", "oceanic", "Mesoplodon spp", "Beaked whales", 8,
                  "Pacific_NCal", "oceanic", "Ziphiid und.", "Beaked whales", 8,
                  "Pacific_NCal", "oceanic", "Pseudorca crassidens", "Large globicephalinids", 5,
                  "Pacific_NCal", "oceanic", "Globicephala / Pseudorca", "Large globicephalinids", 10,
                  "Pacific_NCal", "oceanic", "Globicephala macrorhynchus","Large globicephalinids", 32,
                  "Pacific_NCal", "oceanic", "Tursiops truncatus", "Large delphinids", 9,
                  "Pacific_NCal", "oceanic", "Lagenodelphis hosei", "Large delphinids", 3,
                  "Pacific_NCal", "oceanic", "Large delphinids und.", "Large delphinids", 8,
                  "Pacific_NCal", "oceanic", "Stenella attenuata", "Small delphinids", 2,
                  "Pacific_NCal", "oceanic", "Stenella longirostris", "Small delphinids", 2,
                  "Pacific_NCal", "oceanic", "Stenella spp", "Small delphinids", 18,
  ) |>
    dplyr::filter(!(Species %in% c("Ziphiid und.", "Globicephala / Pseudorca", 
                                   "Large delphinids und.", "Stenella spp", "Balaenoptera spp"))) |>
    dplyr::group_by(Geo_area, Eco_area, Group_sp) |>
    dplyr::mutate(sumnobs = sum(nobs), 
                  ratio = nobs/sumnobs) |>
    dplyr::arrange(Geo_area, Eco_area, Group_sp, Species)
}

# REMMOA Wallis & Futuna
compute_ratio_REMMOA_WFu <- function() {
  # no file associated here, so tibb of sightings reproduced
  tibble::tribble(~ Geo_area, ~ Eco_area, ~ Species, ~ Group_sp, ~ nobs,  
                  "Pacific_WFu", "oceanic", "Indopacetus pacificus", "Beaked whales", 6,
                  "Pacific_WFu", "oceanic", "Mesoplodon spp", "Beaked whales", 4,
                  "Pacific_WFu", "oceanic", "Ziphiid und.", "Beaked whales", 2,
                  "Pacific_WFu", "oceanic", "Pseudorca crassidens", "Large globicephalinids", 3,
                  "Pacific_WFu", "oceanic", "Globicephala / Pseudorca", "Large globicephalinids", 6,
                  "Pacific_WFu", "oceanic", "Globicephala macrorhynchus","Large globicephalinids", 3,
                  "Pacific_WFu", "oceanic", "Orcinus orca","Large globicephalinids", 1,
                  "Pacific_WFu", "oceanic", "Tursiops truncatus", "Large delphinids", 11,
                  "Pacific_WFu", "oceanic", "Steno bredanensis", "Large delphinids", 1,
                  "Pacific_WFu", "oceanic", "Large delphinids und.", "Large delphinids", 1
  ) |>
    dplyr::filter(!(Species %in% c("Ziphiid und.", "Globicephala / Pseudorca", "Large delphinids und."))) |>
    dplyr::group_by(Geo_area, Eco_area, Group_sp) |>
    dplyr::mutate(sumnobs = sum(nobs), 
                  ratio = nobs/sumnobs) |>
    dplyr::arrange(Geo_area, Eco_area, Group_sp, Species) 
}

# Gulf of Mexico 
compute_ratio_GoMex <- function() {
  # no file associated here, so tibb of sightings reproduced
  tibble::tribble(~ Geo_area, ~ Eco_area, ~ Species, ~ n,  
                  "GoMexico", "oceanic", "Mesoplodon und.", 3,
                  "GoMexico", "oceanic", "Gervais's BW", 1,
                  "GoMexico", "oceanic", "Ziphius cavirostris", 1,
                  "GoMexico", "oceanic", "Ziphiid und.", 7) |>
    dplyr::mutate(Species = dplyr::case_when(Species == "Mesoplodon und." ~ "Mesoplodon spp", 
                                             Species == "Gervais's BW" ~ "Mesoplodon spp", 
                                             TRUE ~ Species)) |>
    dplyr::filter(!(Species %in% c("Ziphiid und."))) |>
    dplyr::group_by(Geo_area, Eco_area, Species) |>
    dplyr::summarise(nobs = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::mutate(sumnobs = sum(nobs),
                  ratio = nobs/(sumnobs))
}

# Hawaii
compute_ratio_Hawaii <- function() {
  # no file associated here, so tibb of sightings reproduced
  tibble::tribble(~ Geo_area, ~ Eco_area, ~ Species,  ~ nobs,  
                  "Pacific_Hawai", "oceanic", "Mesoplodon densirostris", 11,
                  "Pacific_Hawai", "oceanic", "Ziphius cavirostris", 13,
                  "Pacific_Hawai", "oceanic", "Indopacetus pacificus", 8,
                  "Pacific_Hawai", "oceanic", "Mesoplodon und.", 5,
                  "Pacific_Hawai", "oceanic", "Ziphiid und.", 18) |>
    dplyr::mutate(Species = dplyr::case_when(Species == "Mesoplodon und." ~ "Mesoplodon spp", 
                                      Species == "Mesoplodon densirostris" ~ "Mesoplodon spp", 
                                      TRUE ~ Species)) |>
    dplyr::group_by(Geo_area, Eco_area, Species) |>
    dplyr::summarise(nobs = sum(nobs)) |>
    dplyr::ungroup() |>
    dplyr::filter(Species != "Ziphiid und.") |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::mutate(sumnobs = sum(nobs),
                  ratio = nobs/(sumnobs)) |>
    dplyr::arrange(Geo_area, Eco_area, Species) 
}

# California current
compute_ratio_Calif <- function() {
  # no file associated here, so tibb of sightings reproduced
  rbind(# first delphinids 
    tibble::tribble(~ Geo_area, ~ Eco_area, ~ Species,  ~ nobs,  
                    "Pacific_Calif_current", "oceanic", "Delphinus delphis", 61+37+11+1+ #2008
                      239+165+52+3, #2005
                    "Pacific_Calif_current", "oceanic", "Delphinus capensis", 5+2+ #2008
                      16+3, #2005
                    "Pacific_Calif_current", "oceanic", "Delphinus delphis/capensis", 3+ #2008
                      17+11+1 #2005
    ) |>
      dplyr::filter(Species != "Delphinus delphis/capensis") |>
      dplyr::group_by(Geo_area, Eco_area) |>
      dplyr::mutate(sumnobs = sum(nobs),
                    ratio = nobs/(sumnobs)) |>
      dplyr::arrange(Geo_area, Eco_area, Species), 
    # then for beaked whales 
    tibble::tribble(~ Geo_area, ~ Eco_area, ~ Species,  ~ nobs, 
                    "Pacific_Calif_current", "oceanic", "Mesoplodon spp", 1+4+4+2, #2005
                    "Pacific_Calif_current", "oceanic", "Ziphius cavirostris", 2+1+ #2008
                      3+10+4, #2005
                    "Pacific_Calif_current", "oceanic", "Berardius bairdii", 1+2+2+ #2008
                      1+3+3+8, #2005
                    "Pacific_Calif_current", "oceanic", "Ziphiid und.", 1+ #2008
                      2+1+3, #2005
    ) |>
      dplyr::filter(Species != "Ziphiid und.") |>
      dplyr::group_by(Geo_area, Eco_area) |>
      dplyr::mutate(sumnobs = sum(nobs),
                    ratio = nobs/(sumnobs)) |>
      dplyr::arrange(Geo_area, Eco_area, Species) 
  )
}


################################ bind ratio tibbles ###############################

bind_ratio_REMMOAs <- function(ratio_FPoly_tib, ratio_df_Ncal_tib, ratio_WFu_tib, 
                               ratio_Ind_tib, ratio_ANTGUY_tib) {
  rbind(ratio_FPoly_tib, ratio_df_Ncal_tib, ratio_WFu_tib, 
        ratio_Ind_tib, ratio_ANTGUY_tib)
}

bind_ratio_others <- function(ratio_NEA_tib, ratio_ASI_tib, 
                               ratio_GoMex_tib, ratio_Hawaii_tib, 
                               ratio_Calif_tib) {
  rbind(ratio_NEA_tib, ratio_ASI_tib, 
        ratio_GoMex_tib, ratio_Hawaii_tib, 
        ratio_Calif_tib)
}
