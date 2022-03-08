################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# March 2022
# 09_oceano_data_graph.R
#
# Script with functions to compute correlation with productivity index data
# and make associated figures
# 
################################################################################


# SEE README FILE TO SEE WHERE AND HOW DATA WAS DOWNLOADED FROM NOAA WEBSITE

#'
#'
#'
#'
#'
# function to load data and split in the different areas 
create_tib_sst_chloro <- function() {
  
  ### FIRST CHLOROPHYLL DATA
  # Charge data 
  r_chloro <- raster("C:/Users/lgilbe01/Desktop/PhD_2020-2023/Analyses/Cetacean.excretion.global/data/oceanographic/A20213352021365.L3m_MO_CHL_chlor_a_4km_NEO.nc", varname = "chlor_a")
  
  chloro_world_to_df.p <- raster::rasterToPoints(r_chloro)
  chloro_world_df <- as.data.frame(r_chloro, xy = TRUE)
  
  # change name of column of interest
  chloro_world_df <- chloro_world_df |>
    tidyr::rename(Long = x, 
           Lat = y, 
           chloro_mg_m3 = `Chlorophyll.Concentration..OCI.Algorithm`)
  
  # define areas 
  # NEA 
  chloro_NEA <- chloro_world_df |>
    dplyr::filter(Lat > 36, Lat< 60, Long > -14, Long < 9.8)
  
  Med <- which(chloro_NEA$Long > -5 & chloro_NEA$Lat < 43)
  chloro_NEA <- chloro_NEA[-Med, ]
  Med_rest <- which(chloro_NEA$Long > 0 & chloro_NEA$Lat < 45)
  chloro_NEA <- chloro_NEA[-Med_rest, ]
  # clean up the mess
  rm(Med, Med_rest)
  
  # GoAlaska 
  chloro_GoAlaska <- chloro_world_df |>
    dplyr::filter(Lat > 55, Lat< 58, Long > -151, Long < -141)
  
  # Med
  chloro_Med <- chloro_world_df |>
    dplyr::filter(Lat > 30, Lat< 47, Long > -6, Long < 40)
  
  Red_sea <- which(chloro_Med$Lat > 41 & chloro_Med$Long > 25.5)
  chloro_Med <- chloro_Med[- Red_sea, ]
  Atl <- which(chloro_Med$Lat > 41 & chloro_Med$Long < 0)
  chloro_Med <- chloro_Med[- Atl, ]
  # clean up the mess
  rm(Red_sea, Atl)
  
  # Natlantic 
  chloro_tNASS <- chloro_world_df |>
    dplyr::filter(Lat > 58, Lat< 72, Long > -45, Long < 0)
  
  # NWAtlantic
  chloro_NWcoast <- chloro_world_df |>
    dplyr::filter(Lat > 37, Lat < 44, Long > -75, Long < -60)
  
  # GoMexico
  chloro_GoMex <- chloro_world_df |>
    dplyr::filter(Lat > 24.5, Lat < 28.5, Long > -95, Long < -83)
  
  # Antilles
  chloro_Ant <- chloro_world_df |>
    dplyr::filter(Lat > 15, Lat < 17, Long > -63, Long < -58)
  
  # Guyana
  chloro_Guy <- chloro_world_df |>
    dplyr::filter(Lat > 4.5, Lat < 8.5, Long > -54, Long < -50)
  
  # l'ocean indien ça va être relou par contre
  # Indian
  chloro_Ind <- chloro_world_df |>
    dplyr::filter(Lat > -23, Lat < -10, Long > 40, Long < 60)
  
  chloro_Seychelles <- chloro_world_df |>
    dplyr::filter(Lat > -7, Lat < -1, Long > 53, Long < 57)
  
  chloro_Ind <- rbind(chloro_Ind, chloro_Seychelles)  
  # clean up the mess
  rm(chloro_Seychelles)
  
  # New Caledonia
  chloro_NCal <- chloro_world_df |>
    dplyr::filter(Lat > -24, Lat < -19, Long > 160, Long < 169)
  
  # Wfu
  chloro_WFu <- chloro_world_df |>
    dplyr::filter(Lat > -15, Lat < -10, Long > -179, Long < -174)
  
  # French Poly
  chloro_FPoly <- chloro_world_df |>
    dplyr::filter(Lat > -20, Lat < -15, Long > -153, Long < -146)
  
  # Hawai
  chloro_Hawai <- chloro_world_df |>
    dplyr::filter(Lat > 16, Lat < 29, Long > -151, Long < -146)
  
  
  # California Current
  chloro_Calif_current <- chloro_world_df |>
    dplyr::filter(Lat >32, Lat < 48, Long > -131, Long < -115)
  
  
  # Bind all 
  Chloro_all_areas <- rbind(chloro_NEA |>
                              dplyr::mutate(Geo_area = "Northeast Atlantic"), 
                            chloro_tNASS |>
                              dplyr::mutate(Geo_area = "Central North Atlantic"), 
                            chloro_NWcoast |>
                              dplyr::mutate(Geo_area = "Northwest Atlantic"), 
                            chloro_GoAlaska |>
                              dplyr::mutate(Geo_area = "Gulf of Alaska"), 
                            chloro_Med |>
                              dplyr::mutate(Geo_area = "Mediterranean Sea"),
                            chloro_GoMex |>
                              dplyr::mutate(Geo_area = "Gulf of Mexico"), 
                            chloro_Ant |>
                              dplyr::mutate(Geo_area = "French Antilles"),
                            chloro_Guy |>
                              dplyr::mutate(Geo_area = "French Guyana"),
                            chloro_Ind |>
                              dplyr::mutate(Geo_area = "West Indian ocean"),
                            chloro_NCal |>
                              dplyr::mutate(Geo_area = "New Caledonia"),
                            chloro_WFu |>
                              dplyr::mutate(Geo_area = "Wallis & Futuna"),
                            chloro_FPoly |>
                              dplyr::mutate(Geo_area = "French Polynesia"),
                            chloro_Hawai |>
                              dplyr::mutate(Geo_area = "Hawaii"),
                            chloro_Calif_current |>
                              dplyr::mutate(Geo_area = "California current"))
  
  
  rm(chloro_NEA, chloro_tNASS, chloro_NWcoast, chloro_Med, chloro_GoMex, 
     chloro_Ant, chloro_Guy, chloro_Ind, chloro_NCal, chloro_WFu, 
     chloro_FPoly, chloro_Hawai, chloro_Calif_current)
  
  
  # THEN SST
  r_sst <- raster::raster("C:/Users/lgilbe01/Desktop/PhD_2020-2023/Analyses/01.Excretion_global/data/chlorophyll_sst/AQUA_MODIS.20210601_20210630.L3m.MO.SST.sst.4km.nc", varname = "sst")
  
  sst_world_to_df.p <- raster::rasterToPoints(r_sst)
  sst_world_df <- as.data.frame(r_sst, xy = TRUE)
  
  
  colnames(sst_world_df)
  sst_world_df <- sst_world_df |>
    tidyr::rename(Long = x, 
           Lat = y, 
           sst = `Sea.Surface.Temperature`)
  
  # define areas 
  # NEA 
  sst_NEA <- sst_world_df |>
    dplyr::filter(Lat > 36, Lat< 60, Long > -14, Long < 9.8)
  
  Med <- which(sst_NEA$Long > -5 & sst_NEA$Lat < 43)
  sst_NEA <- sst_NEA[-Med, ]
  Med_rest <- which(sst_NEA$Long > 0 & sst_NEA$Lat < 45)
  sst_NEA <- sst_NEA[-Med_rest, ]
  # clean up the mess 
  rm(Med, Med_rest)
  
  # GoAlaska 
  sst_GoAlaska <- sst_world_df |>
    dplyr::filter(Lat > 55, Lat< 58, Long > -151, Long < -141)
  
  # Med
  sst_Med <- sst_world_df |>
    dplyr::filter(Lat > 30, Lat< 47, Long > -6, Long < 40)
  
  Red_sea <- which(sst_Med$Lat > 41 & sst_Med$Long > 25.5)
  sst_Med <- sst_Med[- Red_sea, ]
  Atl <- which(sst_Med$Lat > 41 & sst_Med$Long < 0)
  sst_Med <- sst_Med[- Atl, ]
  # clean up the mess
  rm(Red_sea, Atl)
  
  # Natlantic 
  sst_tNASS <- sst_world_df |>
    dplyr::filter(Lat > 58, Lat< 72, Long > -45, Long < 0)
  
  # NWAtlantic
  sst_NWcoast <- sst_world_df |>
    dplyr::filter(Lat > 37, Lat < 44, Long > -75, Long < -60)
  
  # GoMexico
  sst_GoMex <- sst_world_df |>
    dplyr::filter(Lat > 24.5, Lat < 28.5, Long > -95, Long < -83)
  
  # Antilles
  sst_Ant <- sst_world_df |>
    dplyr::filter(Lat > 15, Lat < 17, Long > -63, Long < -58)
  
  # Guyana
  sst_Guy <- sst_world_df |>
    dplyr::filter(Lat > 4.5, Lat < 8.5, Long > -54, Long < -50)
  
  # l'ocean indien ça va être relou par contre
  # Indian
  sst_Ind <- sst_world_df |>
    dplyr::filter(Lat > -23, Lat < -10, Long > 40, Long < 60)
  
  sst_Seychelles <- sst_world_df |>
    dplyr::filter(Lat > -7, Lat < -1, Long > 53, Long < 57)
  
  sst_Ind <- rbind(sst_Ind, sst_Seychelles) 
  # clean up the mess
  rm(sst_Seychelles)
  
  # New Caledonia
  sst_NCal <- sst_world_df |>
    dplyr::filter(Lat > -24, Lat < -19, Long > 160, Long < 169)
  
  # Wfu
  sst_WFu <- sst_world_df |>
    dplyr::filter(Lat > -15, Lat < -10, Long > -179, Long < -174)
  
  # French Poly
  sst_FPoly <- sst_world_df |>
    dplyr::filter(Lat > -20, Lat < -15, Long > -153, Long < -146)
  
  # Hawai
  sst_Hawai <- sst_world_df |>
    dplyr::filter(Lat > 16, Lat < 29, Long > -151, Long < -146)
  
  # California Current
  sst_Calif_current <- sst_world_df |>
    dplyr::filter(Lat >32, Lat < 48, Long > -131, Long < -115)
  
  
  sst_all_areas <- rbind(sst_NEA |>
                           dplyr::mutate(Geo_area = "Northeast Atlantic"), 
                         sst_tNASS |>
                           dplyr::mutate(Geo_area = "Central North Atlantic"), 
                         sst_NWcoast |>
                           dplyr::mutate(Geo_area = "Northwest Atlantic"), 
                         sst_GoAlaska |>
                           dplyr::mutate(Geo_area = "Gulf of Alaska"),
                         sst_Med |>
                           dplyr::mutate(Geo_area = "Mediterranean Sea"),
                         sst_GoMex |>
                           dplyr::mutate(Geo_area = "Gulf of Mexico"), 
                         sst_Ant |>
                           dplyr::mutate(Geo_area = "French Antilles"),
                         sst_Guy |>
                           dplyr::mutate(Geo_area = "French Guyana"),
                         sst_Ind |>
                           dplyr::mutate(Geo_area = "West Indian ocean"),
                         sst_NCal |>
                           dplyr::mutate(Geo_area = "New Caledonia"),
                         sst_WFu |>
                           dplyr::mutate(Geo_area = "Wallis & Futuna"),
                         sst_FPoly |>
                           dplyr::mutate(Geo_area = "French Polynesia"),
                         sst_Hawai |>
                           dplyr::mutate(Geo_area = "Hawaii"),
                         sst_Calif_current |>
                           dplyr::mutate(Geo_area = "California current"))
  
  
  chloro_sst_all <- Chloro_all_areas |>
    dplyr::left_join(sst_all_areas, by = c("Long", "Lat", "Geo_area"))  |>
    dplyr::group_by(Geo_area)  |>
    dplyr::mutate(Geo_area = factor(Geo_area, 
                             levels = c("Northeast Atlantic", "Central North Atlantic", "Gulf of Alaska",
                                        "Northwest Atlantic", "California current", 
                                        "Mediterranean Sea", "West Indian ocean", "Gulf of Mexico", "French Antilles", 
                                        "New Caledonia", "Hawaii",  
                                        "French Guyana", "Wallis & Futuna", "French Polynesia"))) |>
    dplyr::summarize(min_sst = min(sst, na.rm = TRUE), 
                     `2.5_quant_sst` = quantile(sst, probs = c(0.025), na.rm = TRUE), 
                     mean_sst = mean(sst, na.rm = TRUE), 
                     median_sst = median(sst, na.rm = TRUE), 
                     `97.5_quant_sst` = quantile(sst, probs = c(0.975), na.rm = TRUE), 
                     max_sst = max(sst, na.rm = TRUE), 
                     min_chloro = min(chloro_mg_m3, na.rm = TRUE), 
                     `2.5_quant_chloro` = quantile(chloro_mg_m3, probs = c(0.025), na.rm = TRUE), 
                     mean_chloro = mean(chloro_mg_m3, na.rm = TRUE), 
                     median_chloro = median(chloro_mg_m3, na.rm = TRUE), 
                     `97.5_quant_chloro` = quantile(chloro_mg_m3, probs = c(0.975), na.rm = TRUE), 
                     max_chloro = max(chloro_mg_m3, na.rm = TRUE))
  
  # clean up
  rm(sst_NEA, sst_tNASS, sst_NWcoast, sst_Med, sst_GoMex, 
     sst_Ant, sst_Guy, sst_Ind, sst_NCal, sst_WFu, 
     sst_FPoly, sst_Hawai, sst_Calif_current, 
     sst_all_areas, Chloro_all_areas)
  
  
}