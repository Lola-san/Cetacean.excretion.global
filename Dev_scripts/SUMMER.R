##--------------------------------------------------------------------------------------------------------
## SCRIPT : Extract data for SUMMER project
##
## Authors : Lola Gilbert 
## Last update : 2022-07
## R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
##--------------------------------------------------------------------------------------------------------


targets::tar_load(model_input)

# select only areas of interest : Atlantic and Mediterranean, oceanic parts 
unique(model_input$Geo_area)
unique(model_input$Eco_area)

SUMMER <- model_input |>
  dplyr::filter(Geo_area %in% c("NAtlantic", "NEAtlantic", "NWAtlantic", 
                                "GoMexico", "Antilles", "Guyana", "Med"), 
                Eco_area == "oceanic")

# list of species
unique(SUMMER$Species) # 29 species

colnames(SUMMER)

# abundances in areas
unique(SUMMER$Geo_area)
SUMMER |> 
  dplyr::filter(Geo_area == "NAtlantic") |>
  dplyr::select(Species, Surf_tot, Abund) |>
  tidyr::unnest(Abund)

unique(SUMMER$Geo_area) # NEAtlantic we used SAMM and not SCANS so no need 
SUMMER |> 
  dplyr::filter(Geo_area == "NWAtlantic") |>
  dplyr::select(Species, Surf_tot, Abund) |>
  tidyr::unnest(Abund)  

unique(SUMMER$Geo_area)
SUMMER |> 
  dplyr::filter(Geo_area == "GoMexico") |>
  dplyr::select(Species, Surf_tot, Abund) |>
  tidyr::unnest(Abund)  

unique(SUMMER$Geo_area) # we'll finish the Atlantic area first 
SUMMER |> 
  dplyr::filter(Geo_area == "Antilles") |>
  dplyr::select(Species, Surf_tot, Abund) |>
  tidyr::unnest(Abund)  

unique(SUMMER$Geo_area)
SUMMER |> 
  dplyr::filter(Geo_area == "Guyana") |>
  dplyr::select(Species, Surf_tot, Abund) |>
  tidyr::unnest(Abund)

# For the Mediterranean sea
targets::tar_load(abund_sp_all_SUMMER)

abund_sp_all_SUMMER |> 
  dplyr::filter(Geo_area %in% c("Med_E", "Med_W")) 


############### SPECIES CHARACTERISTICS ########################
SUMMER$Species

# select only one line per species (as there is many lines for all the places each species occurs)
SUMMER <- SUMMER[c(1, 4, 6, 7, 8, 12, 15, 16, 18, 
                   22, 27, 28, 32, 34, 36, 37, 39,
                   44, 45, 48, 49, 55, 57, 58, 59, 
                   63, 65, 66, 72),]

colnames(SUMMER)

# Body mass
SUMMER |>
  dplyr::ungroup() |>
  dplyr::select(Species, Mass) |>
  tidyr::unnest(Mass) |>
  print(n = Inf)

colnames(SUMMER)

# Mean Diet Quality
SUMMER |>
  dplyr::ungroup() |>
  dplyr::select(Species, NRJ_diet) |>
  tidyr::unnest(NRJ_diet) |>
  dplyr::group_by(Species) |>
  dplyr::summarise(mean_diet_quali = mean(value)) |>
  print(n = Inf)


# for other parameters it's part of output
targets::tar_load(model_output)

SUMMER2 <- model_output |>
  dplyr::filter(Geo_area %in% c("NAtlantic", "NEAtlantic", "NWAtlantic", 
                                "GoMexico", "Antilles", "Guyana", "Med"), 
                Eco_area == "oceanic")


# select only one line per species (as there is many lines for all the places each species occurs)
SUMMER2 <- SUMMER2[c(1, 4, 6, 7, 8, 12, 15, 16, 18, 
                   22, 27, 28, 32, 34, 36, 37, 39,
                   44, 45, 48, 49, 55, 57, 58, 59, 
                   63, 65, 66, 72),]

colnames(SUMMER2)

# Mean Diet Quality
SUMMER2 |>
  dplyr::ungroup() |>
  dplyr::select(Species, Indi_data) |>
  tidyr::unnest(Indi_data) |>
  dplyr::group_by(Species) |>
  dplyr::summarise(ADMR_mean = mean(ADMR), 
                   ADMR_min = min(ADMR), 
                   ADMR_max = max(ADMR), 
                   ration_mean = mean(Ration), 
                   ration_min = min(Ration), 
                   ration_max = max(Ration)) |>
  print(n = Inf)
