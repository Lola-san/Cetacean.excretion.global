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
  dplyr::filter(Geo_area %in% c("Med_E", "Med_W")) |>
  dplyr::select(Species, Surf_tot, Abund) |>
  tidyr::unnest(Abund)
