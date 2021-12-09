################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# December 2021
# _targets.R
#
# Script decomposing with all steps of the analysis with target
################################################################################

library("targets")

# Source all functions contained in all files in the R directory
lapply(list.files(here::here("R"),
                  recursive = TRUE, full.names = T),
       source)


list(
  ############### compute ratio of cetacean abundances where needed ############
  # define data files
  tar_target(data_ASI_file,
             "data/abundance_surveys/ASI/ASI_Total_Sighting_Exchange_data.xlsx",
             format = "file"), # ASI - Mediterranean Sea
  tar_target(data_SCANSIII_air_file,
             "data/abundance_surveys/SCANSIII/aerial_sightings_by_block.xlsx",
             format = "file"), # SCANS III - air survey
  tar_target(data_SCANSIII_ship_file,
             "data/abundance_surveys/SCANSIII/ship_sightings_by_block.xlsx",
             format = "file"), # SCANS III - ship survey
  tar_target(data_REMMOA_ANTGUY_file,
             "data/abundance_surveys/REMMOA_ANTGUY/Obs-Effort_ANTGUY2017_RapportFinal.xlsx",
             format = "file"),
  # load data 
  tar_target(data_ASI, load_ASI(data_ASI_file)),
  tar_target(data_SCANS_air, load_SCANSIII_air(data_SCANSIII_air_file)),
  tar_target(data_SCANS_ship, load_SCANSIII_ship(data_SCANSIII_ship_file)),
  tar_target(data_REMMOA_ANTGUY, load_ASI(data_REMMOA_ANTGUY_file)),
  # compute ratios separately
  tar_target(ratio_ASI, compute_ratio_ASI(data_ASI)),
  tar_target(ratio_NEA, compute_ratio_NEA(data_SCANS_air, data_SCANS_ship)), 
  tar_target(ratio_REMMOA_ANTGUY, compute_ratio_REMMOA_ANTGUY(data_REMMOA_ANTGUY))
)
