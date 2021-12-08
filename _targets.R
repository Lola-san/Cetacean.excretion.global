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
  ############### compute ratio of cetacean abundances where needed 
  # define data files
  tar_target(data_ASI_file,
             "data/abundance_surveys/ASI/ASI_Total_Sighting_Exchange_data.xlsx",
             format = "file"), # Sea Around us data
  tar_target(data_SCANSIII_air_file,
             "data/abundance_surveys/SCANSIII/aerial_sightings_by_block.xlsx",
             format = "file"), # GHS data
  # load data ASI
  tar_target(data_ASI, load_ASI(data_ASI_file)),
  # compute ratio ASI 
  tar_target(ratio_ASI, compute_ratio_ASI(data_ASI))
)
