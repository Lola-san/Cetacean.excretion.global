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
  # define data files (only ASI, SCANSIII, REMMOA ANTGUY and Indian ocean)
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
             format = "file"), # REMMOA Antilles & Guyana 
  tar_target(data_REMMOA_Ind_file,
             "data/abundance_surveys/REMMOA_Indian/Sighting_OI_REMMOA_LEG_070721.csv",
             format = "file"), # REMMOA indian ocean
  # load data (only ASI, SCANSIII, REMMOA ANTGUY and Indian ocean)
  tar_target(data_ASI, load_xl(data_ASI_file)),
  tar_target(data_SCANS_air, load_xl(data_SCANSIII_air_file)),
  tar_target(data_SCANS_ship, load_xl(data_SCANSIII_ship_file)),
  tar_target(data_REMMOA_ANTGUY, load_xl(data_REMMOA_ANTGUY_file)),
  tar_target(data_REMMOA_Ind, load_csv(data_REMMOA_Ind_file)),
  #### compute ratios separately
  # for areas associated with files 
  tar_target(ratio_ASI, compute_ratio_ASI(data_ASI)),
  tar_target(ratio_NEA, compute_ratio_NEA(data_SCANS_air, data_SCANS_ship)), 
  tar_target(ratio_REMMOA_ANTGUY, compute_ratio_REMMOA_ANTGUY(data_REMMOA_ANTGUY)), 
  tar_target(ratio_REMMOA_Ind, compute_ratio_REMMOA_Ind(data_REMMOA_Ind)), 
  # and for areas not associated with files
  tar_target(ratio_REMMOA_FPol, compute_ratio_REMMOA_FPol()),
  tar_target(ratio_REMMOA_NCal, compute_ratio_REMMOA_NCal()),
  tar_target(ratio_REMMOA_WFu, compute_ratio_REMMOA_WFu()),
  tar_target(ratio_GoMex, compute_ratio_GoMex()),
  tar_target(ratio_Hawaii, compute_ratio_Hawaii()),
  tar_target(ratio_Calif, compute_ratio_Calif()), 
  #### bind ratio tibbles (two: one for REMMOA with groups and one other with 
  # just species)
  tar_target(ratio_full_REMMOAs, bind_ratio_REMMOAs(ratio_REMMOA_ANTGUY, 
                                                    ratio_REMMOA_Ind, 
                                                    ratio_REMMOA_FPol, 
                                                    ratio_REMMOA_NCal,
                                                    ratio_REMMOA_WFu)), 
  tar_target(ratio_full_others, bind_ratio_others(ratio_NEA, 
                                                    ratio_ASI, 
                                                    ratio_GoMex, 
                                                    ratio_Hawaii,
                                                    ratio_Calif)) 
)
