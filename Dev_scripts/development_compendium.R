################################################################################
# Lola Gilbert lola.gilbert@univ-lr.fr
# 2021/12/08
#
# development_compendium.R
#
# Script with basic command to create and develop compendium
################################################################################

############################### TO DO ONLY ONCE
# create folder with development script
dir.create("Dev_scripts")

# initiate renv
renv::init()

# load package to create compendium
renv::install("benmarwick/rrtools")

# create compendium 
rrtools::use_compendium(path = "../Excretion.global.compendium/",
                        open = FALSE)

# create folder that will contain all R function scripts
dir.create("R")

# create data and output folders
dir.create("data")
dir.create("output")
dir.create("manuscript")

# install packages 
renv::install("targets")
renv::install("readxl")
renv::install("dplyr")
renv::install("tidyr")
renv::install("purrr")
renv::install("sensobol")
renv::install("ncdf4")

############################### renv update
# check renv status
renv::status()

# update renv lock file
renv::snapshot()

############################### targets
# to call target 
targets::tar_make()

# restore or activate renv 
renv::activate()
