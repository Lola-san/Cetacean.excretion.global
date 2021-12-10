##--------------------------------------------------------------------------------------------------------
## SCRIPT : % of prey groups in diet of cetaceans 
##
## Authors : Lola Gilbert
## Last update : 2021-06
## R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
##--------------------------------------------------------------------------------------------------------

lapply(c("readxl", "tidyverse", "writexl"), library, character.only=TRUE)


### clean up
rm(list = ls())
wd <- "C:/Users/lgilbe01/Desktop/PhD_2020-2023/Analyses/01.Excretion_global" # 
Datawd <- paste(wd, "data", sep = "/") # 
Outwd <- paste(wd, "output", sep = "/") # 

#### load data
# df with diets for each species/Block
diets_df <- read_excel(paste(Datawd, 
                             "/diets_pred/All_sp_pred_diet.xlsx", 
                             sep = "/"), sheet = 1)
summary(diets_df)


######################################################################################################
############## 1 - Attribution of prey groups to prey species in diets_df  ###########################
######################################################################################################

## extract the list of species to associate them to ecological prey groups (by hand...)
# sp_PG <- read_excel(paste(Datawd,
#                           "/diets_pred/Sp_preys_PG_js_lg.xlsx",
#                           sep = "/"), sheet = 1)
# nutri_df <- read_excel(paste(Datawd,
#                              "/nutrient_prey/Nuts_in_preys_full.xlsx",
#                              sep = "/"), sheet = 1)
#  
# all_sp <- diets_df %>%
#   left_join(nutri_df, by = c("Sp_prey", "Genus", "Family", "Order", "Taxa")) %>%
#   ungroup() %>%
#   select(c("Sp_prey", "Genus", "Family", "Order", "Taxa")) %>%
#   unique() %>%
#   dplyr::mutate(Prey_group = NA)

# # save and complete prey_group by hand
# write.csv2(all_sp, paste0(Datawd, "/diets_pred/Sp_PG.csv"), row.names = FALSE)

# # IF SOME NEW SPECIES/DIETS WERE ADDED, START FROM THE FIRST FILE AND REPLACE IT
# # df with each prey species and corresponding prey groups
# sp_PG <- read_excel(paste(Datawd,
#                           "/diets_pred/Sp_preys_PG_js_lg_20210706.xlsx",
#                           sep = "/"), sheet = 1)
# nutri_df <- read_excel(paste(Datawd,
#                              "/nutrient_prey/Nuts_in_preys_full_corrected.xlsx",
#                              sep = "/"), sheet = 1)
# 
# # find all species from diets_df not already in sp_PG
# newsp_diet <- diets_df %>%
#   left_join(sp_PG, by = c("Sp_prey", "Genus", "Family", "Order", "Taxa"), keep = FALSE) %>%
#   ungroup() %>%
#   dplyr::select(c("Sp_prey", "Genus", "Family", "Order", "Taxa", "Prey_group")) %>%
#   unique()
# 
# newsp_nut <- nutri_df %>%
#   left_join(sp_PG, by = c("Sp_prey", "Genus", "Family", "Order", "Taxa")) %>%
#   ungroup() %>%
#   select(c("Sp_prey", "Genus", "Family", "Order", "Taxa", "Prey_group")) %>%
#   unique()
# 
# all_sp <- rbind(newsp_diet, newsp_nut[which(!(newsp_nut$Sp_prey %in% newsp_diet$Sp_prey)),]) %>%
#   arrange(Order, Family, Genus, Sp_prey)
# 
# sp_PG <- sp_PG %>%
#   arrange(Order, Family, Genus, Sp_prey)
# 
# # save and complete prey_group by hand
# write_xlsx(all_sp, paste0(Datawd, "/diets_pred/Sp_PG_20210723.xlsx"),
#             col_names = TRUE,
#            format_headers = TRUE)
# 
# rm(nutri_df, all_sp, newsp_nut, newsp_diet, sp_PG)

## ONCE IT'S DONE 
# import data
sp_PG <- read_excel(paste(Datawd, 
                          "/diets_pred/Sp_preys_PG_js_lg_20210723.xlsx", 
                          sep = "/"), sheet = 1)
summary(sp_PG)


diets_df <- diets_df %>%
  left_join(sp_PG, key = c("Sp_prey", "Genus", "Family", "Order", "Taxa"), keep = FALSE) 

rm(sp_PG)

######################################################################################################
######################################  2 - Check the data  ##########################################
######################################################################################################

# %W is in character, change it to numeric
diets_df$W <- as.numeric(diets_df$W)
summary(diets_df) # OK

# drop lines where W is NA or zero
diets_df <- diets_df %>% 
  drop_na(W) %>%
  filter(W != 0)
summary(diets_df) # OK

# is sum for each species/Block = 100 ? 
diets_df %>%
  group_by(Species, Block, Source) %>%
  dplyr::summarise(sum = sum(W)) 

diets_df <- diets_df %>%
  group_by(Species, Block, Source) %>%
  dplyr::mutate(W = (100*W)/sum(W)) %>%
  arrange(Code_sp, Block)


######################################################################################################
#############  3 - Attribute diets for each pair of Species/Block where there is no data #############
######################################################################################################

# this has to be done species by species 
# When quantitative data is available, take means of all sources (by prey group), with difference between oceanic/neritic if available
# when nothing is available, an approximate diet per prey group has to be defined from qualitative data


unique(diets_df$Code_sp) # do not forget any species 

##################### Balaenoptera acutus - Minke whale ####################
diets_df %>% 
  filter(Code_sp == "Bala_acu") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# several sources here so we'll take means of these

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  group_by(Code_sp, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) %>% # replace NA with zeros
  filter(Code_sp == "Bala_acu")

# We'll take the means of these sources, at the level of prey groups 

diets_pred <- rbind(diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Bala_acu") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Bala_acu") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# create and fill summary df of all diets used  
diets_summary <- diets_df %>%
  group_by(Code_sp, Species, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) %>% # replace NA with zeros
  filter(Code_sp == "Bala_acu") %>%
  group_by(Code_sp, Species) %>%
  dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                   `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                   `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                   `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                   `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                   `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                   `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                   `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                   `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                   `Fish undetermined` = mean(`Fish undetermined`),
                   `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                   `Crustaceans` = mean(`Crustaceans`), 
                   `Zooplankton` = mean(`Zooplankton`), 
                   Sources = str_c(Source, collapse = ", ")) %>%
  nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
  mutate(waters = "all") %>% 
  dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
  mutate(Type_sources = "quantitative", 
         Copied_from_other_sp = "no", 
         other_sp_code = NA)

# next
unique(diets_df$Code_sp) # do not forget any species 



##################### Berardius bairdii - Baird's beaked whale ####################
diets_df %>% 
  filter(Code_sp == "Bera_bai") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# so there is data in one block only : we'll use the %W per prey group from this

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Bera_bai") %>%
  group_by(Geo_area, Block, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros


diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Bera_bai") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Bera_bai") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Bera_bai") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 


# next
unique(diets_df$Code_sp) # do not forget any species



##################### Balaenoptera bonaerensis - Antarctic Minke whale ####################
diets_df %>% 
  filter(Code_sp == "Bala_bon") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# but from qualitative data we can say it eats mostly zooplankton but also a little of fish

diets_pred <- rbind(diets_pred, 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Bala_bon", "Balaenoptera bonaerensis", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 2.5, 
                             `Miscellanous benthodemersal fish` = 2.5,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 95, 
                             Sources = "Tamura & Kenji 2009,
                              Friedlaender et al 2014") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Bala_bon", "Balaenoptera bonaerensis", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 2.5, 
                             `Miscellanous benthodemersal fish` = 2.5,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 95, 
                             Sources = "Tamura & Kenji 2009,
                              Friedlaender et al 2014") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)



# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                               "Bala_bon", "Balaenoptera bonaerensis", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 0, 
                                `Large demersal energy-rich fish` = 0, 
                                `Small schooling energy-lean fish` = 0, 
                                `Small schooling energy-rich fish` = 2.5, 
                                `Miscellanous benthodemersal fish` = 2.5,
                                `Miscellanous pelagic fish` = 0, 
                                `Muscular pelagic cephalopods` = 0, 
                                `Gelatinous pelagic cephalopods` = 0,
                                `Bottom cephalopods` = 0, 
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0, 
                                `Crustaceans` = 0, 
                                `Zooplankton` = 95, 
                                Sources = "Tamura & Kenji 2009,
                              Friedlaender et al 2014") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 




##################### Balaenoptera borealis - Sei whale ####################
diets_df %>% 
  filter(Code_sp == "Bala_bor") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# but from qualitative data we can say it eats 100 % zooplankton 

diets_pred <- rbind(diets_pred, 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Bala_bor", "Balaenoptera borealis", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 0, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 100, 
                             Sources = "Kawamura 1969, Nishimoto et al 1952,
                              Watkins & Schevill 1979, Flinn et al 2002,
                              Leonardi et al 2011, Horwood 2018") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Bala_bor", "Balaenoptera borealis", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 0, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 100, 
                             Sources = "Kawamura 1969, Nishimoto et al 1952,
                              Watkins & Schevill 1979, Flinn et al 2002,
                              Leonardi et al 2011, Horwood 2018") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)



# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                               "Bala_bor", "Balaenoptera borealis", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 0, 
                                `Large demersal energy-rich fish` = 0, 
                                `Small schooling energy-lean fish` = 0, 
                                `Small schooling energy-rich fish` = 0, 
                                `Miscellanous benthodemersal fish` = 0,
                                `Miscellanous pelagic fish` = 0, 
                                `Muscular pelagic cephalopods` = 0, 
                                `Gelatinous pelagic cephalopods` = 0,
                                `Bottom cephalopods` = 0, 
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0, 
                                `Crustaceans` = 0, 
                                `Zooplankton` = 100, 
                                Sources = "Kawamura 1969, Nishimoto et al 1952,
                              Watkins & Schevill 1979, Flinn et al 2002,
                              Leonardi et al 2011, Horwood 2018") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 



##################### Balaenoptera edeni - Bryde's whale ####################
diets_df %>% 
  filter(Code_sp == "Bala_ede") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# but from qualitative data we can say it eats mostly small schooling nrj-rich fish with a bit of zooplankton 

diets_pred <- rbind(diets_pred, 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Bala_ede", "Balaenoptera edeni", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 90, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 10, 
                             Sources = "Siciliano et al 2004, Tershy et al 1992") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Bala_ede", "Balaenoptera edeni", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 90, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 10, 
                             Sources = "Siciliano et al 2004, Tershy et al 1992") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)



# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                               "Bala_ede", "Balaenoptera edeni", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 0, 
                                `Large demersal energy-rich fish` = 0, 
                                `Small schooling energy-lean fish` = 0, 
                                `Small schooling energy-rich fish` = 90, 
                                `Miscellanous benthodemersal fish` = 0,
                                `Miscellanous pelagic fish` = 0, 
                                `Muscular pelagic cephalopods` = 0, 
                                `Gelatinous pelagic cephalopods` = 0,
                                `Bottom cephalopods` = 0, 
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0, 
                                `Crustaceans` = 0, 
                                `Zooplankton` = 10, 
                                Sources = "Siciliano et al 2004, Tershy et al 1992") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 



##################### Balaenoptera musculus - Blue whale ####################
diets_df %>% 
  filter(Code_sp == "Bala_mus") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# but from qualitative data we can say it eats 100 % zooplankton 

diets_pred <- rbind(diets_pred, 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Bala_mus", "Balaenoptera musculus", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 0, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 100, 
                             Sources = "Figueiredo et al 2014, Gavrilchuck et al 2014,
                              Lesage et al 2018") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Bala_mus", "Balaenoptera musculus", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 0, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 100, 
                             Sources = "Figueiredo et al 2014, Gavrilchuck et al 2014,
                              Lesage et al 2018") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                               "Bala_mus", "Balaenoptera musculus", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 0, 
                                `Large demersal energy-rich fish` = 0, 
                                `Small schooling energy-lean fish` = 0, 
                                `Small schooling energy-rich fish` = 0, 
                                `Miscellanous benthodemersal fish` = 0,
                                `Miscellanous pelagic fish` = 0, 
                                `Muscular pelagic cephalopods` = 0, 
                                `Gelatinous pelagic cephalopods` = 0,
                                `Bottom cephalopods` = 0, 
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0, 
                                `Crustaceans` = 0, 
                                `Zooplankton` = 100, 
                                Sources = "Figueiredo et al 2014, Gavrilchuck et al 2014,
                              Lesage et al 2018") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 



##################### Balaenoptera omurai - Omurai's whale ####################
diets_df %>% 
  filter(Code_sp == "Bala_omu") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# but from qualitative data we can say it eats 100 % zooplankton 

diets_pred <- rbind(diets_pred, 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Bala_omu", "Balaenoptera omurai", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 0, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 100, 
                             Sources = "Cerchio et al 2015,
                              Cerchio & Tadasu 2018, Laboute & Borsa 2018") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Bala_omu", "Balaenoptera omurai", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 0, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 100, 
                             Sources = "Cerchio et al 2015,
                              Cerchio & Tadasu 2018, Laboute & Borsa 2018") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                               "Bala_omu", "Balaenoptera omurai", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 0, 
                                `Large demersal energy-rich fish` = 0, 
                                `Small schooling energy-lean fish` = 0, 
                                `Small schooling energy-rich fish` = 0, 
                                `Miscellanous benthodemersal fish` = 0,
                                `Miscellanous pelagic fish` = 0, 
                                `Muscular pelagic cephalopods` = 0, 
                                `Gelatinous pelagic cephalopods` = 0,
                                `Bottom cephalopods` = 0, 
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0, 
                                `Crustaceans` = 0, 
                                `Zooplankton` = 100, 
                                Sources = "Cerchio et al 2015,
                              Cerchio & Tadasu 2018, Laboute & Borsa 2018") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA))


# next
unique(diets_df$Code_sp) # do not forget any species 




##################### Balaenoptera physalus - Fin whale ####################
diets_df %>% 
  filter(Code_sp == "Bala_phy") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# so there is data in one block only 

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Bala_phy") %>%
  group_by(Geo_area, Block, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

# we use a 100% zooplankton diet everywhere, no quantitative data with %W for fish, even though they were occasionally observed feeding on small schooling fish
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Bala_phy") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Bala_phy") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Bala_phy") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 


# next
unique(diets_df$Code_sp) # do not forget any species


##################### Delphinus capensis - long beaked common dolphin ####################
diets_df %>% 
  filter(Code_sp == "Delp_cap") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

#only one source, W% per prey group

diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Delp_cap") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Delp_cap") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Delp_cap") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 


# next
unique(diets_df$Code_sp) # do not forget any species


##################### Delphinus delphis - Short-beaked common dolphin ####################
diets_df %>% 
  filter(Code_sp == "Delp_del") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# here there is data on several blocks from both shelf (4 refs) and oceanic area (1 ref)
# we'll take the means of shelf ref for shelf areas and values per prey groups of the oceanic ref for oceanic areas

diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Delp_del", Block %in% c("Alboran", "A", "B", "Aegean")) %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Delp_del", Block %in% c("E1")) %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Delp_del", Block %in% c("Alboran", "A", "B", "Aegean")) %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "neritic") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA),
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Delp_del", Block %in% c("E1")) %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "oceanic") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA) )


# next
unique(diets_df$Code_sp) # do not forget any species



##################### Eubalaena glacialis - North Atlantic Right whale ####################
diets_df %>% 
  filter(Code_sp == "Euba_gla") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# but from qualitative data we can say it eats 100 % zooplankton 

diets_pred <- rbind(diets_pred, 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Euba_gla", "Eubalaena glacialis", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 0, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 100, 
                             Sources = "Watkins & Schevill 1979,
                              Kenney 2009, Pendleton et al 2012") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Euba_gla", "Eubalaena glacialis", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 0, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 100, 
                             Sources = "Watkins & Schevill 1979,
                              Kenney 2009, Pendleton et al 2012") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                               "Euba_gla", "Eubalaena glacialis", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 0, 
                                `Large demersal energy-rich fish` = 0, 
                                `Small schooling energy-lean fish` = 0, 
                                `Small schooling energy-rich fish` = 0, 
                                `Miscellanous benthodemersal fish` = 0,
                                `Miscellanous pelagic fish` = 0, 
                                `Muscular pelagic cephalopods` = 0, 
                                `Gelatinous pelagic cephalopods` = 0,
                                `Bottom cephalopods` = 0, 
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0, 
                                `Crustaceans` = 0, 
                                `Zooplankton` = 100, 
                                Sources = "Watkins & Schevill 1979,
                              Kenney 2009, Pendleton et al 2012") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA))

# next
unique(diets_df$Code_sp) # do not forget any species 




##################### Feresa attenuata - pygmy killer whales ####################
diets_df %>% 
  filter(Code_sp == "Fere_att") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# Qualitative data is also really scarce, really qualitative and for strandings or isotope it is just for at most 2 individuals
# Suggests it has a diet with mostly squid, having a teutophageous isotopic signature, but also fish are found

### FOR THE MOMENT, we'll use the mean diet of Glob_melas


# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Glob_mel") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = paste(c("[Fere_att] L?pez-Su?rez et al 2012, O'Dwyer et al 2015, 
                                                            Elorriaga-Verplancken et al 2016, Zerbini & Santos 1997,
                                                            Aguiar dos Santos & Haimovici 2001, Donahue & Perryman 2009, 
                                                            Baird 2018, Sekiguchi et al 1992, [Glob_mel]"), 
                                                       str_c(Source, collapse = ", "), 
                                                       sep = " ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf", 
                             Code_sp = "Fere_att", 
                             Species = "Feresa attenuata") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Glob_mel") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = paste(c("[Fere_att] L?pez-Su?rez et al 2012, O'Dwyer et al 2015, 
                                                            Elorriaga-Verplancken et al 2016, Zerbini & Santos 1997,
                                                            Aguiar dos Santos & Haimovici 2001, Donahue & Perryman 2009, 
                                                            Baird 2018, Sekiguchi et al 1992, [Glob_mel]"), 
                                                       str_c(Source, collapse = ", "), 
                                                       sep = " ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic", 
                             Code_sp = "Fere_att", 
                             Species = "Feresa attenuata") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Glob_mel") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = paste(c("[Fere_att] L?pez-Su?rez et al 2012, O'Dwyer et al 2015, 
                                                            Elorriaga-Verplancken et al 2016, Zerbini & Santos 1997,
                                                            Aguiar dos Santos & Haimovici 2001, Donahue & Perryman 2009, 
                                                            Baird 2018, Sekiguchi et al 1992, [Glob_mel]"), 
                                                          str_c(Source, collapse = ", "), 
                                                          sep = " ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all", 
                                Code_sp = "Fere_att", 
                                Species = "Feresa attenuata") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative (Fere_att) & quantitative (Glob_mel)", 
                                Copied_from_other_sp = "yes", 
                                other_sp_code = "Glob_mel")) 


# next
unique(diets_df$Code_sp) # do not forget any species





##################### Globicephala macrorhynchus - short-finned pilot whales ####################
diets_df %>% 
  filter(Code_sp == "Glob_mac") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# qualitative data suggest a teutophageous diet with mostly pelagic cephalopods and a little bit a miscellanous fish

diets_pred <- rbind(diets_pred,
                    tribble(~ Code_sp, ~ Species, ~ Eco_area,
                            "Glob_mac", "Globicephala macrorhynchus", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 0,
                             `Large demersal energy-rich fish` = 0,
                             `Small schooling energy-lean fish` = 0,
                             `Small schooling energy-rich fish` = 0,
                             `Miscellanous benthodemersal fish` = 2,
                             `Miscellanous pelagic fish` = 0,
                             `Muscular pelagic cephalopods` = 49,
                             `Gelatinous pelagic cephalopods` = 49,
                             `Bottom cephalopods` = 0,
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0,
                             `Crustaceans` = 0,
                             `Zooplankton` = 0,
                             Sources = "Hern?ndez-Garc?a et al 1994, Hacker 1986,
                             Mintzer et al 2008, Fernandez et al 2009") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    tribble(~ Code_sp, ~ Species, ~ Eco_area,
                            "Glob_mac", "Globicephala macrorhynchus", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 0,
                             `Large demersal energy-rich fish` = 0,
                             `Small schooling energy-lean fish` = 0,
                             `Small schooling energy-rich fish` = 0,
                             `Miscellanous benthodemersal fish` = 2,
                             `Miscellanous pelagic fish` = 0,
                             `Muscular pelagic cephalopods` = 49,
                             `Gelatinous pelagic cephalopods` = 49,
                             `Bottom cephalopods` = 0,
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0,
                             `Crustaceans` = 0,
                             `Zooplankton` = 0,
                             Sources = "Hern?ndez-Garc?a et al 1994, Hacker 1986,
                             Mintzer et al 2008, Fernandez et al 2009") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area,
                               "Glob_mac", "Globicephala macrorhynchus", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 0,
                                `Large demersal energy-rich fish` = 0,
                                `Small schooling energy-lean fish` = 0,
                                `Small schooling energy-rich fish` = 0,
                                `Miscellanous benthodemersal fish` = 2,
                                `Miscellanous pelagic fish` = 0,
                                `Muscular pelagic cephalopods` = 49,
                                `Gelatinous pelagic cephalopods` = 49,
                                `Bottom cephalopods` = 0,
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0,
                                `Crustaceans` = 0,
                                `Zooplankton` = 0,
                                Sources = "Hern?ndez-Garc?a et al 1994, Hacker 1986,
                             Mintzer et al 2008, Fernandez et al 2009") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA))


# next
unique(diets_df$Code_sp) # do not forget any species



##################### Globicephala melas - long-finned pilot whales ####################
diets_df %>% 
  filter(Code_sp == "Glob_mel") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# several sources here so we'll take means of these

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Glob_mel") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0)  # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Glob_mel") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Glob_mel") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Glob_mel") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 


# next
unique(diets_df$Code_sp) # do not forget any species



##################### Grampus griseus - Risso's dolphin ####################
diets_df %>% 
  filter(Code_sp == "Gram_gri") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# several sources here so we'll take means of these


# first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Gram_gri") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Gram_gri") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Gram_gri") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Gram_gri") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 


# next
unique(diets_df$Code_sp) # do not forget any species




##################### Hyperoodon ampullatus - Northern bottlenose whale ####################
diets_df %>% 
  filter(Code_sp == "Hype_amp") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# several sources here so we'll take means of these 

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Hype_amp") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Hype_amp") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Hype_amp") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Hype_amp") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 


# next
unique(diets_df$Code_sp) # do not forget any species 




##################### Indopacetus pacificus - Longman's beaked whale ####################
diets_df %>% 
  filter(Code_sp == "Indo_pac") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# No quantitative data for this species, species is mostly unknown
# but qualitive data suggest diet of cephalopods exclusively

### FOR THE MOMENT we'll use mean of Zyph_cav

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Ziph_cav") %>%
  group_by(Geo_area, Block, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Ziph_cav") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf", 
                             Species = "Indopacetus pacificus", 
                             Code_sp = "Indo_pac", 
                             Sources = c("Yatabe et al 2010")) %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Ziph_cav") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic", 
                             Species = "Indopacetus pacificus", 
                             Code_sp = "Indo_pac", 
                             Sources = c("Yatabe et al 2010")) %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Ziph_cav") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = paste(c("[Indo_pac] Yatabe et al 2010, [Ziph_cav]"), 
                                                          str_c(Source, collapse = ", "), 
                                                          sep = " ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all", 
                                Species = "Indopacetus pacificus", 
                                Code_sp = "Indo_pac") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative (Indo_pac) & quantitative (Ziph_cav)", 
                                Copied_from_other_sp = "yes", 
                                other_sp_code = "Ziph_cav")) 


# next
unique(diets_df$Code_sp) # do not forget any species 




##################### Kogia species (K. sima & K. breviceps) - Dwarf and Pygmy sperm whale ####################
diets_df %>% 
  filter(Code_sp == "Kogi_spp") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# several sources here so we'll take means of these

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Kogi_spp") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Kogi_spp") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Kogi_spp") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Kogi_spp") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 


# next
unique(diets_df$Code_sp) # do not forget any species 

##################### Lagenorhynchus acutus - Atlantic white-sided dolphin ####################
diets_df %>% 
  filter(Code_sp == "Lage_acu") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# so there is data in one block only 

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Lage_acu") %>%
  group_by(Geo_area, Block, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Lage_acu") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Lage_acu") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Lage_acu") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 



##################### Lagenorhynchus albirostris - white-beaked dolphin ####################
diets_df %>% 
  filter(Code_sp == "Lage_alb") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# several sources here so we'll take means of these 

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Lage_alb") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0)  # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Lage_alb") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Lage_alb") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Lage_alb") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 



##################### Lissodelphis borealis - Northern right whale dolphins ####################
diets_df %>% 
  filter(Code_sp == "Liss_bor") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species

# Approximate diet defined from qualitative data

diets_pred <- rbind(diets_pred,
                    tribble(~ Code_sp, ~ Species, ~ Eco_area,
                            "Liss_bor", "Lissodelphis borealis", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 5,
                             `Large demersal energy-rich fish` = 0,
                             `Small schooling energy-lean fish` = 5,
                             `Small schooling energy-rich fish` = 40,
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0,
                             `Muscular pelagic cephalopods` = 25,
                             `Gelatinous pelagic cephalopods` = 25,
                             `Bottom cephalopods` = 0,
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0,
                             `Crustaceans` = 0,
                             `Zooplankton` = 0,
                             Sources = "Leatherwood & Walker 1979, Jefferson et al 1994, 
                             Lipsky & Brownell 2018") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    tribble(~ Code_sp, ~ Species, ~ Eco_area,
                            "Liss_bor", "Lissodelphis borealis", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 5,
                             `Large demersal energy-rich fish` = 0,
                             `Small schooling energy-lean fish` = 5,
                             `Small schooling energy-rich fish` = 40,
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0,
                             `Muscular pelagic cephalopods` = 25,
                             `Gelatinous pelagic cephalopods` = 25,
                             `Bottom cephalopods` = 0,
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0,
                             `Crustaceans` = 0,
                             `Zooplankton` = 0,
                             Sources = "Leatherwood & Walker 1979, Jefferson et al 1994, 
                             Lipsky & Brownell 2018") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area,
                               "Liss_bor", "Lissodelphis borealis", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 5,
                                `Large demersal energy-rich fish` = 0,
                                `Small schooling energy-lean fish` = 5,
                                `Small schooling energy-rich fish` = 40,
                                `Miscellanous benthodemersal fish` = 0,
                                `Miscellanous pelagic fish` = 0,
                                `Muscular pelagic cephalopods` = 25,
                                `Gelatinous pelagic cephalopods` = 25,
                                `Bottom cephalopods` = 0,
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0,
                                `Crustaceans` = 0,
                                `Zooplankton` = 0,
                                Sources = "Leatherwood & Walker 1979, Jefferson et al 1994, 
                             Lipsky & Brownell 2018") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA))

# next
unique(diets_df$Code_sp) # do not forget any species




##################### Lagenodelphis hosei - Fraser's dolphin #################
diets_df %>% 
  filter(Code_sp == "Lage_hos") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# quantitative data but n very very limited...

# take a look at existing data 
diets_df %>%
  filter(Code_sp == "Lage_hos") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0)  # replace NA with zeros

# and not really in accordance with qualitative data 
# qualitative data with just %n, %O but with higher n (27 & 37) and bycaught individuals suggest significant consumption of fish 
# most common being small schooling nrj rich and small schooling nrj lean 
# so we "cooked" a diet taking into account both quantitative and qualitative data 


diets_pred <- rbind(diets_pred, 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area,
                            "Lage_hos", "Lagenodelphis hosei", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 0,
                             `Large demersal energy-rich fish` = 0,
                             `Small schooling energy-lean fish` = 15,
                             `Small schooling energy-rich fish` = 15,
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0,
                             `Muscular pelagic cephalopods` = 30,
                             `Gelatinous pelagic cephalopods` = 40,
                             `Bottom cephalopods` = 0,
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0,
                             `Crustaceans` = 0,
                             `Zooplankton` = 0,
                             Sources = "Sekiguchi et al 1992, Fernandez et al 2009, 
                             Di Beneditto et al 2001, Dolar et al 2003, Wang et al 2012") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    tribble(~ Code_sp, ~ Species, ~ Eco_area,
                            "Lage_hos", "Lagenodelphis hosei", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 0,
                             `Large demersal energy-rich fish` = 0,
                             `Small schooling energy-lean fish` = 15,
                             `Small schooling energy-rich fish` = 15,
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0,
                             `Muscular pelagic cephalopods` = 30,
                             `Gelatinous pelagic cephalopods` = 40,
                             `Bottom cephalopods` = 0,
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0,
                             `Crustaceans` = 0,
                             `Zooplankton` = 0,
                             Sources = "Sekiguchi et al 1992, Fernandez et al 2009, 
                              Di Beneditto et al 2001, Dolar et al 2003, Wang et al 2012") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area,
                               "Lage_hos", "Lagenodelphis hosei", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 0,
                                `Large demersal energy-rich fish` = 0,
                                `Small schooling energy-lean fish` = 15,
                                `Small schooling energy-rich fish` = 15,
                                `Miscellanous benthodemersal fish` = 0,
                                `Miscellanous pelagic fish` = 0,
                                `Muscular pelagic cephalopods` = 30,
                                `Gelatinous pelagic cephalopods` = 40,
                                `Bottom cephalopods` = 0,
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0,
                                `Crustaceans` = 0,
                                `Zooplankton` = 0,
                                Sources = "Sekiguchi et al 1992, Fernandez et al 2009, 
                                Di Beneditto et al 2001, Dolar et al 2003, Wang et al 2012") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative & quantitative (n very limited)", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA))


# next
unique(diets_df$Code_sp) # do not forget any species 


############ Lagenorhynchus obliquidens - Pacific white-sided dolphin #########
diets_df %>% 
  filter(Code_sp == "Lage_obl") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# One source with quantitative data 

# Take a look at existing data 
diets_df %>%
  filter(Code_sp == "Lage_obl") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0)  # replace NA with zeros

# we'll take the mean of these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Lage_obl") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Lage_obl") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Lage_obl") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 


##################### Mesospp ####################

diets_df %>% 
  filter(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur"))  %>%
  group_by(Code_sp, Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source),
                   nb_indiv = unique(nb_indiv))

# # # first take a look at existing data 
diets_df %>%
  filter(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur")) %>%
  group_by(Code_sp, Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros


# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur")) %>%
                      mutate(Code_sp = case_when(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur") ~ "Meso_spp", 
                                                 TRUE ~ Code_sp), 
                             Species = case_when(Species %in% c("Mesoplodon bidens", "Mesoplodon layardii", 
                                                                "Mesoplodon densirostris", 
                                                                "Mesoplodon mirus", "Mesoplodon europaeus") ~ "Mesoplodon spp", 
                                                 TRUE ~ Species)) %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur")) %>%
                      mutate(Code_sp = case_when(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur") ~ "Meso_spp", 
                                                 TRUE ~ Code_sp), 
                             Species = case_when(Species %in% c("Mesoplodon bidens", "Mesoplodon layardii", "Mesoplodon densirostris", 
                                                                "Mesoplodon mirus", "Mesoplodon europaeus") ~ "Mesoplodon spp", 
                                                 TRUE ~ Species)) %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur")) %>%
                         mutate(Code_sp = case_when(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur") ~ "Meso_spp", 
                                                    TRUE ~ Code_sp), 
                                Species = case_when(Species %in% c("Mesoplodon bidens", "Mesoplodon layardii", 
                                                                   "Mesoplodon densirostris", 
                                                                   "Mesoplodon mirus", "Mesoplodon europaeus") ~ "Mesoplodon spp", 
                                                    TRUE ~ Species)) %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 


############### Megaptera novaeangliae - Humpback whale ####################
diets_df %>% 
  filter(Code_sp == "Mega_nov") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# its diet may vary depending on areas, but globally they are known to feed on small schooling nrj-rich fish and zooplankton

diets_pred <- rbind(diets_pred, 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Mega_nov", "Megaptera novaeangliae", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 50, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 50, 
                             Sources = "Watkins & Schevill 1979, 
                              Witteven et al 2006, Filatova et al 2013,
                              Ryan et al 2014, Haro et al 2016,
                              Fleming et al 2016, Claham 2018"
                      ) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Mega_nov", "Megaptera novaeangliae", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 0, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 50, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 0, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 50, 
                             Sources = "Watkins & Schevill 1979, 
                              Witteven et al 2006, Filatova et al 2013,
                              Ryan et al 2014, Haro et al 2016,
                              Fleming et al 2016, Claham 2018") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                               "Mega_nov", "Megaptera novaeangliae", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 0, 
                                `Large demersal energy-rich fish` = 0, 
                                `Small schooling energy-lean fish` = 0, 
                                `Small schooling energy-rich fish` = 50, 
                                `Miscellanous benthodemersal fish` = 0,
                                `Miscellanous pelagic fish` = 0, 
                                `Muscular pelagic cephalopods` = 0, 
                                `Gelatinous pelagic cephalopods` = 0,
                                `Bottom cephalopods` = 0, 
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0, 
                                `Crustaceans` = 0, 
                                `Zooplankton` = 50, 
                                Sources = "Watkins & Schevill 1979, 
                              Witteven et al 2006, Filatova et al 2013,
                              Ryan et al 2014, Haro et al 2016,
                              Fleming et al 2016, Claham 2018") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA))

# next
unique(diets_df$Code_sp) # do not forget any species 

############### Orcinus orca - Killer whale ####################
diets_df %>% 
  filter(Code_sp == "Orci_orc") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# we'll consider here only fish eating eco types
# qualitative data mention several prey species, real "cooking" here 

diets_pred <- rbind(diets_pred, 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Orci_orc", "Orcinus orca", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 20, 
                             `Large demersal energy-rich fish` = 35, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 35, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 5, 
                             `Gelatinous pelagic cephalopods` = 5,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 0, 
                             Sources = "Simil? et al 1996, 
                              Saulitis et al 2000, Aguiar dos Santos & Haimovici 2001,
                              Volkova et al 2019"
                      ) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Orci_orc", "Orcinus orca", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 20, 
                             `Large demersal energy-rich fish` = 35, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 35, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 0, 
                             `Muscular pelagic cephalopods` = 5, 
                             `Gelatinous pelagic cephalopods` = 5,
                             `Bottom cephalopods` = 0, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 0, 
                             Sources = "Simil? et al 1996, 
                              Saulitis et al 2000, Aguiar dos Santos & Haimovici 2001,
                              Volkova et al 2019") %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                               "Orci_orc", "Orcinus orca", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 20, 
                                `Large demersal energy-rich fish` = 35, 
                                `Small schooling energy-lean fish` = 0, 
                                `Small schooling energy-rich fish` = 35, 
                                `Miscellanous benthodemersal fish` = 0,
                                `Miscellanous pelagic fish` = 0, 
                                `Muscular pelagic cephalopods` = 5, 
                                `Gelatinous pelagic cephalopods` = 5,
                                `Bottom cephalopods` = 0, 
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0, 
                                `Crustaceans` = 0, 
                                `Zooplankton` = 0, 
                                Sources = "Simil? et al 1996, 
                              Saulitis et al 2000, Aguiar dos Santos & Haimovici 2001,
                              Volkova et al 2019") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA))

# next
unique(diets_df$Code_sp) # do not forget any species 


##################### Pseudorca crassidens ####################
diets_df %>% 
  filter(Code_sp == "Pseu_cra") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# several sources here so we'll take means of these

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Pseu_cra") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0)  # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Pseu_cra") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Pseu_cra") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Pseu_cra") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 


# next
unique(diets_df$Code_sp) # do not forget any species 




##################### Phocoenoides dalli - Dall's porpoise ####################
diets_df %>% 
  filter(Code_sp == "Phoc_dal") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# several sources here so we'll take means of these

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Phoc_dal") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0)  # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Phoc_dal") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Phoc_dal") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Phoc_dal") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 




##################### Peponocephala electra - Melon-headed whale ####################
diets_df %>% 
  filter(Code_sp == "Pepo_ele") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# so there very few quantitative data for this one... but for the moment we'll keep this.....

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Pepo_ele") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0)  # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Pepo_ele") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Pepo_ele") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Pepo_ele") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative but n very limited (6 & 1)", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 


# next
unique(diets_df$Code_sp) # do not forget any species 


##################### Physeter macrocephalus - Sperm whale ####################
diets_df %>% 
  filter(Code_sp == "Phys_mac") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

#  several sources here so we'll take means of these

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Phys_mac") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Phys_mac") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Phys_mac") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Phys_mac") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 

##################### Phocoena phocoena - Harbour porpoise ####################
diets_df %>% 
  filter(Code_sp == "Phoc_pho") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

#  several sources here so we'll take means of these

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Phoc_pho") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Phoc_pho") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Phoc_pho") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Phoc_pho") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 


##################### Stenella attenuata - Pantropical spotted dolphin ####################
diets_df %>% 
  filter(Code_sp == "Sten_att") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# only one source so we'll take the %W per prey group from this one


# first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Sten_att") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros


# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Sten_att") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Sten_att") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Sten_att") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 




############### Steno bredanensis - Rough-toothed dolphin ####################
diets_df %>% 
  filter(Code_sp == "Sten_bre") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species 
# well there is one quantitative study but with n=1 (Fernandez et al 2009)
# so we cooked something from this and additional qualitative data consisting mostly in 
# observations of feeding events
## this is really approximative cooking.... 

diets_pred <- rbind(diets_pred, 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Sten_bre", "Steno bredanensis", "shelf") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 35, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 5, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 10, 
                             `Muscular pelagic cephalopods` = 25, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 25, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 0, 
                             Sources = "Lodi & Hetzel 1999, Di Beneditto et al 2001,
                             Aguiar dos Santos & Haimovici 2001, Pitman & Stinchcomb 2002, 
                             Wedekin et al 2005, Fernandez et al 2009, West et al 2011,
                             Ortega-Ortiz et al 2014"
                      ) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
                    tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                            "Sten_bre", "Steno bredanensis", "oceanic") %>%
                      mutate(`Large demersal energy-lean fish` = 0, 
                             `Large demersal energy-rich fish` = 35, 
                             `Small schooling energy-lean fish` = 0, 
                             `Small schooling energy-rich fish` = 5, 
                             `Miscellanous benthodemersal fish` = 0,
                             `Miscellanous pelagic fish` = 10, 
                             `Muscular pelagic cephalopods` = 25, 
                             `Gelatinous pelagic cephalopods` = 0,
                             `Bottom cephalopods` = 25, 
                             `Fish undetermined` = 0,
                             `Cephalopod undetermined` = 0, 
                             `Crustaceans` = 0, 
                             `Zooplankton` = 0, 
                             Sources = "Lodi & Hetzel 1999, Di Beneditto et al 2001,
                             Aguiar dos Santos & Haimovici 2001, Pitman & Stinchcomb 2002, 
                             Wedekin et al 2005, Fernandez et al 2009, West et al 2011,
                             Ortega-Ortiz et al 2014"
                      ) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                               "Sten_bre", "Steno bredanensis", "shelf") %>%
                         mutate(`Large demersal energy-lean fish` = 0, 
                                `Large demersal energy-rich fish` = 35, 
                                `Small schooling energy-lean fish` = 0, 
                                `Small schooling energy-rich fish` = 5, 
                                `Miscellanous benthodemersal fish` = 0,
                                `Miscellanous pelagic fish` = 10, 
                                `Muscular pelagic cephalopods` = 25, 
                                `Gelatinous pelagic cephalopods` = 0,
                                `Bottom cephalopods` = 25, 
                                `Fish undetermined` = 0,
                                `Cephalopod undetermined` = 0, 
                                `Crustaceans` = 0, 
                                `Zooplankton` = 0, 
                                Sources = "Lodi & Hetzel 1999, Di Beneditto et al 2001,
                             Aguiar dos Santos & Haimovici 2001, Pitman & Stinchcomb 2002, 
                             Wedekin et al 2005, Fernandez et al 2009, West et al 2011,
                             Ortega-Ortiz et al 2014") %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA))

# next
unique(diets_df$Code_sp) # do not forget any species 



##################### Stenella clymene - Clymene dolphin ####################
diets_df %>% 
  filter(Code_sp == "Sten_cly") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# No quantitative data for this species 
# no good qualitative one also 
# we'll take means of all Stenella species diets

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp %in% c("Sten_att", "Sten_coe", "Sten_fro")) %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros


# we'll take the mean of all these
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp %in% c("Sten_att", "Sten_coe", "Sten_fro")) %>%
                      mutate(Code_sp = "Sten_cly", 
                             Species = "Stenella clymene") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarize(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp %in% c("Sten_att", "Sten_coe", "Sten_fro")) %>%
                      mutate(Code_sp = "Sten_cly", 
                             Species = "Stenella clymene") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarize(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp %in% c("Sten_att", "Sten_coe", "Sten_fro")) %>%
                         mutate(Code_sp = "Sten_cly", 
                                Species = "Stenella clymene") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "yes, mean of known other Stenella sp", 
                                other_sp_code = str_c("Sten_att, ", "Sten_coe, ", "Sten_fro", 
                                                      collapse = ", "))) 


# next
unique(diets_df$Code_sp) # do not forget any species 



##################### Stenella coeruleoalba - Striped dolphin ####################
diets_df %>% 
  filter(Code_sp == "Sten_coe") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# here there is data on several blocks so we'll take the means of these

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Sten_coe") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

# HERE several sources both for shelf and oceanic areas, most have notable differences
# we'll take the mean of all these, distinguishing Ringelstein et al that is in oceanic area for sure
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Sten_coe", Block != "E1") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Sten_coe", Block == "E1") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Sten_coe", Block != "E1") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "neritic") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA),
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Sten_coe", Block == "E1") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "oceanic") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA) )

# next
unique(diets_df$Code_sp) # do not forget any species 




##################### Stenella frontalis - Atlantic spotted dolphin ####################
diets_df %>% 
  filter(Code_sp == "Sten_fro") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# here there is data on several blocks 

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Sten_fro") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

# HERE several sources both for shelf and oceanic areas, most have notable differences
# we'll take the mean of all these, distinguishing Ringelstein et al that is in oceanic area for sure
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Sten_fro") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Sten_fro") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Sten_fro") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 



############### Stenella longirostris - long-beaked common whale ####################
diets_df %>%
  filter(Code_sp == "Sten_lon") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source),
                   nb_indiv = unique(nb_indiv))

# There is no quantitative data for this species
# but qualitative data showed from isotope analysis that it might have similar diet than S. attenuata
# for now we'll use this

diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Sten_att") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf", 
                             Code_sp = "Sten_lon", 
                             Species = "Stenella longirostris") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Sten_att") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = paste(c("[Sten_long] Silva et al 2004, Salum Soud 2010, 
                                          Dolar et al 2003, Kiszka et al 2010, Clarke & Young 1998, 
                                          Gross et al 2009, [Sten_att]"), 
                                                       str_c(Source, collapse = ", "), 
                                                       sep = " ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic", 
                             Code_sp = "Sten_lon", 
                             Species = "Stenella longirostris") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources))

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Sten_att") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = paste(c("[Sten_long] Silva et al 2004, Salum Soud 2010, 
                                          Dolar et al 2003, Kiszka et al 2010, Clarke & Young 1998, 
                                          Gross et al 2009, [Sten_att]"), 
                                                          str_c(Source, collapse = ", "), 
                                                          sep = " ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all", 
                                Code_sp = "Sten_lon", 
                                Species = "Stenella longirostris") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative (Sten_long) & quantitative (Sten_att)", 
                                Copied_from_other_sp = "yes", 
                                other_sp_code = "Sten_att")) 


# next
unique(diets_df$Code_sp) # do not forget any species 



##################### Sousa plumbea - Indian ocean humpback dolphin ####################
diets_df %>% 
  filter(Code_sp == "Sous_plu") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# no quantitative data available for this species
# qualitative data suggest a very coastal behavior, a generalist diet and a small diet overlap with bottlenose dolphins 

# FOR THE MOMENT, WE'LL USE THE DIET OF Turs_truC


diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Turs_tru") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf", 
                             Code_sp = "Sous_plu", 
                             Species = "Sousa plumbea", 
                             Sources = "Diet of Tursiops truncatus, in accordance to qualitative data for Sten_plub") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Turs_tru") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic", 
                             Code_sp = "Sous_plu", 
                             Species = "Sousa plumbea", 
                             Sources = "Diet of Tursiops truncatus, in accordance to qualitative data for Sten_plub") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Turs_tru") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = paste(c("[Sous_plu] Perrin 2009, Wang et al 2003, 
                                          Wang et al 2012, Clarke & Young 1998, Gross et al 2009, [Turs_tru]"), 
                                                          str_c(Source, collapse = ", "), 
                                                          sep = " ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all", 
                                Code_sp = "Sous_plu", 
                                Species = "Sousa plumbea") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "qualitative (Sous_plu) & quantitative (Turs_tru)", 
                                Copied_from_other_sp = "yes", 
                                other_sp_code = "Turs_tru")) 

# next
unique(diets_df$Code_sp) # do not forget any species 




##################### Sotalia guianensis - Sotalia dolphin ####################
diets_df %>% 
  filter(Code_sp == "Sota_gui") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# here there is data on several blocks 

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Sota_gui") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros


diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Sota_gui") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Sota_gui") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Sota_gui") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 


# next
unique(diets_df$Code_sp) # do not forget any species 



##################### Tursiops truncatus - Common bottlenose dolphin ####################
diets_df %>% 
  filter(Code_sp == "Turs_tru") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Turs_tru") %>%
  group_by(Geo_area, Block, Source, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

# HERE several sources both for shelf and oceanic areas, most have notable differences
# we'll take the mean of all these, distinguishing Ringelstein et al that is in oceanic area for sure
diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Turs_tru", Source != "Louis et al 2014") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Turs_tru", Source == "Louis et al 2014") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)

# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Turs_tru") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 




##################### Ziphius cavirostris - Cuvier's beaked whale ####################
diets_df %>% 
  filter(Code_sp == "Ziph_cav") %>%
  group_by(Geo_area, Block) %>%
  dplyr::summarise(Source = unique(Source), 
                   nb_indiv = unique(nb_indiv))

# so there is data for several areas 

# #### Give estimates from existing data 
# # first take a look at existing data 
diets_df %>%
  filter(Code_sp == "Ziph_cav") %>%
  group_by(Geo_area, Block, Prey_group) %>%
  dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
  pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
  replace(is.na(.), 0) # replace NA with zeros

diets_pred <- rbind(diets_pred, 
                    # shelf
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Ziph_cav") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "shelf") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
                    # oceanic
                    diets_df %>%
                      group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                      dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                      pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                      replace(is.na(.), 0) %>% # replace NA with zeros
                      filter(Code_sp == "Ziph_cav") %>%
                      group_by(Code_sp, Species) %>%
                      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                       `Fish undetermined` = mean(`Fish undetermined`),
                                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                       `Crustaceans` = mean(`Crustaceans`), 
                                       `Zooplankton` = mean(`Zooplankton`), 
                                       Sources = str_c(Source, collapse = ", ")) %>%
                      nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                      mutate(Eco_area = "oceanic") %>% 
                      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
)


# fill summary df of all diets used  
diets_summary <- rbind(diets_summary, 
                       diets_df %>%
                         group_by(Code_sp, Species, Block, Source, Prey_group) %>%
                         dplyr::summarise(W_prey_group = sum(W)) %>% # compute %W per prey group, for each species in each Block
                         pivot_wider(names_from = Prey_group, values_from = W_prey_group) %>% # format to get a value for each prey group
                         replace(is.na(.), 0) %>% # replace NA with zeros
                         filter(Code_sp == "Ziph_cav") %>%
                         group_by(Code_sp, Species) %>%
                         dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                                          `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                                          `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                                          `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                                          `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                                          `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                                          `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                                          `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                                          `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                                          `Fish undetermined` = mean(`Fish undetermined`),
                                          `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                                          `Crustaceans` = mean(`Crustaceans`), 
                                          `Zooplankton` = mean(`Zooplankton`), 
                                          Sources = str_c(Source, collapse = ", ")) %>%
                         nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) %>%
                         mutate(waters = "all") %>% 
                         dplyr::select(Code_sp, Species, waters, mean_diet, Sources) %>%
                         mutate(Type_sources = "quantitative", 
                                Copied_from_other_sp = "no", 
                                other_sp_code = NA)) 

# next
unique(diets_df$Code_sp) # do not forget any species 


######################################################################################################
######################################### 3 - SAVE RDATA  ############################################
######################################################################################################

# write_xlsx(diets_summary %>%
#              select(c("Code_sp", "Species", "waters", 
#                       "Sources", "Type_sources", "Copied_from_other_sp",
#                       "other_sp_code")), paste0(Datawd, "/diets_pred/Diets_summary.xlsx"),
#                         col_names = TRUE,
#                        format_headers = TRUE)

rm(list = setdiff(ls(), c("wd", "diets_pred", "diets_summary")))

setwd(paste0(wd, "/data/model/"))
save.image("Diet_pred_20211008.RData")




