################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# December 2021
# 02_add_energetic_data.R
#
# Script with function to add energetic data to the table with all population
# abundance data 
################################################################################


# energetic data is:
# body mass, from Trites & Pauly 1998, mainly (or guessed from close species,
# or from other source but it is mentionned when so)
## -> we used balanced mean of male and female body mass with 0.5 ratio
# beta, species-specific parameter accounting for activity costs and metabolic 
# efficiency
## from Spitz et al 2018/2012, and guessed based on close species when not in it

add_nrjtic <- function(pop_tib) {
  
  # tibble with data from litterature
  Ener_data <- tibble::tribble(~ Code_sp, ~ Eco_gp, ~ Mass, ~ Beta,
                       "Bala_acu", "Baleen whales", 6500, 4,
                       #"Bbona", "Baleen whales", 6500, 3, # not in Trites & pauly 1998 but very similar to Bala_acu for weight and eats mostly plankton for beta
                       "Bala_bor", "Baleen whales", 16800, 3, # beta guessed
                       "Bala_ede", "Baleen whales", 16100, 4, # beta guessed, eat small schooling nrj rich fish
                       "Bala_mus", "Baleen whales", 102700, 3, # beta guessed
                       #"Bomu", "Baleen whales", 11500, 3, # beta guessed and weight = mean of Bala_acu and Bore weights as not in Trites & pauly 1998
                       "Bala_phy", "Baleen whales", 55000, 3, 
                       "Bera_bai", "Deep divers", 3130, 2, # beta guessed
                       "Delp_del", "Small delphinids", 80, 4,
                       "Delp_cap", "Small delphinids", 80, 4, # weight is that of Delp_del as no distinction between short beaked and long beaked in Trites & Pauly 1998, beta guessed
                       #"Euba_gla", "Baleen whales", 23300, 3, # beta guessed
                       "Fere_att", "Deep divers", 95, 3, # beta guessed plus Eco group guessed as we applied diet of Glob_mel
                       "Glob_mac", "Deep divers", 640, 3, # beta guessed
                       "Glob_mel", "Deep divers", 850, 3,
                       "Gram_gri", "Deep divers", 220, 2, 
                       "Hype_amp", "Deep divers", 1600, 2, # beta guessed
                       "Indo_pac", "Deep divers", 1050, 2, # beta guessed
                       "Kogi_spp", "Deep divers", 140, 2, # beta guessed 
                       "Lage_alb", "Small delphinids", 140, 3, # beta guessed 
                       "Lage_acu", "Small delphinids", 90, 4, # beta guessed
                       "Lage_hos", "Small delphinids", 95, 3, # beta guessed pourquoi j'ai mis deux ? 
                       "Lage_obl", "Small delphinids", 80, 3, # beta guessed
                       "Liss_bor", "Small delphinids", 100, 3, # beta guessed
                       "Mega_nov", "Baleen whales", 30400, 4, # beta guessed
                       "Meso_spp", "Deep divers", 400, 3, # beta guessed
                       "Orci_orc", "Small delphinids", 2250, 4, # beta guessed
                       "Pseu_cra", "Deep divers", 570, 2, # beta guessed
                       "Phoc_dal", "Small delphinids", 60, 3, # beta guessed
                       "Pepo_ele", "Deep divers", 100, 2, # beta guessed
                       "Phys_mac", "Deep divers", 17000, 2,
                       "Phoc_pho", "Small delphinids", 40, 4, # check with Jerome why not in agreement with Trites & Pauly 1998
                       "Sten_att", "Small delphinids", 65, 3, # beta guessed and weight = that of "spotted dolphin" in Trites & pauly 1998
                       "Sten_bre", "Small delphinids", 90, 3, # beta guessed
                       "Sten_cly", "Small delphinids", 47, 3, # beta guessed
                       "Sten_fro", "Small delphinids", 65, 3, # beta guessed and weight = that of "spotted dolphin" in Trites & pauly 1998
                       "Sten_coe", "Small delphinids", 80, 3, # check with Jerome why not in agreement with Trites & Pauly 1998
                       "Sten_lon", "Small delphinids", 40, 3, # beta guessed
                       "Sous_plu", "Small delphinids", 115, 3, # beta guessed
                       "Sota_gui", "Small delphinids", 120, 3, # beta guessed + weight taken from Flores et al 2018 (encyclopedia of MM)
                       "Turs_tru", "Small delphinids", 185, 3, 
                       "Ziph_cav", "Deep divers", 800, 2) |>
    # add variation around these values
    dplyr::mutate(Mass_min = Mass - Mass*0.1, 
                  Mass_max = Mass + Mass*0.1, 
                  Beta_min = Beta - 0.5,
                  Beta_max = Beta + 0.5)
  
  # Bind with abundance data 
  pop_tib |>
    dplyr::left_join(Ener_data, by = c("Code_sp"))  
}
