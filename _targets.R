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
  
  ##############################################################################
  ############### compute ratio of cetacean abundances where needed ############
  ################# refers to functions of 00_ratio_abundances.R ###############
  
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
                                                  ratio_Calif)), 
  
  ##############################################################################
  ###################### generate population abundance data ####################
  ################# refers to functions of 01_abundance_data.R #################
  
  # generate original data sets from reports and articles, sp per sp
  tar_target(original_tib_Bala_acu, data_from_data_Bala_acu()), 
  tar_target(original_tib_Bala_bor, data_from_data_Bala_bor()), 
  tar_target(original_tib_Bala_ede, data_from_data_Bala_ede()), 
  tar_target(original_tib_Bala_mus, data_from_data_Bala_mus()), 
  tar_target(original_tib_Bala_phy, data_from_data_Bala_phy()),  
  tar_target(original_tib_Fere_att, data_from_data_Fere_att()), 
  tar_target(original_tib_Glob_mac, data_from_data_Glob_mac()), 
  tar_target(original_tib_Glob_mel, data_from_data_Glob_mel()),
  tar_target(original_tib_Gram_gri, data_from_data_Gram_gri()),
  tar_target(original_tib_Kogi_spp, data_from_data_Kogi_spp()),
  tar_target(original_tib_Lage_acu, data_from_data_Lage_acu()),
  tar_target(original_tib_Lage_alb, data_from_data_Lage_alb()),
  tar_target(original_tib_Lage_hos, data_from_data_Lage_hos()),
  tar_target(original_tib_Lage_obl, data_from_data_Lage_obl()),
  tar_target(original_tib_Liss_bor, data_from_data_Liss_bor()),
  tar_target(original_tib_Mega_nov, data_from_data_Mega_nov()),
  tar_target(original_tib_Orci_orc, data_from_data_Orci_orc()),
  tar_target(original_tib_Pepo_ele, data_from_data_Pepo_ele()),
  tar_target(original_tib_Phoc_dal, data_from_data_Phoc_dal()),
  tar_target(original_tib_Phoc_pho, data_from_data_Phoc_pho()),
  tar_target(original_tib_Phys_mac, data_from_data_Phys_mac()),
  tar_target(original_tib_Pseu_cra, data_from_data_Pseu_cra()),
  tar_target(original_tib_Sota_gui, data_from_data_Sota_gui()),
  tar_target(original_tib_Sten_att, data_from_data_Sten_att()),
  tar_target(original_tib_Sten_cly, data_from_data_Sten_cly()),
  tar_target(original_tib_Sten_fro, data_from_data_Sten_fro()),
  tar_target(original_tib_Sten_lon, data_from_data_Sten_lon()),
  tar_target(original_tib_Sten_bre, data_from_data_Sten_bre()),
  tar_target(original_tib_Turs_tru, data_from_data_Turs_tru()),
  # build tibbles with our format
  tar_target(abund_Bala_acu, build_sp_tib(original_tib_Bala_acu, 
                                          "Balaenoptera acutorostrata", 
                                          "Bala_acu")), 
  tar_target(abund_Bala_bor, build_sp_tib(original_tib_Bala_bor, 
                                          "Balaenoptera borealis", 
                                          "Bala_bor")), 
  tar_target(abund_Bala_ede, build_sp_tib(original_tib_Bala_ede, 
                                          "Balaenoptera edeni", 
                                          "Bala_ede")), 
  tar_target(abund_Bala_mus, build_sp_tib(original_tib_Bala_mus, 
                                          "Balaenoptera musculus", 
                                          "Bala_mus")), 
  tar_target(abund_Bala_phy, build_sp_tib(original_tib_Bala_phy, 
                                          "Balaenoptera physalus", 
                                          "Bala_phy")),
  tar_target(abund_Fere_att, build_sp_tib(original_tib_Fere_att, 
                                          "Feresa attenuata", 
                                          "Fere_att")),
  tar_target(abund_Glob_mac, build_sp_tib(original_tib_Glob_mac, 
                                          "Globicephala macrorhynchus", 
                                          "Glob_mac")),
  tar_target(abund_Glob_mel, build_sp_tib(original_tib_Glob_mel, 
                                          "Globicephala melas", 
                                          "Glob_mel")),
  tar_target(abund_Gram_gri, build_sp_tib(original_tib_Gram_gri, 
                                          "Grampus griseus", 
                                          "Gram_gri")),
  tar_target(abund_Kogi_spp, build_sp_tib(original_tib_Kogi_spp, 
                                          "Kogia spp", 
                                          "Kogi_spp")),
  tar_target(abund_Lage_acu, build_sp_tib(original_tib_Lage_acu, 
                                          "Lagenorhynchus acutus", 
                                          "Lage_acu")),
  tar_target(abund_Lage_alb, build_sp_tib(original_tib_Lage_alb, 
                                          "Lagenorhynchus albirostris", 
                                          "Lage_alb")),
  tar_target(abund_Lage_hos, build_sp_tib(original_tib_Lage_hos, 
                                          "Lagenodelphis hosei", 
                                          "Lage_hos")),
  tar_target(abund_Lage_obl, build_sp_tib(original_tib_Lage_obl, 
                                          "Lagenorhynchus obliquidens", 
                                          "Lage_obl")),
  tar_target(abund_Liss_bor, build_sp_tib(original_tib_Liss_bor, 
                                          "Lissodelphis borealis", 
                                          "Liss_bor")),
  tar_target(abund_Mega_nov, build_sp_tib(original_tib_Mega_nov, 
                                          "Megaptera novaeangliae", 
                                          "Mega_nov")),
  tar_target(abund_Orci_orc, build_sp_tib(original_tib_Orci_orc, 
                                          "Orcinus orca", 
                                          "Orci_orc")),
  tar_target(abund_Pepo_ele, build_sp_tib(original_tib_Pepo_ele, 
                                          "Peponocephala electra", 
                                          "Pepo_ele")),
  tar_target(abund_Phoc_dal, build_sp_tib(original_tib_Phoc_dal, 
                                          "Phocoenoides dalli", 
                                          "Phoc_dal")),
  tar_target(abund_Phoc_pho, build_sp_tib(original_tib_Phoc_pho, 
                                          "Phocoena phocoena", 
                                          "Phoc_pho")),
  tar_target(abund_Phys_mac, build_sp_tib(original_tib_Phys_mac, 
                                          "Physeter macrocephalus", 
                                          "Phys_mac")),
  tar_target(abund_Pseu_cra, build_sp_tib(original_tib_Pseu_cra, 
                                          "Pseudorca crassidens", 
                                          "Pseu_cra")),
  tar_target(abund_Sota_gui, build_sp_tib(original_tib_Sota_gui, 
                                          "Sotalia guianensis", 
                                          "Sota_gui")),
  tar_target(abund_Sten_att, build_sp_tib(original_tib_Sten_att, 
                                          "Stenella attenuata", 
                                          "Sten_att")),
  tar_target(abund_Sten_cly, build_sp_tib(original_tib_Sten_cly, 
                                          "Stenella clymene", 
                                          "Sten_cly")),
  tar_target(abund_Sten_fro, build_sp_tib(original_tib_Sten_fro, 
                                          "Stenella frontalis", 
                                          "Sten_fro")),
  tar_target(abund_Sten_lon, build_sp_tib(original_tib_Sten_lon, 
                                          "Stenella longirostris", 
                                          "Sten_lon")),
  tar_target(abund_Sten_bre, build_sp_tib(original_tib_Sten_bre, 
                                          "Steno bredanensis", 
                                          "Sten_bre")),
  tar_target(abund_Turs_tru, build_sp_tib(original_tib_Turs_tru, 
                                          "Tursiops truncatus", 
                                          "Turs_tru")),
  # cases with mixed-species groups 
  tar_target(abund_Dd_Dc_Sc, build_sp_tib_Dd_Dc_Sc(ratio_full_others)),
  tar_target(abund_BW_sp, build_sp_tib_BW(ratio_full_REMMOAs, ratio_full_others)),
  tar_target(abund_sp_REMMOA_smalldel, build_sp_tib_REMMOA_smalldel(ratio_full_REMMOAs)),
  tar_target(abund_sp_REMMOA_largedel, build_sp_tib_REMMOA_largedel(ratio_full_REMMOAs)),
  tar_target(abund_sp_REMMOA_smallglobi, build_sp_tib_REMMOA_smallglobi(ratio_full_REMMOAs)),
  tar_target(abund_sp_REMMOA_largeglobi, build_sp_tib_REMMOA_largeglobi(ratio_full_REMMOAs)), 
  tar_target(abund_sp_all, rbind(abund_Bala_acu, 
                                 abund_Bala_bor, 
                                 abund_Bala_ede, 
                                 abund_Bala_mus, 
                                 abund_Bala_phy, 
                                 abund_Fere_att, 
                                 abund_Glob_mac, 
                                 abund_Glob_mel, 
                                 abund_Gram_gri, 
                                 abund_Kogi_spp, 
                                 abund_Lage_acu, 
                                 abund_Lage_alb, 
                                 abund_Lage_hos, 
                                 abund_Lage_obl, 
                                 abund_Liss_bor, 
                                 abund_Mega_nov,
                                 abund_Orci_orc, 
                                 abund_Pepo_ele, 
                                 abund_Phoc_dal, 
                                 abund_Phoc_pho, 
                                 abund_Phys_mac, 
                                 abund_Pseu_cra, 
                                 abund_Sota_gui, 
                                 abund_Sten_att, 
                                 abund_Sten_cly, 
                                 abund_Sten_fro, 
                                 abund_Sten_lon,
                                 abund_Sten_bre,
                                 abund_Turs_tru, 
                                 abund_Dd_Dc_Sc, 
                                 abund_BW_sp, 
                                 abund_sp_REMMOA_smalldel,
                                 abund_sp_REMMOA_largedel, 
                                 abund_sp_REMMOA_smallglobi, 
                                 abund_sp_REMMOA_largeglobi) |> 
               dplyr::arrange(Code_sp, Species, Geo_area, Eco_area)), 
  
  ##############################################################################
  ###################### add species-specific energetic data ###################
  ################# refers to function of 02_add_energetic_data.R ##############
  tar_target(pop_input, add_nrjtic(abund_sp_all)), 
  
  ##############################################################################
  ############################ define diet per species #########################
  ################# refers to functions of 03_define_sp_diets.R ################
  # define data files (file with all quantitative data from litterature &
  # file with correspondance between prey groups and prey species)
  tar_target(data_diets_file,
             "data/diets_cetaceans/All_sp_pred_diet.xlsx",
             format = "file"), 
  tar_target(data_prey_gps_file,
             "data/diets_cetaceans/Sp_preys_PG_js_lg.xlsx",
             format = "file"), 
  # load data 
  tar_target(data_diets, load_xl(data_diets_file)),
  tar_target(data_prey_gps, load_xl(data_prey_gps_file)), 
  # join the two tables and clean to get a clean full table of diets 
  # from litterature with prey species and prey groups 
  tar_target(data_diets_PG, join_clean_diet_tib(data_diets, 
                                                data_prey_gps)), 
  # create our diet input
  tar_target(diet_input, create_diet_input(data_diets_PG)), 
  
  ##############################################################################
  ########### compute mean nutrient content of diet per species ################
  ################## refers to functions of 04_nut_in_diets.R ##################
  # define data file 
  tar_target(data_prey_compo_file,
             "data/prey_composition/Nuts_in_preys_full_corrected.xlsx",
             format = "file"),
  # load data
  tar_target(data_prey_compo, load_xl(data_prey_compo_file)), 
  # join with tibble with prey groups and clean the tibble
  tar_target(prey_compo, join_clean_compo_tib(data_prey_compo, 
                                              data_prey_gps)), 
  # bootstrap composition of prey groups
  tar_target(prey_compo_boot, bootstrap_compo_pg(prey_compo, nsim = 1e4)), #NSIM HERE!
  # can not go above 1e4 on my laptop
  # compute mean nutrient concentration of diets
  tar_target(diet_nut_input, compute_nut_in_diet(diet_input, prey_compo_boot)), 
  
  ##############################################################################
  ##################### prepare input tibble of the model ######################
  ############## refers to functions of 05_prepare_full_input.R ################
  tar_target(model_input, prepare_input(pop_input, diet_nut_input)), 
  
  ##############################################################################
  ############################### RUN MODEL ####################################
  ############## refers to functions of 06_run_model.R ################
  tar_target(model_output, run_model(model_input, nsim = 1e4)), #NSIM HERE!
  
  ##############################################################################
  #################### RUN sensitivity analysis ################################
  ############## refers to functions of 07_sensitivity_ana.R ###################
  tar_target(sobol_index_all, create_sobol_index_tib(model_input, 
                                                     model_output, 
                                                     nsim = 1e4)), #NSIM HERE!
  
  ##############################################################################
  #################### generate outputs (fig + tables) #########################
  ############## refers to functions of 08_generate_outputs.R ##################
  # format names and order of areas
  tar_target(model_output_clean, format_names(model_output)),
  
  ######### for all areas - tables
  # table with estimates and statistics for total excretion in all areas, 
  # for all elements, in tons/yr
  tar_target(tot_est_stat_tns_yr, create_full_stat_tab_tons_yr(model_output_clean)), 
  # table with estimates and statistics for total excretion in all areas, 
  # for all elements, in kg/km2/yr
  tar_target(tot_est_stat_kg_km2_yr, create_full_stat_tab_kg_km2_yr(model_output_clean)),
  # table with results of statistical test of significance of difference between areas
  # only for N 
  tar_target(test_diff_tot_exc_N, create_tab_stat_diff_tot_exc(model_output_clean)), 
  # table with total surfaces of areas
  tar_target(surf_tot_areas, table_tot_surf(model_output_clean)), 
  
  ######### for all areas - figures
  tar_target(fig_tot_exc_all_areas_facetperel, fig_exc_all_areas_1_facet_element(model_output_clean)),
  tar_target(fig_tot_exc_all_areas_log10, fig_exc_all_areas_log10(model_output_clean)),
  tar_target(fig_tot_exc_vs_surfaces, fig_exc_vs_tot_surf(model_output_clean)),
  tar_target(fig_tot_exc_all_areas_facetperel_BW, fig_exc_all_areas_1_facet_element_taxa(model_output_clean, 
                                                                                         "Baleen whales")),
  tar_target(fig_tot_exc_all_areas_facetperel_DD, fig_exc_all_areas_1_facet_element_taxa(model_output_clean,
                                                                                         "Deep divers")),
  tar_target(fig_tot_exc_all_areas_facetperel_SD, fig_exc_all_areas_1_facet_element_taxa(model_output_clean, 
                                                                                         "Small delphinids")),
  
  ######### for all areas - link with productivity
  # create data with chlor and sst data for all areas
  tar_target(chloro_sst_tib, create_tib_sst_chloro()),
  
  ########## area per area - tables
  # table with estimates and statistics for habitats - just for 8 of the areas with 2 habitats
  # tons/yr
  tar_target(est_stat_hab_tns_yr_NEA, create_hab_stat_tab_tons_yr(model_output_clean, 
                                                                  "Northeast Atlantic")),
  tar_target(est_stat_hab_tns_yr_CNA, create_hab_stat_tab_tons_yr(model_output_clean, 
                                                                  "Central North Atlantic")),
  tar_target(est_stat_hab_tns_yr_NWA, create_hab_stat_tab_tons_yr(model_output_clean, 
                                                                  "Northwest Atlantic")),
  tar_target(est_stat_hab_tns_yr_Med, create_hab_stat_tab_tons_yr(model_output_clean, 
                                                                  "Mediterranean Sea")),
  tar_target(est_stat_hab_tns_yr_Anti, create_hab_stat_tab_tons_yr(model_output_clean, 
                                                                   "French Antilles")),
  tar_target(est_stat_hab_tns_yr_Guy, create_hab_stat_tab_tons_yr(model_output_clean, 
                                                                  "French Guyana")),
  tar_target(est_stat_hab_tns_yr_WIO, create_hab_stat_tab_tons_yr(model_output_clean, 
                                                                  "West Indian ocean")),
  tar_target(est_stat_hab_tns_yr_GoAla, create_hab_stat_tab_tons_yr(model_output_clean, 
                                                                    "Gulf of Alaska")),
  # table with estimates and statistics for habitats - just for 8 of the areas with 2 habitats
  # kg/km2/yr
  tar_target(est_stat_hab_kg_km2_yr_NEA, create_hab_stat_tab_kg_km2_yr(model_output_clean, 
                                                                       "Northeast Atlantic")),
  tar_target(est_stat_hab_kg_km2_yr_CNA, create_hab_stat_tab_kg_km2_yr(model_output_clean, 
                                                                       "Central North Atlantic")),
  tar_target(est_stat_hab_kg_km2_yr_NWA, create_hab_stat_tab_kg_km2_yr(model_output_clean, 
                                                                       "Northwest Atlantic")),
  tar_target(est_stat_hab_kg_km2_yr_Med, create_hab_stat_tab_kg_km2_yr(model_output_clean, 
                                                                       "Mediterranean Sea")),
  tar_target(est_stat_hab_kg_km2_yr_Anti, create_hab_stat_tab_kg_km2_yr(model_output_clean, 
                                                                        "French Antilles")),
  tar_target(est_stat_hab_kg_km2_yr_Guy, create_hab_stat_tab_kg_km2_yr(model_output_clean, 
                                                                       "French Guyana")),
  tar_target(est_stat_hab_kg_km2_yr_WIO, create_hab_stat_tab_kg_km2_yr(model_output_clean, 
                                                                       "West Indian ocean")),
  tar_target(est_stat_hab_kg_km2_yr_GoAla, create_hab_stat_tab_kg_km2_yr(model_output_clean, 
                                                                         "Gulf of Alaska")),
  # table with stat test of difference between habitats - just for 8 of the areas with 2 habitats
  tar_target(test_diff_hab_NEA, test_differences_hab(model_output_clean, 
                                                     "Northeast Atlantic")),
  tar_target(test_diff_hab_CNA, test_differences_hab(model_output_clean, 
                                                     "Central North Atlantic")),
  tar_target(test_diff_hab_NWA, test_differences_hab(model_output_clean, 
                                                     "Northwest Atlantic")),
  tar_target(test_diff_hab_Med, test_differences_hab(model_output_clean, 
                                                     "Mediterranean Sea")),
  tar_target(test_diff_hab_Anti, test_differences_hab(model_output_clean, 
                                                      "French Antilles")),
  tar_target(test_diff_hab_Guy, test_differences_hab(model_output_clean, 
                                                     "French Guyana")),
  tar_target(test_diff_hab_WIO, test_differences_hab(model_output_clean, 
                                                     "West Indian ocean")),
  tar_target(test_diff_hab_GoAla, test_differences_hab(model_output_clean, 
                                                       "Gulf of Alaska")),
  # table with relative contribution of taxa in total 
  tar_target(taxa_contrib_tot_NEA, taxa_contribution_total(model_output_clean, 
                                                           "Northeast Atlantic")), 
  tar_target(taxa_contrib_tot_CNA, taxa_contribution_total(model_output_clean,
                                                           "Central North Atlantic")),
  tar_target(taxa_contrib_tot_NWA, taxa_contribution_total(model_output_clean,
                                                           "Northwest Atlantic")),
  tar_target(taxa_contrib_tot_Med, taxa_contribution_total(model_output_clean,
                                                           "Mediterranean Sea")),
  tar_target(taxa_contrib_tot_GoMex, taxa_contribution_total(model_output_clean,
                                                             "Gulf of Mexico")),
  tar_target(taxa_contrib_tot_Anti, taxa_contribution_total(model_output_clean,
                                                            "French Antilles")),
  tar_target(taxa_contrib_tot_Guy, taxa_contribution_total(model_output_clean,
                                                           "French Guyana")),
  tar_target(taxa_contrib_tot_WIO, taxa_contribution_total(model_output_clean,
                                                           "West Indian ocean")),
  tar_target(taxa_contrib_tot_NCal, taxa_contribution_total(model_output_clean,
                                                            "New Caledonia")),
  tar_target(taxa_contrib_tot_WFu, taxa_contribution_total(model_output_clean,
                                                           "Wallis & Futuna")),
  tar_target(taxa_contrib_tot_FPol, taxa_contribution_total(model_output_clean,
                                                            "French Polynesia")),
  tar_target(taxa_contrib_tot_Haw, taxa_contribution_total(model_output_clean,
                                                           "Hawaii")),
  tar_target(taxa_contrib_tot_GoAla, taxa_contribution_total(model_output_clean,
                                                             "Gulf of Alaska")),
  tar_target(taxa_contrib_tot_Calif, taxa_contribution_total(model_output_clean,
                                                             "California current")),
  # table with stat test of difference between taxa's contribution in total
  tar_target(test_diff_taxa_tot_NEA, test_differences_taxa(model_output_clean,
                                                           "Northeast Atlantic")),
  tar_target(test_diff_taxa_tot_CNA, test_differences_taxa(model_output_clean,
                                                           "Central North Atlantic")),
  tar_target(test_diff_taxa_tot_NWA, test_differences_taxa(model_output_clean,
                                                           "Northwest Atlantic")),
  tar_target(test_diff_taxa_tot_Med, test_differences_taxa(model_output_clean,
                                                           "Mediterranean Sea")),
  tar_target(test_diff_taxa_tot_GoMex, test_differences_taxa(model_output_clean,
                                                             "Gulf of Mexico")),
  tar_target(test_diff_taxa_tot_Anti, test_differences_taxa(model_output_clean,
                                                            "French Antilles")),
  tar_target(test_diff_taxa_tot_Guy, test_differences_taxa(model_output_clean,
                                                           "French Guyana")),
  tar_target(test_diff_taxa_tot_WIO, test_differences_taxa(model_output_clean,
                                                           "West Indian ocean")),
  tar_target(test_diff_taxa_tot_NCal, test_differences_taxa(model_output_clean,
                                                            "New Caledonia")),
  tar_target(test_diff_taxa_tot_WFu, test_differences_taxa(model_output_clean,
                                                           "Wallis & Futuna")),
  tar_target(test_diff_taxa_tot_FPol, test_differences_taxa(model_output_clean,
                                                            "French Polynesia")),
  tar_target(test_diff_taxa_tot_Haw, test_differences_taxa(model_output_clean,
                                                           "Hawaii")),
  tar_target(test_diff_taxa_tot_GoAla, test_differences_taxa(model_output_clean,
                                                             "Gulf of Alaska")),
  tar_target(test_diff_taxa_tot_Calif, test_differences_taxa(model_output_clean,
                                                             "California current")),
  # table with relative contribution of taxa in habitats
  tar_target(taxa_contrib_hab_NEA, taxa_contribution_hab(model_output_clean,
                                                         "Northeast Atlantic")),
  tar_target(taxa_contrib_hab_CNA, taxa_contribution_hab(model_output_clean,
                                                         "Central North Atlantic")),
  tar_target(taxa_contrib_hab_NWA, taxa_contribution_hab(model_output_clean,
                                                         "Northwest Atlantic")),
  tar_target(taxa_contrib_hab_Med, taxa_contribution_hab(model_output_clean,
                                                         "Mediterranean Sea")),
  tar_target(taxa_contrib_hab_GoMex, taxa_contribution_hab(model_output_clean,
                                                           "Gulf of Mexico")),
  tar_target(taxa_contrib_hab_Anti, taxa_contribution_hab(model_output_clean,
                                                          "French Antilles")),
  tar_target(taxa_contrib_hab_Guy, taxa_contribution_hab(model_output_clean,
                                                         "French Guyana")),
  tar_target(taxa_contrib_hab_WIO, taxa_contribution_hab(model_output_clean,
                                                         "West Indian ocean")),
  tar_target(taxa_contrib_hab_NCal, taxa_contribution_hab(model_output_clean,
                                                          "New Caledonia")),
  tar_target(taxa_contrib_hab_WFu, taxa_contribution_hab(model_output_clean,
                                                         "Wallis & Futuna")),
  tar_target(taxa_contrib_hab_FPol, taxa_contribution_hab(model_output_clean,
                                                          "French Polynesia")),
  tar_target(taxa_contrib_hab_Haw, taxa_contribution_hab(model_output_clean,
                                                         "Hawaii")),
  tar_target(taxa_contrib_hab_GoAl, taxa_contribution_hab(model_output_clean,
                                                          "Gulf of Alaska")),
  tar_target(taxa_contrib_hab_Calif, taxa_contribution_hab(model_output_clean,
                                                           "California current")), 
  # table with stat test of difference between taxa's contribution in the habitats - just for 8 of the areas with 2 habitats
  tar_target(test_diff_taxa_hab_NEA, test_differences_taxa_hab(model_output_clean,
                                                               "Northeast Atlantic")),
  tar_target(test_diff_taxa_hab_CNA, test_differences_taxa_hab(model_output_clean,
                                                               "Central North Atlantic")),
  tar_target(test_diff_taxa_hab_NWA, test_differences_taxa_hab(model_output_clean,
                                                               "Northwest Atlantic")),
  tar_target(test_diff_taxa_hab_Med, test_differences_taxa_hab(model_output_clean,
                                                               "Mediterranean Sea")),
  tar_target(test_diff_taxa_hab_Anti, test_differences_taxa_hab(model_output_clean,
                                                                "French Antilles")),
  tar_target(test_diff_taxa_hab_Guy, test_differences_taxa_hab(model_output_clean,
                                                               "French Guyana")),
  tar_target(test_diff_taxa_hab_WIO, test_differences_taxa_hab(model_output_clean,
                                                               "West Indian ocean")),
  tar_target(test_diff_taxa_hab_GoAla, test_differences_taxa_hab(model_output_clean,
                                                                 "Gulf of Alaska")), 
  
  ########## area per area - figures
  
  # figure with relative contribution of taxa
  tar_target(fig_contrib_taxa_NEA, fig_exc_taxa_log10(model_output_clean, 
                                                      "Northeast Atlantic")),
  tar_target(fig_contrib_taxa_CNA, fig_exc_taxa_log10(model_output_clean,
                                                      "Central North Atlantic")),
  tar_target(fig_contrib_taxa_NWA, fig_exc_taxa_log10(model_output_clean,
                                                      "Northwest Atlantic")),
  tar_target(fig_contrib_taxa_Med, fig_exc_taxa_log10(model_output_clean,
                                                      "Mediterranean Sea")),
  tar_target(fig_contrib_taxa_GoMex, fig_exc_taxa_log10(model_output_clean,
                                                        "Gulf of Mexico")),
  tar_target(fig_contrib_taxa_Ant, fig_exc_taxa_log10(model_output_clean,
                                                      "French Antilles")),
  tar_target(fig_contrib_taxa_Guy, fig_exc_taxa_log10(model_output_clean,
                                                      "French Guyana")),
  tar_target(fig_contrib_taxa_WIO, fig_exc_taxa_log10(model_output_clean,
                                                      "West Indian ocean")),
  tar_target(fig_contrib_taxa_NCal, fig_exc_taxa_log10(model_output_clean,
                                                       "New Caledonia")),
  tar_target(fig_contrib_taxa_WFu, fig_exc_taxa_log10(model_output_clean,
                                                      "Wallis & Futuna")),
  tar_target(fig_contrib_taxa_FPol, fig_exc_taxa_log10(model_output_clean,
                                                       "French Polynesia")),
  tar_target(fig_contrib_taxa_Haw, fig_exc_taxa_log10(model_output_clean,
                                                      "Hawaii")),
  tar_target(fig_contrib_taxa_GoAl, fig_exc_taxa_log10(model_output_clean,
                                                       "Gulf of Alaska")),
  tar_target(fig_contrib_taxa_Calif, fig_exc_taxa_log10(model_output_clean,
                                                        "California current")),
  
  # figure with excretion in each habitat in absolute values 
  tar_target(fig_hab_abs_NEA, fig_exc_hab_log10(model_output_clean, 
                                                "Northeast Atlantic", 
                                                "weight")),
  tar_target(fig_hab_abs_CNA, fig_exc_hab_log10(model_output_clean,
                                                "Central North Atlantic",
                                                "weight")),
  tar_target(fig_hab_abs_NWA, fig_exc_hab_log10(model_output_clean,
                                                "Northwest Atlantic",
                                                "weight")),
  tar_target(fig_hab_abs_Med, fig_exc_hab_log10(model_output_clean,
                                                "Mediterranean Sea",
                                                "weight")),
  tar_target(fig_hab_abs_Ant, fig_exc_hab_log10(model_output_clean,
                                                "French Antilles",
                                                "weight")),
  tar_target(fig_hab_abs_Guy, fig_exc_hab_log10(model_output_clean,
                                                "French Guyana",
                                                "weight")),
  tar_target(fig_hab_abs_WIO, fig_exc_hab_log10(model_output_clean,
                                                "West Indian ocean",
                                                "weight")),
  tar_target(fig_hab_abs_GoAl, fig_exc_hab_log10(model_output_clean,
                                                 "Gulf of Alaska",
                                                 "weight")),
  
  # figure with excretion in each habitat per unit area
  tar_target(fig_hab_km2_NEA, fig_exc_hab_log10(model_output_clean, 
                                                "Northeast Atlantic", 
                                                "weight_per_km2")),
  tar_target(fig_hab_km2_CNA, fig_exc_hab_log10(model_output_clean,
                                                "Central North Atlantic",
                                                "weight_per_km2")),
  tar_target(fig_hab_km2_NWA, fig_exc_hab_log10(model_output_clean,
                                                "Northwest Atlantic",
                                                "weight_per_km2")),
  tar_target(fig_hab_km2_Med, fig_exc_hab_log10(model_output_clean,
                                                "Mediterranean Sea",
                                                "weight_per_km2")),
  tar_target(fig_hab_km2_Ant, fig_exc_hab_log10(model_output_clean,
                                                "French Antilles",
                                                "weight_per_km2")),
  tar_target(fig_hab_km2_Guy, fig_exc_hab_log10(model_output_clean,
                                                "French Guyana",
                                                "weight_per_km2")),
  tar_target(fig_hab_km2_WIO, fig_exc_hab_log10(model_output_clean,
                                                "West Indian ocean",
                                                "weight_per_km2")),
  tar_target(fig_hab_km2_GoAl, fig_exc_hab_log10(model_output_clean,
                                                 "Gulf of Alaska",
                                                 "weight_per_km2")), 
  
  # figure with excretion per taxa in each habitat (per unit area)
  tar_target(fig_hab_taxa_NEA, fig_exc_hab_taxa_log10(model_output_clean, 
                                                "Northeast Atlantic")),
  tar_target(fig_hab_taxa_CNA, fig_exc_hab_taxa_log10(model_output_clean,
                                                "Central North Atlantic")),
  tar_target(fig_hab_taxa_NWA, fig_exc_hab_taxa_log10(model_output_clean,
                                                "Northwest Atlantic")),
  tar_target(fig_hab_taxa_Med, fig_exc_hab_taxa_log10(model_output_clean,
                                                "Mediterranean Sea")),
  tar_target(fig_hab_taxa_Ant, fig_exc_hab_taxa_log10(model_output_clean,
                                                "French Antilles")),
  tar_target(fig_hab_taxa_Guy, fig_exc_hab_taxa_log10(model_output_clean,
                                                "French Guyana")),
  tar_target(fig_hab_taxa_WIO, fig_exc_hab_taxa_log10(model_output_clean,
                                                "West Indian ocean")),
  tar_target(fig_hab_taxa_GoAl, fig_exc_hab_taxa_log10(model_output_clean,
                                                 "Gulf of Alaska"))
  
)
