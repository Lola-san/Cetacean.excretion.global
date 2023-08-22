################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# clean_target_objects.R
################################################################################

# some target objects were super heavy (> 1 Go), and were only of use in the 
# exploratory analysis of results and are of no use in the final study 
# so they were deleted using this function

# objects deleted are created as object in the target script, but should be in 
# the "Additional material" section, and commented #

targets::tar_delete(
  c(fig_hab_taxa_CNA_output,
  fig_hab_taxa_NEA_output,
  fig_hab_taxa_NWA_output,
  fig_hab_taxa_Med_output,
  fig_hab_taxa_GoAl_output,
  fig_hab_taxa_Guy_output,
  fig_hab_taxa_WIO_output,
  fig_hab_taxa_Ant_output,
  fig_tot_exc_all_areas_log10_output,
  fig_all_el_log10_vs_sst_output,
  fig_all_el_log10_vs_chloro_output,
  fig_hab_km2_NEA_output,
  fig_contrib_taxa_Ant_output,
  fig_contrib_taxa_WIO_output,
  fig_hab_abs_Med_output,
  fig_hab_abs_Ant_output,
  fig_contrib_taxa_WFu_output,
  fig_contrib_taxa_Guy_output,
  fig_contrib_taxa_NCal_output,
  fig_hab_km2_CNA_output,
  fig_hab_km2_NWA_output,
  fig_hab_km2_Med_output,
  fig_hab_km2_Ant_output,
  fig_contrib_taxa_FPol_output,
  fig_hab_abs_CNA_output,
  fig_hab_abs_NEA_output,
  fig_hab_abs_NWA_output,
  fig_hab_km2_GoAl_output,
  fig_hab_km2_WIO_output,
  fig_hab_km2_Guy_output,
  fig_hab_abs_GoAl_output,
  fig_hab_abs_WIO_output,
  fig_hab_abs_Guy_output,
  fig_tot_exc_all_areas_facetperel_output,
  fig_tot_exc_all_areas_facetperel_SD_output,
  fig_tot_exc_all_areas_facetperel_DD_output,
  fig_contrib_taxa_NWA_output,
  fig_contrib_taxa_Haw_output,
  fig_contrib_taxa_GoAl_output,
  fig_contrib_taxa_Med_output,
  fig_contrib_taxa_NEA_output,
  fig_contrib_taxa_CNA_output,
  fig_contrib_taxa_Calif_output,
  fig_contrib_taxa_GoMex_output,
  fig_tot_exc_all_areas_facetperel_BW_output,
  fig_all_el_vs_sst_norm_output,
  fig_all_el_vs_chloro_output,
  fig_all_el_vs_sst_output,
  fig_C_in_biomass_areas_tns_eco_gps_areas,
  fig_N_vs_sst_output,
  fig_C_in_biomass_areas_tns_eco_gps_tot,
  fig_C_in_biomass_areas_tns,
  fig_C_in_biomass_areas_tns_per_km2,
  fig_tot_exc_vs_surfaces_output, 
  fig_sensi_all_taxa_all_nut_output,
  fig_sensi_all_taxa_N_output,
  fig_sensi_DD_output,
  fig_sensi_SD_output,
  fig_sensi_BW_output,
  # files that were from previous analysis not even kept
  fig_all_el_log10_vs_chloro,
  fig_all_el_log10_vs_sst, 
  fig_N_vs_chloro_Guy,
  fig_N_vs_sst,
  fig_N_vs_chloro_noGuy,
  N_vs_chloro_Guy,
  fig_all_el_vs_sst_fold,
  fig_all_el_vs_chloro_fold,
  fig_all_el_vs_chloro_norm,
  fig_all_el_vs_sst_norm,
  fig_all_el_vs_chloro,
  fig_all_el_vs_sst,
  fig_all_el_vs_chloro_fold_output,
  fig_all_el_vs_sst_fold_output,
  fig_compo_poop_boxplot_output,
  fig_sensi_all_taxa_output)
)
