################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# June 2022
# 10_figures_tab_article.R
#
# Script with functions to generate all output figures used to make figures of 
# final article 
# 
################################################################################


############# relative composition of poop ##############


#'
#'
#'
#'
#'
# function to create boxplot with relative compo of poop
all_fig_compo_poop_boxplot <- function(output_tib, 
                                       object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                       name_file # should be a character string
) {
  output_tib |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Element, y = Exc_norm, fill = Eco_gp),  
                          position = ggplot2::position_dodge(.9),
                          outlier.shape = NA) +
    ggplot2::scale_fill_manual(values = c("#cf7474ff", "slategray3", "#365579ff")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::ylab("Individual daily excretion in mg/day/kg of \n food ingested (normalized per nutrient)") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 1, size = 12),
                   axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(angle = 0, hjust = 1, size = 12),
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(face = "bold", size = 12, 
                                                       margin = ggplot2::margin(t = 5)))
  
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/article/", 
                           name_file, 
                           ".jpg"), 
                    scale = 1, 
                    height = 5, width  = 7)
  } else {
    output_tib |>
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = Element, y = Exc_norm, fill = Eco_gp),  
                            position = ggplot2::position_dodge(.9),
                            outlier.shape = NA) +
      ggplot2::scale_fill_manual(values = c("#cf7474ff", "slategray3", "#365579ff")) +
      ggplot2::xlab("Nutrient") +
      ggplot2::ylab("Individual daily excretion in mg/day/kg of \n food ingested (normalized per nutrient)") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 1, size = 12),
                     axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                     axis.text.y = ggplot2::element_text(angle = 0, hjust = 1, size = 12),
                     axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                     legend.position = "bottom",
                     legend.title = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(face = "bold", size = 12, 
                                                         margin = ggplot2::margin(t = 5)))
  }
}


############## relative contribution of taxa in each area ###############

#'
#'
#'
#'
#'
# stack barplot to show on the map, one per area
fig_taxa_contrib_stacked_barplot <- function(output_tib, 
                                             geo_area,
                                             object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                             name_file # should be a character string
) {
  output_tib |> 
    dplyr::filter(Element != "As") |>
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("Co", "Zn", "Se", "Mn",
                                              "Cu", "Fe", "P", "N")), 
                  ratio_contribution = ratio_contribution*100) |>
    ggplot2::ggplot(ggplot2::aes(x = Element, y = ratio_contribution, fill = Eco_gp)) +
    ggplot2::geom_col(position = "stack", 
                      width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                          `Deep divers` = "slategray3", 
                                          `Baleen whales` = "#cf7474ff")) +
    ggplot2::theme_minimal() +
    ggplot2::geom_text(ggplot2::aes(label = round(ratio_contribution, 0)), 
                       #vjust = 0.5,
                       position = "stack",
                       hjust = 1.5,
                       colour = "white") +
    ggplot2::theme(legend.position = "none", 
                   axis.text.x = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 14)) 
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/article/Inkscape/", 
                           name_file, 
                           ".svg"), 
                    scale = 1, 
                    width = 3.5, 
                    height = 3, dpi = 300)
  } else {
    output_tib |> 
      dplyr::filter(Element != "As") |>
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("Co", "Zn", "Se", "Mn",
                                                "Cu", "Fe", "P", "N")), 
                    ratio_contribution = ratio_contribution*100) |>
      ggplot2::ggplot(ggplot2::aes(x = Element, y = ratio_contribution, fill = Eco_gp)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                            `Deep divers` = "slategray3", 
                                            `Baleen whales` = "#cf7474ff")) +
      ggplot2::theme_minimal() +
      ggplot2::geom_text(ggplot2::aes(label = round(ratio_contribution, 0)), 
                         #vjust = 0.5,
                         position = "stack",
                         hjust = 1.5,
                         colour = "white") +
      ggplot2::theme(legend.position = "none", 
                     axis.text.x = ggplot2::element_text(face = "bold", size = 14),
                     axis.text.y = ggplot2::element_text(face = "bold", size = 14))
  }
}



############ differences between oceanic and neritic nutrient release ##########

#'
#'
#'
#'
#'
#

fig_neritic_vs_oceanic_diff <- function(output_tib, 
                                        geo_area,
                                        object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                        name_file) # should be a character string) 
{
  
  table_oceanic <- output_tib |>
    # keep only areas with both neritic and oceanic waters
    dplyr::filter(!(Geo_area %in% c("California current", "Gulf of Mexico",  
                                    "New Caledonia", "Hawaii",  
                                    "Wallis & Futuna",
                                    "French Polynesia"))) |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = Excretion*1e3/Surf, # from tons to kg/km2
                  Geo_area = factor(Geo_area, 
                                    levels = c("Gulf of Alaska", "Central North Atlantic", "Northeast Atlantic", 
                                               "Northwest Atlantic", "California current", 
                                               "Mediterranean Sea", "West Indian ocean", "Gulf of Mexico", "French Antilles", 
                                               "New Caledonia", "Hawaii",  
                                               "French Guyana", "Wallis & Futuna", "French Polynesia"))
    )  |>
    dplyr::group_by(Geo_area, Eco_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    dplyr::group_by(Element) |>
    dplyr::mutate(mean_norm = (mean - min(mean))/(max(mean) - min(mean))) |> # normalize between zero and 1 across all areas
    dplyr::filter(Eco_area == "oceanic") |>
    dplyr::group_by(Geo_area, Element) |>
    tidyr::pivot_wider(names_from = Eco_area, 
                       values_from = mean_norm) |>
    dplyr::select(Geo_area, Element, oceanic)
  
  table_shelf <- output_tib |>
    # keep only areas with both neritic and oceanic waters
    dplyr::filter(!(Geo_area %in% c("California current", "Gulf of Mexico",  
                                    "New Caledonia", "Hawaii",  
                                    "Wallis & Futuna",
                                    "French Polynesia"))) |>
    dplyr::group_by(Geo_area, Eco_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = Excretion*1e3/Surf, # from tons to kg/km2
                  Geo_area = factor(Geo_area, 
                                    levels = c("Gulf of Alaska", "Central North Atlantic", "Northeast Atlantic", 
                                               "Northwest Atlantic", "California current", 
                                               "Mediterranean Sea", "West Indian ocean", "Gulf of Mexico", "French Antilles", 
                                               "New Caledonia", "Hawaii",  
                                               "French Guyana", "Wallis & Futuna", "French Polynesia"))
    )  |>
    dplyr::group_by(Geo_area, Eco_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    dplyr::group_by(Element) |>
    dplyr::mutate(mean_norm = (mean - min(mean))/(max(mean) - min(mean))) |> # normalize between zero and 1 across all areas
    dplyr::filter(Eco_area == "shelf") |>
    dplyr::group_by(Geo_area, Element) |>
    tidyr::pivot_wider(names_from = Eco_area, 
                       values_from = mean_norm) |>
    dplyr::select(Geo_area, Element, shelf) 
  
  table_diff <- table_oceanic |>
    dplyr::left_join(table_shelf, 
                     by = c("Geo_area", "Element")) |>
    dplyr::mutate(diff = (oceanic - shelf)) |> 
    dplyr::filter(Element != "As") 
  
  
  
  table_diff |>
    dplyr::filter(Geo_area == geo_area) |>
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co"))) |>
    ggplot2::ggplot(ggplot2::aes(x = diff, y = Geo_area)) +
    ggplot2::geom_violin(fill = "#69b3a2", 
                         color = "#69b3a2", 
                         alpha = 0.5, 
                         size = 1) +
    ggplot2::geom_point(ggplot2::aes(color = Element), size = 8) + 
    ggplot2::scale_color_manual(values = c("#4c413fff", "#5a6f80ff", "#278b9aff", "#e75b64ff", 
                                           "#de7862ff", "#d8af39ff", "#e8c4a2ff", "#6fb382ff")) +
    ggplot2::scale_x_continuous(minor_breaks = seq(-1, 1, 0.1),
                                limits = c(-1, 
                                           1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "gray", 
                                                              size = 0.5), 
                   panel.grid.minor.x = ggplot2::element_line(color = "gray", 
                                                              size = 0.2, 
                                                              linetype = "dashed"), 
                   panel.grid.major.y = ggplot2::element_line(color = "gray", 
                                                              size = 0.5),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(), 
                   legend.position = "none") 
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/article/Inkscape/", 
                           name_file, 
                           ".svg"), 
                    scale =1, 
                    width = 16, 
                    height = 2, dpi = 300)
  } else {
    table_diff |>
      dplyr::filter(Geo_area == geo_area) |>
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co"))) |>
      ggplot2::ggplot(ggplot2::aes(x = diff, y = Geo_area)) +
      ggplot2::geom_violin(fill = "#69b3a2", 
                           color = "#69b3a2", 
                           alpha = 0.5, 
                           size = 1) +
      ggplot2::geom_point(ggplot2::aes(color = Element), size = 8) + 
      ggplot2::scale_color_manual(values = c("#4c413fff", "#5a6f80ff", "#278b9aff", "#e75b64ff", 
                                             "#de7862ff", "#d8af39ff", "#e8c4a2ff", "#6fb382ff")) +
      ggplot2::scale_x_continuous(minor_breaks = seq(-1, 1, 0.1),
                                  limits = c(-1, 
                                             1)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "gray", 
                                                                size = 0.5), 
                     panel.grid.minor.x = ggplot2::element_line(color = "gray", 
                                                                size = 0.2, 
                                                                linetype = "dashed"), 
                     panel.grid.major.y = ggplot2::element_line(color = "gray", 
                                                                size = 0.5),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(), 
                     axis.text.y = ggplot2::element_blank()) 
  }
  
}


################################### SUPPLEMENTARY TABLES ######################################################


### first supplementary tables: statistics of total release at all levels - i.e. areas, habitats, taxa in areas
# and taxa in habitats, with one sheet for each level
# in tons and kg/km2
# each sheet is generated as a seperated xlsx file, then they are compiled by hand.


# Supplementary table with statistics for nutrient release at area levels
# must bind two tables, the one in tons and the one in kg/km2
#'
#'
#'
#'
#'
#' function to generate supplementary material table with all parameters summary values
supp_table1a_area <- function(list_tib, # list containing two tibbles that are target objects generated by functions
                              # create_full_stat_tab_tons_yr and create_full_stat_tab_kg_km2_yr
                              # first one in tons and second in kg_km2_yr ### ORDER MATTERS!
                              name_file) {
  
  options(scipen = 999)
  
  table <- list_tib[[1]] |>
    dplyr::mutate(unit = "tons/yr") |>
    # next parameters
    dplyr::bind_rows(list_tib[[2]] |>
                       dplyr::mutate(unit = "kg/km2/yr")
    )
  
  openxlsx::write.xlsx(table,
                         file =paste0("output/article/", name_file, ".xlsx"))
  
}


# Supplementary table with statistics for nutrient release at habitat level
# must bind 16 tables (2 per area with both habitats, the one in tons and the one in kg/km2)
#'
#'
#'
#'
#'
#' function to generate supplementary material table with all parameters summary values
supp_table1b_hab <- function(list_tib_tons, # list containing eight tibbles that are target objects generated by function
                              # create_hab_stat_tab_tons_yr
                              list_tib_kg_km2, # list containing eight tibbles that are target objects generated by function
                              # create_hab_stat_tab_kg_km2_yr
                              name_file) {
  
  options(scipen = 999)
  
  table_tons <- rbind(list_tib_tons[[1]], 
                      list_tib_tons[[2]], 
                      list_tib_tons[[3]], 
                      list_tib_tons[[4]], 
                      list_tib_tons[[5]], 
                      list_tib_tons[[6]], 
                      list_tib_tons[[7]], 
                      list_tib_tons[[8]]) |>
    dplyr::mutate(unit = "tons/yr") 
  
  table_kg_km2 <- rbind(list_tib_kg_km2[[1]], 
                      list_tib_kg_km2[[2]], 
                      list_tib_kg_km2[[3]], 
                      list_tib_kg_km2[[4]], 
                      list_tib_kg_km2[[5]], 
                      list_tib_kg_km2[[6]], 
                      list_tib_kg_km2[[7]], 
                      list_tib_kg_km2[[8]]) |>
    dplyr::mutate(unit = "kg/km2/yr") 
  
  table <- rbind(table_tons, table_kg_km2)
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/article/", name_file, ".xlsx"))
  
}



# Supplementary table with statistics for nutrient release at taxa and area level
# must bind 14 tables - one per area (all is in tons here)
#'
#'
#'
#'
#'
#' function to generate supplementary material table with all parameters summary values
supp_table1c_taxa_area <- function(list_tib, # list containing 14 tibbles that are target objects generated by function
                                   # taxa_contribution_total
                                   name_file) {
  
  options(scipen = 999)
  
  table <- rbind(list_tib_tons[[1]], 
                 list_tib_tons[[2]], 
                 list_tib_tons[[3]], 
                 list_tib_tons[[4]], 
                 list_tib_tons[[5]], 
                 list_tib_tons[[6]], 
                 list_tib_tons[[7]], 
                 list_tib_tons[[8]], 
                 list_tib_tons[[9]], 
                 list_tib_tons[[10]], 
                 list_tib_tons[[11]], 
                 list_tib_tons[[12]], 
                 list_tib_tons[[13]], 
                 list_tib_tons[[14]]) |>
    dplyr::mutate(unit = "tons/yr") 
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/article/", name_file, ".xlsx"))
  
}


# Supplementary table with statistics for nutrient release at taxa and habitat level
# must bind 8 tables - one per area with both habitats(all is in tons here)
#'
#'
#'
#'
#'
#' function to generate supplementary material table with all parameters summary values
supp_table1d_taxa_hab <- function(list_tib, # list containing 8 tibbles that are target objects generated by function
                                   # taxa_contribution_hab
                                   name_file) {
  
  options(scipen = 999)
  
  table <- rbind(list_tib_tons[[1]], 
                 list_tib_tons[[2]], 
                 list_tib_tons[[3]], 
                 list_tib_tons[[4]], 
                 list_tib_tons[[5]], 
                 list_tib_tons[[6]], 
                 list_tib_tons[[7]], 
                 list_tib_tons[[8]]) |>
    dplyr::mutate(unit = "tons/yr") 
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/article/", name_file, ".xlsx"))
  
}



### second supplementary tables: statistical test of differences between areas - habitats in areas - taxa in areas - taxa in habitats release at all levels 
# each sheet is generated as a seperated xlsx file, then they are compiled by hand.


# Supplementary table with test of difference for nutrient release at area levels
# must bind eight tables, one for each area with both habitats
#'
#'
#'
#'
#'
#' 
supp_table2a_area <- function(list_tib, # list containing eight tibbles that are target objects generated by function
                              # test_differences_hab
                              name_file) {

  options(scipen = 999)
  
  table <- rbind(list_tib_tons[[1]], 
                 list_tib_tons[[2]], 
                 list_tib_tons[[3]], 
                 list_tib_tons[[4]], 
                 list_tib_tons[[5]], 
                 list_tib_tons[[6]], 
                 list_tib_tons[[7]], 
                 list_tib_tons[[8]])
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/article/", name_file, ".xlsx"))
  
}


# Supplementary table with test of difference for nutrient release at habitat level
# must bind 8 tables one for each area with both habitats
#'
#'
#'
#'
#'
#'
supp_table2b_hab <- function(list_tib, # list containing eight tibbles that are target objects generated by function
                             # test_differences_hab
                             name_file) {
  
  options(scipen = 999)
  
  table_tons <- rbind(list_tib_tons[[1]], 
                      list_tib_tons[[2]], 
                      list_tib_tons[[3]], 
                      list_tib_tons[[4]], 
                      list_tib_tons[[5]], 
                      list_tib_tons[[6]], 
                      list_tib_tons[[7]], 
                      list_tib_tons[[8]]) |>
    dplyr::mutate(unit = "tons/yr") 
  
  table_kg_km2 <- rbind(list_tib_kg_km2[[1]], 
                        list_tib_kg_km2[[2]], 
                        list_tib_kg_km2[[3]], 
                        list_tib_kg_km2[[4]], 
                        list_tib_kg_km2[[5]], 
                        list_tib_kg_km2[[6]], 
                        list_tib_kg_km2[[7]], 
                        list_tib_kg_km2[[8]]) |>
    dplyr::mutate(unit = "kg/km2/yr") 
  
  table <- rbind(table_tons, table_kg_km2)
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/article/", name_file, ".xlsx"))
  
}



# Supplementary table with test of difference for nutrient release at taxa and area level
# must bind 14 tables - one per area 
#'
#'
#'
#'
#'
#' 
supp_table2c_taxa_area <- function(list_tib, # list containing 14 tibbles that are target objects generated by function
                                   # test_differences_taxa
                                   name_file) {
  
  options(scipen = 999)
  
  table <- rbind(list_tib_tons[[1]], 
                 list_tib_tons[[2]], 
                 list_tib_tons[[3]], 
                 list_tib_tons[[4]], 
                 list_tib_tons[[5]], 
                 list_tib_tons[[6]], 
                 list_tib_tons[[7]], 
                 list_tib_tons[[8]], 
                 list_tib_tons[[9]], 
                 list_tib_tons[[10]], 
                 list_tib_tons[[11]], 
                 list_tib_tons[[12]], 
                 list_tib_tons[[13]], 
                 list_tib_tons[[14]]) |>
    dplyr::mutate(unit = "tons/yr") 
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/article/", name_file, ".xlsx"))
  
}


# Supplementary table with test of difference for nutrient release at taxa and habitat level
# must bind 8 tables - one per area with both habitats(all is in tons here)
#'
#'
#'
#'
#'
#' 
supp_table2d_taxa_hab <- function(list_tib, # list containing 8 tibbles that are target objects generated by function
                                  # test_differences_taxa_hab
                                  name_file) {
  
  options(scipen = 999)
  
  table <- rbind(list_tib_tons[[1]], 
                 list_tib_tons[[2]], 
                 list_tib_tons[[3]], 
                 list_tib_tons[[4]], 
                 list_tib_tons[[5]], 
                 list_tib_tons[[6]], 
                 list_tib_tons[[7]], 
                 list_tib_tons[[8]]) |>
    dplyr::mutate(unit = "tons/yr") 
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/article/", name_file, ".xlsx"))
  
}





# Supplementary table with statistics for nutrient release at area levels
# must bind two tables, the one in tons and the one in kg/km2
#'
#'
#'
#'
#'
#' function to generate supplementary material table with all parameters summary values
supp_table1a_area <- function(list_tib, # list containing two tibbles that are target objects generated by functions
                              # create_full_stat_tab_tons_yr and create_full_stat_tab_kg_km2_yr
                              # first one in tons and second in kg_km2_yr ### ORDER MATTERS!
                              name_file) {
  
  options(scipen = 999)
  
  table <- list_tib[[1]] |>
    dplyr::mutate(unit = "tons/yr") |>
    # next parameters
    dplyr::bind_rows(list_tib[[2]] |>
                       dplyr::mutate(unit = "kg/km2/yr")
    )
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/article/", name_file, ".xlsx"))
  
}



# Supplementary table with statistics for all model parameters, and all species
#'
#'
#'
#'
#'
#' function to generate supplementary material table with all parameters summary values
supp_table_param_all_param_sp <- function(output_tib,
                                          object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                          name_file) {
  
  options(scipen = 999)
  
  table <- output_tib |>
    dplyr::ungroup() |>
    dplyr::group_by(Species) |>
    tidyr::unnest(Mass) |>
    dplyr::summarize(min = min(value),
                     `2.5_quant` = quantile(value, probs = c(0.025)),
                     mean = mean(value),
                     median = median(value),
                     `97.5_quant` = quantile(value, probs = c(0.975)),
                     max = max(value)) |>
    dplyr::mutate(Parameter = "Body mass", 
                  Nutrient = NA) |>
    # next parameters
    dplyr::bind_rows(output_tib |>
                       dplyr::ungroup() |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(Beta) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Parameter = "Beta", 
                                     Nutrient = NA),
                     output_tib |>
                       dplyr::ungroup() |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(NRJ_diet) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Parameter = "Mean diet energy content (mg/kg)", 
                                     Nutrient = NA),
                     output_tib |>
                       dplyr::ungroup() |>
                       tidyr::unnest(Nut_diet) |>
                       tidyr::pivot_longer(cols = c(N:Zn), 
                                           names_to = "Nutrient", 
                                           values_to = "Mean diet nutrient content (mg/kg)") |>
                       dplyr::group_by(Species, Nutrient) |>
                       dplyr::summarize(min = min(`Mean diet nutrient content (mg/kg)`),
                                        `2.5_quant` = quantile(`Mean diet nutrient content (mg/kg)`, probs = c(0.025)),
                                        mean = mean(`Mean diet nutrient content (mg/kg)`),
                                        median = median(`Mean diet nutrient content (mg/kg)`),
                                        `97.5_quant` = quantile(`Mean diet nutrient content (mg/kg)`, probs = c(0.975)),
                                        max = max(`Mean diet nutrient content (mg/kg)`)) |>
                       dplyr::mutate(Parameter = "Mean diet nutrient content (mg/kg)"),
                     output_tib |>
                       dplyr::ungroup() |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(Indi_data) |>
                       tidyr::pivot_longer(cols = c(ADMR:`PercentBM`),
                                           names_to = "Parameter",
                                           values_to = "value") |>
                       dplyr::group_by(Species, `Parameter`) |>
                       dplyr::mutate(Parameter = dplyr::case_when(Parameter == "A_rate" ~ "Assimilation rate",
                                                                  Parameter == "PercentBM" ~ "% of body mass (daily ration)",
                                                                  Parameter == "Ration" ~ "Daily ration (kg)",
                                                                  Parameter == "ADMR" ~ "Average Daily Metabolic Rate (kJ)")) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Nutrient = NA) |>
                       dplyr::arrange(Parameter),
                     output_tib |>
                       dplyr::ungroup() |>
                       tidyr::unnest(conso_nut_ind) |>
                       tidyr::pivot_longer(cols = c(N:Zn), 
                                           names_to = "Nutrient", 
                                           values_to = "ind_nut_ing") |>
                       dplyr::group_by(Species, Nutrient) |>
                       dplyr::summarize(min = min(ind_nut_ing),
                                        `2.5_quant` = quantile(ind_nut_ing, probs = c(0.025)),
                                        mean = mean(ind_nut_ing),
                                        median = median(ind_nut_ing),
                                        `97.5_quant` = quantile(ind_nut_ing, probs = c(0.975)),
                                        max = max(ind_nut_ing)) |>
                       dplyr::mutate(Parameter = "Individual daily amount of nutrient ingested (mg)"),
                     output_tib |>
                       dplyr::ungroup() |>
                       tidyr::unnest(excrete_nut_ind) |>
                       tidyr::pivot_longer(cols = c(N:Zn), 
                                           names_to = "Nutrient", 
                                           values_to = "ind_nut_exc") |>
                       dplyr::group_by(Species, Nutrient) |>
                       dplyr::summarize(min = min(ind_nut_exc),
                                        `2.5_quant` = quantile(ind_nut_exc, probs = c(0.025)),
                                        mean = mean(ind_nut_exc),
                                        median = median(ind_nut_exc),
                                        `97.5_quant` = quantile(ind_nut_exc, probs = c(0.975)),
                                        max = max(ind_nut_exc)) |>
                       dplyr::mutate(Parameter = "Individual daily amount of nutrient released (mg)")
    )
  
  if (object_type == "file") {
    openxlsx::write.xlsx(table,
                         file =paste0("output/article/", name_file, ".xlsx"))
  } else {
    table
  }
  
}



# SUPER LONG FUNCTION 
# Supplementary table with details of all simplified diet and references used for that 
#'
#'
#'
#'
#'Supplementary table with details of all simplified diet and references used for that
#' 
supp_table_diets <- function(clean_diet_tib,
                             object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                             name_file) {
  
  options(scipen = 999)
  
  table <- clean_diet_tib |>
    dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
    dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
    tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
    tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                           `Zooplankton` = 0)) |> # replace NA with zeros
    dplyr::filter(Code_sp == "Bala_acu") |>
    dplyr::group_by(Code_sp, Species) |>
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
                     Sources = stringr::str_c(Source, collapse = ", ")) |>
    dplyr::mutate(waters = "all", 
                  Type_sources = "quantitative", 
                  Copied_from_other_sp = "no", 
                  other_sp_code = NA) |> 
    # next parameters
    dplyr::bind_rows(clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Bera_bai") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       tidyr::nest(mean_diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
                       dplyr::mutate(waters = "all") |> 
                       dplyr::select(Code_sp, Species, waters, mean_diet, Sources) |>
                       dplyr::mutate(Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                                     "Bala_bon", "Balaenoptera bonaerensis", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 0, 
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
                              Friedlaender et al 2014") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                                     "Bala_bor", "Balaenoptera borealis", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 0, 
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
                              Leonardi et al 2011, Horwood 2018") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                                     "Bala_ede", "Balaenoptera edeni", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 0, 
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
                                     Sources = "Siciliano et al 2004, Tershy et al 1992") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                                     "Bala_mus", "Balaenoptera musculus", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 0, 
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
                              Lesage et al 2018") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                                     "Bala_omu", "Balaenoptera omurai", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 0, 
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
                              Cerchio & Tadasu 2018, Laboute & Borsa 2018") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Bala_phy") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Delp_cap") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Delp_del", Block %in% c("Alboran", "A", "B", "Aegean")) |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "neritic",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Delp_del", Block %in% c("E1")) |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "oceanic",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Glob_mel") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                                        stringr::str_c(Source, collapse = ", "), 
                                                        sep = " ")) |>
                       dplyr::mutate(waters = "all", 
                                     Code_sp = "Fere_att", 
                                     Species = "Feresa attenuata",
                                     Type_sources = "qualitative (Fere_att) & quantitative (Glob_mel)", 
                                     Copied_from_other_sp = "yes", 
                                     other_sp_code = "Glob_mel"),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area,
                                     "Glob_mac", "Globicephala macrorhynchus", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 0,
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
                             Mintzer et al 2008, Fernandez et al 2009") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Glob_mel") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Gram_gri") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Hype_amp") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Ziph_cav") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                                        stringr::str_c(Source, collapse = ", "), 
                                                        sep = " ")) |>
                       dplyr::mutate(waters = "all", 
                                     Species = "Indopacetus pacificus", 
                                     Code_sp = "Indo_pac",
                                     Type_sources = "qualitative (Indo_pac) & quantitative (Ziph_cav)", 
                                     Copied_from_other_sp = "yes", 
                                     other_sp_code = "Ziph_cav"), 
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Kogi_spp") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Lage_acu") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA), 
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Lage_alb") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area,
                                     "Liss_bor", "Lissodelphis borealis", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 5,
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
                             Lipsky & Brownell 2018") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area,
                                     "Lage_hos", "Lagenodelphis hosei", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 0,
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
                                Di Beneditto et al 2001, Dolar et al 2003, Wang et al 2012") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative & quantitative (n very limited)", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Lage_obl") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur")) |>
                       dplyr::mutate(Code_sp = dplyr::case_when(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur") ~ "Meso_spp", 
                                                                TRUE ~ Code_sp), 
                                     Species = dplyr::case_when(Species %in% c("Mesoplodon bidens", "Mesoplodon layardii", 
                                                                               "Mesoplodon densirostris", 
                                                                               "Mesoplodon mirus", "Mesoplodon europaeus") ~ "Mesoplodon spp", 
                                                                TRUE ~ Species)) |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                                     "Mega_nov", "Megaptera novaeangliae", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 0, 
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
                              Fleming et al 2016, Clapham 2018") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                                     "Orci_orc", "Orcinus orca", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 20, 
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
                              Volkova et al 2019") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Pseu_cra") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Phoc_dal") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Pepo_ele") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative but n very limited (6 & 1)", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Phys_mac") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Phoc_pho") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Sten_att") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                                     "Sten_bre", "Steno bredanensis", "shelf") |>
                       dplyr::mutate(`Large demersal energy-lean fish` = 0, 
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
                             Ortega-Ortiz et al 2014") |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "qualitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp %in% c("Sten_att", "Sten_coe", "Sten_fro")) |>
                       dplyr::mutate(Code_sp = "Sten_cly", 
                                     Species = "Stenella clymene") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "yes, mean of known other Stenella sp", 
                                     other_sp_code = stringr::str_c("Sten_att, ", "Sten_coe, ", "Sten_fro", 
                                                                    collapse = ", ")),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Sten_coe", Block != "E1") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "neritic",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Sten_coe", Block == "E1") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "oceanic",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Sten_fro") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Sten_att") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = paste(c("[Sten_long] Silva-Jr et al 2004, Salum Soud 2010, 
                                          Dolar et al 2003, Kiszka et al 2010, Clarke & Young 1998, 
                                          Gross et al 2009, [Sten_att]"), 
                                                        stringr::str_c(Source, collapse = ", "), 
                                                        sep = " ")) |>
                       dplyr::mutate(waters = "all", 
                                     Code_sp = "Sten_lon", 
                                     Species = "Stenella longirostris",
                                     Type_sources = "qualitative (Sten_long) & quantitative (Sten_att)", 
                                     Copied_from_other_sp = "yes", 
                                     other_sp_code = "Sten_att"),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Turs_tru") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                                        stringr::str_c(Source, collapse = ", "), 
                                                        sep = " ")) |>
                       dplyr::mutate(waters = "all", 
                                     Code_sp = "Sous_plu", 
                                     Species = "Sousa plumbea",
                                     Type_sources = "qualitative (Sous_plu) & quantitative (Turs_tru)", 
                                     Copied_from_other_sp = "yes", 
                                     other_sp_code = "Turs_tru"),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Sota_gui") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Turs_tru") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA),
                     clean_diet_tib |>
                       dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
                       dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
                       tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
                       tidyr::replace_na(list(`Large demersal energy-lean fish` = 0, 
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
                                              `Zooplankton` = 0)) |> # replace NA with zeros
                       dplyr::filter(Code_sp == "Ziph_cav") |>
                       dplyr::group_by(Code_sp, Species) |>
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
                                        Sources = stringr::str_c(Source, collapse = ", ")) |>
                       dplyr::mutate(waters = "all",
                                     Type_sources = "quantitative", 
                                     Copied_from_other_sp = "no", 
                                     other_sp_code = NA)
    )
  
  if (object_type == "file") {
    openxlsx::write.xlsx(table,
                         file =paste0("output/article/", name_file, ".xlsx"))
  } else {
    table
  }
  
}
