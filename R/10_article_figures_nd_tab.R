################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# June 2022
# 10_article_figures_nd_tab.R
#
# Script with functions to generate all output figures used to make figures of 
# final article 
# 
################################################################################


############ for fig 1 : fold-change ratio to compare with nutrient release in
############ French Polynesia
# also added in Supplementary table 1 first tab "areas"
#'
#'
#'
#'
#'
# function to create table with fold change ratio between areas
table_fold_change <- function(output_tib,
                              object_type, # either "output" or "file" 
                              name_file) {
  inter_table <- output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     sum = list(sum_tibb(excrete_nut))) |>
    tidyr::unnest(sum) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As")), 
                  Excretion = Excretion*1e3/Surf # from tons to kg/km2
    )  |>
    dplyr::group_by(Geo_area, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     median = median(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    dplyr::filter(Element != "As")
  
  
  minimum_df <- inter_table |> 
    dplyr::group_by(Element) |>
    dplyr::summarize(min_all_mean = min(mean), 
                     max_all_mean = max(mean))
  
  # compute the fold-change ratio
  table <- inter_table |>
    dplyr::left_join(minimum_df, by = "Element", keep = FALSE) |>
    dplyr::mutate(fold = round(mean/min_all_mean) )
  
  if (object_type == "file") {
    write.table(table, paste0("output/tables/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    table
  }
}



############ For Fig 2 : see 11_article_oceano_data_graph



############ For Fig 3 differences between oceanic and neritic nutrient release 
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
    dplyr::left_join(output_tib |>
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
                       dplyr::group_by(Geo_area, Element) |>
                       dplyr::summarize(min_area = min(Excretion), 
                                        `2.5_quant` = quantile(Excretion, probs = c(0.025)),
                                        mean_area = mean(Excretion), 
                                        `97.5_quant` = quantile(Excretion, probs = c(0.975)),
                                        max_area = max(Excretion))) |>
    # select only values between the lowest and highest quantiles as extreme values tend 
    # to shred the standardized release values 
    dplyr::filter(Excretion > `2.5_quant`, Excretion < `97.5_quant` ) |>
    dplyr::mutate(ex_norm = (Excretion - `2.5_quant`)/(`97.5_quant` - `2.5_quant`)) |> # normalize between zero and 1 per area
    dplyr::group_by(Geo_area, Eco_area, Element) |>
    dplyr::summarize(mean_hab = mean(ex_norm)) |> 
    dplyr::filter(Eco_area == "oceanic") |>
    dplyr::group_by(Geo_area, Element) |>
    tidyr::pivot_wider(names_from = Eco_area, 
                       values_from = mean_hab) |>
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
    dplyr::left_join(output_tib |>
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
                       dplyr::group_by(Geo_area, Element) |>
                       dplyr::summarize(min_area = min(Excretion), 
                                        `2.5_quant` = quantile(Excretion, probs = c(0.025)),
                                        mean_area = mean(Excretion), 
                                        `97.5_quant` = quantile(Excretion, probs = c(0.975)),
                                        max_area = max(Excretion))) |>
    # select only values between the lowest and highest quantiles as extreme values tend 
    # to shred the standardized release values 
    dplyr::filter(Excretion > `2.5_quant`, Excretion < `97.5_quant` ) |>
    dplyr::mutate(ex_norm = (Excretion - `2.5_quant`)/(`97.5_quant` - `2.5_quant`)) |> # normalize between zero and 1 per area
    dplyr::group_by(Geo_area, Eco_area, Element) |>
    dplyr::summarize(mean_hab = mean(ex_norm)) |> 
    dplyr::filter(Eco_area == "shelf") |>
    dplyr::group_by(Geo_area, Element) |>
    tidyr::pivot_wider(names_from = Eco_area, 
                       values_from = mean_hab) |>
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

############# relative composition of poop ##############


############ For Fig 4 
#'
#'
#'
#'
#'
# function to create boxplot with relative compo of poop
compo_poop_boxplot <- function(compo_output_tib, 
                               object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                               name_file # should be a character string
) {
  
  
  compo_output_tib |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Element, y = Exc_norm, fill = Eco_gp),  
                          position = ggplot2::position_dodge(.9),
                          outlier.shape = NA) +
    ggplot2::scale_fill_manual(values = c("#cf7474ff", "slategray3", "#365579ff")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::ylab("Individual nutrient release in mg/kg of \n food ingested (normalized per nutrient)") +
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
    compo_output_tib |>
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = Element, y = Exc_norm, fill = Eco_gp),  
                            position = ggplot2::position_dodge(.9),
                            outlier.shape = NA) +
      ggplot2::scale_fill_manual(values = c("#cf7474ff", "slategray3", "#365579ff")) +
      ggplot2::xlab("Nutrient") +
      ggplot2::ylab("Individual nutrient release in mg/kg of \n food ingested (normalized per nutrient)") +
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


############### For Fig 5 

#'
#'
#'
#'
#'
# function to create biplot for PCA analysis
compo_poop_PCA_biplot <- function(output_tib, 
                                  name_file # should be a character string
) {
  
  profile_excretion <- output_tib |>
    dplyr::ungroup() |>
    dplyr::select(c(Eco_gp, Species, Indi_data, excrete_nut_ind, Mass)) 
  
  
  # select only one line per species (as there is many lines for all the places each species occurs)
  profile_excretion <- profile_excretion[c(1, 9, 13, 15, 19, 
                                           31, 32, 33, 40, 
                                           42, 51, 58, 73, 
                                           76, 80, 92, 96, 
                                           100, 106, 107, 108, 
                                           115, 129, 136, 146, 
                                           149, 152, 168, 176, 
                                           177, 179, 186, 190, 
                                           191, 199, 201, 208,
                                           225),]
  
  
  profile_excretion <- profile_excretion |>
    dplyr::group_by(Eco_gp) |>
    dplyr::mutate(excrete_ind_perkg_food = seq_along(excrete_nut_ind) |>
                    purrr::map(~ purrr::pluck(excrete_nut_ind, .)/purrr::pluck(Indi_data, ., "Ration"))) |>
    dplyr::select(-c(Indi_data, excrete_nut_ind, Mass)) |>
    tidyr::unnest(excrete_ind_perkg_food) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn),
                        names_to = "Element",
                        values_to = "Excretion_ind") |>
    dplyr::mutate(Element = factor(Element,
                                   levels = c("N", "P", "Fe", "Cu", "Mn",
                                              "Se", "Zn", "Co", "As"))) |>
    dplyr::filter(Element != "As") |>
    dplyr::group_by(Species, Eco_gp, Element) |>
    dplyr::summarize(min = min(Excretion_ind), 
                     `2.5_quant` = quantile(Excretion_ind, probs = c(0.025)), 
                     mean = mean(Excretion_ind), 
                     `97.5_quant` = quantile(Excretion_ind, probs = c(0.975)), 
                     max = max(Excretion_ind)) |>
    tidyr::pivot_wider(names_from = Element,
                       values_from = c(min, `2.5_quant`, 
                                       mean, 
                                       `97.5_quant`, max)) |>
    dplyr::select(c("2.5_quant_N", mean_N, "97.5_quant_N", 
                    "2.5_quant_P", mean_P, "97.5_quant_P", 
                    "2.5_quant_Fe", mean_Fe, "97.5_quant_Fe", 
                    "2.5_quant_Cu", mean_Cu, "97.5_quant_Cu", 
                    "2.5_quant_Mn", mean_Mn, "97.5_quant_Mn", 
                    "2.5_quant_Se", mean_Se, "97.5_quant_Se", 
                    "2.5_quant_Zn", mean_Zn, "97.5_quant_Zn", 
                    "2.5_quant_Co", mean_Co, "97.5_quant_Co"))
  
  profile_excretion <- as.data.frame(profile_excretion)
  rownames(profile_excretion) <- profile_excretion$Species
  profile_excretion <- profile_excretion[, -1]
  
  # extract active variables and observations
  data_act <- profile_excretion[, 2:25]
  
  # perfom PCA
  res_pca <- FactoMineR::PCA(data_act,
                             ncp = 5, graph = FALSE)
  
  factoextra::fviz_pca_biplot(res_pca, 
                              axes = c(1, 2),
                              # individuals
                              geom.ind = "point",
                              mean.point = FALSE,
                              col.ind = profile_excretion$Eco_gp,
                              addEllipses = TRUE,
                              palette = c("#cf7474ff", "slategray3", "#365579ff"),
                              #variables
                              geom.var = c("arrow", "text"),
                              col.var = "black",
                              select.var = list(cos2 = 0.5),
                              repel = 1,
                              ggtheme = ggplot2::theme_minimal(),
                              title = ggplot2::element_blank()
  ) +
    ggplot2::theme(legend.title = ggplot2::element_blank(), 
                   legend.text = ggplot2::element_text(size = 12, face = "bold"),
                   legend.position = "bottom",
                   axis.title = ggplot2::element_text(size = 12, face = "bold"), 
                   axis.text = ggplot2::element_text(size = 11), 
                   text = ggplot2::element_text(size = 11))
  ggplot2::ggsave(paste0("output/article/", 
                         name_file, 
                         ".jpg"), 
                  scale = 1, 
                  height = 5, width  = 6)
  
}



############## For Fig 6: relative contribution of taxa in each area ###########

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

#### For Fig 7 (sensitivity analysis)

#'
#'
#'
#'
#' function to plot results of sensitivity analysis for all three taxa
fig_sensitivy_indices_all_taxa_all_nut <- function(sensi_tib, 
                                                   object_type, 
                                                   name_file) {
  
  figure <- sensi_tib |> 
    dplyr::mutate(Input = dplyr::case_when(Input == "abundance" ~ "Abun-dance", 
                                           Input == "mass" ~ "Body mass",
                                           Input == "beta" ~ "Beta",
                                           Input == "ndays" ~ "Nb of days of presence",
                                           Input == "nrj_in_diet" ~ "Mean energy content of diet",
                                           Input == "nut_in_diet" ~ "Mean nutrient content of diet",
                                           Input == "assi_rate" ~ "Assimi-lation rate",
                                           Input == "nut_abs_rate" ~ "Nutrient release rate")) |>
    dplyr::mutate(Input = factor(Input, 
                                 levels = c("Body mass", "Beta",
                                            "Mean energy content of diet", "Mean nutrient content of diet",
                                            "Assimi-lation rate", "Nutrient release rate",  
                                            "Abun-dance", "Nb of days of presence"))) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("N", "P", "Fe", "Cu", 
                                               "Mn", "Se", "Zn", "Co"))) |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Input, y = original, fill = Sensitivity), color = "gray40", 
                          width = 0.5, 
                          #position = ggplot2::position_dodge(width=0.9)
    ) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF")) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 6)) +
    ggplot2::facet_wrap(~ Nutrient, nrow = 3) +
    ggplot2::ylab("Sobol sensivity indice") +
    ggplot2::xlab("Model parameter") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12),
                   axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(size = 12),
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   strip.text = ggplot2::element_text(face = "bold", size = 14),
                   legend.title = ggplot2::element_blank(), 
                   legend.spacing.y = ggplot2::unit(1.5, 'cm'), 
                   legend.text = ggplot2::element_text(size = 12),
                   legend.position = "bottom")
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/article/", name_file, ".jpg"),
                    width = 16,
                    height = 14)
  } else {
    figure
  }
}