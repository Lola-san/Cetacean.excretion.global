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
    ggplot2::ggsave(paste0("output/article/", 
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
    ggplot2::ggsave(paste0("output/article/", 
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


#### Supplementary table with statistics for all model parameters, and all species
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


