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