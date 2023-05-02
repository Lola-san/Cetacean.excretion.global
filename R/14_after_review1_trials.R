################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# May 2023
# 14_after_review1_trials.R
#
# Script with functions to compute a few-reanalysis after the first round of 
# reviews
# 
################################################################################


#'
#'
#'
#'
#'
# function to compair mean relative contribution to total nutrient release (across nutrient) 
# to relative abundance and relative biomass at the level of areas 
compair_rel_abund_biomass_area <- function(output_tib, 
                                           ratio_contrib_tib,
                                           geo_area,
                                           object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                           name_file # should be a character string
) {
  
  # define legend depending on nb of taxa in each area
  if (geo_area %in% c("Northeast Atlantic", "Central North Atlantic", 
                      "Northwest Atlantic", "Gulf of Alaska", 
                      "California current", "Gulf of Mexico", 
                      "Mediterranean Sea", "Hawaii")) {
    legend_values <- c(`Small delphinids` = "#365579ff", 
                       `Deep divers` = "slategray3", 
                       `Baleen whales` = "#cf7474ff")
  } else if (geo_area %in% c("French Antilles", "French Guyana", 
                             "West Indian ocean", "French Polynesia", 
                             "New Caledonia", "Wallis & Futuna")) {
    legend_values <- c(`Small delphinids` = "#365579ff", 
                       `Deep divers` = "slategray3")
  }
  
  rbind(
    # tibble with mean contribution across nutrients 
    ratio_contrib_tib |>
      dplyr::ungroup() |>
      dplyr::group_by(Geo_area, Eco_gp) |>
      dplyr::summarise(mean_ratio_nut = mean(ratio_contribution*100)) |>
      tidyr::pivot_longer(cols = mean_ratio_nut,
                          names_to = "variable", 
                          values_to = "ratio") |>
      dplyr::mutate(variable = "Mean contribution to \nnutrient release"), 
    # tibble with total biomass and abundance per taxa
    output_tib |>
      # first total biomass per species 
      dplyr::mutate(tot_biomass = seq_along(Mass) |>
                      purrr::map(~ purrr::pluck(Mass, ., 1)*purrr::pluck(Abund, ., 1))) |>
      dplyr::group_by(Geo_area, Eco_gp) |>
      dplyr::filter(Geo_area == geo_area) |>
      # then total biomass and total abundance per taxa 
      dplyr::summarise(sum_abund = list(sum_vec(Abund)), 
                       sum_biomass = list(sum_vec(tot_biomass))) |>
      tidyr::unnest(c(sum_abund, sum_biomass)) |>
      dplyr::rename(sum_abund = value) |>
      dplyr::group_by(Geo_area, Eco_gp) |>
      dplyr::summarize(min_abund = min(sum_abund), 
                       `2.5_quant_abund` = quantile(sum_abund, probs = c(0.025)), 
                       mean_abund = mean(sum_abund), 
                       median_abund = median(sum_abund), 
                       `97.5_quant_abund` = quantile(sum_abund, probs = c(0.975)), 
                       max_abund = max(sum_abund), 
                       # biomass
                       min_biomass= min(sum_biomass), 
                       `2.5_quant_biomass` = quantile(sum_biomass, probs = c(0.025)), 
                       mean_biomass = mean(sum_biomass), 
                       median_biomass = median(sum_biomass), 
                       `97.5_quant_biomass` = quantile(sum_biomass, probs = c(0.975)), 
                       max_biomass = max(sum_biomass)) |>
      # compute total abundance and total biomass across all taxa
      dplyr::left_join(output_tib |>
                         # compute total biomass per species 
                         dplyr::mutate(tot_biomass = seq_along(Mass) |>
                                         purrr::map(~ purrr::pluck(Mass, ., 1)*purrr::pluck(Abund, ., 1))) |>
                         dplyr::group_by(Geo_area) |>
                         dplyr::filter(Geo_area == geo_area) |>
                         dplyr::summarise(sum_abund = list(sum_vec(Abund)), 
                                          sum_biomass = list(sum_vec(tot_biomass))) |>
                         tidyr::unnest(c(sum_abund, sum_biomass)) |>
                         dplyr::rename(sum_abund = value) |>
                         dplyr::group_by(Geo_area) |>
                         dplyr::summarize(mean_total_abund = mean(sum_abund), 
                                          mean_total_biomass = mean(sum_biomass))) |>
      # compute ratio between total abund/biomass per taxa and total across taxa
      dplyr::mutate(ratio_abund = (mean_abund/mean_total_abund)*100, 
                    ratio_biomass = (mean_biomass/mean_total_biomass)*100) |>
      tidyr::pivot_longer(cols = c(ratio_abund, ratio_biomass), 
                          names_to = "variable",
                          values_to = "ratio") |>
      dplyr::mutate(variable = dplyr::case_when(variable == "ratio_abund" ~ "Total abundance", 
                                                variable == "ratio_biomass" ~ "Total biomass"))
  ) |>
    # plot output
    ggplot2::ggplot(ggplot2::aes(x = variable, y = ratio, fill = Eco_gp)) +
    ggplot2::geom_col(position = "stack", 
                      width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_fill_manual(values = legend_values) +
    ggplot2::theme_minimal() +
    ggplot2::geom_text(ggplot2::aes(label = round(ratio, 0)), 
                       #vjust = 0.5,
                       position = "stack",
                       hjust = 1.5,
                       colour = "white") +
    ggplot2::theme(legend.position = "bottom", 
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 13),
                   axis.text.x = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 14)) 
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/review/", 
                           name_file, 
                           ".jpg"), 
                    scale = 1, 
                    width = 7, 
                    height = 4, dpi = 300)
  } 
}