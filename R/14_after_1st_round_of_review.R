################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# May 2023
# 14_after_1st_round_of_review.R
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
    ggplot2::ggsave(paste0("output/1st_review/", 
                           name_file, 
                           ".jpg"), 
                    scale = 1, 
                    width = 7, 
                    height = 4, dpi = 300)
  } 
}



#'
#'
#'
#'
#'
# function to compair mean relative contribution to total nutrient release (across nutrient) 
# to relative abundance and relative biomass at the level of areas 
scatterplot_rel_abund_biomass <- function(output_tib, 
                                          name_file # should be a character string
) {
  
  rbind(
    # tibble with mean contribution across nutrients 
    output_tib |>
      dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
      dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                       sum = list(sum_tibb(excrete_nut)),
                       n_sp = dplyr::n_distinct(Species)) |>
      tidyr::unnest(sum) |>
      tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                          names_to = "Element", 
                          values_to = "Excretion") |> 
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co", "As"))
      )  |>
      dplyr::group_by(Geo_area, Eco_area, Eco_gp, Element) |>
      dplyr::summarize(mean = mean(Excretion), 
                       n_sp = unique(n_sp)) |>
      dplyr::left_join(output_tib |> 
                         dplyr::group_by(Geo_area, Eco_area) |>
                         dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                                          sum = list(sum_tibb(excrete_nut))) |>
                         tidyr::unnest(sum) |>
                         tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                                             names_to = "Element", 
                                             values_to = "Excretion") |> 
                         dplyr::mutate(Element = factor(Element, 
                                                        levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                                   "Se", "Zn", "Co", "As"))
                         )  |>
                         dplyr::group_by(Geo_area, Eco_area, Element) |>
                         dplyr::summarize(mean_total = mean(Excretion))) |>
      dplyr::mutate(ratio_contribution = mean/mean_total) |>
      dplyr::ungroup() |>
      dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
      dplyr::summarise(mean_ratio_nut = mean(ratio_contribution*100), 
                       n_sp = unique(n_sp)) |>
      tidyr::pivot_longer(cols = mean_ratio_nut,
                          names_to = "variable", 
                          values_to = "ratio") |>
      dplyr::mutate(variable = "Nutrient release"), 
    # tibble with total biomass and abundance per taxa
    output_tib |>
      # first total biomass per species 
      dplyr::mutate(tot_biomass = seq_along(Mass) |>
                      purrr::map(~ purrr::pluck(Mass, ., 1)*purrr::pluck(Abund, ., 1))) |>
      dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
      # then total biomass and total abundance per taxa 
      dplyr::summarise(sum_abund = list(sum_vec(Abund)), 
                       sum_biomass = list(sum_vec(tot_biomass)),
                       n_sp = dplyr::n_distinct(Species)) |>
      tidyr::unnest(c(sum_abund, sum_biomass)) |>
      dplyr::rename(sum_abund = value) |>
      dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
      dplyr::summarize(mean_abund = mean(sum_abund), 
                       # biomass
                       mean_biomass = mean(sum_biomass), 
                       n_sp = unique(n_sp)) |>
      # compute total abundance and total biomass across all taxa
      dplyr::left_join(output_tib |>
                         # compute total biomass per species 
                         dplyr::mutate(tot_biomass = seq_along(Mass) |>
                                         purrr::map(~ purrr::pluck(Mass, ., 1)*purrr::pluck(Abund, ., 1))) |>
                         dplyr::group_by(Geo_area, Eco_area) |>
                         dplyr::summarise(sum_abund = list(sum_vec(Abund)), 
                                          sum_biomass = list(sum_vec(tot_biomass))) |>
                         tidyr::unnest(c(sum_abund, sum_biomass)) |>
                         dplyr::rename(sum_abund = value) |>
                         dplyr::group_by(Geo_area, Eco_area) |>
                         dplyr::summarize(mean_total_abund = mean(sum_abund), 
                                          mean_total_biomass = mean(sum_biomass))) |>
      # compute ratio between total abund/biomass per taxa and total across taxa
      dplyr::mutate(ratio_abund = (mean_abund/mean_total_abund)*100, 
                    ratio_biomass = (mean_biomass/mean_total_biomass)*100) |>
      tidyr::pivot_longer(cols = c(ratio_abund, ratio_biomass), 
                          names_to = "variable",
                          values_to = "ratio") |>
      dplyr::mutate(variable = dplyr::case_when(variable == "ratio_abund" ~ "Abundance", 
                                                variable == "ratio_biomass" ~ "Biomass")) |>
      dplyr::select(Geo_area, Eco_area, Eco_gp, n_sp, variable, ratio)
  )  |>
    ### BIPLOT
    tidyr::pivot_wider(names_from = variable,
                       values_from = ratio) |>
    tidyr::pivot_longer(cols = c(Abundance, Biomass),
                        names_to = "variable", 
                        values_to = "ratio") |>
    ggplot2::ggplot(ggplot2::aes(x = ratio, 
                                 y = `Nutrient release`, 
                                 color = Eco_gp)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(method = "lm", 
                         size = 2) +
    ggplot2::facet_wrap(~ variable, ncol = 2) +
    ggplot2::scale_color_manual(values = c(`Small delphinids` = "#365579ff", 
                                           `Deep divers` = "slategray3", 
                                           `Baleen whales` = "#cf7474ff")) +
    ggplot2::xlab("% of total abundance/biomass") +
    ggplot2::ylab("mean % of contribution \nto total nutrient release") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(), 
                   legend.text = ggplot2::element_text(size = 12, face = "bold"),
                   legend.position = "bottom",
                   axis.title = ggplot2::element_text(size = 12, face = "bold"), 
                   axis.text = ggplot2::element_text(size = 11), 
                   text = ggplot2::element_text(size = 11))
  
  ggplot2::ggsave(paste0("output/1st_review/", 
                         name_file, 
                         ".jpg"), 
                  scale = 1, 
                  height = 4, width  = 6)
  
  
}



#'
#'
#'
#'
#'
# function to compair mean relative contribution to total nutrient release (across nutrient) 
# to relative abundance and relative biomass at the level of areas 
other_trials_rel_abund_biomass <- function(output_tib, 
                                          name_file # should be a character string
) {
  
  tib <- rbind(
    # tibble with mean contribution across nutrients 
    output_tib |>
      dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
      dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                       sum = list(sum_tibb(excrete_nut)),
                       n_sp = dplyr::n_distinct(Species)) |>
      tidyr::unnest(sum) |>
      tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                          names_to = "Element", 
                          values_to = "Excretion") |> 
      dplyr::mutate(Element = factor(Element, 
                                     levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                "Se", "Zn", "Co", "As"))
      )  |>
      dplyr::group_by(Geo_area, Eco_area, Eco_gp, Element) |>
      dplyr::summarize(mean = mean(Excretion), 
                       n_sp = unique(n_sp)) |>
      dplyr::left_join(output_tib |> 
                         dplyr::group_by(Geo_area, Eco_area) |>
                         dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                                          sum = list(sum_tibb(excrete_nut))) |>
                         tidyr::unnest(sum) |>
                         tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                                             names_to = "Element", 
                                             values_to = "Excretion") |> 
                         dplyr::mutate(Element = factor(Element, 
                                                        levels = c("N", "P", "Fe", "Cu", "Mn", 
                                                                   "Se", "Zn", "Co", "As"))
                         )  |>
                         dplyr::group_by(Geo_area, Eco_area, Element) |>
                         dplyr::summarize(mean_total = mean(Excretion))) |>
      dplyr::mutate(ratio_contribution = mean/mean_total) |>
      dplyr::ungroup() |>
      dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
      dplyr::summarise(mean_ratio_nut = mean(ratio_contribution*100), 
                       n_sp = unique(n_sp)) |>
      tidyr::pivot_longer(cols = mean_ratio_nut,
                          names_to = "variable", 
                          values_to = "ratio") |>
      dplyr::mutate(variable = "Nutrient release"), 
    # tibble with total biomass and abundance per taxa
    output_tib |>
      # first total biomass per species 
      dplyr::mutate(tot_biomass = seq_along(Mass) |>
                      purrr::map(~ purrr::pluck(Mass, ., 1)*purrr::pluck(Abund, ., 1))) |>
      dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
      # then total biomass and total abundance per taxa 
      dplyr::summarise(sum_abund = list(sum_vec(Abund)), 
                       sum_biomass = list(sum_vec(tot_biomass)),
                       n_sp = dplyr::n_distinct(Species)) |>
      tidyr::unnest(c(sum_abund, sum_biomass)) |>
      dplyr::rename(sum_abund = value) |>
      dplyr::group_by(Geo_area, Eco_area, Eco_gp) |>
      dplyr::summarize(mean_abund = mean(sum_abund), 
                       # biomass
                       mean_biomass = mean(sum_biomass), 
                       n_sp = unique(n_sp)) |>
      # compute total abundance and total biomass across all taxa
      dplyr::left_join(output_tib |>
                         # compute total biomass per species 
                         dplyr::mutate(tot_biomass = seq_along(Mass) |>
                                         purrr::map(~ purrr::pluck(Mass, ., 1)*purrr::pluck(Abund, ., 1))) |>
                         dplyr::group_by(Geo_area, Eco_area) |>
                         dplyr::summarise(sum_abund = list(sum_vec(Abund)), 
                                          sum_biomass = list(sum_vec(tot_biomass))) |>
                         tidyr::unnest(c(sum_abund, sum_biomass)) |>
                         dplyr::rename(sum_abund = value) |>
                         dplyr::group_by(Geo_area, Eco_area) |>
                         dplyr::summarize(mean_total_abund = mean(sum_abund), 
                                          mean_total_biomass = mean(sum_biomass))) |>
      # compute ratio between total abund/biomass per taxa and total across taxa
      dplyr::mutate(ratio_abund = (mean_abund/mean_total_abund)*100, 
                    ratio_biomass = (mean_biomass/mean_total_biomass)*100) |>
      tidyr::pivot_longer(cols = c(ratio_abund, ratio_biomass), 
                          names_to = "variable",
                          values_to = "ratio") |>
      dplyr::mutate(variable = dplyr::case_when(variable == "ratio_abund" ~ "Abundance", 
                                                variable == "ratio_biomass" ~ "Biomass")) |>
      dplyr::select(Geo_area, Eco_area, Eco_gp, n_sp, variable, ratio)
  )  
  
  
  # diversity : e.g. is diversity lower in tropical regions than in temperate ones? 
  tib |>
    ggplot2::ggplot(ggplot2::aes(x = Geo_area, y = n_sp, fill = Eco_gp)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c(`Small delphinids` = "#365579ff", 
                                           `Deep divers` = "slategray3", 
                                           `Baleen whales` = "#cf7474ff")) +
    ggplot2::coord_flip()
  
  
  # relation abundance/biomass
  tib |>
    tidyr::pivot_wider(names_from = variable,
                       values_from = ratio) |>
    ggplot2::ggplot(ggplot2::aes(x = Abundance, 
                                 y = Biomass, 
                                 color = Eco_gp)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(method = "lm", 
                         size = 2) +
    ggplot2::scale_color_manual(values = c(`Small delphinids` = "#365579ff", 
                                           `Deep divers` = "slategray3", 
                                           `Baleen whales` = "#cf7474ff")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(), 
                   legend.text = ggplot2::element_text(size = 12, face = "bold"),
                   legend.position = "bottom",
                   axis.title = ggplot2::element_text(size = 12, face = "bold"), 
                   axis.text = ggplot2::element_text(size = 11), 
                   text = ggplot2::element_text(size = 11))
  
  ggplot2::ggsave(paste0("output/1st_review/", 
                         name_file, 
                         ".jpg"), 
                  scale = 1, 
                  height = 4, width  = 6)
  
  
}



############# Individual release rate per taxa ##############


############  
#'
#'
#'
#'
#'
# function to create boxplot with individual release rate per taxa and nutrient
ind_release_rate_taxa_boxplot <- function(output_tib, 
                                          name_file # should be a character string
) {
  sp_excretion <- output_tib |>
    dplyr::ungroup() |>
    dplyr::select(c(Eco_gp, Species, Indi_data, excrete_nut_ind, Mass)) 
  
  
  # select only one line per species (as there is many lines for all the places each species occurs)
  sp_excretion <- sp_excretion[c(1, 9, 13, 15, 19, 
                                           31, 32, 33, 40, 
                                           42, 51, 58, 73, 
                                           76, 80, 92, 96, 
                                           100, 106, 107, 108, 
                                           115, 129, 136, 146, 
                                           149, 152, 168, 176, 
                                           177, 179, 186, 190, 
                                           191, 199, 201, 208,
                                           225),]

  sp_excretion |>
    tidyr::unnest(excrete_nut_ind) |>
    tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                        names_to = "Element", 
                        values_to = "Excretion") |> 
    dplyr::mutate(Element = factor(Element, 
                                   levels = c("N", "P", "Fe", "Cu", "Mn", 
                                              "Se", "Zn", "Co", "As"))) |>
    dplyr::filter(Element != "As") |>
    dplyr::group_by(Eco_gp, Element) |>
    dplyr::summarize(min = min(Excretion), 
                     `2.5_quant` = quantile(Excretion, probs = c(0.025)), 
                     mean = mean(Excretion), 
                     `97.5_quant` = quantile(Excretion, probs = c(0.975)), 
                     max = max(Excretion)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Eco_gp, y = mean, fill = Eco_gp),  
                      stat = "identity", alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = Eco_gp, y = mean),
                        color = "gray40",
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Eco_gp, ymin = `2.5_quant`,
                                        ymax = `97.5_quant`),
                           color = "gray40",
                           width = 0, size = 1) +
    ggplot2::scale_y_continuous(trans='log10') +
    ggplot2::scale_fill_manual(values = c("#cf7474ff", "slategray3", "#365579ff")) +
    ggplot2::facet_wrap(~ Element, scales = "free") +
    ggplot2::ylab("Individual daily release rate (mg/day)") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(angle = 0, hjust = 1, size = 12),
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(face = "bold", size = 12, 
                                                       margin = ggplot2::margin(t = 5)))
  ggplot2::ggsave(paste0("output/1st_review/", 
                           name_file, 
                           ".jpg"), 
                    scale = 1, 
                    height = 5, width  = 7)
}