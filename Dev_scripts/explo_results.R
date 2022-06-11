##--------------------------------------------------------------------------------------------------------
## SCRIPT : Explore results 
##
## Authors : Lola Gilbert 
## Last update : 2022-03
## R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
##--------------------------------------------------------------------------------------------------------

# increase memory in R 
memory.limit(size=250000)

### clean up
rm(list = ls())

# to disable scientific notations
options(scipen = 999)


# load output table
targets::tar_load(model_output_clean)


# base function to sum tibbles 
# compute the addition of each vector of values with uncertainties to get values for several areas/sp
sum_tibb <- function(list_of_tibb) {
  summed_tibb <- matrix(0, 
                        nrow = nrow(list_of_tibb[[1]]), 
                        ncol = ncol(list_of_tibb[[1]]))
  colnames(summed_tibb) <- colnames(list_of_tibb[[1]])
  
  for (j in seq_along(list_of_tibb)) {
    summed_tibb <- summed_tibb + list_of_tibb[[j]]
  }
  return(tibble::as_tibble(summed_tibb))
}

# explore if cocktails of nutrients released in each zone are significantly different
# normalization per element 
model_output_clean |>
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
                Excretion = Excretion/Surf
  )  |>
  dplyr::filter(Element != "As") |>
  # normalize excretion data 
  dplyr::group_by(Element) |>
  dplyr::mutate(Excretion = (Excretion - min(Excretion))/(max(Excretion) - min(Excretion))) |>
  dplyr::group_by(Geo_area, Element) |>
  dplyr::summarize(min_exc = min(Excretion), 
                   `2.5_quant_exc` = quantile(Excretion, probs = c(0.025)), 
                   `10_quant_exc` = quantile(Excretion, probs = c(0.1)), 
                   mean_exc = mean(Excretion), 
                   median_exc = median(Excretion), 
                   `90_quant_exc` = quantile(Excretion, probs = c(0.90)), 
                   `97.5_quant_exc` = quantile(Excretion, probs = c(0.975)), 
                   max_exc = max(Excretion)) |>
  ggplot2::ggplot() +
  ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant_exc`, ymax = `97.5_quant_exc`, color = Element), 
                         size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean_exc, color = Element)) +
  ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                8, # nb of areas
                                                                type = "continuous"), 
                              name = "Area") +
  ggplot2::facet_wrap(~ Geo_area) +
  ggplot2::scale_y_continuous(trans = "log10", 
                              breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
  #guides(color = FALSE) + 
  ggplot2::xlab("Nutrient") +
  ggplot2::ylab("Normalized Excretion (in kg/km2/yr)") +
  ggplot2::theme(axis.ticks.length.x = ggplot2::unit(0, "cm"))



# normalization per area
model_output_clean |>
  dplyr::group_by(Geo_area) |>
  dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                   sum = list(sum_tibb(excrete_nut))) |>
  tidyr::unnest(sum) |>
  dplyr::mutate(Excretion_tot_area = N+P+Fe+Cu+Mn+Se+Zn+Co) |>
  tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                      names_to = "Element", 
                      values_to = "Excretion") |> 
  dplyr::filter(Element != "As") |>
  dplyr::mutate(Element = factor(Element, 
                                 levels = c("N", "P", "Fe", "Cu", "Mn", 
                                            "Se", "Zn", "Co")), 
                Excretion_ratio = Excretion/Excretion_tot_area
  )  |>
  dplyr::group_by(Geo_area) |>
  dplyr::mutate(Exc_norm = (Excretion - min(Excretion))/(max(Excretion) - min(Excretion))) |>
  dplyr::group_by(Geo_area, Element) |> 
  dplyr::summarize(min_exc = min(Exc_norm), 
                   `2.5_quant_exc` = quantile(Exc_norm, probs = c(0.025)), 
                   `10_quant_exc` = quantile(Exc_norm, probs = c(0.1)), 
                   mean_exc = mean(Exc_norm), 
                   median_exc = median(Exc_norm), 
                   `90_quant_exc` = quantile(Exc_norm, probs = c(0.90)), 
                   `97.5_quant_exc` = quantile(Exc_norm, probs = c(0.975)), 
                   max_exc = max(Exc_norm)) |>
  ggplot2::ggplot() +
  ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant_exc`, ymax = `97.5_quant_exc`, color = Element), 
                         size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean_exc, color = Element)) +
  ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                8, # nb of areas
                                                                type = "continuous"), 
                              name = "Area") +
  ggplot2::facet_wrap(~ Geo_area) +
  ggplot2::scale_y_continuous(trans = "log10",
                              breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
  #guides(color = FALSE) + 
  ggplot2::xlab("Nutrient") +
  ggplot2::ylab("Normalized Excretion (in kg/km2/yr)") +
  ggplot2::theme(axis.ticks.length.x = ggplot2::unit(0, "cm"))


tableau <- model_output_clean |>
  dplyr::group_by(Geo_area) |>
  dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                   sum = list(sum_tibb(excrete_nut))) |>
  tidyr::unnest(sum) |>
  # compute the fold change with Co as a reference
  dplyr::mutate(Co_ref = Co) |> 
  tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                      names_to = "Element", 
                      values_to = "Excretion") |> 
  dplyr::filter(Element != "As") |>
  dplyr::mutate(Element = factor(Element, 
                                 levels = c("N", "P", "Fe", "Cu", "Mn", 
                                            "Se", "Zn", "Co")))  |>
  dplyr::group_by(Geo_area, Element) |>
  dplyr::summarize(min_exc = min(Excretion), 
                   `2.5_quant_exc` = quantile(Excretion, probs = c(0.025)), 
                   `10_quant_exc` = quantile(Excretion, probs = c(0.1)), 
                   mean_exc = mean(Excretion), 
                   median_exc = median(Excretion), 
                   `90_quant_exc` = quantile(Excretion, probs = c(0.90)), 
                   `97.5_quant_exc` = quantile(Excretion, probs = c(0.975)), 
                   max_exc = max(Excretion))


# stoichiometry
model_output_clean |>
  dplyr::group_by(Geo_area) |>
  dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                   sum = list(sum_tibb(excrete_nut))) |>
  tidyr::unnest(sum) |>
  # compute the fold change with Co as a reference
  dplyr::mutate(Co_ref = Co) |> 
  tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                      names_to = "Element", 
                      values_to = "Excretion") |> 
  dplyr::filter(Element != "As") |>
  dplyr::mutate(Element = factor(Element, 
                                 levels = c("N", "P", "Fe", "Cu", "Mn", 
                                            "Se", "Zn", "Co")))  |>
  dplyr::group_by(Geo_area) |>
  dplyr::mutate(Excretion_fold = Excretion/0.001) |>
  dplyr::group_by(Geo_area, Element) |>
  dplyr::summarize(min_exc = min(Excretion_fold), 
                   `2.5_quant_exc` = quantile(Excretion_fold, probs = c(0.025)), 
                   `10_quant_exc` = quantile(Excretion_fold, probs = c(0.1)), 
                   mean_exc = mean(Excretion_fold), 
                   median_exc = median(Excretion_fold), 
                   `90_quant_exc` = quantile(Excretion_fold, probs = c(0.90)), 
                   `97.5_quant_exc` = quantile(Excretion_fold, probs = c(0.975)), 
                   max_exc = max(Excretion_fold)) |>
  #dplyr::mutate(mean_norm = (mean_exc - min(mean_exc))/(max(mean_exc) - min(mean_exc))) |>
  ggplot2::ggplot() +
  # ggplot2::geom_errorbar(ggplot2::aes(x = Element, 
  #                                     ymin = `2.5_quant_exc`, 
  #                                     ymax = `97.5_quant_exc`, 
  #                                     color = Element), 
  #                        size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean_exc, color = Element)) +
  ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                8, # nb of areas
                                                                type = "continuous"), 
                              name = "Area") +
  ggplot2::facet_wrap(~ Geo_area) +
  ggplot2::scale_y_continuous(trans = "log10",
                                breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
  #guides(color = FALSE) + 
  ggplot2::xlab("Nutrient") +
  ggplot2::ylab("Fold-change ratio of excretion (in kg/km2/yr) \n with Co as reference") +
  ggplot2::theme(axis.ticks.length.x = ggplot2::unit(0, "cm"))



# normalization per area
model_output_clean |>
  dplyr::group_by(Geo_area) |>
  dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                   sum = list(sum_tibb(excrete_nut))) |>
  tidyr::unnest(sum) |>
  dplyr::mutate(Excretion_tot_area = N+P+Fe+Cu+Mn+Se+Zn+Co) |>
  tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                      names_to = "Element", 
                      values_to = "Excretion") |> 
  dplyr::filter(Element != "As") |>
  dplyr::mutate(Element = factor(Element, 
                                 levels = c("N", "P", "Fe", "Cu", "Mn", 
                                            "Se", "Zn", "Co")), 
                Excretion_ratio = Excretion/Excretion_tot_area
  )  |>
  dplyr::group_by(Geo_area) |> 
  dplyr::mutate(Excretion_norm = (Excretion - min(Excretion))/(max(Excretion) - min(Excretion))) |>
  dplyr::group_by(Geo_area, Element) |> 
  dplyr::summarize(min_exc = min(Excretion_norm), 
                   `2.5_quant_exc` = quantile(Excretion_norm, probs = c(0.025)), 
                   `10_quant_exc` = quantile(Excretion_norm, probs = c(0.1)), 
                   mean_exc = mean(Excretion_norm), 
                   median_exc = median(Excretion_norm), 
                   `90_quant_exc` = quantile(Excretion_norm, probs = c(0.90)), 
                   `97.5_quant_exc` = quantile(Excretion_norm, probs = c(0.975)), 
                   max_exc = max(Excretion_norm)) |>
  ggplot2::ggplot() +
  ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant_exc`, ymax = `97.5_quant_exc`, color = Element), 
                         size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean_exc, color = Element)) +
  ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                8, # nb of areas
                                                                type = "continuous"), 
                              name = "Area") +
  ggplot2::facet_wrap(~ Geo_area) +
  ggplot2::scale_y_continuous(trans = "log10",
                              breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
  #guides(color = FALSE) + 
  ggplot2::xlab("Nutrient") +
  ggplot2::ylab("Normalized Excretion (in kg/km2/yr)") +
  ggplot2::theme(axis.ticks.length.x = ggplot2::unit(0, "cm"))


## with the fold change ratio
inter_table <- model_output_clean |>
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
fold_table <- inter_table |>
  dplyr::left_join(minimum_df, by = "Element", keep = FALSE) |>
  dplyr::mutate(fold = round(mean/min_all_mean) )

# plot with flod change ratio normalized per area
inter_table |>
  dplyr::left_join(minimum_df, by = "Element", keep = FALSE) |>
  dplyr::mutate(fold = round(mean/min_all_mean)) |>
  dplyr::filter(Geo_area != "French Polynesia") |>
  dplyr::select(c(Geo_area, Element, fold)) |>
  dplyr::group_by(Geo_area) |>
  dplyr::mutate(fold_norm = (fold - min(fold))/(max(fold) - min(fold))) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = fold_norm, color = Element), size = 2) +
  ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                8, # nb of areas
                                                                type = "continuous"), 
                              name = "Area") +
  ggplot2::facet_wrap(~ Geo_area, ncol = 5) +
  # ggplot2::scale_y_continuous(trans = "log10", 
  #                             breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
  ggplot2::xlab("Nutrient") +
  ggplot2::ylab("Fold-change ratio of excretion \n(in kg/km2/yr, normalized per area)") +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.ticks.length.x = ggplot2::unit(0, "cm"), 
                 axis.title.x = ggplot2::element_text(size = 15, 
                                                      face = "bold"), 
                 axis.title.y = ggplot2::element_text(size = 15, 
                                                      face = "bold"), 
                 strip.text.x = ggplot2::element_text(size = 13, 
                                                      face = "bold"),  
                 axis.text.x = ggplot2::element_text(size = 12, 
                                                     angle = 20), 
                 axis.text.y = ggplot2::element_text(size = 12), 
                 legend.position = "right",
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 12, 
                                                      face = "bold"),)


################# composition of poop

profile_excretion <- model_output_clean |>
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


profile_excretion |>
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
                                     "Se", "Zn", "Co", "As")))  |>
  dplyr::group_by(Eco_gp, Element) |>
  dplyr::summarize(min = min(Excretion_ind),
                `2.5_quant` = quantile(Excretion_ind, probs = c(0.025)),
                mean = mean(Excretion_ind),
                median = median(Excretion_ind),
                `97.5_quant` = quantile(Excretion_ind, probs = c(0.975)),
                max = max(Excretion_ind)) |>
  #filter(Element != "N", Element != "P") |>
  ggplot2::ggplot() +
  ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_gp), size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp)) +
  ggplot2::scale_color_manual(values = wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")) +
  ggplot2::facet_wrap(~ Eco_gp, ncol = 3) +
  ggplot2::scale_y_continuous(trans = "log10" ) +
  ggplot2::guides(color = FALSE) +
  ggplot2::xlab("Element") +
  ggplot2::ylab("Individual daily excretion (in mg/day)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
        #legend.spacing.y = unit(2, "cm"),
        legend.text = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5)))


# tab with normalization per element
tab <- profile_excretion |>
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
                                     "Se", "Zn", "Co", "As")))  |>
  dplyr::group_by(Element) |>
  dplyr::mutate(Exc_norm = (Excretion_ind - min(Excretion_ind))/(max(Excretion_ind) - min(Excretion_ind))) |>
  dplyr::group_by(Eco_gp, Element) |>
  dplyr::summarize(min = min(Exc_norm),
                   `2.5_quant` = quantile(Exc_norm, probs = c(0.025)),
                   `10_quant` = quantile(Exc_norm, probs = c(0.1)),
                   mean = mean(Exc_norm),
                   median = median(Exc_norm),
                   `90_quant` = quantile(Exc_norm, probs = c(0.9)),
                   `97.5_quant` = quantile(Exc_norm, probs = c(0.975)),
                   max = max(Exc_norm), 
                   sd = sd(Exc_norm), 
                   se = sd(Exc_norm)/sqrt(10000))  

# normalization per element - one facet per group
tab |>
  dplyr::mutate(Eco_gp = factor(Eco_gp, 
                                levels = c("Small delphinids", "Deep divers", "Baleen whales"))) |> 
  dplyr::filter(Element != "As") |>
  ggplot2::ggplot() +
  #ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_gp), size = 1) +
  #ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `10_quant`, ymax = `90_quant`, color = Eco_gp), size = 2) +
  ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = mean - se, ymax =  mean + se, color = Eco_gp), size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp), size = 5) +
  ggplot2::scale_color_manual(values = c("#365579ff", "#540e0eff", "#cf7474ff")) +
  ggplot2::facet_wrap(~ Eco_gp, ncol = 3) +
  #scale_y_continuous(trans = "log10" ) +
  ggplot2::guides(color = FALSE) +
  #ggplot2::theme_classic() +
  ggplot2::xlab("Element") +
  ggplot2::ylab("Individual daily excretion \n (in mg/day/kg of food ingested, normalized per element)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 19, hjust = 1, size = 20, face = "bold"),
                 axis.title.x = ggplot2::element_text(size = 20, face = "bold"),
                 axis.text.y = ggplot2::element_text(size = 20, face = "bold"),
                 axis.title.y = ggplot2::element_text(size = 20, face = "bold"),
        #legend.spacing.y = unit(2, "cm"),
        legend.text = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5)))

# normalization per element - one facet for all

# with se instead of quantiles 
tab |>
  ggplot2::ggplot() +
  #ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_gp), size = 1) +
  ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = mean - se, ymax =  mean + se, color = Eco_gp), size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp), size = 3) +
  #ggplot2::scale_color_manual(values = wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")) +
  ggplot2::scale_color_manual(values = c("#365579ff", "#540e0eff", "#cf7474ff")) +
  #ggplot2::facet_wrap(~ Eco_gp, ncol = 3) +
  #scale_y_continuous(trans = "log10" ) +
  #ggplot2::guides(color = FALSE) +
  ggplot2::xlab("Element") +
  ggplot2::ylab("Individual daily excretion in mg/day/kg of \n food ingested (normalized per element)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
                 #legend.spacing.y = unit(2, "cm"),
                 legend.position = "bottom",
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5)))


# with quantiles
tab |>
  ggplot2::ggplot() +
  ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_gp), size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp), size = 3) +
  #ggplot2::scale_color_manual(values = wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")) +
  ggplot2::scale_color_manual(values = c("#365579ff", "#540e0eff", "#cf7474ff")) +
  #ggplot2::facet_wrap(~ Eco_gp, ncol = 3) +
  #scale_y_continuous(trans = "log10" ) +
  #ggplot2::guides(color = FALSE) +
  ggplot2::xlab("Element") +
  ggplot2::ylab("Individual daily excretion in mg/day/kg of \n food ingested (normalized per element)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
                 #legend.spacing.y = unit(2, "cm"),
                 legend.position = "bottom",
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5)))


# normalization per element - one facet for all but with sd instead of quartiles 
tab |>
  ggplot2::ggplot() +
  #ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `10_quant`, ymax = `90_quant`, color = Eco_gp), size = 1) +
  ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = mean-sd, ymax = mean+sd, color = Eco_gp), size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp), size =3) +
  #ggplot2::scale_color_manual(values = wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")) +
  ggplot2::scale_color_manual(values = c("#365579ff", "#540e0eff", "#cf7474ff")) +
  #facet_wrap(~ Eco_gp, ncol = 3) +
  #scale_y_continuous(trans = "log10" ) +
  #guides(color = FALSE) +
  ggplot2::xlab("Element") +
  ggplot2::ylab("Individual daily excretion in mg/day/kg of \n food ingested  (normalized per element)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
        #legend.spacing.y = unit(2, "cm"),
        legend.position = "bottom",
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(face = "bold", 
                                            margin = ggplot2::margin(t = 5)))


# with boxplots
profile_excretion |>
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
                                            "Se", "Zn", "Co", "As")))  |>
  dplyr::group_by(Element) |>
  dplyr::mutate(Exc_norm = (Excretion_ind - min(Excretion_ind))/(max(Excretion_ind) - min(Excretion_ind))) |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = Element, y = Exc_norm, fill = Eco_gp),  
                        outlier.shape = NA) +
  #ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_gp), size = 1) +
  #ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp), size = 3) +
  #ggplot2::scale_color_manual(values = wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")) +
  ggplot2::scale_fill_manual(values = c("#365579ff", "#540e0eff", "#cf7474ff")) +
  #ggplot2::facet_wrap(~ Eco_gp, ncol = 3) +
  #scale_y_continuous(trans = "log10" ) +
  #ggplot2::guides(color = FALSE) +
  ggplot2::xlab("Element") +
  ggplot2::ylab("Individual daily excretion in mg/day/kg of \n food ingested (normalized per element)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
                 #legend.spacing.y = unit(2, "cm"),
                 legend.position = "bottom",
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5)))




# normalization per element - one facet for all but with sd instead of quartiles 
tab |>
  ggplot2::ggplot() +
  #ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `10_quant`, ymax = `90_quant`, color = Eco_gp), size = 1) +
  ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = mean-sd, ymax = mean+sd, color = Eco_gp), size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp), size =3) +
  ggplot2::scale_color_manual(values = wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")) +
  #ggplot2::facet_wrap(~ Eco_gp, ncol = 3) +
  #scale_y_continuous(trans = "log10" ) +
  #guides(color = FALSE) +
  ggplot2::xlab("Element") +
  ggplot2::ylab("Individual relative daily excretion (normalized mg/day/kg of food ingested)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
                 #legend.spacing.y = unit(2, "cm"),
                 legend.text = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5)))


profile_excretion |>
  dplyr::group_by(Eco_gp) |>
  tidyr::unnest(excrete_nut_ind) |>
  tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn),
               names_to = "Element",
               values_to = "Excretion_ind") |>
  dplyr::mutate(Element = factor(Element,
                                 levels = c("N", "P", "Fe", "Cu", "Mn",
                                            "Se", "Zn", "Co", "As")))  |>
  dplyr::group_by(Eco_gp, Element) |>
  dplyr::summarize(min = min(Excretion_ind),
                   `2.5_quant` = quantile(Excretion_ind, probs = c(0.025)),
                   mean = mean(Excretion_ind),
                   median = median(Excretion_ind),
                   `97.5_quant` = quantile(Excretion_ind, probs = c(0.975)),
                   max = max(Excretion_ind)) |>
  #filter(Element != "N", Element != "P") |>
  ggplot2::ggplot() +
  ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_gp), size = 1) +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp)) +
  ggplot2::scale_color_manual(values = wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")) +
  ggplot2::facet_wrap(~ Eco_gp, ncol = 3) +
  ggplot2::scale_y_continuous(trans = "log10" ) +
  ggplot2::guides(color = FALSE) + 
  ggplot2::xlab("Element") +
  ggplot2::ylab("Individual daily excretion (in mg/day/kg)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
        #legend.spacing.y = unit(2, "cm"), 
        legend.text = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5)))




#'
#'
#'
#'
#'
# statistics of estimates of the relative composition of poop of taxa
create_stat_tab_compo_poop <- function(output_tib) {
  
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
  
  profile_excretion |>
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
                                              "Se", "Zn", "Co", "As")))  |>
    dplyr::group_by(Eco_gp, Element) |>
    dplyr::summarize(min = min(Excretion_ind),
                     `2.5_quant` = quantile(Excretion_ind, probs = c(0.025)),
                     mean = mean(Excretion_ind),
                     median = median(Excretion_ind),
                     `97.5_quant` = quantile(Excretion_ind, probs = c(0.975)),
                     max = max(Excretion_ind))
  
}


#'
#'
#'
#'
#'
# statistics of estimates of the relative composition of poop of taxa 
# with normalisation per element 

create_stat_tab_compo_poop_norm <- function(output_tib) {
  
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
  
  profile_excretion |>
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
                                              "Se", "Zn", "Co", "As")))  |>
    dplyr::group_by(Element) |>
    dplyr::mutate(Exc_norm = (Excretion_ind - min(Excretion_ind))/(max(Excretion_ind) - min(Excretion_ind))) |>
    dplyr::group_by(Eco_gp, Element) |>
    dplyr::summarize(min = min(Exc_norm),
                     `2.5_quant` = quantile(Exc_norm, probs = c(0.025)),
                     `10_quant` = quantile(Exc_norm, probs = c(0.1)),
                     mean = mean(Exc_norm),
                     median = median(Exc_norm),
                     `90_quant` = quantile(Exc_norm, probs = c(0.9)),
                     `97.5_quant` = quantile(Exc_norm, probs = c(0.975)),
                     max = max(Exc_norm))
  
}




#'
#'
#'
#'
#'
# function to compute statistical test on the relative composition of poop of the three taxa
test_differences_compo_poop <- function(output_tib) {
  
  final_table <- tibble::tibble(Element = NA, 
                                Group1 = NA,
                                Group2 = NA, 
                                ratio_group1_superior_to_group2 = NA)
  
  
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
  
  
  for (i in c("N", "P", "Fe", "Cu", "Mn", 
              "Se", "Zn", "Co")) {
    
    el_table <- profile_excretion |>
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
                                                "Se", "Zn", "Co", "As")))  |> 
      dplyr::filter(Element == i) |>
      dplyr::ungroup() |>
      dplyr::select(Eco_gp, Excretion_ind) |>
      tidyr::pivot_wider(names_from = Eco_gp, 
                         values_from = Excretion_ind, 
                         values_fn = list) |>
      tidyr::unnest(cols = c(`Baleen whales`, `Deep divers`, `Small delphinids`)) |>
      dplyr::mutate(t_baleen_deep = dplyr::case_when(`Baleen whales` > `Deep divers` ~ 1,
                                                     TRUE ~ 0), 
                    t_baleen_delphi = dplyr::case_when(`Baleen whales` > `Small delphinids` ~ 1,
                                                       TRUE ~ 0),
                    t_deep_delphi = dplyr::case_when(`Deep divers` > `Small delphinids` ~ 1,
                                                     TRUE ~ 0),
      ) |>
      dplyr::summarise(t_baleen_deep = mean(t_baleen_deep), 
                       t_baleen_delphi = mean(t_baleen_delphi),
                       t_deep_delphi = mean(t_deep_delphi)) |>
      tidyr::pivot_longer(cols = c("t_baleen_deep":"t_deep_delphi"),
                          names_to = "Test", 
                          values_to = "ratio_group1_superior_to_group2") |>
      dplyr::mutate(Geo_area = geo_area, 
                    Element = i, 
                    Group1 = dplyr::case_when(stringr::str_starts(Test, "t_baleen") ~ "Baleen whales",
                                              stringr::str_starts(Test, "t_deep") ~ "Deep divers"), 
                    Group2 = dplyr::case_when(stringr::str_ends(Test, "_deep") ~ "Deep divers",
                                              stringr::str_ends(Test, "_delphi") ~ "Small delphinids")) |>
      dplyr::select(-Test) 
    
    final_table <- rbind(final_table, el_table)
    
    rm(el_table)
    
  }
  
  final_table <- final_table[-1,]
  
  return(final_table)
  
}



profile_excretion |>
  dplyr::group_by(Eco_gp) |>
  dplyr::mutate(excrete_ind_perkg_food = seq_along(excrete_nut_ind) |>
                  purrr::map(~ purrr::pluck(excrete_nut_ind, .)/purrr::pluck(Indi_data, ., "Ration"))) |>
  dplyr::select(-c(Indi_data, excrete_nut_ind, Mass)) |>
  tidyr::unnest(excrete_ind_perkg_food) |>
  tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn),
                      names_to = "Element",
                      values_to = "Excretion_ind") |> 
  dplyr::filter(Eco_gp == "Baleen whales") |> 
  tidyr::pivot_wider(names_from = Species, 
                     values_from = Excretion_ind, 
                     values_fn = list) |>
  tidyr::unnest(c("Balaenoptera acutorostrata", "Balaenoptera borealis",     
                  "Balaenoptera edeni", "Balaenoptera musculus",     
                  "Balaenoptera physalus", "Megaptera novaeangliae")) |>
  dplyr::mutate(mean_gp = mean(`Balaenoptera acutorostrata`, `Balaenoptera borealis`,     
                               `Balaenoptera edeni`, `Balaenoptera musculus`,     
                               `Balaenoptera physalus`, `Megaptera novaeangliae`)) |> 
  dplyr::select(Eco_gp, Element, mean_gp)



  ####################################################################################################################
################################ relative contribution of each taxa in nutrients excretion #########################

# we aim to see the deviation to the mean contribution per area depending on the nutrient 
# so per area, we want to center contribution


table <- model_output_clean |>
  dplyr::group_by(Geo_area, Eco_gp) |>
  dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                   sum = list(sum_tibb(excrete_nut))) |>
  tidyr::unnest(sum) |>
  tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                      names_to = "Element", 
                      values_to = "Excretion") |> 
  dplyr::mutate(Element = factor(Element, 
                                 levels = c("N", "P", "Fe", "Cu", "Mn", 
                                            "Se", "Zn", "Co", "As")), 
                Excretion = Excretion,
                Geo_area = factor(Geo_area, 
                                  levels = c("Northeast Atlantic", "Central North Atlantic", "Gulf of Alaska",
                                             "Northwest Atlantic", "California current", 
                                             "Mediterranean Sea", "West Indian ocean", "Gulf of Mexico", "French Antilles", 
                                             "New Caledonia", "Hawaii",  
                                             "French Guyana", "Wallis & Futuna", "French Polynesia"))
  )  |>
  dplyr::filter(Element != "As") |>
  dplyr::group_by(Geo_area, Eco_gp, Element) |>
  dplyr::summarize(mean = mean(Excretion)) |>
  dplyr::left_join(model_output_clean |> 
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
                                   Excretion = Excretion, 
                                   Geo_area = factor(Geo_area, 
                                                     levels = c("Gulf of Alaska", "Central North Atlantic", "Northeast Atlantic", 
                                                                "Northwest Atlantic", "California current", 
                                                                "Mediterranean Sea", "West Indian ocean", "Gulf of Mexico", "French Antilles", 
                                                                "New Caledonia", "Hawaii",  
                                                                "French Guyana", "Wallis & Futuna", "French Polynesia"))
                     )  |>
                     dplyr::filter(Element != "As") |>
                     dplyr::group_by(Geo_area, Element) |>
                     dplyr::summarize(mean_total = mean(Excretion))) |>
  dplyr::mutate(ratio_contribution = mean/mean_total) |>
  dplyr::ungroup() |>
  dplyr::group_by(Geo_area, Eco_gp) |>
  dplyr::mutate(mean_ratio = mean(ratio_contribution), 
                contrib_norm = round((ratio_contribution - min(ratio_contribution))/(max(ratio_contribution) - min(ratio_contribution)), 2))


table |>
  ggplot2::ggplot(ggplot2::aes(x = contrib_norm, y = Geo_area, color = Element)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ Eco_gp)
