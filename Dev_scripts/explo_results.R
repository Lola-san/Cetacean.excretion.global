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

# explore if cocktails of nutrients released in each zone are significantely different
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



##
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
  dplyr::group_by(Geo_area, Element) |> 
  dplyr::summarize(min_exc = min(Excretion_ratio), 
                   `2.5_quant_exc` = quantile(Excretion_ratio, probs = c(0.025)), 
                   `10_quant_exc` = quantile(Excretion_ratio, probs = c(0.1)), 
                   mean_exc = mean(Excretion_ratio), 
                   median_exc = median(Excretion_ratio), 
                   `90_quant_exc` = quantile(Excretion_ratio, probs = c(0.90)), 
                   `97.5_quant_exc` = quantile(Excretion_ratio, probs = c(0.975)), 
                   max_exc = max(Excretion_ratio)) |>
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



##
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
inter_table |>
  dplyr::left_join(minimum_df, by = "Element", keep = FALSE) |>
  dplyr::mutate(fold = round(mean/min_all_mean) )

# plot 
inter_table |>
  dplyr::left_join(minimum_df, by = "Element", keep = FALSE) |>
  dplyr::mutate(fold = round(mean/min_all_mean)) |>
  dplyr::filter(Geo_area != "French Polynesia") |>
  dplyr::select(c(Geo_area, fold)) |>
  dplyr::group_by(Geo_area) |>
  dplyr::mutate(fold_norm = (fold - min(fold))/(max(fold) - min(fold))) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Element, y = fold_norm, color = Element)) +
  ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1", 
                                                                8, # nb of areas
                                                                type = "continuous"), 
                              name = "Area") +
  ggplot2::facet_wrap(~ Geo_area) +
  ggplot2::scale_y_continuous(trans = "log10", 
                              breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
  ggplot2::xlab("Nutrient") +
  ggplot2::ylab("Normalized Excretion (in kg/km2/yr)") +
  ggplot2::theme(axis.ticks.length.x = ggplot2::unit(0, "cm"))
