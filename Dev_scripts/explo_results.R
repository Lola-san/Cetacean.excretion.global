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

targets::tar_load(tib_full_poop_compo_norm)


# with boxplots
tib_full_poop_compo_norm |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = Element, y = Exc_norm, fill = Eco_gp),  
                        position = ggplot2::position_dodge(.9),
                        outlier.shape = NA) +
  #ggplot2::geom_errorbar(ggplot2::aes(x = Element, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Eco_gp), size = 1) +
  #ggplot2::geom_point(ggplot2::aes(x = Element, y = mean, color = Eco_gp), size = 3) +
  #ggplot2::scale_color_manual(values = wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")) +
  ggplot2::scale_fill_manual(values = c("#cf7474ff", "slategray3", "#365579ff")) +
  #ggplot2::facet_wrap(~ Eco_gp, ncol = 3) +
  #scale_y_continuous(trans = "log10" ) +
  #ggplot2::guides(color = FALSE) +
  ggplot2::xlab("Nutrient") +
  ggplot2::ylab("Individual daily excretion in mg/day/kg of \n food ingested (normalized per nutrient)") +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 1, size = 12),
                 axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                 axis.text.y = ggplot2::element_text(angle = 0, hjust = 1, size = 12),
                 axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                 #legend.spacing.y = unit(2, "cm"),
                 legend.position = "bottom",
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(face = "bold", size = 12, margin = ggplot2::margin(t = 5)))
ggplot2::ggsave("output/poop_compo_boxplots.jpg", scale = 1, 
                height = 5, width  = 7)




####################################################################################################################
################################ relative contribution of each taxa in nutrients excretion #########################

# we aim to see the deviation to the mean contribution per area depending on the nutrient 
# so per area, we want to center contribution
targets::tar_load(model_output_clean)

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





# stacked bar plot relative contribution of taxa in each area 


targets::tar_load(taxa_contrib_tot_WFu)


taxa_contrib_tot_Med |> 
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
ggplot2::ggsave("output/Med_contrib_taxa.svg", 
                scale =1, 
                width = 3.5, 
                height = 3, dpi = 300)







################# oceanic vs neritic spatial variation

############ new try again with standardization by element
# and then diff so each diff is between -1 and 1 

table_oceanic <- model_output_clean |>
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

table_shelf <- model_output_clean |>
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
dplyr::filter(Geo_area %in% c("French Guyana")) |>
  dplyr::mutate(Element = factor(Element, 
                                 levels = c("N", "P", "Fe", "Cu", "Mn", 
                                            "Se", "Zn", "Co")), 
                Geo_area = factor(Geo_area, 
                                  levels = c("French Guyana", "French Antilles", "West Indian ocean", 
                                             "Mediterranean Sea", "Northwest Atlantic",  "Gulf of Alaska",
                                             "Central North Atlantic", "Northeast Atlantic"))) |>
  ggplot2::ggplot(ggplot2::aes(x = diff, y = Geo_area)) +
  ggplot2::geom_violin(fill = "#69b3a2", 
                       color = "#69b3a2", 
                       alpha = 0.5, 
                       size = 1) +
  ggplot2::geom_point(ggplot2::aes(color = Element), size = 8) + 
  ggplot2::scale_color_manual(values = c("#4c413fff", "#5a6f80ff", "#278b9aff", "#e75b64ff", 
                                         "#de7862ff", "#d8af39ff", "#e8c4a2ff", "#6fb382ff")) +
  #ggplot2::coord_cartesian(xlim = c(0, 1)) +
  ggplot2::scale_x_continuous(minor_breaks = seq(-1, 1, 0.1),
                              limits = c(-1, 
                                         1)) +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "gray", 
                                                            size = 0.5), 
                 panel.grid.minor.x = ggplot2::element_line(color = "gray", 
                                                            size = 0.2, 
                                                            linetype = "dashed"), 
                 panel.grid.major.y = ggplot2::element_line(color = "gray", 
                                                            size = 0.5)) 
  
ggplot2::ggsave("output/NEA_neritic_vs_oceanic.jpg", 
       scale =1, 
       width = 16, 
       height = 2)

ggplot2::ggsave("output/Guy_neritic_vs_oceanic.svg", 
                scale =1, 
                width = 16, 
                height = 2, dpi = 300)





############################### sensitivity analysis 

targets::tar_load(sobol_index_all_sensi)

sobol_index_all_sensi

sobol_index_all_sensi |> 
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = Input, y = original, fill = Sensitivity))



############################## figures for ISEC ######################
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


targets::tar_load(model_output_clean)


options(scipen = 999)

# results of total nutrient release for two areas 
model_output_clean |>
  dplyr::group_by(Geo_area) |>
  dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                   sum = list(sum_tibb(excrete_nut))) |>
  tidyr::unnest(sum) |>
  tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                      names_to = "Element", 
                      values_to = "Excretion") |> 
  dplyr::mutate(Excretion = Excretion*1e3/Surf, # from tons to kg/km2
                Element = factor(Element, 
                                 levels = c("N", "P", "Fe", "Cu", "Mn", 
                                            "Se", "Zn", "Co", "As"))) |>
  dplyr::filter(Geo_area %in% c("Northeast Atlantic", 
                                "Northwest Atlantic"), 
                Element %in% c("N", "P", "Fe", "Cu", "Mn", "Se")) |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = Element, 
                                     y = Excretion, 
                                     fill = Geo_area)) +
  ggplot2::scale_y_log10() +
  ggplot2::ylab("Total amounts of nutrients released \nby cetacean communities \n(kg/km2/yr)") +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_manual(values = c("#278B9AFF", 
                                        "#E75B64FF")) +
  ggplot2::theme(legend.title = ggplot2::element_blank(), 
                 legend.position = "bottom", 
                 legend.text = ggplot2::element_text(size = 14, 
                                                     face = "bold"),
                 axis.title.x = ggplot2::element_blank(), 
                 axis.title.y = ggplot2::element_text(face = "bold", 
                                                      size = 15),
                 axis.text.x = ggplot2::element_text(size = 14), 
                 axis.text.y = ggplot2::element_text(size = 14))
ggplot2::ggsave("output/ISEC_1.jpg", 
                #scale =1, 
                width = 6, 
                height = 5)


# Sensitivity analysis
#do the analysis for all nutrients
# create function to compute y from mat 
compute_y_sensi <- function(param_mat) {
  # param_mat is the matrix of parameters of which the sensitivity is analyzed
  # for parameters on which we did bootstrap (ie.NRJ in diet and nutrient in diet), we can't analyze sensitivity as we did not infer about the distribution of these parameters)
  ADMR <- param_mat[, 3]*293.1*(param_mat[, 2]^0.737)
  Ration <- ADMR / (param_mat[, 7]*param_mat[, 5])
  conso_pop <- param_mat[, 1]*param_mat[, 4]*Ration
  conso_nut <- (conso_pop*param_mat[, 6])/1e9
  excrete_nut <- conso_nut*param_mat[, 8]
  
  return(excrete_nut)
}

create_sobol_index_tib_sensi <- function(results_tib, 
                                         nutrient, # character string
                                         nsim) {
  
  # tibble where results we be stored
  df_Si_Sti <- tibble::tibble(Code_sp = NA, 
                              Geo_area = NA, 
                              Eco_area = NA, 
                              Analysis = NA,
                              Input = NA,
                              Sensitivity = NA, # wether it's first order or total sobol indices
                              original = NA, #mean
                              bias = NA,
                              "std. error" = NA, 
                              "min. c.i." = NA,
                              "max. c.i." = NA)
  
  # for micronutrients, filter out migratory species 
  if (nutrient %in% c("Fe", "Cu", "Mn", "Se", "Zn", "Co")) {
    results_tib <- results_tib |>
      dplyr::filter(Geo_area != "Hawaii" & !(Code_sp %in% c("Bala_ede", "Bala_phy")))
  }
  
  for (rw in 1:nrow(results_tib)) {
    
    # sampling matrix 
    # change distributions of inputs according to data or bibliography/assumptions
    parammatX1 <- matrix(data = c(sample(purrr::pluck(results_tib, "Abund", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Mass", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Beta", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Ndays", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "NRJ_diet", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Nut_diet", rw, nutrient), size = nsim/5, replace = FALSE), 
                                  sample(rnorm(mean = 0.8, sd = 0.05, n = 1e5), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Nut_excrete", rw, nutrient), size = nsim/5, replace = FALSE)), 
                         ncol = 8, nrow = nsim/5) 
    
    parammatX2 <- matrix(data = c(sample(purrr::pluck(results_tib, "Abund", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Mass", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Beta", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Ndays", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "NRJ_diet", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Nut_diet", rw, nutrient), size = nsim/5, replace = FALSE), 
                                  sample(rnorm(mean = 0.8, sd = 0.05, n = 1e5), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Nut_excrete", rw, nutrient), size = nsim/5, replace = FALSE)), 
                         ncol = 8, nrow = nsim/5)
    
    #output <- sample(purrr::pluck(results_tib, "excrete_nut", rw, "N"), size = nsim/5, replace = FALSE)
    
    sens <- sensitivity::sobolSalt(model = compute_y_sensi, X1 = parammatX1, X2 = parammatX2, 
                                   scheme = "A", nboot = 1e3, conf = 0.95)
    
    
    df_Si_first <- tibble::tibble(Code_sp = purrr::pluck(results_tib, "Code_sp", rw), 
                                  Geo_area = purrr::pluck(results_tib, "Geo_area", rw), 
                                  Eco_area = purrr::pluck(results_tib, "Eco_area", rw), 
                                  Analysis = nutrient, 
                                  Input = rownames(sens$S),
                                  Sensitivity = "First order indices", 
                                  original = purrr::pluck(sens$S, "original"), 
                                  bias = purrr::pluck(sens$S, "bias"),
                                  "std. error" = purrr::pluck(sens$S, "std. error"),
                                  "min. c.i." = purrr::pluck(sens$S, "min. c.i."),
                                  "max. c.i." = purrr::pluck(sens$S, "max. c.i.")) |>
      dplyr::mutate(Input = dplyr::case_when(Input == "X1" ~ "abundance", 
                                             Input == "X2" ~ "mass",
                                             Input == "X3" ~ "beta",
                                             Input == "X4" ~ "ndays",
                                             Input == "X5" ~ "nrj_in_diet",
                                             Input == "X6" ~ "nut_in_diet",
                                             Input == "X7" ~ "assi_rate",
                                             Input == "X8" ~ "nut_abs_rate"
      ))
    
    df_Si_tot <- tibble::tibble(Code_sp = purrr::pluck(results_tib, "Code_sp", rw), 
                                Geo_area = purrr::pluck(results_tib, "Geo_area", rw), 
                                Eco_area = purrr::pluck(results_tib, "Eco_area", rw),
                                Analysis = nutrient, 
                                Input = rownames(sens$T),
                                Sensitivity = "Total order indices", 
                                original = purrr::pluck(sens$T, "original"), 
                                bias = purrr::pluck(sens$S, "bias"),
                                "std. error" = purrr::pluck(sens$T, "std. error"),
                                "min. c.i." = purrr::pluck(sens$T, "min. c.i."),
                                "max. c.i." = purrr::pluck(sens$T, "max. c.i.")) |>
      dplyr::mutate(Input = dplyr::case_when(Input == "X1" ~ "abundance", 
                                             Input == "X2" ~ "mass",
                                             Input == "X3" ~ "beta",
                                             Input == "X4" ~ "ndays",
                                             Input == "X5" ~ "nrj_in_diet",
                                             Input == "X6" ~ "nut_in_diet",
                                             Input == "X7" ~ "assi_rate",
                                             Input == "X8" ~ "nut_abs_rate"
      ))
    
    
    df_Si_Sti <- rbind(df_Si_Sti, 
                       df_Si_first, 
                       df_Si_tot)
    
    
  }
  
  df_Si_Sti <- df_Si_Sti[-1, ]
  
  df_Si_Sti
}

tab_N <- create_sobol_index_tib_sensi(model_output_clean, 
                                      "N", 
                                      1e2)
tab_N_old <- create_sobol_index_tib_sensi(computed_data,
                                      "N",
                                      1e5)

tab_P <- create_sobol_index_tib_sensi(model_output_clean, 
                                      "P", 
                                      1e2)
tab_Fe <- create_sobol_index_tib_sensi(model_output_clean, 
                                      "Fe", 
                                      1e2)

tab_Fe_old <- create_sobol_index_tib_sensi(computed_data,
                                       "Fe",
                                       1e5)

tab_Cu <- create_sobol_index_tib_sensi(model_output_clean, 
                                       "Cu", 
                                       1e2)


rbind(tab_N, tab_P, tab_Fe, tab_Cu) |> 
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = Input, y = original, fill = Sensitivity))

sobol_index_all_sensi |> 
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = Input, y = original, fill = Sensitivity))


rbind(tab_N_old, tab_Fe_old) |> 
  #dplyr::filter(Geo_area %in% c("Northeast Atlantic",
  #                              "Northwest Atlantic")) |>
  dplyr::filter(Geo_area %in% c("NEAtlantic", 
                               "NWAtlantic")) |>
  dplyr::mutate(Analysis = factor(Analysis, 
                                  levels = c("N", "Fe")), 
                Input = factor(Input, 
                                  levels = c("beta",  
                                             "mass", 
                                             "assi_rate",      
                                             "nrj_in_diet",
                                             "nut_in_diet",
                                             "nut_abs_rate",        
                                             "abundance",         
                                             "ndays"))) |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = Input, y = original, fill = Sensitivity)) +
  ggplot2::facet_wrap(~ Analysis, nrow = 2) +
  ghibli::scale_fill_ghibli_d("YesterdayMedium", 
                              direction = - 1) +
  ggplot2::ylab("Sobol sensitivity indices") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(), 
                 legend.position = "bottom", 
                 legend.text = ggplot2::element_text(size = 14, 
                                                     face = "bold"),
                 axis.title.x = ggplot2::element_blank(), 
                 axis.title.y = ggplot2::element_text(face = "bold", 
                                                      size = 15),
                 axis.text.x = ggplot2::element_blank(), 
                 axis.text.y = ggplot2::element_text(size = 14), 
                 strip.text.x = ggplot2::element_text(face = "bold", 
                                                      size = 15)
                 )
ggplot2::ggsave("output/ISEC_2_old-data.jpg", 
                #scale =1, 
                width = 6, 
                height = 5)


#### excretion for just one species in one place
purrr::pluck(model_output_clean, "excrete_nut", 102)|>
  tidyr::pivot_longer(cols = c(N, P, As, Co, Cu, Fe, Mn, Se, Zn), 
                      names_to = "Element", 
                      values_to = "Excretion") |> 
  dplyr::mutate(Element = factor(Element, 
                                 levels = c("N", "P", "Fe", "Cu", "Mn", 
                                            "Se", "Zn", "Co", "As"))
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = Element, y = Excretion, fill = Element)) +
  ggplot2::scale_y_continuous(trans = "log10", 
                              breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) 



