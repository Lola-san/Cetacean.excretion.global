########################### MEETING WITH CHARLOTTE LAMBERT #####################
# Estimates of prey consumption and model parameters in the Mediterranean Sea

# Input parameter values
targets::tar_load(pop_input)

pop_input_med <- pop_input |>
  dplyr::filter(Geo_area == "Med") |>
  dplyr::ungroup() |>
  dplyr::select(-c(Code_sp, Geo_area, Surf_tot, Eco_gp))

# diets
targets::tar_load(diet_input)

diet_input_med <- diet_input |>
  dplyr::filter(Code_sp %in% c("Bala_phy", "Delp_del", "Sten_coe", "Turs_tru",
                               "Gram_gri", "Glob_mel", "Ziph_cav", "Phys_mac")) |>
  tidyr::unnest(Diet)

openxlsx::write.xlsx(diet_input_med,
                     file = "C:/Users/lgilbe01/Desktop/Consumption-contract 2024/Meetings/Med-sea_cetacean-diets.xlsx")

# plot
diet_input_med |>
  tidyr::pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'), 
                      names_to = "prey_group", 
                      values_to = "prop_in_diet") |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = Code_sp, 
                                 y = prop_in_diet, 
                                 fill = prey_group), 
                    stat = "identity", 
                    position = "stack") +
  ggplot2::scale_fill_manual(values = wesanderson::wes_palette("FantasticFox1",
                                                                13, # nb of areas
                                                                type = "continuous"), 
                             name = "Prey group") +
  ggplot2::ylab("Proportion in diet (in % wet weight)") +
  ggplot2::xlab("Species") +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "right",
                 legend.text = ggplot2::element_text(size = 11),
                 axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                 axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                 axis.text.y = ggplot2::element_text(face = "bold", size = 12),
                 axis.text.x = ggplot2::element_text(face = "bold", size = 12))
ggplot2::ggsave("C:/Users/lgilbe01/Desktop/Consumption-contract 2024/Meetings/cetacean_diets.png", 
                scale = 1, 
                height = 9, 
                width = 9)
  
  

prey_nrj <- targets::tar_read(prey_compo_boot) |>
  dplyr::group_by(Prey_group) |>
  dplyr::summarise(mean_nrj = round(mean(NRJ), 2)) |>
  dplyr::arrange(Prey_group)

openxlsx::write.xlsx(prey_nrj,
                     file = "C:/Users/lgilbe01/Desktop/Consumption-contract 2024/Meetings/prey-gp_nrj.xlsx")


# Inputs and intermediary parameters (ADMR, ration, ration in % body mass) after Monte-Carlo simulations
targets::tar_load(model_output_clean)

model_output_Med_sp <- model_output_clean |>
  dplyr::filter(Geo_area == "Mediterranean Sea")

param_outputs_med <- model_output_Med_sp |>
  dplyr::ungroup() |>
  dplyr::group_by(Species) |>
  tidyr::unnest(Mass) |>
  dplyr::summarize(min = round(min(value), 0),
                   `2.5_quant` = round(quantile(value, probs = c(0.025)), 0),
                   mean = round(mean(value), 0),
                   `97.5_quant` = round(quantile(value, probs = c(0.975)), 0),
                   max = round(max(value), 0), 
                   sd = round(sd(value), 2), 
                   cv = round(sd/mean, 2)) |>
  dplyr::mutate(Parameter = "Body mass") |>
  # next parameters
  dplyr::bind_rows(model_output_Med_sp |>
                     dplyr::ungroup() |>
                     dplyr::group_by(Species) |>
                     tidyr::unnest(Beta) |>
                     dplyr::summarize(min = round(min(value), 1),
                                      `2.5_quant` = round(quantile(value, probs = c(0.025)), 1),
                                      mean = round(mean(value), 1),
                                      `97.5_quant` = round(quantile(value, probs = c(0.975)), 1),
                                      max = round(max(value), 1), 
                                      sd = round(sd(value), 2), 
                                      cv = round(sd/mean, 2)) |>
                     dplyr::mutate(Parameter = "Beta"),
                   model_output_Med_sp |>
                     dplyr::ungroup() |>
                     dplyr::group_by(Species) |>
                     tidyr::unnest(NRJ_diet) |>
                     dplyr::summarize(min = round(min(value), 1),
                                      `2.5_quant` = round(quantile(value, probs = c(0.025)), 1),
                                      mean = round(mean(value), 1),
                                      `97.5_quant` = round(quantile(value, probs = c(0.975)), 1),
                                      max = round(max(value), 1), 
                                      sd = round(sd(value), 2), 
                                      cv = round(sd/mean, 2)) |>
                     dplyr::mutate(Parameter = "Mean diet energy content (mg/kg)"),
                   model_output_Med_sp |>
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
                     dplyr::summarize(min = round(min(value), 3),
                                      `2.5_quant` = round(quantile(value, probs = c(0.025)), 3),
                                      mean = round(mean(value), 3),
                                      `97.5_quant` = round(quantile(value, probs = c(0.975)), 3),
                                      max = round(max(value), 3), 
                                      sd = round(sd(value), 3), 
                                      cv = round(sd/mean, 3)) |>
                     dplyr::arrange(Parameter))

openxlsx::write.xlsx(param_outputs_med,
                     file = "C:/Users/lgilbe01/Desktop/Consumption-contract 2024/Meetings/Med-sea_param-outputs.xlsx")


# load output table


# 