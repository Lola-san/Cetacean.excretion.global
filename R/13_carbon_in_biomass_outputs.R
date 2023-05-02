################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# February 2023
# 13_carbon_in_biomass_outputs.R
#
# Script with functions to generate figures and tables about carbon in biomass 
# results 
# 
################################################################################


############## tables ############


#'
#'
#'
#'
#'
## function to generate table with estimation of carbon in biomass 
# of living cetaceans in each area, in tons and tons_km2
create_tab_C_in_bio <- function(output_tib,
                                object_type, # either "output" or "file" 
                                name_file) {
  
  table <- rbind(output_tib |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     c = list(sum_vec(c_in_biomass))) |>
    tidyr::unnest(c) |>
    dplyr::group_by(Geo_area) |>
    dplyr::summarize(min = min(c), 
                     `2.5_quant` =  quantile(c, probs = c(0.025)),
                     mean = mean(c), 
                     median = median(c), 
                     `97.5_quant` = quantile(c, probs = c(0.975)),  
                     max = max(c)) |>
    dplyr::mutate(unit = "tons"), 
    output_tib |>
      dplyr::group_by(Geo_area) |>
      dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                       c = list(sum_vec(c_in_biomass))) |>
      tidyr::unnest(c) |>
      dplyr::mutate(C_t_per_km2 = seq_along(c) |>
                      purrr::map(~ c[[.]]/Surf[.])) |>
      tidyr::unnest(c(c, C_t_per_km2)) |>
      dplyr::group_by(Geo_area) |>
      dplyr::summarize(min = min(C_t_per_km2), 
                       `2.5_quant` =  quantile(C_t_per_km2, probs = c(0.025)),
                       mean = mean(C_t_per_km2), 
                       median = median(C_t_per_km2), 
                       `97.5_quant` = quantile(C_t_per_km2, probs = c(0.975)),  
                       max = max(C_t_per_km2)) |>
      dplyr::mutate(unit = "tons per km2")) 
    
  if (object_type == "file") {
    write.table(table, paste0("output/carbon/", 
                              name_file,
                              ".txt"), sep = "\t")
  } else {
    table
  }
  
      
  }

################## figures ##############################
#'
#'
#'
#'
# 
create_fig_C_per_area_tons <- function(output_tib,
                                  object_type, # either "output" or "file" 
                                  name_file 
                                  ) {
  table <- output_tib |>
                   dplyr::group_by(Geo_area) |>
                   dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                                    c = list(sum_vec(c_in_biomass))) |>
                   tidyr::unnest(c) |>
                   dplyr::group_by(Geo_area) |>
                   dplyr::summarize(min = min(c), 
                                    `2.5_quant` =  quantile(c, probs = c(0.025)),
                                    mean = mean(c), 
                                    median = median(c), 
                                    `97.5_quant` = quantile(c, probs = c(0.975)),  
                                    max = max(c)) 
  
  n_areas <- length(unique(table$Geo_area))
  
  table |>
    ggplot2::ggplot(ggplot2::aes(x = Geo_area, y = mean, color = Geo_area)) +
    ggplot2::geom_point(shape = 16, size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Geo_area, 
                                        ymin = `2.5_quant`, 
                                        ymax = `97.5_quant`, 
                                        color = Geo_area), size = 1) +
    ggplot2::theme_classic() +
    ggplot2::scale_color_manual(values = c("#4E9F50", "#87D180", "#EF8A0C", "#FCC66D", "#3CA8BC", "#98D9E4", 
                                                    "#94A323", "#C3CE3D", "#A08400", "#F7D42A", "#26897E", "#8DBFA8", 
                                                    "#CF3E53", "#F1788D")) +
    ggplot2::guides(color = FALSE) + 
    ggplot2::xlab("Area") +
    ggplot2::ylab("C in cetacean biomass (in tons/yr)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
  
}


#'
#'
#'
#'
# 
create_fig_C_per_area_tons_per_km2 <- function(output_tib,
                                       object_type, # either "output" or "file" 
                                       name_file 
) {
  table <- output_tib |>
                   dplyr::ungroup() |>
                   dplyr::group_by(Geo_area) |>
                   dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                                    c = list(sum_vec(c_in_biomass))) |>
                   tidyr::unnest(c) |>
                   dplyr::mutate(C_t_per_km2 = seq_along(c) |>
                                   purrr::map(~ c[[.]]/Surf[.])) |>
                   tidyr::unnest(c(c, C_t_per_km2)) |>
                   dplyr::group_by(Geo_area) |>
                   dplyr::summarize(min = min(C_t_per_km2), 
                                    `2.5_quant` =  quantile(C_t_per_km2, probs = c(0.025)),
                                    mean = mean(C_t_per_km2), 
                                    median = median(C_t_per_km2), 
                                    `97.5_quant` = quantile(C_t_per_km2, probs = c(0.975)),  
                                    max = max(C_t_per_km2)) |>
                   dplyr::mutate(unit = "tons per km2")
  
  n_areas <- length(unique(table$Geo_area))
  
  table |>
    ggplot2::ggplot(ggplot2::aes(x = Geo_area, y = mean, color = Geo_area)) +
    ggplot2::geom_point(shape = 16, size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Geo_area, 
                                        ymin = `2.5_quant`, 
                                        ymax = `97.5_quant`, 
                                        color = Geo_area), size = 1) +
    ggplot2::theme_classic() +
    ggplot2::scale_color_manual(values = c("#4E9F50", "#87D180", "#EF8A0C", "#FCC66D", "#3CA8BC", "#98D9E4", 
                                                    "#94A323", "#C3CE3D", "#A08400", "#F7D42A", "#26897E", "#8DBFA8", 
                                                    "#CF3E53", "#F1788D")) +
                                                      ggplot2::guides(color = FALSE) + 
    ggplot2::xlab("Area") +
    ggplot2::ylab("C in cetacean biomass (in tons/km2)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
  
}


#'
#'
#'
#'
#
create_fig_C_per_taxa_tot <- function(output_tib,
                                      object_type, # either "output" or "file" 
                                      name_file 
) {
  output_tib |>
    dplyr::group_by(Eco_gp) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     c = list(sum_vec(c_in_biomass))) |>
    tidyr::unnest(c) |>
    dplyr::group_by(Eco_gp) |>
    dplyr::summarize(min = min(c), 
                     `2.5_quant` =  quantile(c, probs = c(0.025)),
                     mean = mean(c), 
                     median = median(c), 
                     `97.5_quant` = quantile(c, probs = c(0.975)),  
                     max = max(c)) |>
    ggplot2::ggplot(ggplot2::aes(x = Eco_gp, y = mean, color = Eco_gp)) +
    ggplot2::geom_point(shape = 16, size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Eco_gp, 
                                        ymin = `2.5_quant`, 
                                        ymax = `97.5_quant`, 
                                        color = Eco_gp), size = 1) +
    ggplot2::theme_classic() +
    ggplot2::scale_color_manual(values = c("#4E9F50", "#3CA8BC", "#F1788D")) +
                                                      ggplot2::guides(color = FALSE) + 
    ggplot2::xlab("Ecological group") +
    ggplot2::ylab("C in cetacean biomass (in tons") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
  
}

#'
#'
#'
#'
#'
create_fig_C_per_taxa_per_area <- function(output_tib,
                                           object_type, # either "output" or "file" 
                                           name_file) {
  
  output_tib |>
    dplyr::group_by(Eco_gp, Geo_area) |>
    dplyr::summarise(Surf = sum(unique(Surf_tot)), 
                     c = list(sum_vec(c_in_biomass))) |>
    tidyr::unnest(c) |>
    dplyr::group_by(Geo_area, Eco_gp) |>
    dplyr::summarize(min = min(c), 
                     `2.5_quant` =  quantile(c, probs = c(0.025)),
                     mean = mean(c), 
                     median = median(c), 
                     `97.5_quant` = quantile(c, probs = c(0.975)),  
                     max = max(c)) |>
    ggplot2::ggplot(ggplot2::aes(x = Eco_gp, y = mean, color = Eco_gp)) +
    ggplot2::geom_point(shape = 16, size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Eco_gp, 
                                        ymin = `2.5_quant`, 
                                        ymax = `97.5_quant`, 
                                        color = Eco_gp), 
                           size = .5) +
    ggplot2::facet_wrap(~ Geo_area, scales = "free_y") +
    ggplot2::theme_classic() +
    ggplot2::scale_color_manual(values = c("#4E9F50", "#3CA8BC", "#F1788D")) +
    ggplot2::guides(color = FALSE) + 
    ggplot2::xlab("Ecological group") +
    ggplot2::ylab("C in cetacean biomass (in tons)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
  
}