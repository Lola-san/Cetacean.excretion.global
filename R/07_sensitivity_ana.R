################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# January 2022
# 07_sensitivity_ana.R
#
# Script with function to run the sensitivity analysis for the random parameters 
# of the model
################################################################################

# We use Sobol global method

############################ WITH SENSITIVITY PACKAGE ###########################

### small functions for the sensitivity analysis 
# create function to compute y from mat 
compute_y_sensi <- function(param_mat) {
  # param_mat is the matrix of parameters
  ADMR <- param_mat[, 3]*293.1*(param_mat[, 2]^0.737)
  Ration <- ADMR / (param_mat[, 7]*param_mat[, 5])
  conso_pop <- param_mat[, 1]*param_mat[, 4]*Ration
  conso_nut <- (conso_pop*param_mat[, 6])/1e9
  excrete_nut <- conso_nut*param_mat[, 8]
  
  return(excrete_nut)
}

#'
#'
#'
#'
# function to:
# compute the sensitivity analysis index for all lines of the result table
# each line being a species in an Eco_area in a Geo_area
create_sobol_index_tib_sensi <- function(results_tib, 
                                         nsim) {
  
  # tibble where results we be stored
  df_Si_Sti <- tibble::tibble(Species = NA, 
                              Geo_area = NA, 
                              Eco_area = NA, 
                              Eco_gp = NA,
                              Nutrient = NA,
                              Input = NA,
                              Sensitivity = NA, # wether it's first order or total sobol indices
                              original = NA, #mean
                              bias = NA,
                              "std. error" = NA, 
                              "min. c.i." = NA,
                              "max. c.i." = NA)
  
  
  
  for (rw in 1:nrow(results_tib)) {
    # for migratory baleen whales species we can do only for N and P (excretion of micronutrients is set to 0)
    if (purrr::pluck(results_tib, "Code_sp", rw) %in% c("Bala_mus", "Bala_ede", 
                                                        "Bala_phy", "Bala_bor", 
                                                        "Mega_nov") & !(purrr::pluck(results_tib, "Geo_area", rw) %in% c("Med", 
                                                                                                                         "GoMexico"))) {
      list_nutrients <- c("N", "P")
      
    } else {
      list_nutrients <- colnames(purrr::pluck(results_tib, "Nut_excrete", 1) |> 
                                   dplyr::select(-As))
    }
    
    for (nut in list_nutrients) {
      # sampling matrix 
      # change distributions of inputs according to data or bibliography/assumptions
      parammatX1 <- matrix(data = c(sample(purrr::pluck(results_tib, "Abund", rw, 1), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "Mass", rw, 1), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "Beta", rw, 1), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "Ndays", rw, 1), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "NRJ_diet", rw, 1), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "Nut_diet", rw, nut), size = nsim/5, replace = FALSE), 
                                    sample(rnorm(mean = 0.8, sd = 0.05, n = 1e5), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "Nut_excrete", rw, nut), size = nsim/5, replace = FALSE)), 
                           ncol = 8, nrow = nsim/5) 
      
      parammatX2 <- matrix(data = c(sample(purrr::pluck(results_tib, "Abund", rw, 1), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "Mass", rw, 1), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "Beta", rw, 1), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "Ndays", rw, 1), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "NRJ_diet", rw, 1), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "Nut_diet", rw, nut), size = nsim/5, replace = FALSE), 
                                    sample(rnorm(mean = 0.8, sd = 0.05, n = 1e5), size = nsim/5, replace = FALSE), 
                                    sample(purrr::pluck(results_tib, "Nut_excrete", rw, nut), size = nsim/5, replace = FALSE)), 
                           ncol = 8, nrow = nsim/5)
      
      #output <- sample(purrr::pluck(results_tib, "excrete_nut", rw, "N"), size = nsim/5, replace = FALSE)
      
      sens <- sensitivity::sobolSalt(model = compute_y_sensi, X1 = parammatX1, X2 = parammatX2, 
                                     scheme = "A", nboot = 1e3, conf = 0.95)
      
      
      df_Si_first <- tibble::tibble(Species = purrr::pluck(results_tib, "Species", rw), 
                                    Geo_area = purrr::pluck(results_tib, "Geo_area", rw), 
                                    Eco_area = purrr::pluck(results_tib, "Eco_area", rw), 
                                    Eco_gp = purrr::pluck(results_tib, "Eco_gp", rw), 
                                    Nutrient = nut, 
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
      
      df_Si_tot <- tibble::tibble(Species = purrr::pluck(results_tib, "Species", rw), 
                                  Geo_area = purrr::pluck(results_tib, "Geo_area", rw), 
                                  Eco_area = purrr::pluck(results_tib, "Eco_area", rw),
                                  Eco_gp = purrr::pluck(results_tib, "Eco_gp", rw), 
                                  Nutrient = nut, 
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
    
  }
  
  df_Si_Sti <- df_Si_Sti[-1, ]
  
  df_Si_Sti
}



#'
#'
#'
#'
#' function to plot results of sensitivity analysis for all three taxa
fig_sensitivy_indices_all_taxa_N <- function(sensi_tib, 
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
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Input, y = original, fill = Sensitivity), color = "gray40", 
                          width = 0.5, 
                          #position = ggplot2::position_dodge(width=0.9)
    ) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF"), 
                               labels = function(x) stringr::str_wrap(x, width = 7)) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 6)) +
    ggplot2::ylab("Sobol sensivity indice") +
    ggplot2::xlab("Model parameter") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12),
                   axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(size = 12),
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   legend.title = ggplot2::element_blank(), 
                   legend.text = ggplot2::element_text(size = 12),
                   legend.spacing.y = ggplot2::unit(1.5, 'cm'))
  
  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/article/", name_file, ".jpg"),
                    width = 8,
                    height = 5)
  } else {
    figure
  }
}


#'
#'
#'
#'
#' function to plot results of sensitivity analysis for baleen whales only
fig_sensitivy_indices_BW <- function(sensi_tib, 
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
    dplyr::filter(Eco_gp == "Baleen whales") |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Input, y = original, fill = Sensitivity)) +
    ggplot2::facet_wrap(~ Nutrient, nrow = 2) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF")) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 6)) +
    ggplot2::ylab("Sobol sensivity indice") +
    ggplot2::xlab("Model parameter") +
    ggplot2::ggtitle("Baleen whales") +
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
                    width = 22,
                    height = 8)
  } else {
    figure
  }
}


#'
#'
#'
#'
#' function to plot results of sensitivity analysis for deep divers only
fig_sensitivy_indices_DD <- function(sensi_tib, 
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
    # for deep divers, nb of days of presence is set to 365 so no need to take it into account
    dplyr::filter(Input != "Nb of days of presence") |>
    dplyr::mutate(Input = factor(Input, 
                                 levels = c("Body mass", "Beta",
                                            "Mean energy content of diet", "Mean nutrient content of diet",
                                            "Assimi-lation rate", "Nutrient release rate",  
                                            "Abun-dance"))) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("N", "P", "Fe", "Cu", 
                                               "Mn", "Se", "Zn", "Co"))) |>
    dplyr::filter(Eco_gp == "Deep divers") |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Input, y = original, fill = Sensitivity)) +
    ggplot2::facet_wrap(~ Nutrient, nrow = 2) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF")) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 6)) +
    ggplot2::ylab("Sobol sensivity indice") +
    ggplot2::xlab("Model parameter") +
    ggplot2::ggtitle("Deep divers") +
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
                    width = 22,
                    height = 8)
  } else {
    figure
  }
}



#'
#'
#'
#'
#' function to plot results of sensitivity analysis for small delphinids only
fig_sensitivy_indices_SD <- function(sensi_tib, 
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
    # for small delph, nb of days of presence is set to 365 so no need to take it into account
    dplyr::filter(Input != "Nb of days of presence") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("N", "P", "Fe", "Cu", 
                                               "Mn", "Se", "Zn", "Co"))) |>
    dplyr::filter(Eco_gp == "Small delphinids") |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Input, y = original, fill = Sensitivity)) +
    ggplot2::facet_wrap(~ Nutrient, nrow = 2) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF")) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 6)) +
    ggplot2::ylab("Sobol sensivity indice") +
    ggplot2::xlab("Model parameter") +
    ggplot2::ggtitle("Small delphinids") +
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
                    width = 22,
                    height = 8)
  } else {
    figure
  }
}