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

### small functions for the sensitivity analysis 
# create function to compute y from mat 
compute_y <- function(param_mat, mat_bootstrap) {
  # param_mat is the matrix of parameters of which the sensitivity is analyzed
  # for parameters on which we did bootstrap (ie.NRJ in diet and nutrient in diet), we can't analyze sensitivity as we did not infer about the distribution of these parameters)
  ADMR <- param_mat[, "Beta"]*293.1*(param_mat[, "Mass"]^0.737)
  Ration <- ADMR / (param_mat[, "A_rate"]*purrr::pluck(mat_bootstrap, "NRJ_diet"))
  conso_pop <- param_mat[, "Abund"]*param_mat[, "Ndays"]*Ration
  conso_nut <- (conso_pop*purrr::pluck(mat_bootstrap, "Nut_diet"))/1e9
  excrete_nut <- conso_nut*param_mat[, "Nut_excrete"]
  
  return(excrete_nut)
}

# create function to compute y from mat 
compute_y_c_biomass <- function(param_mat) {
  ww_to_c_biomass <- (param_mat[, "Abund"]*param_mat[, "Mass"]* param_mat[, "ww_to_c"])/1e3
  
  return(ww_to_c_biomass)
}

# function to:
# compute the sensitivity analysis index for all lines of the result table
# each line being a species in an Eco_area in a Geo_area
create_sobol_index_tib <- function(data_tib, 
                                   results_tib, 
                                   nsim) {

  # tibble where results we be stored
  df_Si_Sti <- tibble::tibble(Code_sp = NA, 
                              Geo_area = NA, 
                              Eco_area = NA, 
                              Analysis = NA, 
                              Input = NA,
                              Sensitivity = NA, 
                              Mean = NA,
                              Se = NA,
                              Low_ci = NA,
                              High_ci = NA)
  
  for (rw in 1:nrow(results_tib)) {
    
    # parameters for the sensitivity analysis 
    #N <- nsim # nb of samples
    R <- 10^3 # nb of bootstrap replicas
    type <- "norm" # bootstrap confidence interval method
    conf <- 0.95 # length of the confidence interval
    paramsN <- c("Abund", "Mass", "Beta", 
                 "Ndays", "A_rate", "Nut_excrete")
    paramsC_biomass <- c("Abund", "Mass", "ww_to_c")
    
    # construct the sample matrix of inputs
    matN <- sensobol::sobol_matrices(N = nsim, params = paramsN)
    matC_biomass <- sensobol::sobol_matrices(N = nsim, params = paramsC_biomass)
    
    # change distributions of inputs according to data or bibliography/assumptions
    matN[, "Abund"] <- qlnorm(matN[, "Abund"], 
                              meanlog = log(purrr::pluck(data_tib, 
                                                         "Abund", 
                                                         rw, 
                                                         "Abund") /sqrt(1 + purrr::pluck(data_tib, 
                                                                                         "Abund", 
                                                                                         rw, 
                                                                                         "Abund_CV")*purrr::pluck(data_tib, 
                                                                                                                  "Abund",
                                                                                                                  rw, 
                                                                                                                  "Abund_CV"))),
                              sdlog = sqrt(log1p(purrr::pluck(data_tib, 
                                                              "Abund", 
                                                              rw, 
                                                              "Abund_CV")*purrr::pluck(data_tib, "Abund", rw, "Abund_CV"))))
    matN[, "Mass"] <- qnorm(matN[, "Mass"], 
                            mean = purrr::pluck(data_tib, "Mass", rw, "Mass"), 
                            sd = (purrr::pluck(data_tib, "Mass", rw, "Mass_max")-purrr::pluck(data_tib, "Mass", rw, "Mass_min"))/4)
    matN[, "Beta"] <- qnorm(matN[, "Beta"], 
                            mean = purrr::pluck(data_tib, "Beta", rw, "Beta"), 
                            sd = (purrr::pluck(data_tib, "Beta", rw, "Beta_max")-purrr::pluck(data_tib, "Beta", rw, "Beta_min"))/4)
    matN[, "Ndays"] <- qunif(matN[, "Ndays"], 
                             min = purrr::pluck(data_tib, "Ndays", rw, "Ndays_min"), 
                             max = purrr::pluck(data_tib, "Ndays", rw, "Ndays_max"))
    matN[, "A_rate"] <- qnorm(matN[, "A_rate"], mean = 0.8, sd = 0.1*0.025)
    matN[, "Nut_excrete"] <- qunif(matN[, "Nut_excrete"], 
                                   min = purrr::pluck(data_tib, "Nut_excrete", rw, "N") - 0.1,  
                                   max = purrr::pluck(data_tib, "Nut_excrete", rw, "N") + 0.1)
    
    
    matC_biomass[, "Abund"] <- qlnorm(matC_biomass[, "Abund"], 
                                      meanlog = log(purrr::pluck(data_tib, "Abund", rw, "Abund") /sqrt(1 + purrr::pluck(data_tib, "Abund", rw, "Abund_CV")*purrr::pluck(data_tib, "Abund", rw, "Abund_CV"))),
                                      sdlog = sqrt(log1p(purrr::pluck(data_tib, "Abund", rw, "Abund_CV")*purrr::pluck(data_tib, "Abund", rw, "Abund_CV"))))
    matC_biomass[, "Mass"] <- qnorm(matC_biomass[, "Mass"], 
                                    mean = purrr::pluck(data_tib, "Mass", rw, "Mass"), 
                                    sd = (purrr::pluck(data_tib, "Mass", rw, "Mass_max")-purrr::pluck(data_tib, "Mass", rw, "Mass_min"))/4)
    matC_biomass[, "ww_to_c"] <-  qunif(matC_biomass[, "ww_to_c"], 
                                        min = 0.1,  
                                        max = 0.15)
    
    mat_bootstrap_par <- tibble::tibble(NRJ_diet = purrr::pluck(data_tib, "NRJ_diet", rw, 1),
                                Nut_diet = purrr::pluck(data_tib, "Nut_diet", rw, "N"))
    
    # compute output
    yN <- compute_y(matN, mat_bootstrap_par)
    yC_biomass <- compute_y_c_biomass(matC_biomass)
    
    # compute sensitivity index 
    indN <- sensobol::sobol_indices(Y = yN, N = nsim, params = paramsN, 
                          boot = TRUE, R = R, type = type, conf = conf)
    cols <- colnames(indN$results)[1:5]
    indN$results[, (cols):= round(.SD, 3), .SDcols = (cols)]
    
    indC_biomass <- sensobol::sobol_indices(Y = yC_biomass, N = nsim, params = paramsC_biomass, boot = TRUE, R = R, type = type, conf = conf)
    cols <- colnames(indC_biomass$results)[1:5]
    indC_biomass$results[, (cols):= round(.SD, 3), .SDcols = (cols)]
    
    df_Si_Sti_N <- tibble::tibble(Code_sp = purrr::pluck(results_tib, "Code_sp", rw), 
                          Geo_area = purrr::pluck(results_tib, "Geo_area", rw), 
                          Eco_area = purrr::pluck(results_tib, "Eco_area", rw), 
                          Analysis = "N", 
                          Input = purrr::pluck(indN, "results", "parameters"),
                          Sensitivity = purrr::pluck(indN, "results", "sensitivity"), 
                          Mean = purrr::pluck(indN, "results", "original"), 
                          Se = purrr::pluck(indN, "results", "std.error"),
                          Low_ci = purrr::pluck(indN, "results", "low.ci"),
                          High_ci = purrr::pluck(indN, "results", "high.ci"))
    
    df_Si_Sti_C_biomass <- tibble::tibble(Code_sp = purrr::pluck(results_tib, "Code_sp", rw), 
                                  Geo_area = purrr::pluck(results_tib, "Geo_area", rw), 
                                  Eco_area = purrr::pluck(results_tib, "Eco_area", rw),
                                  Analysis = "C_biomass", 
                                  Input = purrr::pluck(indC_biomass, "results", "parameters"),
                                  Sensitivity = purrr::pluck(indC_biomass, "results", "sensitivity"), 
                                  Mean = purrr::pluck(indC_biomass, "results", "original"), 
                                  Se = purrr::pluck(indC_biomass, "results", "std.error"),
                                  Low_ci = purrr::pluck(indC_biomass, "results", "low.ci"),
                                  High_ci = purrr::pluck(indC_biomass, "results", "high.ci"))
    
    
    df_Si_Sti <- rbind(df_Si_Sti, 
                       df_Si_Sti_N, 
                       df_Si_Sti_C_biomass 
    )
    
  }
  
  df_Si_Sti <- df_Si_Sti[-1, ]
  
  df_Si_Sti
}

############################ WITH SENSITIVITY PACKAGE 

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
  
  
  for (rw in 1:nrow(results_tib)) {
    
    # sampling matrix 
    # change distributions of inputs according to data or bibliography/assumptions
    parammatX1 <- matrix(data = c(sample(purrr::pluck(results_tib, "Abund", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Mass", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Beta", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Ndays", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "NRJ_diet", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Nut_diet", rw, "N"), size = nsim/5, replace = FALSE), 
                                  sample(rnorm(mean = 0.8, sd = 0.05, n = 1e5), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Nut_excrete", rw, "N"), size = nsim/5, replace = FALSE)), 
                         ncol = 8, nrow = nsim/5) 
    
    parammatX2 <- matrix(data = c(sample(purrr::pluck(results_tib, "Abund", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Mass", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Beta", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Ndays", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "NRJ_diet", rw, 1), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Nut_diet", rw, "N"), size = nsim/5, replace = FALSE), 
                                  sample(rnorm(mean = 0.8, sd = 0.05, n = 1e5), size = nsim/5, replace = FALSE), 
                                  sample(purrr::pluck(results_tib, "Nut_excrete", rw, "N"), size = nsim/5, replace = FALSE)), 
                         ncol = 8, nrow = nsim/5)
    
    #output <- sample(purrr::pluck(results_tib, "excrete_nut", rw, "N"), size = nsim/5, replace = FALSE)
    
    sens <- sensitivity::sobolSalt(model = compute_y_sensi, X1 = parammatX1, X2 = parammatX2, 
                                   scheme = "A", nboot = 1e3, conf = 0.95)
    
    
    df_Si_first <- tibble::tibble(Code_sp = purrr::pluck(results_tib, "Code_sp", rw), 
                                  Geo_area = purrr::pluck(results_tib, "Geo_area", rw), 
                                  Eco_area = purrr::pluck(results_tib, "Eco_area", rw), 
                                  Analysis = "N", 
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
                                Analysis = "N", 
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