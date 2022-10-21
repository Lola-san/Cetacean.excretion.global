################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# December 2021
# 4_nut_in_diet.R
#
# Script with functions to compute mean nutrient content of diets of each sp
################################################################################


############################# load data ########################################
# not needed: use function load_xl defined in script 00_ratio_abundances.R

############################# Data wrangling ###################################
# join compo table with prey groups to have prey groups corresponding to 
# prey species 
join_clean_compo_tib <- function(compo_tib, preygps_tib) {
  nutri_df <- compo_tib |>
    # change outlier value to quantile 99% values
    dplyr::mutate(Se = dplyr::case_when(Sp_prey == "Hyperoplus lanceolatus" ~ 2.95, 
                                        TRUE ~ Se)) |>
    dplyr::left_join(preygps_tib, key = c("Sp_prey", "Genus", 
                                          "Family", "Order", "Taxa"), 
                     keep = FALSE) |>
    # get rid of elements of no interest here, ie non essential ones 
    dplyr::select(-c(Pb, Ag, Cd))
  
  # add Prey_groups artificially :
  # - Gelatinous pelagic cephalopod with species of Muscular pelagic cephalopods
  # - Fish undetermined with all species of fish
  # - Cephalopod undetermined with all species of cephalopods 
  rbind(nutri_df,
        nutri_df |>
          dplyr::filter(Prey_group == "Muscular pelagic cephalopods") |>
          dplyr::mutate(Prey_group = "Gelatinous pelagic cephalopods"),
        nutri_df |>
          dplyr::filter(Taxa  == "Fish") |>
          dplyr::mutate(Prey_group = "Fish undetermined"),
        nutri_df |>
          dplyr::filter(Taxa == "Cephalopod") |>
          dplyr::mutate(Prey_group = "Cephalopod undetermined")
  ) |>
    dplyr::group_by(Prey_group) |>
    dplyr::filter(!is.na(Prey_group)) |> # one sample of jellyfish
    dplyr::mutate(N = dplyr::case_when(is.na(N) ~ mean(N, na.rm = TRUE),
                                       TRUE ~ N)) |>
    dplyr::select(-c(Sp_prey, Genus, Family, Order, Taxa, Habitat))
  
  
}

###############################################################################
######################## bootstrapping ########################################
#'
#'
#'
#'
#'
# function to perform kernel based inversion using Nicolas Bousquet (Sorbonnes University) procedure 
boot_kernel_inv <- function(compo_tib, nsim) {
  
  kernel_inv <- function(x, nsim, kernel = "gaussian") {
    # approche par noyaux ===========================================
    min = floor(min(x, na.rm = TRUE))
    max = ceiling(max(x, na.rm = TRUE))
    
    dens.old <- density(x,from=min,to=max,kernel=kernel) 
    dens = kdensity::kdensity(x, support=c(min,max), kernel = kernel) # on utilise des noyaux gaussiens par defaut
    
    xo=c(0.9*min(x),1.1*max(x))
    yo=c(0,1.2*max(dens.old$y))
    
    #==================== GENERATION BOOTSTRAP =================================
    #==================== GENERATION PAR INVERSION KERNEL-BASED ===================
    # construction "lisse" de la fonction de repartition
    cdf.lisse <- function(y)
    {
      integrate(function(x) dens(x), lower = min, upper = y)$value
    }	 
    # inverse "lisse" de la fonction de repartition
    # attention y est ici une valeur scalaire dans [0,1]
    inv.cdf.lisse <- function(y)
    {
      f.to.min <- function(x){abs(cdf.lisse(x)-y)}
      optimize(f.to.min,interval=c(min(xo),max(xo)))$minimum
    }
    
    # generation iid (on ne resimule pas dans les donnees mais dans la distribution)
    gen.sample.kernel = replicate(nsim,inv.cdf.lisse(runif(1)))	 # long ? tourner
    
    gen.sample.kernel
  }
  
  # there is only one sample in the prey group of zooplankton, so instead of doing bootstrapping we used Monte-Carlo simulations 
  # based on the mean concentration of that sample avec a % of this mean as a standard deviation
  # for all other groups sd seems to be higher for micronutrients than for N and P, so we set 20% of sd for N, P and 40% for micronutrients
  rbind(compo_tib |> 
          dplyr::group_by(Prey_group) |>
          dplyr::filter(Prey_group !="Zooplankton") |>
          tidyr::nest(compo = c(NRJ, N, P, Fe, Cu, Mn, Se, Zn, Co, As)) |>
          dplyr::mutate(compo = seq_along(compo) |> # nutrient excretion rate
                          purrr::map(~ tibble::tibble(NRJ = kernel_inv(purrr::pluck(compo, ., "NRJ"), nsim), 
                                                      N = kernel_inv(purrr::pluck(compo, ., "N"), nsim),
                                                      P = kernel_inv(purrr::pluck(compo, ., "P"), nsim),
                                                      Fe = kernel_inv(purrr::pluck(compo, ., "Fe"), nsim),
                                                      Cu = kernel_inv(purrr::pluck(compo, ., "Cu"), nsim),
                                                      Mn = kernel_inv(purrr::pluck(compo, ., "Mn"), nsim),
                                                      Se = kernel_inv(purrr::pluck(compo, ., "Se"), nsim),
                                                      Zn = kernel_inv(purrr::pluck(compo, ., "Zn"), nsim),
                                                      Co = kernel_inv(purrr::pluck(compo, ., "Co"), nsim),
                                                      As = kernel_inv(purrr::pluck(compo, ., "As"), nsim)
                          ))), 
        compo_tib |> 
          dplyr::group_by(Prey_group) |>
          dplyr::filter(Prey_group =="Zooplankton") |>
          tidyr::nest(compo = c(NRJ, N, P, Fe, Cu, Mn, Se, Zn, Co, As)) |>
          dplyr::mutate(compo = seq_along(compo) |> # nutrient excretion rate
                          purrr::map(~ tibble::tibble(NRJ = truncnorm::rtruncnorm(n = nsim, 
                                                                  mean = purrr::pluck(compo, ., "NRJ", 1), 
                                                                  sd = 0.2*purrr::pluck(compo, ., "NRJ", 1),
                                                                  a = 0.2*purrr::pluck(compo, ., "NRJ", 1)), 
                                                      N = truncnorm::rtruncnorm(n = nsim, 
                                                                mean = purrr::pluck(compo, ., "N", 1), 
                                                                sd = 0.2*purrr::pluck(compo, ., "N", 1),
                                                                a = 0.2*purrr::pluck(compo, ., "N", 1)),
                                                      P = truncnorm::rtruncnorm(n = nsim, 
                                                                mean = purrr::pluck(compo, ., "P", 1), 
                                                                sd = 0.2*purrr::pluck(compo, ., "P", 1),
                                                                a = 0.2*purrr::pluck(compo, ., "P", 1)),
                                                      Fe = truncnorm::rtruncnorm(n = nsim, 
                                                                 mean = purrr::pluck(compo, ., "Fe", 1), 
                                                                 sd = 0.4*purrr::pluck(compo, ., "Fe", 1),
                                                                 a = 0.2*purrr::pluck(compo, ., "Fe", 1)),
                                                      Cu = truncnorm::rtruncnorm(n = nsim, 
                                                                 mean = purrr::pluck(compo, ., "Cu", 1), 
                                                                 sd = 0.4*purrr::pluck(compo, ., "Cu", 1),
                                                                 a = 0.2*purrr::pluck(compo, ., "Cu", 1)),
                                                      Mn = truncnorm::rtruncnorm(n = nsim, 
                                                                 mean = purrr::pluck(compo, ., "Mn", 1), 
                                                                 sd = 0.4*purrr::pluck(compo, ., "Mn", 1),
                                                                 a = 0.2*purrr::pluck(compo, ., "Mn", 1)),
                                                      Se = truncnorm::rtruncnorm(n = nsim, 
                                                                 mean = purrr::pluck(compo, ., "Se", 1), 
                                                                 sd = 0.4*purrr::pluck(compo, ., "Se", 1),
                                                                 a = 0.2*purrr::pluck(compo, ., "Se", 1)),
                                                      Zn = truncnorm::rtruncnorm(n = nsim, 
                                                                 mean = purrr::pluck(compo, ., "Zn", 1), 
                                                                 sd = 0.4*purrr::pluck(compo, ., "Zn", 1),
                                                                 a = 0.2*purrr::pluck(compo, ., "Zn", 1)),
                                                      Co = truncnorm::rtruncnorm(n = nsim, 
                                                                 mean = purrr::pluck(compo, ., "Co", 1), 
                                                                 sd = 0.4*purrr::pluck(compo, ., "Co", 1),
                                                                 a = 0.2*purrr::pluck(compo, ., "Co", 1)),
                                                      As = truncnorm::rtruncnorm(n = nsim, 
                                                                 mean = purrr::pluck(compo, ., "As", 1), 
                                                                 sd = 0.4*purrr::pluck(compo, ., "As", 1),
                                                                 a =  0.2*purrr::pluck(compo, ., "As", 1))
                          )))) |>
    tidyr::unnest(compo)
  
  
  
}




#'
#'
#'
#'
#'
# doesn't work where the number of sample per prey group is low, which is for most....
# # function to perform gaussian mixture bootstrap using Nicolas Bousquet (Sorbonnes University) procedure
# boot_gauss_mix <- function(compo_tib, nsim) {
#   
#   gauss_mix <- function(x, nsim) {
#     # approche par modele de melange de gaussiennes ===========================================
#     dens2 = mixtools::normalmixEM(x) # estimation menee par algo EM
#     weights <- dens2$lambda
#     mu <- dens2$mu
#     sigma <- dens2$sigma
#     d <- length(weights)
#     dens2 <- function(x)
#     {
#       y <- 0
#       for (i in c(1:d))
#       {
#         y <- y + weights[i]*dnorm(x,mu[i],sigma[i])
#       }
#       return(y)
#     }
#     # simulation d'un melange de gaussiennes
#     r.dens2 <- function(n)
#     {
#       components <- sample(1:d,prob=weights,size=n,replace=TRUE)
#       rnorm(n)*sigma[components]+mu[components]
#     }
#     
#     #==================== GENERATION PAR MODELE DE MELANGE ===================
#     gen.sample.mixture = r.dens2(nsim) 
#   }
#   
#   # there is only one sample in the prey group of zooplankton, so instead of doing bootstrapping we used Monte-Carlo simulations 
#   # based on the mean concentration of that sample avec a % of this mean as a standard deviation
#   # for all other groups sd seems to be higher for micronutrients than for N and P, so we set 20% of sd for N, P and 40% for micronutrients
#   rbind(compo_tib |> 
#           dplyr::group_by(Prey_group) |>
#           dplyr::filter(Prey_group !="Zooplankton") |>
#           tidyr::nest(compo = c(NRJ, N, P, Fe, Cu, Mn, Se, Zn, Co, As)) |>
#           dplyr::mutate(compo = seq_along(compo) |> # nutrient excretion rate
#                           purrr::map(~ tibble::tibble(NRJ = gauss_mix(purrr::pluck(compo, ., "NRJ"), nsim), 
#                                                       N = gauss_mix(purrr::pluck(compo, ., "N"), nsim),
#                                                       P = gauss_mix(purrr::pluck(compo, ., "P"), nsim),
#                                                       Fe = gauss_mix(purrr::pluck(compo, ., "Fe"), nsim),
#                                                       Cu = gauss_mix(purrr::pluck(compo, ., "Cu"), nsim),
#                                                       Mn = gauss_mix(purrr::pluck(compo, ., "Mn"), nsim),
#                                                       Se = gauss_mix(purrr::pluck(compo, ., "Se"), nsim),
#                                                       Zn = gauss_mix(purrr::pluck(compo, ., "Zn"), nsim),
#                                                       Co = gauss_mix(purrr::pluck(compo, ., "Co"), nsim),
#                                                       As = gauss_mix(purrr::pluck(compo, ., "As"), nsim)
#                           ))), 
#         compo_tib |> 
#           dplyr::group_by(Prey_group) |>
#           dplyr::filter(Prey_group =="Zooplankton") |>
#           tidyr::nest(compo = c(NRJ, N, P, Fe, Cu, Mn, Se, Zn, Co, As)) |>
#           dplyr::mutate(compo = seq_along(compo) |> # nutrient excretion rate
#                           purrr::map(~ tibble::tibble(NRJ = rnorm(n = nsim, 
#                                                                   mean = purrr::pluck(compo, ., "NRJ", 1), 
#                                                                   sd = 0.2*purrr::pluck(compo, ., "NRJ", 1)), 
#                                                       N = rnorm(n = nsim, 
#                                                                 mean = purrr::pluck(compo, ., "N", 1), 
#                                                                 sd = 0.2*purrr::pluck(compo, ., "N", 1)),
#                                                       P = rnorm(n = nsim, 
#                                                                 mean = purrr::pluck(compo, ., "P", 1), 
#                                                                 sd = 0.2*purrr::pluck(compo, ., "P", 1)),
#                                                       Fe = rnorm(n = nsim, 
#                                                                  mean = purrr::pluck(compo, ., "Fe", 1), 
#                                                                  sd = 0.4*purrr::pluck(compo, ., "Fe", 1)),
#                                                       Cu = rnorm(n = nsim, 
#                                                                  mean = purrr::pluck(compo, ., "Cu", 1), 
#                                                                  sd = 0.4*purrr::pluck(compo, ., "Cu", 1)),
#                                                       Mn = rnorm(n = nsim, 
#                                                                  mean = purrr::pluck(compo, ., "Mn", 1), 
#                                                                  sd = 0.4*purrr::pluck(compo, ., "Mn", 1)),
#                                                       Se = rnorm(n = nsim, 
#                                                                  mean = purrr::pluck(compo, ., "Se", 1), 
#                                                                  sd = 0.4*purrr::pluck(compo, ., "Se", 1)),
#                                                       Zn = rnorm(n = nsim, 
#                                                                  mean = purrr::pluck(compo, ., "Zn", 1), 
#                                                                  sd = 0.4*purrr::pluck(compo, ., "Zn", 1)),
#                                                       Co = rnorm(n = nsim, 
#                                                                  mean = purrr::pluck(compo, ., "Co", 1), 
#                                                                  sd = 0.4*purrr::pluck(compo, ., "Co", 1)),
#                                                       As = rnorm(n = nsim, 
#                                                                  mean = purrr::pluck(compo, ., "As", 1), 
#                                                                  sd = 0.4*purrr::pluck(compo, ., "As", 1))
#                           )))) |>
#     tidyr::unnest(compo)
#   
#   
# }

########################################################################
################# kernel inversion - first way #########################
## we get a warning with this one 

# function to perform kernel based inversion using Nicolas Bousquet (Sorbonnes University) procedure 
boot_kernel_inv_1st <- function(compo_tib, x, nutrient, nsim, kernel="gaussian") {
  nut_all <- compo_tib |>
    dplyr::ungroup() |>
    dplyr::select(nutrient)
  
  # approche par noyaux ===========================================
  min = floor(min(nut_all, na.rm = TRUE))
  max = ceiling(max(nut_all, na.rm = TRUE))
  
  dens.old <- density(x,from=min,to=max,kernel=kernel) 
  dens = kdensity::kdensity(x, support=c(min,max), kernel = kernel) # on utilise des noyaux gaussiens par defaut
  
  xo=c(0.9*min(x),1.1*max(x))
  yo=c(0,1.2*max(dens.old$y))
  
  #==================== GENERATION BOOTSTRAP =================================
  #==================== GENERATION PAR INVERSION KERNEL-BASED ===================
  # construction "lisse" de la fonction de repartition
  cdf.lisse <- function(y)
  {
    integrate(function(x) dens(x), lower = min, upper = y)$value
  }	 
  # inverse "lisse" de la fonction de repartition
  # attention y est ici une valeur scalaire dans [0,1]
  inv.cdf.lisse <- function(y)
  {
    f.to.min <- function(x){abs(cdf.lisse(x)-y)}
    optimize(f.to.min,interval=c(min(xo),max(xo)))$minimum
  }
  
  # generation iid (on ne resimule pas dans les donnees mais dans la distribution)
  gen.sample.kernel = replicate(nsim,inv.cdf.lisse(runif(1)))	 # long à tourner
  
  gen.sample.kernel
}

#'
#'
#'
#'
#'
# function to bootstrap the composition of each prey group : just simple bootstrap (first used) - NOP
#
bootstrap_compo_pg <- function(compo_tib, nsim) {

  compo_tib |>
    dplyr::filter(!is.na(Prey_group)) |> # should be just one sample jellyfish
    dplyr::group_by(Prey_group) |>
    # artificially generate nsim sample per prey group
    dplyr::slice_sample(n = nsim, replace = TRUE) |>
    # replace NA values for N concentration by the mean of associated prey group
    dplyr::mutate(N = dplyr::case_when(is.na(N) ~ mean(N, na.rm = TRUE),
                                       TRUE ~ N)) |>
    # and get rid of useless columns
    dplyr::select(-c(Sp_prey, Genus, Family, Order, Taxa, Habitat)) |>
    # perform the bootstrapping
    dplyr::mutate(NRJ = dplyr::case_when(Prey_group != "Zooplankton" ~ boot_kernel_inv_1st(compo_tib, NRJ, "NRJ", nsim),
                                         TRUE ~ rnorm(n = nsim,
                                                      mean = mean(NRJ),
                                                      sd = 0.15*mean(NRJ))),
                  N = dplyr::case_when(Prey_group != "Zooplankton" ~ boot_kernel_inv_1st(compo_tib, N, "N", nsim),
                                       TRUE ~ rnorm(n = nsim,
                                                    mean = mean(N),
                                                    sd = 0.15*mean(N))),
                  P = dplyr::case_when(Prey_group != "Zooplankton" ~ boot_kernel_inv_1st(compo_tib, P, "P", nsim),
                                       TRUE ~ rnorm(n = nsim,
                                                    mean = mean(P),
                                                    sd = 0.15*mean(P))),
                  Fe = dplyr::case_when(Prey_group != "Zooplankton" ~ boot_kernel_inv_1st(compo_tib, Fe, "Fe", nsim),
                                        TRUE ~ rnorm(n = nsim,
                                                     mean = mean(Fe),
                                                     sd = 0.15*mean(Fe))),
                  Cu = dplyr::case_when(Prey_group != "Zooplankton" ~ boot_kernel_inv_1st(compo_tib, Cu, "Cu", nsim),
                                        TRUE ~ rnorm(n = nsim,
                                                     mean = mean(Cu),
                                                     sd = 0.15*mean(Cu))),
                  Mn = dplyr::case_when(Prey_group != "Zooplankton" ~ boot_kernel_inv_1st(compo_tib, Mn, "Mn", nsim),
                                        TRUE ~ rnorm(n = nsim,
                                                     mean = mean(Mn),
                                                     sd = 0.15*mean(Mn))),
                  Se = dplyr::case_when(Prey_group != "Zooplankton" ~ boot_kernel_inv_1st(compo_tib, Se, "Se", nsim),
                                        TRUE ~ rnorm(n = nsim,
                                                     mean = mean(Se),
                                                     sd = 0.15*mean(Se))),
                  Zn = dplyr::case_when(Prey_group != "Zooplankton" ~ boot_kernel_inv_1st(compo_tib, Zn, "Zn", nsim),
                                        TRUE ~ rnorm(n = nsim,
                                                     mean = mean(Zn),
                                                     sd = 0.15*mean(Zn))),
                  Co = dplyr::case_when(Prey_group != "Zooplankton" ~ boot_kernel_inv_1st(compo_tib, Co, "Co", nsim),
                                        TRUE ~ rnorm(n = nsim,
                                                     mean = mean(Co),
                                                     sd = 0.15*mean(Co))),
                  As = dplyr::case_when(Prey_group != "Zooplankton" ~ boot_kernel_inv_1st(compo_tib, As, "As", nsim),
                                        TRUE ~ rnorm(n = nsim,
                                                     mean = mean(As),
                                                     sd = 0.15*mean(As))
                  ))


}


###############################################################################
##################### compute nutrient content of diet ########################
###############################################################################

# using the bootstrap table of composition of preys and diet description 

compute_nut_in_diet <- function(diet_tib, compo_tib_boot) {
  
  diet_tib |>
    # 1 - join the nut concentration bootstrapped tables per prey_group to the %W of each prey
    dplyr::mutate(Nut_W = seq_along(Diet) |>
                    purrr::map(~ purrr::pluck(Diet, .) |>
                                 tidyr::pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'),
                                                     names_to = "Prey_group",
                                                     values_to = "W") |>
                                 # add column with %W in diet associated to each prey_group, for each pred (ie.line)
                                 dplyr::left_join(y = compo_tib_boot,
                                                  by = "Prey_group")
                    )) |>
    # 2 - compute W*elemental concentration
    dplyr::mutate(
      Nut_W = seq_along(Nut_W) |>
        purrr::map(~ purrr::pluck(Nut_W, .) |>
                     dplyr::mutate(NRJ = NRJ*(W/100), 
                                   N = N*(W/100),
                                   P = P*(W/100),
                                   Fe = Fe*(W/100),
                                   Se = Se*(W/100),
                                   Cu = Cu*(W/100),
                                   Zn = Zn*(W/100),
                                   Mn = Mn*(W/100),
                                   Co = Co*(W/100),
                                   As = As*(W/100)) |>
                     # change it to get one column per prey_group
                     # and one line, each cell containing a full bootstrap tibble 
                     # of elemental concentration, size nsim*nelements
                     dplyr::select(-c(W)) |>
                     tidyr::nest(Nut = c("NRJ":"As")) |>
                     tidyr::pivot_wider(names_from = Prey_group, 
                                        values_from = Nut)),
      # 3 - compute the mean concentration of diet by summing these values across prey_groups
      Nut_diet = seq_along(Nut_W) |>
        purrr::map(~ purrr::pluck(Nut_W, ., 1, 1) +
                     purrr::pluck(Nut_W, ., 2, 1) +
                     purrr::pluck(Nut_W, ., 3, 1) +
                     purrr::pluck(Nut_W, ., 4, 1) +
                     purrr::pluck(Nut_W, ., 5, 1) +
                     purrr::pluck(Nut_W, ., 6, 1) +
                     purrr::pluck(Nut_W, ., 7, 1) +
                     purrr::pluck(Nut_W, ., 8, 1) +
                     purrr::pluck(Nut_W, ., 9, 1) +
                     purrr::pluck(Nut_W, ., 10, 1) +
                     purrr::pluck(Nut_W, ., 11, 1) +
                     purrr::pluck(Nut_W, ., 12, 1) +
                     purrr::pluck(Nut_W, ., 13, 1) ), 
      # NRJ should be a separated column as it will be used to compute the daily ration
      NRJ_diet = seq_along(Nut_diet) |>
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Nut_diet, ., "NRJ")*1e3)), # from kJ per g to kJ per kg
      # delete it from Nut_diet tibbles 
      Nut_diet = seq_along(Nut_diet) |>
        purrr::map(~ purrr::pluck(Nut_diet, .) |>
                     dplyr::select(-NRJ))
    ) |>
    # delete now unused (and very heavy!) column
    dplyr::select(-c(Nut_W))
  
}


