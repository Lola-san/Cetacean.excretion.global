################################################################################
# Cetacean.excretion.global project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# December 2021
# 03_define_sp_diets.R
#
# Script with all functions defining diets for each species of cetacean 
# included in the analysis
################################################################################


############################# load data ########################################
# not needed: use function load_xl defined in script 00_ratio_abundances.R

############################# Data wrangling ###################################
# join diet table with prey groups to have prey groups corresponding to 
# prey species 
join_clean_diet_tib <- function(diet_tib, preygps_tib) {
    diet_tib |>
    dplyr::left_join(preygps_tib, key = c("Sp_prey", "Genus", 
                                    "Family", "Order", "Taxa"), 
                     keep = FALSE) |>
    # %W is in character, change it to numeric
    dplyr::mutate(W = as.numeric(W)) |>
    # drop lines where W is NA or zero
    tidyr::drop_na(W) |>
    dplyr::filter(W != 0) |>
    # set sum for each species/Block to 100 (sometimes below) 
    dplyr::group_by(Species, Block, Source) |>
    dplyr::mutate(W = (100*W)/sum(W)) |>
    dplyr::arrange(Code_sp, Block)
}

