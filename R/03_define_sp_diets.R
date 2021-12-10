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

########################### DEFINE DIETS #######################################
# very long function : 38 species each with oceanic and neritic diets (even
# though it is the same for most)

# when there was quantitative data on diets, diets was defined based on it (from
# the diet table), but when not rough definition from qualitative data or 
# replication of the diet of an ecologically close species 

create_diet_input <- function(diet_pg_tib) {
  # species per species... 
  
  rbind(
    ##################### Balaenoptera acutus - Minke whale ####################
    #Bala_acu neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Bala_acu") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Bala_acu oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Bala_acu") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    
    ##################### Berardius bairdii - Baird's beaked whale ####################
    # Bera_bai neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Bera_bai") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Bera_bai oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Bera_bai") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Balaenoptera bonaerensis - Antarctic Minke whale ####################
    # no quantitative data for this species 
    # Bala_bon neritic 
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Bala_bon", "Balaenoptera bonaerensis", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 2.5, 
                    `Miscellanous benthodemersal fish` = 2.5,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 95, 
                    Sources = "Tamura & Kenji 2009,
                              Friedlaender et al 2014") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Bala_bon oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Bala_bon", "Balaenoptera bonaerensis", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 2.5, 
                    `Miscellanous benthodemersal fish` = 2.5,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 95, 
                    Sources = "Tamura & Kenji 2009,
                              Friedlaender et al 2014") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Balaenoptera borealis - Sei whale ####################
    # no quantitative data for this species
    # but from qualitative data it eats 100 % zooplankton 
    # Bala_bor neritic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Bala_bor", "Balaenoptera borealis", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 0, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 100, 
                    Sources = "Kawamura 1969, Nishimoto et al 1952,
                              Watkins & Schevill 1979, Flinn et al 2002,
                              Leonardi et al 2011, Horwood 2018") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Bala_bor oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Bala_bor", "Balaenoptera borealis", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 0, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 100, 
                    Sources = "Kawamura 1969, Nishimoto et al 1952,
                              Watkins & Schevill 1979, Flinn et al 2002,
                              Leonardi et al 2011, Horwood 2018") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Balaenoptera edeni - Bryde's whale ####################
    # no quantitative data for this species
    # but from qualitative data we can say it eats mostly small schooling nrj-rich fish with a bit of zooplankton 
    # Bala_ede neritic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Bala_ede", "Balaenoptera edeni", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 90, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 10, 
                    Sources = "Siciliano et al 2004, Tershy et al 1992") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Bala_ede oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Bala_ede", "Balaenoptera edeni", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 90, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 10, 
                    Sources = "Siciliano et al 2004, Tershy et al 1992") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Balaenoptera musculus - Blue whale ####################
    # no quantitative data for this species
    # but from qualitative data we can say it eats 100 % zooplankton 
    # Bala_mus neritic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Bala_mus", "Balaenoptera musculus", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 0, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 100, 
                    Sources = "Figueiredo et al 2014, Gavrilchuck et al 2014,
                              Lesage et al 2018") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Bala_mus oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Bala_mus", "Balaenoptera musculus", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 0, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 100, 
                    Sources = "Figueiredo et al 2014, Gavrilchuck et al 2014,
                              Lesage et al 2018") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Balaenoptera omurai - Omurai's whale ####################
    # no quantitative data for this species
    # but from qualitative data we can say it eats 100 % zooplankton 
    # Bala_omu neritic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Bala_omu", "Balaenoptera omurai", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 0, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 100, 
                    Sources = "Cerchio et al 2015,
                              Cerchio & Tadasu 2018, Laboute & Borsa 2018") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Bala_omu oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Bala_omu", "Balaenoptera omurai", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 0, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 100, 
                    Sources = "Cerchio et al 2015,
                              Cerchio & Tadasu 2018, Laboute & Borsa 2018") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Balaenoptera physalus - Fin whale ####################
    # Bala_phy neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Bala_phy") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Bala_phy oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Bala_phy") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Delphinus capensis - long beaked common dolphin ####################
    # Del_cap neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Delp_cap") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # # Del_cap oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Delp_cap") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Delphinus delphis - Short-beaked common dolphin ####################
    # Delp_del neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Delp_del", Block %in% c("Alboran", "A", "B", "Aegean")) |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Delp_del oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Delp_del", Block %in% c("E1")) |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Eubalaena glacialis - North Atlantic Right whale ####################
    # no quantitative data for this species
    # but from qualitative data we can say it eats 100 % zooplankton 
    # Euba_gla neritic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Euba_gla", "Eubalaena glacialis", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 0, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 100, 
                    Sources = "Watkins & Schevill 1979,
                              Kenney 2009, Pendleton et al 2012") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Euba_gla oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Euba_gla", "Eubalaena glacialis", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 0, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 100, 
                    Sources = "Watkins & Schevill 1979,
                              Kenney 2009, Pendleton et al 2012") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Feresa attenuata - pygmy killer whales ####################
    # no quantitative data for this species
    # Qualitative data is also really scarce, really qualitative and for strandings or isotope it is just for at most 2 individuals
    # Suggests it has a diet with mostly squid, having a teutophageous isotopic signature, but also fish are found
    ### FOR THE MOMENT, we'll use the mean diet of Glob_melas
    # Fere_att neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Glob_mel") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = paste(c("[Fere_att] L?pez-Su?rez et al 2012, O'Dwyer et al 2015, 
                                                            Elorriaga-Verplancken et al 2016, Zerbini & Santos 1997,
                                                            Aguiar dos Santos & Haimovici 2001, Donahue & Perryman 2009, 
                                                            Baird 2018, Sekiguchi et al 1992, [Glob_mel]"), 
                                       stringr::str_c(Source, collapse = ", "), 
                                       sep = " ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf", 
                    Code_sp = "Fere_att", 
                    Species = "Feresa attenuata") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Fere_att oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Glob_mel") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = paste(c("[Fere_att] L?pez-Su?rez et al 2012, O'Dwyer et al 2015, 
                                                            Elorriaga-Verplancken et al 2016, Zerbini & Santos 1997,
                                                            Aguiar dos Santos & Haimovici 2001, Donahue & Perryman 2009, 
                                                            Baird 2018, Sekiguchi et al 1992, [Glob_mel]"), 
                                       stringr::str_c(Source, collapse = ", "), 
                                       sep = " ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic", 
                    Code_sp = "Fere_att", 
                    Species = "Feresa attenuata") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Globicephala macrorhynchus - short-finned pilot whales ####################
    # There is no quantitative data for this species
    # qualitative data suggest a teutophageous diet with mostly pelagic cephalopods
    # and a little bit a miscellanous fish
    # Glob mac neritic 
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area,
                    "Glob_mac", "Globicephala macrorhynchus", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0,
                    `Large demersal energy-rich fish` = 0,
                    `Small schooling energy-lean fish` = 0,
                    `Small schooling energy-rich fish` = 0,
                    `Miscellanous benthodemersal fish` = 2,
                    `Miscellanous pelagic fish` = 0,
                    `Muscular pelagic cephalopods` = 49,
                    `Gelatinous pelagic cephalopods` = 49,
                    `Bottom cephalopods` = 0,
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0,
                    `Crustaceans` = 0,
                    `Zooplankton` = 0,
                    Sources = "Hern?ndez-Garc?a et al 1994, Hacker 1986,
                             Mintzer et al 2008, Fernandez et al 2009") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Glob mac oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area,
                    "Glob_mac", "Globicephala macrorhynchus", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0,
                    `Large demersal energy-rich fish` = 0,
                    `Small schooling energy-lean fish` = 0,
                    `Small schooling energy-rich fish` = 0,
                    `Miscellanous benthodemersal fish` = 2,
                    `Miscellanous pelagic fish` = 0,
                    `Muscular pelagic cephalopods` = 49,
                    `Gelatinous pelagic cephalopods` = 49,
                    `Bottom cephalopods` = 0,
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0,
                    `Crustaceans` = 0,
                    `Zooplankton` = 0,
                    Sources = "Hern?ndez-Garc?a et al 1994, Hacker 1986,
                             Mintzer et al 2008, Fernandez et al 2009") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Globicephala melas - long-finned pilot whales ####################
    # Glob_mel neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Glob_mel") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Glob_mel oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Glob_mel") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Grampus griseus - Risso's dolphin ####################
    # Gram_gri neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Gram_gri") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Gram_gri oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Gram_gri") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Hyperoodon ampullatus - Northern bottlenose whale ####################
    # Hype_amp neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Hype_amp") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Hype_amp oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Hype_amp") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    
    ##################### Indopacetus pacificus - Longman's beaked whale ####################
    # No quantitative data for this species, species is mostly unknown
    # but qualitive data suggest diet of cephalopods exclusively
    ### FOR THE MOMENT we'll use mean of Zyph_cav
    # Indo_pac neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Ziph_cav") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf", 
                    Species = "Indopacetus pacificus", 
                    Code_sp = "Indo_pac", 
                    Sources = c("Yatabe et al 2010")) |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Indo_pac oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Ziph_cav") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic", 
                    Species = "Indopacetus pacificus", 
                    Code_sp = "Indo_pac", 
                    Sources = c("Yatabe et al 2010")) |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Kogia species (K. sima & K. breviceps) - Dwarf and Pygmy sperm whale ####################
    # Kogi_spp neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Kogi_spp") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Kogi_spp oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Kogi_spp") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Lagenorhynchus acutus - Atlantic white-sided dolphin ####################
    # Lage_acu neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Lage_acu") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Lage_acu oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Lage_acu") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Lagenorhynchus albirostris - white-beaked dolphin ####################
    # Lage_alb neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Lage_alb") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Lage_alb oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Lage_alb") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Lissodelphis borealis - Northern right whale dolphins ####################
    # There is no quantitative data for this species
    # Approximate diet defined from qualitative data
    # Liss_bor neritic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area,
                    "Liss_bor", "Lissodelphis borealis", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 5,
                    `Large demersal energy-rich fish` = 0,
                    `Small schooling energy-lean fish` = 5,
                    `Small schooling energy-rich fish` = 40,
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0,
                    `Muscular pelagic cephalopods` = 25,
                    `Gelatinous pelagic cephalopods` = 25,
                    `Bottom cephalopods` = 0,
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0,
                    `Crustaceans` = 0,
                    `Zooplankton` = 0,
                    Sources = "Leatherwood & Walker 1979, Jefferson et al 1994, 
                             Lipsky & Brownell 2018") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Liss_bor oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area,
                    "Liss_bor", "Lissodelphis borealis", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 5,
                    `Large demersal energy-rich fish` = 0,
                    `Small schooling energy-lean fish` = 5,
                    `Small schooling energy-rich fish` = 40,
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0,
                    `Muscular pelagic cephalopods` = 25,
                    `Gelatinous pelagic cephalopods` = 25,
                    `Bottom cephalopods` = 0,
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0,
                    `Crustaceans` = 0,
                    `Zooplankton` = 0,
                    Sources = "Leatherwood & Walker 1979, Jefferson et al 1994, 
                             Lipsky & Brownell 2018") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Lagenodelphis hosei - Fraser's dolphin #################
    # quantitative data but n very very limited...
    # and not really in accordance with qualitative data 
    # qualitative data with just %n, %O but with higher n (27 & 37) and bycaught individuals suggest significant consumption of fish 
    # most common being small schooling nrj rich and small schooling nrj lean 
    # so we "cooked" a diet taking into account both quantitative and qualitative data 
    # Lage_hos neritic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area,
                    "Lage_hos", "Lagenodelphis hosei", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0,
                    `Large demersal energy-rich fish` = 0,
                    `Small schooling energy-lean fish` = 15,
                    `Small schooling energy-rich fish` = 15,
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0,
                    `Muscular pelagic cephalopods` = 30,
                    `Gelatinous pelagic cephalopods` = 40,
                    `Bottom cephalopods` = 0,
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0,
                    `Crustaceans` = 0,
                    `Zooplankton` = 0,
                    Sources = "Sekiguchi et al 1992, Fernandez et al 2009, 
                             Di Beneditto et al 2001, Dolar et al 2003, Wang et al 2012") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Lage_hos oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area,
                    "Lage_hos", "Lagenodelphis hosei", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0,
                    `Large demersal energy-rich fish` = 0,
                    `Small schooling energy-lean fish` = 15,
                    `Small schooling energy-rich fish` = 15,
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0,
                    `Muscular pelagic cephalopods` = 30,
                    `Gelatinous pelagic cephalopods` = 40,
                    `Bottom cephalopods` = 0,
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0,
                    `Crustaceans` = 0,
                    `Zooplankton` = 0,
                    Sources = "Sekiguchi et al 1992, Fernandez et al 2009, 
                              Di Beneditto et al 2001, Dolar et al 2003, Wang et al 2012") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ############ Lagenorhynchus obliquidens - Pacific white-sided dolphin #########
    # Lage_obl neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Lage_obl") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Lage_obl oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Lage_obl") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Mesospp ####################
    # Meso_spp neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur")) |>
      dplyr::mutate(Code_sp = dplyr::case_when(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur") ~ "Meso_spp", 
                                        TRUE ~ Code_sp), 
                    Species = dplyr::case_when(Species %in% c("Mesoplodon bidens", "Mesoplodon layardii", 
                                                       "Mesoplodon densirostris", 
                                                       "Mesoplodon mirus", "Mesoplodon europaeus") ~ "Mesoplodon spp", 
                                        TRUE ~ Species)) |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Meso_spp oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur")) |>
      dplyr::mutate(Code_sp = dplyr::case_when(Code_sp %in% c("Meso_bid", "Meso_lay", "Meso_den", "Meso_mir", "Meso_eur") ~ "Meso_spp", 
                                        TRUE ~ Code_sp), 
                    Species = dplyr::case_when(Species %in% c("Mesoplodon bidens", "Mesoplodon layardii", "Mesoplodon densirostris", 
                                                       "Mesoplodon mirus", "Mesoplodon europaeus") ~ "Mesoplodon spp", 
                                        TRUE ~ Species)) |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ############### Megaptera novaeangliae - Humpback whale ####################
    # There is no quantitative data for this species
    # its diet may vary depending on areas, but globally they are known to feed on
    # small schooling nrj-rich fish and zooplankton
    # Mega_nov neritic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Mega_nov", "Megaptera novaeangliae", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 50, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 50, 
                    Sources = "Watkins & Schevill 1979, 
                              Witteven et al 2006, Filatova et al 2013,
                              Ryan et al 2014, Haro et al 2016,
                              Fleming et al 2016, Claham 2018"
      ) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Mega_nov oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Mega_nov", "Megaptera novaeangliae", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 0, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 50, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 0, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 50, 
                    Sources = "Watkins & Schevill 1979, 
                              Witteven et al 2006, Filatova et al 2013,
                              Ryan et al 2014, Haro et al 2016,
                              Fleming et al 2016, Claham 2018") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ############### Orcinus orca - Killer whale ####################
    # There is no quantitative data for this species
    # we'll consider here only fish eating eco types
    # qualitative data mention several prey species, real "cooking" here 
    # Orci_orc neritic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Orci_orc", "Orcinus orca", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 20, 
                    `Large demersal energy-rich fish` = 35, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 35, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 5, 
                    `Gelatinous pelagic cephalopods` = 5,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 0, 
                    Sources = "Simil? et al 1996, 
                              Saulitis et al 2000, Aguiar dos Santos & Haimovici 2001,
                              Volkova et al 2019"
      ) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Orci_orc oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Orci_orc", "Orcinus orca", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 20, 
                    `Large demersal energy-rich fish` = 35, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 35, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 0, 
                    `Muscular pelagic cephalopods` = 5, 
                    `Gelatinous pelagic cephalopods` = 5,
                    `Bottom cephalopods` = 0, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 0, 
                    Sources = "Simil? et al 1996, 
                              Saulitis et al 2000, Aguiar dos Santos & Haimovici 2001,
                              Volkova et al 2019") |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Pseudorca crassidens ####################
    # Pseu_cra neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Pseu_cra") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Pseu_cra oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Pseu_cra") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Phocoenoides dalli - Dall's porpoise ####################
    # Phoc_dal neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Phoc_dal") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Phoc_dal oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Phoc_dal") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Peponocephala electra - Melon-headed whale ####################
    # so there very few quantitative data for this one... but for the moment we'll keep this.....
    # Pepo_ele neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Pepo_ele") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Pepo_ele oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Pepo_ele") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Physeter macrocephalus - Sperm whale ####################
    # Phys_mac neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Phys_mac") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Phys_mac oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Phys_mac") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Phocoena phocoena - Harbour porpoise ####################
    # Phoc_pho neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Phoc_pho") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Phoc_pho oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Phoc_pho") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Stenella attenuata - Pantropical spotted dolphin ####################
    # Sten_att neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Sten_att") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Sten_att oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Sten_att") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ############### Steno bredanensis - Rough-toothed dolphin ####################
    # There is no quantitative data for this species 
    # well there is one quantitative study but with n=1 (Fernandez et al 2009)
    # so we cooked something from this and additional qualitative data consisting mostly in 
    # observations of feeding events
    ## this is really approximative cooking.... 
    # Sten_bre neritic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Sten_bre", "Steno bredanensis", "shelf") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 35, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 5, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 10, 
                    `Muscular pelagic cephalopods` = 25, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 25, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 0, 
                    Sources = "Lodi & Hetzel 1999, Di Beneditto et al 2001,
                             Aguiar dos Santos & Haimovici 2001, Pitman & Stinchcomb 2002, 
                             Wedekin et al 2005, Fernandez et al 2009, West et al 2011,
                             Ortega-Ortiz et al 2014"
      ) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources), 
    # Sten_bre oceanic
    tibble::tribble(~ Code_sp, ~ Species, ~ Eco_area, 
                    "Sten_bre", "Steno bredanensis", "oceanic") |>
      dplyr::mutate(`Large demersal energy-lean fish` = 0, 
                    `Large demersal energy-rich fish` = 35, 
                    `Small schooling energy-lean fish` = 0, 
                    `Small schooling energy-rich fish` = 5, 
                    `Miscellanous benthodemersal fish` = 0,
                    `Miscellanous pelagic fish` = 10, 
                    `Muscular pelagic cephalopods` = 25, 
                    `Gelatinous pelagic cephalopods` = 0,
                    `Bottom cephalopods` = 25, 
                    `Fish undetermined` = 0,
                    `Cephalopod undetermined` = 0, 
                    `Crustaceans` = 0, 
                    `Zooplankton` = 0, 
                    Sources = "Lodi & Hetzel 1999, Di Beneditto et al 2001,
                             Aguiar dos Santos & Haimovici 2001, Pitman & Stinchcomb 2002, 
                             Wedekin et al 2005, Fernandez et al 2009, West et al 2011,
                             Ortega-Ortiz et al 2014"
      ) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Stenella clymene - Clymene dolphin ####################
    # Sten_cly neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp %in% c("Sten_att", "Sten_coe", "Sten_fro")) |>
      dplyr::mutate(Code_sp = "Sten_cly", 
                    Species = "Stenella clymene") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarize(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Sten_cly oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp %in% c("Sten_att", "Sten_coe", "Sten_fro")) |>
      dplyr::mutate(Code_sp = "Sten_cly", 
                    Species = "Stenella clymene") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarize(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Stenella coeruleoalba - Striped dolphin ####################
    # Sten_coe neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Sten_coe", Block != "E1") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Sten_coe oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Sten_coe", Block == "E1") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ##################### Stenella frontalis - Atlantic spotted dolphin ####################
    # Sten_fro neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Sten_fro") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Sten_fro oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Sten_fro") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    
    ############### Stenella longirostris - long-beaked common whale ####################
    # Sten_att neritic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Sten_att") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = stringr::str_c(Source, collapse = ", ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "shelf", 
                    Code_sp = "Sten_lon", 
                    Species = "Stenella longirostris") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
    # Sten_att oceanic
    diet_pg_tib |>
      dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
      dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
      tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
      dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
      dplyr::filter(Code_sp == "Sten_att") |>
      dplyr::group_by(Code_sp, Species) |>
      dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                       `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                       `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                       `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                       `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                       `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                       `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                       `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                       `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                       `Fish undetermined` = mean(`Fish undetermined`),
                       `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                       `Crustaceans` = mean(`Crustaceans`), 
                       `Zooplankton` = mean(`Zooplankton`), 
                       Sources = paste(c("[Sten_long] Silva et al 2004, Salum Soud 2010, 
                                          Dolar et al 2003, Kiszka et al 2010, Clarke & Young 1998, 
                                          Gross et al 2009, [Sten_att]"), 
                                       stringr::str_c(Source, collapse = ", "), 
                                       sep = " ")) |>
      tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
      dplyr::mutate(Eco_area = "oceanic", 
                    Code_sp = "Sten_lon", 
                    Species = "Stenella longirostris") |> 
      dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
  
  ##################### Sousa plumbea - Indian ocean humpback dolphin ####################
  # no quantitative data available for this species
  # qualitative data suggest a very coastal behavior, a generalist diet and a small diet overlap with bottlenose dolphins 
  # FOR THE MOMENT, WE'LL USE THE DIET OF Turs_truC
  # Turs_tru neritic
  diet_pg_tib |>
    dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
    dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
    tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
    dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
    dplyr::filter(Code_sp == "Turs_tru") |>
    dplyr::group_by(Code_sp, Species) |>
    dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                     `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                     `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                     `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                     `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                     `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                     `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                     `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                     `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                     `Fish undetermined` = mean(`Fish undetermined`),
                     `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                     `Crustaceans` = mean(`Crustaceans`), 
                     `Zooplankton` = mean(`Zooplankton`), 
                     Sources = stringr::str_c(Source, collapse = ", ")) |>
    tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
    dplyr::mutate(Eco_area = "shelf", 
                  Code_sp = "Sous_plu", 
                  Species = "Sousa plumbea", 
                  Sources = "Diet of Tursiops truncatus, in accordance to qualitative data for Sten_plub") |> 
    dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
  # Turs_tru oceanic
  diet_pg_tib |>
    dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
    dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
    tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
    dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
    dplyr::filter(Code_sp == "Turs_tru") |>
    dplyr::group_by(Code_sp, Species) |>
    dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                     `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                     `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                     `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                     `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                     `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                     `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                     `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                     `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                     `Fish undetermined` = mean(`Fish undetermined`),
                     `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                     `Crustaceans` = mean(`Crustaceans`), 
                     `Zooplankton` = mean(`Zooplankton`), 
                     Sources = stringr::str_c(Source, collapse = ", ")) |>
    tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
    dplyr::mutate(Eco_area = "oceanic", 
                  Code_sp = "Sous_plu", 
                  Species = "Sousa plumbea", 
                  Sources = "Diet of Tursiops truncatus, in accordance to qualitative data for Sten_plub") |> 
    dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
  
  ##################### Sotalia guianensis - Sotalia dolphin ####################
  # Sota_gui neritic
  diet_pg_tib |>
    dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
    dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
    tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
    dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
    dplyr::filter(Code_sp == "Sota_gui") |>
    dplyr::group_by(Code_sp, Species) |>
    dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                     `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                     `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                     `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                     `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                     `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                     `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                     `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                     `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                     `Fish undetermined` = mean(`Fish undetermined`),
                     `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                     `Crustaceans` = mean(`Crustaceans`), 
                     `Zooplankton` = mean(`Zooplankton`), 
                     Sources = stringr::str_c(Source, collapse = ", ")) |>
    tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
    dplyr::mutate(Eco_area = "shelf") |> 
    dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
  # Sota_gui oceanic
  diet_pg_tib |>
    dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
    dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
    tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
    dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
    dplyr::filter(Code_sp == "Sota_gui") |>
    dplyr::group_by(Code_sp, Species) |>
    dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                     `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                     `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                     `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                     `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                     `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                     `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                     `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                     `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                     `Fish undetermined` = mean(`Fish undetermined`),
                     `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                     `Crustaceans` = mean(`Crustaceans`), 
                     `Zooplankton` = mean(`Zooplankton`), 
                     Sources = stringr::str_c(Source, collapse = ", ")) |>
    tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
    dplyr::mutate(Eco_area = "oceanic") |> 
    dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
  
  ##################### Tursiops truncatus - Common bottlenose dolphin ####################
  # Turs_tru neritic
  diet_pg_tib |>
    dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
    dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
    tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
    dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
    dplyr::filter(Code_sp == "Turs_tru", Source != "Louis et al 2014") |>
    dplyr::group_by(Code_sp, Species) |>
    dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                     `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                     `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                     `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                     `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                     `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                     `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                     `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                     `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                     `Fish undetermined` = mean(`Fish undetermined`),
                     `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                     `Crustaceans` = mean(`Crustaceans`), 
                     `Zooplankton` = mean(`Zooplankton`), 
                     Sources = stringr::str_c(Source, collapse = ", ")) |>
    tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
    dplyr::mutate(Eco_area = "shelf") |> 
    dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
  # Turs_tru oceanic
  diet_pg_tib |>
    dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
    dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
    tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
    dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
    dplyr::filter(Code_sp == "Turs_tru", Source == "Louis et al 2014") |>
    dplyr::group_by(Code_sp, Species) |>
    dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                     `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                     `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                     `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                     `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                     `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                     `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                     `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                     `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                     `Fish undetermined` = mean(`Fish undetermined`),
                     `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                     `Crustaceans` = mean(`Crustaceans`), 
                     `Zooplankton` = mean(`Zooplankton`), 
                     Sources = stringr::str_c(Source, collapse = ", ")) |>
    tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
    dplyr::mutate(Eco_area = "oceanic") |> 
    dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
  
  ##################### Ziphius cavirostris - Cuvier's beaked whale ####################
  # Ziph_cav neritic
  diet_pg_tib |>
    dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
    dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
    tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
    dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
    dplyr::filter(Code_sp == "Ziph_cav") |>
    dplyr::group_by(Code_sp, Species) |>
    dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                     `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                     `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                     `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                     `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                     `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                     `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                     `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                     `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                     `Fish undetermined` = mean(`Fish undetermined`),
                     `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                     `Crustaceans` = mean(`Crustaceans`), 
                     `Zooplankton` = mean(`Zooplankton`), 
                     Sources = stringr::str_c(Source, collapse = ", ")) |>
    tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
    dplyr::mutate(Eco_area = "shelf") |> 
    dplyr::select(Code_sp, Species, Eco_area, Diet, Sources),
  # Ziph_cav oceanic
  diet_pg_tib |>
    dplyr::group_by(Code_sp, Species, Block, Source, Prey_group) |>
    dplyr::summarise(W_prey_group = sum(W)) |> # compute %W per prey group, for each species in each Block
    tidyr::pivot_wider(names_from = Prey_group, values_from = W_prey_group) |> # format to get a value for each prey group
    dplyr::mutate_all(~ tidyr::replace_na(., 0)) |> # replace NA with zeros
    dplyr::filter(Code_sp == "Ziph_cav") |>
    dplyr::group_by(Code_sp, Species) |>
    dplyr::summarise(`Large demersal energy-lean fish` = mean(`Large demersal energy-lean fish`), 
                     `Large demersal energy-rich fish` = mean(`Large demersal energy-rich fish`), 
                     `Small schooling energy-lean fish` = mean(`Small schooling energy-lean fish`), 
                     `Small schooling energy-rich fish` = mean(`Small schooling energy-rich fish`), 
                     `Miscellanous benthodemersal fish` = mean(`Miscellanous benthodemersal fish`),
                     `Miscellanous pelagic fish` = mean(`Miscellanous pelagic fish`), 
                     `Muscular pelagic cephalopods` = mean(`Muscular pelagic cephalopods`), 
                     `Gelatinous pelagic cephalopods` = mean(`Gelatinous pelagic cephalopods`),
                     `Bottom cephalopods` = mean(`Bottom cephalopods`), 
                     `Fish undetermined` = mean(`Fish undetermined`),
                     `Cephalopod undetermined` = mean(`Cephalopod undetermined`), 
                     `Crustaceans` = mean(`Crustaceans`), 
                     `Zooplankton` = mean(`Zooplankton`), 
                     Sources = stringr::str_c(Source, collapse = ", ")) |>
    tidyr::nest(Diet = c(`Large demersal energy-lean fish`:`Zooplankton`)) |>
    dplyr::mutate(Eco_area = "oceanic") |> 
    dplyr::select(Code_sp, Species, Eco_area, Diet, Sources)
  ) # close rbind

}
