##--------------------------------------------------------------------------------------------------------
## SCRIPT : Compute mean nutrient contents of cetaceans diets
##
## Authors : Lola Gilbert 
## Last update : 2021-12
## R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
##--------------------------------------------------------------------------------------------------------

lapply(c("readxl", "tidyverse", "wesanderson"), library, character.only=TRUE)



# increase memory in R 
memory.limit(size=250000)

### clean up
rm(list = ls())
wd <- "C:/Users/lgilbe01/Desktop/PhD_2020-2023/Analyses/01.Excretion_global" # 
Datawd <- paste(wd, "data", sep = "/") # 
Outwd <- paste(wd, "output", sep = "/") # 

#### load data
# df with diets for each species/Block
load(paste0(Datawd, "/model/Diet_pred_20211008.Rdata"))
summary(diets_pred)


# df with each prey species and corresponding prey groups 
sp_PG <- read_excel(paste(Datawd, 
                          "/diets_pred/Sp_preys_PG_js_lg_20210706.xlsx", 
                          sep = "/"), sheet = 1)
summary(sp_PG)

# df with composition data for prey species 
nutri_df <- read_excel(paste(Datawd, 
                             "/nutrient_prey/Nuts_in_preys_full_corrected.xlsx", 
                             sep = "/"), sheet = 1)
summary(nutri_df)

# change outlier value to quantile 99% values 
nutri_df <- nutri_df %>%
  mutate(Se = case_when(Sp_prey == "Hyperoplus lanceolatus" ~ 2.95, 
                        TRUE ~ Se))

######################################################################################################
################### 1 - Attribution of prey groups to prey species in  nutri_df ######################
######################################################################################################

nutri_df <- nutri_df %>%
  left_join(sp_PG, key = c("Sp_prey", "Genus", "Family", "Order", "Taxa"), keep = FALSE) %>% 
  relocate(Prey_group, .before = Habitat)

rm(sp_PG)

# try <- nutri_df %>%
#   group_by(Sp_prey, Habitat) %>%
#   dplyr::summarise(meanN = mean(N, na.rm = TRUE),
#             meanP = mean(P, na.rm = TRUE),
#             meanFe = mean(Fe, na.rm = TRUE))

# get rid of elements of no interest here, ie non essential ones 
nutri_df <- nutri_df %>%
  select(-c(Pb, Ag, Cd))

######################################################################################################
######## 2 - A bit of exploration : what variability there is in composition ? #######################
######################################################################################################
# 
# # Variability of composition amoung prey groups 
# 
# # visualization 
# ggplot(nutri_df %>%
#          pivot_longer(cols = c(N, P, Co, Cu, Fe, Mn, Se, Zn, As),
#                       names_to = "Element", 
#                       values_to = "Concentration") %>%
#          mutate(Prey_group = factor(Prey_group, 
#                                     levels = c("Small schooling energy-rich fish",
#                                                "Small schooling energy-lean fish", 
#                                                "Large demersal energy-rich fish", 
#                                                "Large demersal energy-lean fish", 
#                                                "Miscellanous pelagic fish", 
#                                                "Miscellanous benthodemersal fish", 
#                                                "Muscular pelagic cephalopods", 
#                                                "Bottom cephalopods", 
#                                                "Crustaceans", 
#                                                "Zooplankton", 
#                                                NA)), 
#                 Element = factor(Element, 
#                                  levels = c("N", "P", "Fe", "Cu", 
#                                             "Zn", "Mn", "Se", "Co", 
#                                             "As")))) +
#   geom_boxplot(aes(x = Prey_group, y = Concentration, fill = Prey_group)) +
#   geom_jitter(aes(x = Prey_group, y = Concentration, color = Prey_group)) +
#   scale_fill_manual(values = wes_palette("FantasticFox1", 
#                                          10, type = "continuous"), na.value = "grey") +
#   scale_color_manual(values = wes_palette("FantasticFox1", 
#                                          10, type = "continuous"), na.value = "grey") +
#   guides(fill = FALSE, color = FALSE) +
#   facet_wrap(~ Element, scales = "free") +
#   xlab("Prey group") +
#   ylab("Concentration (mg/kg") +
#   theme(strip.background = element_rect(colour = "black", fill = "grey", 
#                                         size = 1, linetype = "solid"),
#         panel.background = element_rect(colour = "black", fill = "white", 
#                                         size = 0.5, linetype = "solid"),
#         panel.grid = element_line(colour = "gray95", size = 0.5, linetype = "solid"),
#         strip.text.x = element_text(size = 12, color = "black", face = "bold"),
#         axis.text.x=element_text(face = "plain", color = "black", size = 12, 
#                                  angle = 50, hjust = 1),
#         axis.text.y = element_text(face = "plain", color = "black", size = 12),
#         axis.title.x = element_text(color = "black", size = 16, face = "bold"),
#         axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
#         legend.title = element_text(color = "black", size = 12, face = "bold"), 
#         legend.text = element_text(color = "black", size = 11)) 
# #ggsave("Variability_prey-groups_comp.jpg", scale = 1, height = 15, width = 10)  
#   
# ggplot(nutri_df %>%
#          pivot_longer(cols = c(N, P, Co, Cu, Fe, Mn, Se, Zn, As),
#                       names_to = "Element", 
#                       values_to = "Concentration") %>%
#          mutate(Prey_group = factor(Prey_group, 
#                                     levels = c("Small schooling energy-rich fish",
#                                                "Small schooling energy-lean fish", 
#                                                "Large demersal energy-rich fish", 
#                                                "Large demersal energy-lean fish", 
#                                                "Miscellanous pelagic fish", 
#                                                "Miscellanous benthodemersal fish", 
#                                                "Muscular pelagic cephalopods", 
#                                                "Bottom cephalopods", 
#                                                "Crustaceans", 
#                                                "Zooplankton", 
#                                                NA)), 
#                 Element = factor(Element, 
#                                  levels = c("N", "P", "Fe", "Cu", 
#                                             "Zn", "Mn", "Se", "Co", 
#                                             "As")))) +
#   geom_histogram(aes(x = Concentration, fill = Prey_group)) +
#   scale_fill_manual(values = wes_palette("FantasticFox1", 
#                                          10, type = "continuous"), na.value = "grey") +
#   facet_wrap(~ Element, scales = "free")
# #ggsave("Distribution_prey-groups_comp.jpg", scale = 1, height = 6, width = 8)  
# 
# 
# # summaries 
# summary_df <- nutri_df %>%
#   pivot_longer(cols = c(N, P, Co, Cu, Fe, Mn, Se, Zn, As),
#                names_to = "Element", 
#                values_to = "Concentration") %>%
#   mutate(Prey_group = factor(Prey_group, 
#                              levels = c("Small schooling energy-rich fish",
#                                         "Small schooling energy-lean fish", 
#                                         "Large demersal energy-rich fish", 
#                                         "Large demersal energy-lean fish", 
#                                         "Miscellanous pelagic fish", 
#                                         "Miscellanous benthodemersal fish", 
#                                         "Muscular pelagic cephalopods", 
#                                         "Bottom cephalopods", 
#                                         "Crustaceans", 
#                                         "Zooplankton", 
#                                         NA)), 
#          Element = factor(Element, 
#                           levels = c("N", "P", "Fe", "Cu", 
#                                      "Zn", "Mn", "Se", "Co", 
#                                      "As"))) %>%
#   group_by(Prey_group, Element) %>%
#   summarise(min = min(Concentration, na.rm = TRUE),
#             `qt0.05`= quantile(Concentration, 0.05, na.rm = TRUE), 
#             median = median(Concentration, na.rm = TRUE), 
#             mean = mean(Concentration, na.rm = TRUE), 
#             `qt0.95` = quantile(Concentration, 0.95, na.rm = TRUE), 
#             max = max(Concentration, na.rm = TRUE), 
#             sd = sd(Concentration, na.rm = TRUE)
#             )
# 
# 
# nutri_df %>%
#   pivot_longer(cols = c(N, P, Co, Cu, Fe, Mn, Se, Zn, As),
#                names_to = "Element", 
#                values_to = "Concentration") %>%
#   group_by(Element) %>%
#   summarise(min = min(Concentration, na.rm = TRUE),
#             `qt0.05`= quantile(Concentration, 0.05, na.rm = TRUE), 
#             median = median(Concentration, na.rm = TRUE), 
#             mean = mean(Concentration, na.rm = TRUE), 
#             `qt0.95` = quantile(Concentration, 0.95, na.rm = TRUE), 
#             max = max(Concentration, na.rm = TRUE), 
#             sd = sd(Concentration, na.rm = TRUE)
#   )
#   
# 
# ggplot(nutri_df %>%
#          pivot_longer(cols = c(N, P, Co, Cu, Fe, Mn, Se, Zn, As),
#                       names_to = "Element", 
#                       values_to = "Concentration") %>%
#          filter(Element == "Cu", Prey_group == "Small schooling energy-rich fish")
#          ) +
#   geom_histogram(aes(x = Concentration), bins = 100)
# 
# hist(rnorm(1e5, mean = 8.334090e-01, sd = 5.853834e-01))
# 
# nutri_df %>%
#   group_by(Prey_group) %>%
#   summarise(n = n())



######################################################################################################
####### 3a - Computation of mean nutrient and NRJ contents of diets of each pred in each Eco_area #####
######################################################################################################

# PG_means <- nutri_df %>% 
#   group_by(Prey_group) %>%
#   summarise(mean_NRJ = mean(NRJ),
#             mean_N = mean(N, na.rm = TRUE),
#             mean_P = mean(P),
#             mean_Ag = mean(Ag),
#             mean_As = mean(As),
#             mean_Cd = mean(Cd),
#             mean_Pb = mean(Pb),
#             mean_Co = mean(Co),
#             mean_Cu = mean(Cu),
#             mean_Fe = mean(Fe),
#             mean_Mn = mean(Mn),
#             mean_Se = mean(Se),
#             mean_Zn = mean(Zn)) %>%
#   filter(!is.na(Prey_group)) %>%
#   add_row()
# # There is no composition data for Gelatinous pelagic cephalopods,
# # so take values of Muscular pelagic cephalopods not to take those of all cephalopods
# PG_means[11, ] <- PG_means %>% 
#   filter(Prey_group == "Muscular pelagic cephalopods") %>%
#   mutate(Prey_group = "Gelatinous pelagic cephalopods")
# 
# # add "Fish undetermined" and "Cephalopod undetermined" categories 
# PG_means <- rbind(PG_means, 
#                   nutri_df %>%
#                     filter(Taxa == "Fish") %>%
#                     group_by(Taxa) %>%
#                     summarise(mean_NRJ = mean(NRJ),
#                               mean_N = mean(N, na.rm = TRUE),
#                               mean_P = mean(P),
#                               mean_Ag = mean(Ag),
#                               mean_As = mean(As),
#                               mean_Cd = mean(Cd),
#                               mean_Pb = mean(Pb),
#                               mean_Co = mean(Co),
#                               mean_Cu = mean(Cu),
#                               mean_Fe = mean(Fe),
#                               mean_Mn = mean(Mn),
#                               mean_Se = mean(Se),
#                               mean_Zn = mean(Zn)) %>%
#                     rename(Prey_group = Taxa) %>%
#                     mutate(Prey_group = "Fish undetermined"), 
#                   nutri_df %>%
#                     filter(Taxa == "Cephalopod") %>%
#                     group_by(Taxa) %>%
#                     summarise(mean_NRJ = mean(NRJ),
#                               mean_N = mean(N, na.rm = TRUE),
#                               mean_P = mean(P),
#                               mean_Ag = mean(Ag),
#                               mean_As = mean(As),
#                               mean_Cd = mean(Cd),
#                               mean_Pb = mean(Pb),
#                               mean_Co = mean(Co),
#                               mean_Cu = mean(Cu),
#                               mean_Fe = mean(Fe),
#                               mean_Mn = mean(Mn),
#                               mean_Se = mean(Se),
#                               mean_Zn = mean(Zn)) %>%
#                     rename(Prey_group = Taxa) %>%
#                     mutate(Prey_group = "Cephalopod undetermined")
#   )
# 
# ######################## computation 
# Diet_data <- diets_pred %>%
#   mutate(Diet_nested = Diet) %>%
#   unnest(Diet) %>%
#   pivot_longer(cols = c("Large demersal energy-lean fish":"Zooplankton"), 
#                names_to = "Prey_group", 
#                values_to = "W") %>%
#   left_join(PG_means, by = "Prey_group", keep = FALSE) %>%
#   group_by(Code_sp, Species, Eco_area, Sources, Diet_nested) %>%
#   summarise(NRJ_diet = sum(mean_NRJ*(W/100)*1e3), # from kJ per g to kJ per kg 
#             N = sum(mean_N*(W/100)), 
#             P = sum(mean_P*(W/100)), 
#             Ag = sum(mean_Ag*(W/100)),
#             As = sum(mean_As*(W/100)),
#             Cd = sum(mean_Cd*(W/100)),
#             Pb = sum(mean_Pb*(W/100)),
#             Co = sum(mean_Co*(W/100)),
#             Cu = sum(mean_Cu*(W/100)),
#             Fe = sum(mean_Fe*(W/100)),
#             Mn = sum(mean_Mn*(W/100)),
#             Se = sum(mean_Se*(W/100)),
#             Zn = sum(mean_Zn*(W/100))) %>%
#   nest(Nut_diet = c(N, P, Ag, As, Cd, Pb, Co, Cu, Fe, Mn, Se, Zn)) %>%
#   rename(Diet = Diet_nested)



######################################################################################################
######## 3b - New way : Bootstrapping of composition in each prey group  ##############################
######################################################################################################

# nb of bootstrap items - MUST BE THE SAME AS THE ONE FOR MONTE CARLO SIMULATIONS
nsim <- 1e5

# add Prey_groups artificially :
# - Gelatinous pelagic cephalopod with species of Muscular pelagic cephalopods
# - Fish undetermined with all species of fish
# - Cephalopod undetermined with all species of cephalopods 

nutri_df <- rbind(nutri_df, 
                  nutri_df %>%
                    filter(Prey_group == "Muscular pelagic cephalopods") %>%
                    mutate(Prey_group = "Gelatinous pelagic cephalopods"), 
                  nutri_df %>%
                    filter(Taxa  == "Fish") %>%
                    mutate(Prey_group = "Fish undetermined"), 
                  nutri_df %>%
                    filter(Taxa == "Cephalopod") %>%
                    mutate(Prey_group = "Cephalopod undetermined"))

# check 
nutri_df %>%
  group_by(Prey_group) %>%
  summarise(n = n())
# OK # remaining NA is jellyfish

mins_max <- nutri_df %>%
  summarise(minNRJ = min(NRJ), 
            minN = min(N, na.rm = TRUE),
            minP = min(P),
            minFe = min(Fe),
            minCu = min(Cu),
            minMn = min(Mn),
            minSe = min(Se),
            minZn = min(Zn),
            minCo = min(Co),
            minAs = min(As),
            maxNRJ = max(NRJ), 
            maxN = max(N, na.rm = TRUE),
            maxP = max(P),
            maxFe = max(Fe),
            maxCu = max(Cu),
            maxMn = max(Mn),
            maxSe = max(Se),
            maxZn = max(Zn),
            maxCo = max(Co),
            maxAs = max(As)
            )

variation <- nutri_df %>%
  group_by(Prey_group) %>%
  summarise(meanNRJ = mean(NRJ), 
            meanN = mean(N, na.rm = TRUE), 
            meanP = mean(P), 
            meanFe = mean(Fe), 
            meanCu = mean(Cu), 
            meanMn = mean(Mn), 
            meanSe = mean(Se), 
            meanZn = mean(Zn), 
            meanCo = mean(Co), 
            meanAs = mean(As),
            sdNRJ = sd(NRJ), 
            sdN = sd(N, na.rm = TRUE), 
            sdP = sd(P), 
            sdFe = sd(Fe), 
            sdCu = sd(Cu), 
            sdMn = sd(Mn), 
            sdSe = sd(Se), 
            sdZn = sd(Zn), 
            sdCo = sd(Co), 
            sdAs = sd(As)) |>
  dplyr::mutate(var_NRJ = sdNRJ/meanNRJ, 
                var_N = sdN/meanN,
                var_P = sdP/meanP,
                var_Fe = sdFe/meanFe,
                var_Cu = sdCu/meanCu,
                var_Mn = sdMn/meanMn,
                var_Se = sdSe/meanSe,
                var_Zn = sdZn/meanZn,
                var_Co = sdCo/meanCo,
                var_As = sdAs/meanAs) 

mean(variation$var_NRJ, na.rm = TRUE)
mean(variation$var_N, na.rm = TRUE)
mean(variation$var_P, na.rm = TRUE)
mean(variation$var_Fe, na.rm = TRUE)
mean(variation$var_Cu, na.rm = TRUE)
mean(variation$var_Mn, na.rm = TRUE)
mean(variation$var_Se, na.rm = TRUE)
mean(variation$var_Zn, na.rm = TRUE)
mean(variation$var_Co, na.rm = TRUE)
mean(variation$var_As, na.rm = TRUE)


nutri_df %>%
  group_by(Prey_group) %>%
  summarise(n = n(), 
            n_nrj = length(unique(NRJ)), 
            n_N = length(unique(N)),
            n_P = length(unique(P)), 
            n_Fe = length(unique(Fe)), 
            n_Cu = length(unique(Cu)))

pluck(diets_pred, "Diet", 1) 


# bootstrap : sample nsim species from each prey group
nutri_df <- nutri_df %>%
  filter(!is.na(Prey_group)) %>%
  group_by(Prey_group) %>%
  slice_sample(n = nsim, replace = TRUE)

# replace NA values for N concentration by the mean of associated prey group
# and get rid of useless columns
nutri_df <- nutri_df %>%
  group_by(Prey_group) %>%
  mutate(N = case_when(is.na(N) ~ mean(N, na.rm = TRUE),
                       TRUE ~ N)) %>%
  dplyr::select(-c(Sp_prey, Genus, Family, Order, Taxa, Habitat))

nutri_df %>%
  pivot_longer(cols = c(N:Zn), 
               names_to = "Element", 
               values_to = "Conc") %>%
  mutate(Prey_group = factor(Prey_group, 
                             levels = c("Bottom cephalopods", "Gelatinous pelagic cephalopods", "Muscular pelagic cephalopods", "Cephalopod undetermined", 
                                        "Large demersal energy-lean fish", "Large demersal energy-rich fish", "Small schooling energy-lean fish", "Small schooling energy-rich fish",
                                        "Miscellanous benthodemersal fish", "Miscellanous pelagic fish", "Fish undetermined","Zooplankton", "Crustaceans" ))) %>%
  group_by(Prey_group, Element) %>%
  summarise(min_conc = min(Conc), 
            `2.5_quant_conc` = quantile(Conc, probs = c(0.025)), 
            mean_conc = mean(Conc), 
            median_conc = median(Conc), 
            `97.5_quant_conc` = quantile(Conc, probs = c(0.975)), 
            max_conc = max(Conc)) %>%
  ggplot() +
  geom_errorbar(aes(x = Element, ymin = `2.5_quant_conc`, ymax = `97.5_quant_conc`, color = Prey_group), position = position_dodge(width = 0.9), size = 1) +
  geom_point(aes(x = Element, y = mean_conc, color = Prey_group), position = position_dodge(width = 0.9)) +
  scale_color_manual(values = wes_palette("FantasticFox1", 13, type = "continuous")) +
  #facet_wrap(~ Prey_group) +
  scale_y_continuous(trans = "log10")


# Full_diet <- diets_pred %>%
#   dplyr::mutate(Nut_PG = seq_along(Diet) %>%
#                   map(~ nutri_df),
#                 Diet = seq_along(Diet) %>%
#                   map(~ pluck(Diet, .) %>%
#                         pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'),
#                                      names_to = "Prey_group",
#                                      values_to = "W"))) %>%
#   # add column with %W in diet associated to each prey_group, for each pred (ie.line)
#   dplyr::mutate(Nut_PG = seq_along(Nut_PG) %>%
#                   map(~ pluck(Nut_PG, .) %>%
#                         left_join(pluck(Diet, .),
#                                   by = "Prey_group")
#                   )
#   )
# ##### THE SECOND MAP CAUSES AN UNRESOLVED ERROR
# # what's in works well independently :
# Full_diet <- diets_pred %>%
#   dplyr::mutate(Nut_PG = seq_along(Diet) %>%
#                   map(~ nutri_df),
#                 Diet = seq_along(Diet) %>%
#                   map(~ pluck(Diet, .) %>%
#                         pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'),
#                                      names_to = "Prey_group",
#                                      values_to = "W"))) 
# # and
# pluck(Full_diet, "Nut_PG", 5) %>%
#                  left_join(pluck(Full_diet, "Diet", 5),
#                            by = "Prey_group")
# # and this for all lines of Full_diet/diets_pred
# # So I can't seem to figure out why it doesn't work in this usual map syntax here 
# # I had to find another solution
# # which is not very elegant.... 

# I found a solution!
Full_diets <- diets_pred %>%
  dplyr::mutate(Nut_W = seq_along(Diet) %>%
                  map(~ pluck(Diet, .) %>%
                        pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'),
                                     names_to = "Prey_group",
                                     values_to = "W") %>%
                        # add column with %W in diet associated to each prey_group, for each pred (ie.line)
                        left_join(y = nutri_df,
                                  by = "Prey_group")
                  )
  )

pluck(Full_diets, "Nut_W", 1) %>%
  summarise(n = length(unique(NRJ)))

# # join the nut concentration bootstrapped tables per prey_group to the %W of each prey
# # group in the diet
# # the list will contain tibble associated to each predator/line in the final df 
# list_df_Nut_W <- list()
# 
# for (i in c(1:nrow(diets_pred))) {
#   list_df_Nut_W <- append(list_df_Nut_W, 
#                           list(nutri_df %>%
#                                  left_join(pluck(diets_pred, "Diet", i) %>%
#                                              pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'), 
#                                                           names_to = "Prey_group", 
#                                                           values_to = "W"), 
#                                            by = "Prey_group")))
# }
# 
# # define final diets df that will contain all diet info 
# Full_diets <- diets_pred
# Full_diets$Nut_W <- list_df_Nut_W

Full_diets <- Full_diets %>%
  mutate(# compute W*elemental concentration
    Nut_W = seq_along(Nut_W) %>%
      map(~ pluck(Nut_W, .) %>%
            mutate(NRJ = NRJ*(W/100), 
                   N = N*(W/100),
                   P = P*(W/100),
                   Fe = Fe*(W/100),
                   Se = Se*(W/100),
                   Cu = Cu*(W/100),
                   Zn = Zn*(W/100),
                   Mn = Mn*(W/100),
                   Co = Co*(W/100),
                   As = As*(W/100)) %>%
            # change it to get one column per prey_group
            # and one line, each cell containing a full bootstrap tibble 
            # of elemental concentration, size nsim*nelements
            select(-c(W)) %>%
            nest(Nut = c("NRJ":"Zn")) %>%
            pivot_wider(names_from = Prey_group, 
                        values_from = Nut)),
    # compute the mean concentration of diet by summing these values across prey_groups
    Nut_diet = seq_along(Nut_W) %>%
      map(~ pluck(Nut_W, ., 1, 1) +
            pluck(Nut_W, ., 2, 1) +
            pluck(Nut_W, ., 3, 1) +
            pluck(Nut_W, ., 4, 1) +
            pluck(Nut_W, ., 5, 1) +
            pluck(Nut_W, ., 6, 1) +
            pluck(Nut_W, ., 7, 1) +
            pluck(Nut_W, ., 8, 1) +
            pluck(Nut_W, ., 9, 1) +
            pluck(Nut_W, ., 10, 1) +
            pluck(Nut_W, ., 11, 1) +
            pluck(Nut_W, ., 12, 1) +
            pluck(Nut_W, ., 13, 1) ), 
    # NRJ should be a separated column as it will be used to compute the daily ration
    NRJ_diet = seq_along(Nut_diet) %>%
      map(~ as_tibble_col(pluck(Nut_diet, ., "NRJ")*1e3)), # from kJ per g to kJ per kg
    # delete it from Nut_diet tibbles 
    Nut_diet = seq_along(Nut_diet) %>%
      map(~ pluck(Nut_diet, .) %>%
            select(-NRJ))
  ) %>%
  select(-c(Nut_W))




