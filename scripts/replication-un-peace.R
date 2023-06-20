## replication file 1

library(here)
library(data.table)
library(tidyverse)
library(fixest)
library(statar)
# Load dataset of analysis
un <- fread(here("Artigos", "replication", "Peacekeeping", "dataverse_files", "1_replicationData.tab"))
glimpse(un)

#** Only keep countries and elections where are UN PKO is present
# Only keep years for which we have GEOPKO data

un1 <- un %>%
  filter( PKO_presNoPol==1) %>%
  filter(year>=1994 & year<=2017)

glimpse(un1)
#  ** Drop period around election month that is longer than 13 months
un1 <- un1 %>%
  filter(collapsed_time_si_el <= 6, collapsed_time_to_el <= 6)

# ** Create a new election_id that is the same for consecutive elections
un1 <- un1 %>%
  group_by(collapsed_election_id_spell) %>%
  mutate(election_id_new = cur_group_id()) %>%
  ungroup()

## Make data panel: By admin units - election spells - months
un1 <- un1 %>%
  mutate(adminUnitElection_id = paste(unique_id, election_id_new, sep= "" ),
         adminUnitElection_id = as.numeric(adminUnitElection_id))


# Create treatment variable for matching

un1 <- un1 %>%
  mutate(un_military_base_du = ifelse(un_military_base2 > 0 , 1 , un_military_base2))


# Create treatment variable for matching
# df$un_military_base_du <- ifelse(df$un_military_base2 > 0, df$un_military_base2, NA)
# df$un_military_base_du <- ifelse(!is.na(df$un_military_base_du), 1, 0)

# Create variables for violence trends
# Note: Positive means a previous DECREASE in violence; Negative means an INCREASE in electoral violence

un1 <- un1 %>%
  group_by(adminUnitElection_id) %>%
  arrange(adminUnitElection_id, date) %>%
  mutate(date = as.Date(date)) %>%
  mutate(L1.veco_viol = tlag(veco_viol, n = 1, date),
         L2.veco_viol = tlag(veco_viol, n = 2, date),
         L3.veco_viol =  tlag(veco_viol, n = 3, date), 
         veco_viol_trend1 = L1.veco_viol - (L2.veco_viol+L3.veco_viol)/2,
         veco_viol_trend2 = (L1.veco_viol+L2.veco_viol)/2 - (L3.veco_viol))

un1 %>%
  ungroup() %>%
  summarise(n(), n_distinct(adminUnitElection_id))

# Create variables for robustness tests
un1$ecav_event_pre_du <- ifelse(!is.na(un1$ecav_event_pre), 1, 0)
un1$ecav_event_post_du <- ifelse(!is.na(un1$ecav_event_post), 1, 0)
un1$ecav_anti_gov_du <- ifelse(un1$ecav_anti_gov > 0, 1, 0)
un1$ecav_pro_gov_du <- ifelse(un1$ecav_pro_gov > 0, 1, 0)

# Add controls
controls <- c("viol_stateBased_preDep", "viol_stateBased_preDep_trend", "viol_OneSided_preDep", "viol_OneSided_preDep_trend", "pop_gpw_sum", "un_military_base_du_spL")
controls_cem <- c("viol_stateBased_preDep", "viol_stateBased_preDep_trend", "viol_OneSided_preDep", "viol_OneSided_preDep_trend", "pop_gpw_sum", "roadDensity", "imr_mean", "bdist1", "capdist", "un_military_base_du_spL", "un_military_base_du2Lag1_std")

# Create sample of estimation with controls
set.seed(1234)
un1$logit <- ifelse(!is.na(un1$veco_viol_du), 1, 0)

set.seed(1234)

# Fit logistic regression with controls
model <- glm(veco_viol_du ~ un_military_base_du + 
               viol_stateBased_preDep + viol_stateBased_preDep_trend + 
               viol_OneSided_preDep + viol_OneSided_preDep_trend + 
               pop_gpw_sum + roadDensity + imr_mean + bdist1 + 
               capdist + un_military_base_du_spL, 
             data = data %>% filter(!is.na(veco_viol_du) & 
                                      !is.na(un_military_base_du) &
                                      !is.na(viol_stateBased_preDep) &
                                      !is.na(viol_stateBased_preDep_trend) &
                                      !is.na(viol_OneSided_preDep) &
                                      !is.na(viol_OneSided_preDep_trend) &
                                      !is.na(pop_gpw_sum) &
                                      !is.na(roadDensity) & 
                                      !is.na(imr_mean) &
                                      !is.na(bdist1) &
                                      !is.na(capdist) &
                                      !is.na(un_military_base_du_spL)),
             family = binomial(link = "logit"))

# Get number of events of electoral violence in DECO
un1 %>% filter(!is.na(veco_viol_du)) %>% 
  summarize(n_events = sum(veco_viol_du))

# Get number of events of electoral violence and contention in ECAV
un1 %>% filter(!is.na(ecav_event_du)) %>% 
  summarize(n_events = sum(ecav_event_du))

# Standardize control variables in the sample
std_vars <- un1 %>% filter(!is.na(veco_viol_du) & 
                              !is.na(un_military_base_du) &
                              !is.na(viol_stateBased_preDep) &
                              !is.na(viol_stateBased_preDep_trend) &
                              !is.na(viol_OneSided_preDep) &
                              !is.na(viol_OneSided_preDep_trend) &
                              !is.na(pop_gpw_sum) &
                              !is.na(roadDensity) & 
                              !is.na(imr_mean) &
                              !is.na(bdist1) &
                              !is.na(capdist) &
                              !is.na(un_military_base_du_spL)) %>%
  select(viol_stateBased_preDep, viol_stateBased_preDep_trend, 
         viol_OneSided_preDep, viol_OneSided_preDep_trend, 
         pop_gpw_sum, roadDensity, imr_mean, bdist1, capdist, 
         un_military_base_du_spL) %>%
  scale() %>%
  as_tibble() %>%
  set_names(paste0(colnames(.), "_std"))

un1 <- un1 %>% left_join(std_vars, by = NULL)  # Join standardized variables to the original data


                   
                   
                   
# controls

# ** Add controls 
# global controls "viol_stateBased_preDep viol_stateBased_preDep_trend viol_OneSided_preDep viol_OneSided_preDep_trend pop_gpw_sum un_military_base_du_spL"
# global controls_cem "viol_stateBased_preDep viol_stateBased_preDep_trend viol_OneSided_preDep viol_OneSided_preDep_trend pop_gpw_sum roadDensity imr_mean bdist1 capdist un_military_base_du_spL" ///  un_military_base_du2Lag1_std

# xtreg veco_viol_du un_military_base2, fe vce(cl adminUnitElection_id)

reglog <- glm(veco_viol_du ~ un_military_base_du+ viol_stateBased_preDep + viol_stateBased_preDep_trend +
                viol_OneSided_preDep + viol_OneSided_preDep_trend + pop_gpw_sum + un_military_base_du_spL +
                roadDensity + imr_mean+ bdist1 + capdist, data = un1) 
rep_fe = feols(veco_viol_du ~ un_military_base2 | adminUnitElection_id , data = un1, vcov = ~adminUnitElection_id)
summary(rep_fe)

rep_fe1 = feols(veco_viol_du ~ un_military_base2 + viol_stateBased_preDep + viol_stateBased_preDep_trend +
                  viol_OneSided_preDep + viol_OneSided_preDep_trend + pop_gpw_sum + un_military_base_du_spL +
                  roadDensity + imr_mean+ bdist1 + capdist
                | adminUnitElection_id , data = un1)
summary(rep_fe1)





