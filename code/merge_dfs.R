## ---------------------------
##
## Script name: merge_dfs
##
## Purpose of script:
##
## Author: Samuel Woodman
##
## Date Created: 2024-02-01
##
## Email: samuel.woodman@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

options(scipen = 6, digits = 4)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)


# Load and preprocess data ------------------------------------------------

skeleton_df <- read_csv(here("data/raw/skeleton_df.csv"))
veg_df <- read_xlsx(here("data/raw/veg_biomass.xlsx")) %>% 
  # fix columns
  mutate(site = str_replace_all(site, "_", "-"), # convert _ to -
         date = as_date(date), # convert date to date format
         mass_kg_m2 = mass_kg/0.1) %>% # convert to m2 using sample frame dimensions (0.5 cm x 0.2 cm)
  group_by(site) %>% 
  # calculate site level values
  summarise(mean_veg_kg_m2 = mean(mass_kg_m2, na.rm = T),
            sd_veg_kg_m2 = sd(mass_kg_m2, na.rm = T),
            n = n(), 
            se_veg_kg_m2 = sd_veg_kg_m2/sqrt(n))
aqua_prod_df <- read_csv(here("data/processed/aqua_prod_2022.csv")) %>% 
  #mutate(site = str_replace(site, "-", "_")) %>% 
  group_by(site, depth) %>% 
  # summarize all numeric variables
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>% 
  # calculate aerial productivity values according to Matt
  mutate(aerial_gpp = gpp.985*(depth/100),
         aerial_r = r.985*(depth/100)) %>% 
  # remove lat/lon to prevent redundancy with skeleton df
  select(-c(lat, long))
emergent_veg_area_df <- read_csv(here("data/processed/emergent_veg_area_df.csv")) %>% 
  # fix site to match skeleton df
  mutate(site = str_replace(site, "_", "-"))
spectra_biomass_df <- read_csv(here("data/processed/spectra_biomass_df.csv")) %>% 
  # fix site to match skeleton df
  mutate(site = str_replace(site, "_", "-"))
wetland_area_df <- read_csv(here("data/processed/wetland_area_df.csv")) %>% 
  # fix site to match skeleton df
  mutate(site = str_replace(site, "_", "-"))
flux_df <- read_xlsx("/Users/sam/Downloads/BCRC-ppr-ghg-2022.xlsx",
                     sheet = 2) %>%
  janitor::clean_names() %>% 
  unite("site", c(province,site), sep = "-") %>% 
  dplyr::select(site, date, starts_with("flux")) %>% 
  mutate(flux_n2o = as.numeric(flux_n2o),
         date = as.Date(date), 
         year = year(date), 
         month = month(date, label = T)) %>% 
  group_by(site, year) %>% 
  summarise(across(where(is.numeric), \(x) mean(x,  na.rm = T)))
bubble_trap_df <- read_xlsx("/Users/sam/Downloads/Bubble trap calculations_withsamplelosscorrection.xlsx") %>% 
  janitor::clean_names() %>% 
  # fix site to match skeleton df
  mutate(site = str_replace(site, "_", "-")) %>% 
  dplyr::select(site, sampling_date, ch4mmol_m_2_d_1) %>% 
  group_by(site) %>% 
  summarise(ebullition_mean_ch4mmol_m_2_d_1 = mean(ch4mmol_m_2_d_1, na.rm = T))
diversity_df <- read_csv(here("data/processed/diversity_df.csv")) %>% 
  # fix site to match skeleton df
  mutate(site = str_replace(site, "_", "-"))



# Join dfs ----------------------------------------------------------------


df <- skeleton_df %>% 
  left_join(., veg_df) %>% 
  left_join(., aqua_prod_df) %>% 
  left_join(., emergent_veg_area_df) %>% 
  left_join(., wetland_area_df) %>% 
  left_join(., spectra_biomass_df) %>% 
  left_join(., flux_df) %>% 
  left_join(., bubble_trap_df) %>% 
  # calculate total density of emergent vegetation across the entire wetland complex
  mutate(total_density = (total_mass_Mg*1000)/full_wetland_area_m2) %>% 
  left_join(., diversity_df)

view(df)

write_csv(df, here("data/processed/tradeoff_df.csv"))

