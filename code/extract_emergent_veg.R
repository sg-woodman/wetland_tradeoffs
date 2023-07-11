## ---------------------------
##
## Script name: extract_emergent_veg
##
## Purpose of script:
##
## Author: Samuel Woodman
##
## Date Created: 2023-06-29
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
library(terra)

r <- as.numeric(predLC)

test <- terra::extract(r, fl_veg_zones_b1, ID = T, weights = T) %>%
  as.data.frame() %>%   
  # join wedge section areas using the ID column as guide
  left_join(., fl_veg_zones_b1 %>%
              expanse(unit = "m") %>% 
              as.data.frame() %>%
              rename(zone_area_m2 = ".") %>% 
              mutate(ID = row_number())) %>%
  # calc area of each class as function of amount of cell in section
  mutate(area_m2 = weight*(1.2*1.2)) %>%
  # sum within class and section id
  group_by(ID, class) %>%
  summarise(class_area_m2 = sum(area_m2),
            zone_area_m2 = mean(zone_area_m2)) %>%
  # calc total area and prop of each class
  mutate(total_area_m2 = sum(class_area_m2),
         class_area_ha = class_area_m2/10000,
         zone_area_ha = zone_area_m2/10000,
         prop_area = class_area_m2/total_area_m2,
         method = "clara_all_data")
test %>% 
  mutate(veg_type = case_when(Coastal == 2 | Coastal == 3 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg")

test %>% 
  mutate(veg_type = case_when(class == 4 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg")

