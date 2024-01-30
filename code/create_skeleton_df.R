## ---------------------------
##
## Script name: create_skeleton_df
##
## Purpose of script: Create a DF with the key site information, including site
##    names, numbers, landuse, etc. 
##
## Author: Samuel Woodman
##
## Date Created: 2023-11-20
##
## Email: samuel.woodman@gmail.com
##
## ---------------------------
##
## Notes: There was no region ID for the Camrose Alberta sites so I called the 
##        region "Camrose". Can adjust if DUC has a more specific code (i.e. 
##        soil zone) 
##   
##
## ---------------------------

options(scipen = 6, digits = 4)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)

# Load data ---------------------------------------------------------------

skeleton_df <- read_xlsx(here("documents/bcrc_site_guide.xlsx")) %>% 
  janitor::clean_names() %>% 
  separate_wider_delim(
    cols = site_id,
    delim = "_", 
    names = c("province", "region", "site_number")
  ) %>% 
  unite("site", c(province, region, site_number), sep = "_", remove = FALSE) %>%
  mutate(site = str_replace(site, "_Camrose_", "_")) %>% 
  select(site, province, region, site_number, latitude, longitude, landuse) %>% 
  mutate(land_class = case_when(landuse == "Grassland/Cropland" ~ "Both",
                                str_detect(landuse, "Grassland|Pasture") ~ "Grassland", 
                                 str_detect(landuse, "Cropland") ~ "Cropland" 
                                ),
         site = as.factor(site),
         province = as.factor(province))

write_csv(skeleton_df, here("data/raw/skeleton_df.csv"))
 s
         