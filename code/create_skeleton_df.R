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

skeleton_df <- read_xlsx(here("data/raw/BCRC_CAAF_sites_02Jan2024.xlsx")) %>% 
  # clean names to lowercase
  janitor::clean_names() %>% 
  # select only columns of use
  select(objectid, site_id = site_id_current_study_bcrc, province, 
         latitude_y, longtitude_x, landuse) %>% 
  # create column of sub-regions and site numbers
  mutate(region = str_replace_all(site_id, "[:digit:]|AB|SK|MB|ON|BC|\\-", ""), # sub-regions
         site_no = as.numeric(str_replace_all(site_id, "[:alpha:]|\\-", ""))) %>% # site number
  # replace empty cells with NA where no region is specified
  mutate_at("region", ~na_if(., '')) %>% 
  # create new land classes
  mutate(land_class = case_when(landuse == "Grassland/Cropland" ~ "Both",
                                str_detect(landuse, "Grassland|Pasture") ~ "Grassland", 
                                 str_detect(landuse, "Cropland") ~ "Cropland" 
                                ),
         site = as.factor(site_id),
         region = as.factor(region),
         province = as.factor(province))

write_csv(skeleton_df, here("data/raw/skeleton_df.csv"))

         