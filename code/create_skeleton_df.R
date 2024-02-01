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
  # get sites that don't have a dash (i.e. no regional classification)
  filter(!str_detect(site_id, "-")) %>% 
  # separate letters (province code) and numbers (site number)
  separate(site_id, into = c("prov", "site_no"),
           sep = "(?<=[A-Za-z])(?=[0-9])", remove = F) %>%
  # put in leading zero on numbers
  mutate(site_no = str_pad(site_no, 2, pad = "0")) %>%
  # combine province and site number to get site column 
  unite("site", 3:4, sep = "-", remove = F) %>% 
  # create region column for binding
  mutate(region = NA) %>% 
  # drop redundant province column
  select(-prov) %>% 
  # bind rows with region included in site_id
  # requires a different approach than regions above since those lack a 
  # - in the names
  bind_rows(read_xlsx(here("data/raw/BCRC_CAAF_sites_02Jan2024.xlsx")) %>% 
              # clean names to lowercase
              janitor::clean_names() %>% 
              # select only columns of use
              select(objectid, site_id = site_id_current_study_bcrc, province, 
                     latitude_y, longtitude_x, landuse) %>% 
              # get sites that have a dash (i.e. no regional classification)
              filter(str_detect(site_id, "-")) %>% 
              # create column of sub-regions and site numbers
              mutate(region = str_replace_all(site_id, "[:digit:]|AB|SK|MB|ON|BC|\\-", ""), # sub-regions
                     site_no = as.numeric(str_replace_all(site_id, "[:alpha:]|\\-", "")), # site number
                     site_no = str_pad(site_no, 2, pad = "0")) %>% #  add leading zero to site number
              # replace empty cells with NA where no region is specified
              mutate_at("region", ~na_if(., ''))) %>% 
  # create new land classes
  mutate(landuse = str_to_title(landuse), # convert all capital landuse string to title case
         land_class = case_when(landuse == "Grassland/Cropland" ~ "Both",
                                str_detect(landuse, "Grassland|Pasture|Forest|wetland") ~ "Grassland", # Paige said all these can be considered Grassland/Natural cover
                                str_detect(landuse, "Cropland") ~ "Cropland" 
         ),
         site = as.factor(site),
         region = as.factor(region),
         province = as.factor(province))



write_csv(skeleton_df, here("data/raw/skeleton_df.csv"))


