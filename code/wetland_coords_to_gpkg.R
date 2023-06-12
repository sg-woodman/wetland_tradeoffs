## ---------------------------
##
## Script name: wetland_coords_to_gpkg
##
## Purpose of script: Covert lat/lon coordinates provided by Molly to a spatial
## object. 
##
## Author: Samuel Woodman
##
## Date Created: 2023-05-02
##
## Email: samuel.woodman@gmail.com
##
## ---------------------------
##
## Notes: 
## Data were originally from Google Earth and had the ESPG:4326 projection
##   
## ---------------------------

options(scipen = 6, digits = 4)


# Load packages -----------------------------------------------------------

require(tidyverse)
require(here)

library(readxl)
library(sf)

# Process -----------------------------------------------------------------

## read data and convert to sf object
pnts <- read_excel(here("data/raw/PrairieBCRC_CAAF_sites_27April2023.xls")) %>% 
  st_as_sf(., coords = c("Longtitude_X", "Latitude_Y"),
           crs = 4326)


# Save output -------------------------------------------------------------

st_write(pnts, here("data/raw/wetland_sites.gpkg"), "GPKG")
