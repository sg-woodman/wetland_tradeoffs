## ---------------------------
##
## Script name: process_veg_coords
##
## Purpose of script: Take the coordinates from the wetland vegeation sampling
##  and convert them from degrees, minutes, seconds to decimal degrees. 
##
## Author: Samuel Woodman
##
## Date Created: 2023-11-06
##
## Email: samuel.woodman@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

options(scipen = 6, digits = 10)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(sf)


# Load data ---------------------------------------------------------------

veg_coords <- readxl::read_xlsx(here("data/raw/veg_sample_mass.xlsx"))


# Function to convert DMS to DD
convert_dms_to_dd <- function(degrees, minutes, seconds, direction) {
  dd <- degrees + minutes/60 + seconds/3600
  if (toupper(direction) %in% c("S", "W")) {
    dd <- -dd
  }
  return(dd)
}

veg_coords %>% 
  mutate(lat2 = convert_dms_to_dd(lat_deg, lat_min, lat_sec, "N"),
         lon2 = convert_dms_to_dd(lon_deg, lon_min, lon_sec, "W"),
         lat = coalesce(lat, lat2),
         lon = coalesce(lon, lon2)) %>%
  st_as_sf(., coords = c("lon", "lat"),
           crs = 4326) %>% 
  st_write(., here("tmp/veg_coords.gpkg"))

