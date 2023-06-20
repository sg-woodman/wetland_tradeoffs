## ---------------------------
##
## Script name: fl_veg_classification
##
## Purpose of script: Use band indices, unsupervised, and supervised 
##  classification models to determine the amount of emergent vegetation 
##  around Frank Lake
##
## Author: Samuel Woodman
##
## Date Created: 2023-06-20
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


# Load data ---------------------------------------------------------------

## Raster 
fl_multi_spec <- rast(here("data/raw/fl_multi_spec.tif"))
fl_indices <- rast(here("data/processed/fl_indices.tif"))

## Vector
fl_veg_zone <- vect(here("data/processed/fl_veg_zone.gpkg"))


# Extract veg zone --------------------------------------------------------

fl_ms_zone <- crop(mask(fl_multi_spec, fl_veg_zone), fl_veg_zone)
plot(fl_ms_zone)

fl_ind_zone <- crop(mask(fl_indices, fl_veg_zone), fl_veg_zone)
plot(fl_ind_zone[[4]])
