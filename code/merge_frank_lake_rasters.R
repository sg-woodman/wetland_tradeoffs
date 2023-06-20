## ---------------------------
##
## Script name: merge_fl_raster
##
## Purpose of script: The high resolution raster images provided by Larry were
##  split into to images to conserve space. To properly process these images 
##  they need to be merged into a single image
## 
##
## Author: Samuel Woodman
##
## Date Created: 2023-05-12
##
## Email: samuel.woodman@gmail.com
##
## ---------------------------
##
## Notes:
##   Bands according to WorldView multispectral images
##   https://earth.esa.int/eogateway/missions/worldview/description
##    B1 Coastal: 400-450 nm
##    B2 Blue: 450-510 nm
##    B3 Green: 510-580 nm
##    B4 Yellow: 585-625 nm
##    B5 Red: 630-690 nm
##    B6 Red Edge: 705-745 nm
##    B7 Near-IR1: 770-895 nm
##    B8 Near-IR2: 860-1040 nm
##
## ---------------------------

options(scipen = 6, digits = 4)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(terra)


# Load data ---------------------------------------------------------------

fl_ms_1 <- rast(here("data/raw/Frank Lake WV Images 2016/Multi-Spec Images/16JUL21185727-M2AS_R1C1-014790366010_01_P001.TIF"))
fl_ms_2 <- rast(here("data/raw/Frank Lake WV Images 2016/Multi-Spec Images/16JUL21185727-M2AS_R2C1-014790366010_01_P001.TIF"))

fl_pan_1 <- rast(here("data/raw/Frank Lake WV Images 2016/PAN Images/16JUL21185727-P2AS_R1C1-014790366010_01_P001.TIF"))
fl_pan_2 <- rast(here("data/raw/Frank Lake WV Images 2016/PAN Images/16JUL21185727-P2AS_R2C1-014790366010_01_P001.TIF"))

fl_shape <- vect(here("data/raw/Frank Lake WV Images 2016/GIS Files/014790366010_01_ORDER_SHAPE.shp")) %>% 
  project(., fl_ms_1)


# Process -----------------------------------------------------------------

## Join multi spec imagery
### no overlapping pixel should exist but if they do the mean is taken
fl_multi_spec <- mosaic(fl_ms_1, fl_ms_2, fun = "mean")

### mask to remove pixels outside surveyed area
fl_multi_spec <- mask(fl_multi_spec, fl_shape)

plot(fl_multi_spec[[2]])
minmax(fl_multi_spec)

## Join panchromatic imagery
### no overlapping pixel should exist but if they do the mean is taken
fl_pan <- mosaic(fl_pan_1, fl_pan_2, fun = "mean") 

### mask to remove pixels outside surveyed area
fl_pan <- mask(fl_pan, fl_shape)


plot(fl_pan)
minmax(fl_pan)


# Save output -------------------------------------------------------------

writeRaster(fl_multi_spec, here("data/raw/fl_multi_spec.tif"), overwrite = T,
            names = c("Coastal", "Blue", "Green", "Yellow", 
              "Red", "Red_Edge", "Near_IR1", "Near_IR2"))

writeRaster(fl_pan, here("data/raw/fl_panchrom.tif"), overwrite = T)

