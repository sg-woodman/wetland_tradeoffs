## ---------------------------
##
## Script name: aggregate_drone_imagery
##
## Purpose of script:
##
## Author: Samuel Woodman
##
## Date Created: 2023-11-13
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
library(sf)
library(raster)
library(terra)
library(uavRst)


# Projection --------------------------------------------------------------

## Get projection
## There may be a simpler appraoch but I can't find it
r <- rast() # create an empty raster
crs(r, proj = T) # check default CRS (Coordinate Reference System)
crs(r) <- "ESRI:102001" # set CRS for empty raster, use ESRI: for ESRI CRS, EPSG: other
crs_102001 <- crs(r) # pull CRS string from empty raster

# Load data ---------------------------------------------------------------

raster <- rast(here("data/raw/drone_imagery/AB15_Orthomosaic_MonJul31031057841422/AB15_Orthomosaic_export_MonJul31031057841422.tif")) %>%
  subset(., 1:3)

res(raster)
ncell(raster) / 10

glcm_variables <- tibble::tribble(
  # https://www.researchgate.net/figure/Definition-of-gray-level-co-occurrence-matrix-GLCM-descriptors_tbl3_262568114
  ~"GLCM",
  ~"Meaning",
  "Energy",
  "A measure of uniformity/homogeneity of an image. It is also known as angular second moment",
  "Entropy",
  "A measure of randomness degree",
  "Contrast",
  "A measure of contrast/local intensity variation",
  "Cluster shade",
  "The lack of symmetry in an image",
  "Correlation",
  "A measure of gray-level linear dependence in an image between the pixels at the specified positions relative to each other",
  "Homegeneity",
  "A measure of uniformity",
  "Maximum probability",
  "It shows the emergence of the gray-level value g adjacent to the gray-level value g; more dominant in the image",
  "Inverse difference moment",
  "A measure of local homogeneity"
)


## calculate greyscale value
raster$grey <- app(raster, mean)

raster_aea <- project(raster,
  crs_102001,
  res = 0.3,
  method = "bilinear"
)
ncell(raster_aea)

library(glcm)
test <- glcm(raster(raster_aea$AB2_ElevationToolbox_export_TueAug08214929347904_1))

t_r <- rast(test)
r2 <- c(raster_aea, t_r)

# Aggregate ---------------------------------------------------------------

cropped_raster_30cm <- terra::aggregate(cropped_raster, 10, fun = mean)

# Save output -------------------------------------------------------------

writeRaster(cropped_raster_30cm, here("data/processed/cropped_raster_30cm.tif"))

# Visualize ---------------------------------------------------------------

plot(cropped_raster_30cm)
plotRGB(cropped_raster_30cm)
