## ---------------------------
##
## Script name: clean_fl_polygon
##
## Purpose of script: Use positive and negative buffers to clean the Frank Lake
##  polygon from HydroLakes. 
##
## Author: Samuel Woodman
##
## Date Created: 2023-06-12
##
## Email: samuel.woodman@gmail.com
##
## ---------------------------
##
## Notes:
##    The polygon has been initially cleaned in QGIS to remove the streams 
##    coming into the lake. This was done by deleting the vectors that made up 
##    the stream. 
##   
##
## ---------------------------

options(scipen = 6, digits = 4)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

fl <- vect(here("data/raw/fl_hydrolake_polygon.gpkg"))

fl_b1_shoreline_boundary <- vect(here("data/raw/fl_b1_shoreline_boundary.gpkg"))
fl_b1_shoreline_edge <- vect(here("data/raw/fl_b1_shoreline_edge.gpkg"))

fl_b2_shoreline_boundary <- vect(here("data/raw/fl_b2_shoreline_boundary.gpkg"))
fl_b2_shoreline_edge <- vect(here("data/raw/fl_b2_shoreline_edge.gpkg"))


# Process data ------------------------------------------------------------

## Manual delineation ----

### Shoreline boundary was delineated using Google Earth Imagery in QGIS. The 
### outer boundary was defined as the area just before any crop or range land. 
### There is also an accumulation of salt around the boundary that can help 
### identify the edge. The boundary edge was also manually delineated to 
### capture any visible vegetation. This boundary does not need to be as 
### precise since adding more area will only include water pixels that are more 
### easily identifiable.

### Google Earth View
#### visualize zones 
plot(fl_b1_shoreline_boundary)
lines(fl_b1_shoreline_edge)

plot(fl_b2_shoreline_boundary)
lines(fl_b2_shoreline_edge)

#### remove inner boundary from outer
fl_b1_veg_zone <- erase(fl_b1_shoreline_boundary, fl_b1_shoreline_edge)
plot(fl_b1_veg_zone)

fl_b2_veg_zone <- erase(fl_b2_shoreline_boundary, fl_b2_shoreline_edge)
plot(fl_b2_veg_zone)

#### save output
writeVector(fl_b1_veg_zone, here("data/processed/fl_b1_veg_zone.gpkg"))

writeVector(fl_b2_veg_zone, here("data/processed/fl_b2_veg_zone.gpkg"))



