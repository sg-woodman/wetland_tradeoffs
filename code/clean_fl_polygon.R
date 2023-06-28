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

fl_shoreline_boundary <- vect(here("data/raw/fl_shoreline_boundary.gpkg"))
fl_shoreline_edge <- vect(here("data/raw/fl_shoreline_edge.gpkg"))

fl_shoreline_boundary_pan <- vect(here("data/raw/fl_shoreline_boundary_pan.gpkg"))
fl_shoreline_edge_pan <- vect(here("data/raw/fl_shoreline_edge_pan.gpkg"))


# Process data ------------------------------------------------------------

## Lake Buffer ----

### Use the HydroLakes polygon to determine vegetation zone. 

#### capture terrestrial vegetation
fl_outer_buff_100m <- buffer(fl, 100)

#### capture aquatic vegetation
fl_inner_buff_100m <- buffer(fl, -100)

#### remove inner buffer from outer
fl_buff_zone <- erase(fl_outer_buff_100m, fl_inner_buff_100m)
plot(fl_buff_zone)

writeVector(fl_buff_zone, here("tmp/fl_100m_buff.gpkg"))

#### extract indeces values for buffer zone
test <- mask(fl_exg, fl_buff_zone)
plot(test)
length(cells(test))

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
plot(fl_shoreline_boundary)
lines(fl_shoreline_edge)

#### remove inner boundary from outer
fl_veg_zone <- erase(fl_shoreline_boundary, fl_shoreline_edge)
plot(fl_veg_zone)

#### save output
writeVector(fl_veg_zone, here("data/processed/fl_veg_zone.gpkg"))

### WorldView Pan
#### visualize zones 
plot(fl_shoreline_boundary_pan)
lines(fl_shoreline_edge_pan)

#### remove inner boundary from outer
fl_veg_zone_pan <- erase(fl_shoreline_boundary_pan, fl_shoreline_edge_pan)
plot(fl_veg_zone_pan)

#### save output
writeVector(fl_veg_zone_pan, here("data/processed/fl_veg_zone_pan.gpkg"))


