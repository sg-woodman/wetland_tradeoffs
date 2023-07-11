## ---------------------------
##
## Script name: create_veg_zones
##
## Purpose of script:
##
## Author: Samuel Woodman
##
## Date Created: 2023-06-29
##
## Email: samuel.woodman@gmail.com
##
## ---------------------------
##
## Notes:
##    Veg zones for basin 1 were determined by INFLIGHT data. Basin 2 zones were 
##    determined by Sam Woodman in attempts to match basin 1. 
##   
##
## ---------------------------

options(scipen = 6, digits = 4)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)

library(sf)
library(lwgeom)
library(tidyterra)

# Load data ---------------------------------------------------------------

## Veg zones
fl_b1_veg_zone <- vect(here("data/processed/fl_b1_veg_zone.gpkg"))
fl_b2_veg_zone <- vect(here("data/processed/fl_b2_veg_zone.gpkg"))

## Split lines
b1_lines <- vect(here("data/raw/fl_basin1_split_lines.gpkg"))
b2_lines <- vect(here("data/raw/fl_basin2_split_lines.gpkg"))

# Process -----------------------------------------------------------------

## Basin 1
### split veg zone polygon by lines
fl_b1_veg_zone_split <- terra::split(fl_b1_veg_zone, b1_lines)
plot(fl_b1_veg_zone_split)

### assign each split segment an id number
fl_b1_veg_zone_split$id <- 1:nrow(fl_b1_veg_zone_split)
### fix polygon 6 by converting to a single rather than multiploygon
### 6.1 is changed to 12
id_fix <- st_cast(st_as_sf(subset(fl_b1_veg_zone_split, fl_b1_veg_zone_split$id == 6)), "POLYGON") %>% 
  mutate(id2 = c(6, 12)) %>% 
  dplyr::select(-id) %>% 
  rename(id = id2) %>% 
  vect()
### remove mutlipolygon version of 6 from vector 
fl_b1_veg_zone_split <- subset(fl_b1_veg_zone_split, fl_b1_veg_zone_split$id != 6)
### add in the fixed id version and sort by id
fl_b1_veg_zone_split <- c(fl_b1_veg_zone_split, id_fix) %>% vect() %>% sort(., "id")

### confirm id fix
fl_b1_veg_zone_split$id 

plot(fl_b1_veg_zone_split, "id", col=rainbow(nrow(fl_b1_veg_zone_split)))
text(fl_b1_veg_zone_split, "id", cex=.8, halo=TRUE)

### manually assign id to each zone based on above plot
fl_b1_veg_zone_split$id2 <- c(5, 5, 5, 1, 2, 5, 4, 3, 3, 3, 2, 4)

plot(fl_b1_veg_zone_split, "id2", col=rainbow(5))
text(fl_b1_veg_zone_split, "id2", cex=.8, halo=TRUE)

### Aggregate by id and rename
fl_b1_veg_zone_split <- aggregate(fl_b1_veg_zone_split, "id2")  %>% 
  rename(id = id2) %>% 
  select(id)

plot(fl_b1_veg_zone_split, "id", col=rainbow(5))
text(fl_b1_veg_zone_split, "id", cex=.8, halo=TRUE)

### save output
writeVector(fl_b1_veg_zone_split, here("data/processed/fl_b1_veg_zones.gpkg"), overwrite = T)

## Basin 2
### split veg zone polygon by lines
fl_b2_veg_zone_split <- terra::split(fl_b2_veg_zone, b2_lines)
plot(fl_b2_veg_zone_split)

### assign each split segment an id number
fl_b2_veg_zone_split$id <- 1:nrow(fl_b2_veg_zone_split)

### confirm id fix
fl_b2_veg_zone_split$id

plot(fl_b2_veg_zone_split, "id", col=rainbow(nrow(fl_b2_veg_zone_split)))
text(fl_b2_veg_zone_split, "id", cex=.8, halo=TRUE)

### manually assign id to each zone based on above plot
fl_b2_veg_zone_split$id2 <- c(4, 4, 3, 3, 5, 5, 1, 1, 2, 4)

plot(fl_b2_veg_zone_split, "id2", col=rainbow(5))
text(fl_b2_veg_zone_split, "id2", cex=.8, halo=TRUE)

fl_b2_veg_zone_split <- aggregate(fl_b2_veg_zone_split, "id2")  %>% 
  rename(id = id2) %>% 
  select(id)

plot(fl_b2_veg_zone_split, "id", col=rainbow(5))
text(fl_b2_veg_zone_split, "id", cex=.8, halo=TRUE)


writeVector(fl_b2_veg_zone_split, here("data/processed/fl_b2_veg_zones.gpkg"), overwrite = T)
