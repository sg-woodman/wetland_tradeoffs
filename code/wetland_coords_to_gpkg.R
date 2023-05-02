


library(sf)
library(tidyverse)
library(readxl)

pnts <- read_excel("/Users/sam/Downloads/OneDrive_4_4-28-2023/PrairieBCRC_CAAF_sites_27April2023.xls") %>% 
  st_as_sf(., coords = c("Longtitude_X", "Latitude_Y"),
           crs = 4326)

st_write(pnts, "/Users/sam/Documents/wetland_flux/test.gpkg", "GPKG")
