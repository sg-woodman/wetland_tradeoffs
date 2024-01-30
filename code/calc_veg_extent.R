

library(tidyverse)
library(here)
library(terra)
library(sf)
library(uavRst)
library(tidyterra)
library(cluster)

# Load data

ab_02_rgb <- rast(here("data/raw/drone_imagery/AB2_Orthomosaic_TueAug08213908375051/AB2_Orthomosaic_export_TueAug08213908375051.tif"))
ab_02_outer <- st_read(here("data/raw/wetland_polygons/AB_02_veg_outer.gpkg"))
ab_02_inner <- st_read(here("data/raw/wetland_polygons/AB_02_veg_inner.gpkg"))
ab_02_elev <- rast(here("data/raw/drone_imagery/AB2_ElevationToolbox_TueAug08214929347904/AB2_ElevationToolbox_export_TueAug08214929347904.tif")) %>% 
  subset(., 1:3)
ab_02_rgb_resamp <- resample(ab_02_rgb, ab_02_elev)
  
ab_03_rgb <- rast(here("data/raw/drone_imagery/AB3_Orthomosaic_TueAug08213933508190/AB3_Orthomosaic_export_TueAug08213933508190.tif"))
ab_03_outer <- st_read(here("data/raw/wetland_polygons/AB_03_veg_outer.gpkg"))
ab_03_inner <- st_read(here("data/raw/wetland_polygons/AB_03_veg_inner.gpkg"))
ab_03_elev <- rast(here("data/raw/drone_imagery/AB3_ElevationToolbox_TueAug08213940992601/AB3_ElevationToolbox_export_TueAug08213940992601.tif")) %>% 
  subset(., 1:3)
ab_03_rgb_resamp <- resample(ab_03_rgb, ab_03_elev)

ab_04_rgb <- rast(here("data/raw/drone_imagery/AB45_Orthomosaic_TueAug08213944278324/AB45_Orthomosaic_export_TueAug08213944278324.tif"))
ab_04_outer <- st_read(here("data/raw/wetland_polygons/AB_04_veg_outer.gpkg"))
ab_04_inner <- st_read(here("data/raw/wetland_polygons/AB_04_veg_inner.gpkg"))

ab_05_rgb <- rast(here("data/raw/drone_imagery/AB45_Orthomosaic_TueAug08213944278324/AB45_Orthomosaic_export_TueAug08213944278324.tif"))
ab_05_outer <- st_read(here("data/raw/wetland_polygons/AB_05_veg_outer.gpkg"))
ab_05_inner <- st_read(here("data/raw/wetland_polygons/AB_05_veg_inner.gpkg"))
ab_05_elev <- rast(here("data/raw/drone_imagery/AB45_ElevationToolbox_TueAug08214052108982/AB45_ElevationToolbox_export_TueAug08214052108982.tif")) %>% 
  subset(., 1:3)
ab_05_rgb_resamp <- resample(ab_05_rgb, ab_05_elev)


ab_06_rgb <- rast(here("data/raw/drone_imagery/AB6_Orthomosaic_TueAug08213552153499/AB6_Orthomosaic_export_TueAug08213552153499.tif"))
ab_06_outer <- st_read(here("data/raw/wetland_polygons/AB_06_veg_outer.gpkg"))
ab_06_inner <- st_read(here("data/raw/wetland_polygons/AB_06_veg_inner.gpkg"))

ab_10_rgb <- rast(here("data/raw/drone_imagery/AB10_Orthomosaic_TueAug08213715118652/AB10_Orthomosaic_export_TueAug08213715118652.tif"))
ab_10_outer <- st_read(here("data/raw/wetland_polygons/AB_10_veg_outer.gpkg"))
ab_10_inner <- st_read(here("data/raw/wetland_polygons/AB_10_veg_inner.gpkg"))

ab_11_rgb <- rast(here("data/raw/drone_imagery/AB11_Orthomosaic_TueAug08213808285057/AB11_Orthomosaic_export_TueAug08213808285057.tif"))
ab_11_outer <- st_read(here("data/raw/wetland_polygons/AB_11_veg_outer.gpkg"))
ab_11_inner <- st_read(here("data/raw/wetland_polygons/AB_11_veg_inner.gpkg"))

ab_12_rgb <- rast(here("data/raw/drone_imagery/AB12_Orthomosaic_TueAug08213554274647/AB12_Orthomosaic_export_TueAug08213554274647.tif"))
ab_12_outer <- st_read(here("data/raw/wetland_polygons/AB_12_veg_outer.gpkg"))
ab_12_inner <- st_read(here("data/raw/wetland_polygons/AB_12_veg_inner.gpkg"))

ab_15_rgb <- rast(here("data/raw/drone_imagery/AB15_Orthomosaic_MonJul31031057841422/AB15_Orthomosaic_export_MonJul31031057841422.tif"))
ab_15_elev <- rast(here("data/raw/drone_imagery/AB15_ElevationToolbox_TueAug08213357906466/AB15_ElevationToolbox_export_TueAug08213357906466.tif"))
ab_15_outer <- st_read(here("data/raw/wetland_polygons/AB_15_veg_outer.gpkg"))
ab_15_inner <- st_read(here("data/raw/wetland_polygons/AB_15_veg_inner.gpkg"))



# Function

get_wetland_cluster <- function(wetland, outer, inner, n_clus) {
  print("Process inputs")
  # preprocess wetland raster
  r <- wetland %>% subset(., 1:3) # subset to RGB
  names(r) <- c("red", "green", "blue") # rename bands
  
  # preprocess wetland veg edges
  out_edge <- outer %>% 
    st_transform(crs = 2956) # project to match raster
  inn_edge <- inner %>% 
    st_transform(crs = 2956) # project to match raster
  
  # extract veg zone between polygons
  veg_zone <- st_difference(out_edge, inn_edge)
  
  # crop raster and mask ouside pixels
  r_crop <- mask(r, veg_zone) %>% 
    crop(out_edge)
  
  plotRGB(r_crop)
  
  print("Calculate indices")
  r_ind <- rgb_indices(red = r_crop[[1]], # red band
                     green = r_crop[[2]], # blue band
                     blue  = r_crop[[3]], # green band
                     # all indices provided by the uavRst package
                     rgbi = c("VVI", "VARI", "NDTI", "RI", "SCI",
                              "BI", "SI", "HI", "TGI", "GLI",
                              "NGRDI", "GLAI", "CI", "SAT", "SHP")) %>% 
    rast(.)
  
  r_all <- c(r_crop, r_ind) 
  
  print("Cluster")
  ##### convert raster to df without removing NAs
  df <- as.data.frame(r_all, na.rm = F) %>% 
    mutate_if(is.integer, as.numeric)
  ##### create an index of cell values 
  idx <- as.data.frame(r_all, na.rm = F, cells = T) %>% 
    pull(cell)
  ##### remove all NAs from idex
  idx <- idx[-unique(which(is.na(df), arr.ind=TRUE)[,1])]  
  ##### cluster, remove NAs and scale
  set.seed(1284)
  clust <- cluster::clara(janitor::remove_empty(scale(na.omit(df)), "cols"), 
                          k=n_clus)
  ##### plot
  r_clust <- r_all[[1]] 
  r_clust[] <- NA
  r_clust[idx] <- clust$clustering
  plot(r_clust) 
  
  print("Output")
  return(r_clust)
}


# Process elevation -------------------------------------------------------

ab_02_elev_grey <- app(ab_02_elev %>% 
                         subset(., 1:3), 
                       "mean")
NAflag(ab_02_elev_grey) <- 0

plotRGB(ab_02_elev, legend = "topright")
plot(ab_02_elev_grey)
inv_ab_02_elev_grey <- raster.invert(ab_02_elev_grey)

inv_ab_02_elev_grey <- terra::stretch(inv_ab_02_elev_grey)

plot(inv_ab_02_elev_grey)

RGB(ab_02_elev) <- 1:3
test <- colorize(ab_02_elev, "hsl")

plot(test)

# Classify vegetation -----------------------------------------------------



ab_02_clust <- get_wetland_cluster(
  ab_02_rgb_resamp,
  ab_02_outer, 
  ab_02_inner,
  4
)
plot(ab_02_clust)
#writeRaster(ab_02_clust, here("tmp/ab_02_clust_k2.tif"), overwrite = T)
#writeRaster(ab_02_clust, here("tmp/ab_02_clust_k3.tif"), overwrite = T)
writeRaster(ab_02_clust, here("data/processed/ab_02_clust_k4.tif"), overwrite = T) # use bands 2,3&4
#writeRaster(ab_02_clust, here("tmp/ab_02_clust_k5.tif"), overwrite = T)


ab_03_clust <- get_wetland_cluster(
  ab_03_rgb_resamp,
  ab_03_outer, 
  ab_03_inner,
  4
)
plot(ab_03_clust)
#writeRaster(ab_03_clust, here("tmp/ab_03_clust_k3.tif"), overwrite = T)
writeRaster(ab_03_clust, here("data/processed/ab_03_clust_k4.tif"), overwrite = T) # use bans 3&4


ab_04_clust <- get_wetland_cluster(
  ab_04_rgb, 
  ab_04_outer, 
  ab_04_inner, 
  4)
plot(ab_04_clust)
writeRaster(ab_04_clust, here("data/processed/ab_04_clust_k4.tif")) # use band 2


ab_05_clust <- get_wetland_cluster(
  ab_05_rgb_resamp,
  ab_05_outer,
  ab_05_inner,
  4
)
plot(ab_05_clust)
#writeRaster(ab_05_clust, here("tmp/ab_05_clust_k3.tif"))
writeRaster(ab_05_clust, here("data/processed/ab_05_clust_k4.tif")) #use bands 1&2


ab_06_clust <- get_wetland_cluster(
  ab_06_rgb,
  ab_06_outer,
  ab_06_inner,
  3
)
plot(ab_06_clust)
writeRaster(ab_06_clust, here("data/processed/ab_06_clust_k3.tif")) # use band 3

ab_10_clust <- get_wetland_cluster(
  ab_10_rgb,
  ab_10_outer,
  ab_10_inner,
  4
)
plot(ab_10_clust)
writeRaster(ab_10_clust, here("data/processed/ab_10_clust_k4.tif"), overwrite = T) # use bands 1&2

ab_11_clust <- get_wetland_cluster(
  ab_11_rgb,
  ab_11_outer,
  ab_11_inner,
  3
)
plot(ab_11_clust)
writeRaster(ab_11_clust, here("tmp/ab_11_clust_k2.tif"), overwrite = T)
writeRaster(ab_11_clust, here("data/processed/ab_11_clust_k3.tif"), overwrite = T) # use bands 1&3
writeRaster(ab_11_clust, here("tmp/ab_11_clust_k4.tif"), overwrite = T)
writeRaster(ab_11_clust, here("tmp/ab_11_clust_k5.tif"), overwrite = T)

ab_12_clust <- get_wetland_cluster(
  ab_12_rgb,
  ab_12_outer,
  ab_12_inner,
  3
)
plot(ab_12_clust)
writeRaster(ab_12_clust, here("data/processed/ab_12_clust_k3.tif"), overwrite = T) # use bands 2&3

ab_15_clust <- get_wetland_cluster(
  ab_15_rgb, 
  ab_15_outer, 
  ab_15_inner, 
  3)
plot(ab_15_clust)

freq(ab_15_clust)
expanse(ab_15_clust, unit = "m", byValue = T)
writeRaster(ab_15_clust, here("data/processed/ab_15_clust_k3.tif"), overwrite = T) # use band 2


# Extract veg area --------------------------------------------------------

ab_02_clust <- rast(here("data/processed/ab_02_clust_k4.tif")) # use bands 2,3&4
ab_03_clust <- rast(here("data/processed/ab_03_clust_k4.tif")) # use bands 3&4
ab_04_clust <- rast(here("data/processed/ab_04_clust_k4.tif")) # use band 2
ab_05_clust <- rast(here("data/processed/ab_05_clust_k4.tif")) #use bands 1&2
ab_06_clust <- rast(here("data/processed/ab_06_clust_k3.tif")) # use band 3
ab_10_clust <- rast(here("data/processed/ab_10_clust_k4.tif")) # use bands 1&2
ab_11_clust <- rast(here("data/processed/ab_11_clust_k3.tif")) # use bands 1&3
ab_12_clust <- rast(here("data/processed/ab_12_clust_k3.tif")) # use bands 2&3
ab_15_clust <- rast(here("data/processed/ab_15_clust_k3.tif")) # use band 2


ab_02_area <- expanse(ab_02_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(2, 3, 4)) %>% 
  mutate(site = "AB_02")
write_csv(ab_02_area, here("data/processed/ab_02_clust_area.csv"))

ab_03_area <- expanse(ab_03_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(3, 4)) %>% 
  mutate(site = "AB_03")
write_csv(ab_03_area, here("data/processed/ab_03_clust_area.csv"))

ab_04_area <- expanse(ab_04_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(2)) %>% 
  mutate(site = "AB_04")
write_csv(ab_04_area, here("data/processed/ab_04_clust_area.csv"))

ab_05_area <- expanse(ab_05_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(1, 2)) %>% 
  mutate(site = "AB_05")
write_csv(ab_05_area, here("data/processed/ab_05_clust_area.csv"))

ab_06_area <- expanse(ab_06_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(3)) %>% 
  mutate(site = "AB_06")
write_csv(ab_06_area, here("data/processed/ab_06_clust_area.csv"))

ab_10_area <- expanse(ab_10_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(1, 2)) %>% 
  mutate(site = "AB_10")
write_csv(ab_10_area, here("data/processed/ab_10_clust_area.csv"))

ab_11_area <- expanse(ab_11_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(1, 3)) %>% 
  mutate(site = "AB_11")
write_csv(ab_11_area, here("data/processed/ab_11_clust_area.csv"))

## AB_12 crashes when using expanse
ab_12_area <- freq(ab_12_clust) %>% 
  as.data.frame() %>% 
  filter(value %in% c(2, 3)) %>% 
  mutate(area = count*global(cellSize(ab_12_clust, unit="m"), "mean")[[1]]) %>% 
  mutate(site = "AB_12") %>% 
  select(-count)
write_csv(ab_12_area, here("data/processed/ab_12_clust_area.csv"))

ab_15_area <- expanse(ab_15_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(2)) %>% 
  mutate(site = "AB_15")
write_csv(ab_15_area, here("data/processed/ab_15_clust_area.csv"))




# Calculate veg extent ----------------------------------------------------

get_veg_zone <- function(outer, inner) {
  out_edge <- outer %>% 
    st_transform(crs = 2956) # project to match raster
  inn_edge <- inner %>% 
    st_transform(crs = 2956)
  veg_zone <- st_difference(out_edge, inn_edge)
  return(veg_zone)
}

outer_veg_poly_ls <- list.files(path = here("data/raw/wetland_polygons/"), 
                       pattern = "veg_outer", 
                       full.names = T) %>% 
  map(st_read)

inner_veg_poly_ls <- list.files(path = here("data/raw/wetland_polygons/"), 
                             pattern = "veg_inner", 
                             full.names = T) %>% 
  map(st_read)

veg_zones_ls <- map2(outer_veg_poly_ls, inner_veg_poly_ls, get_veg_zone)

clust_rast_ls <- list.files(path = here("data/processed"), 
           pattern = "ab_\\d+_clust_k\\d.tif", 
           full.names = T) %>% 
  map(rast)

names_ls <- list.files(path = here("data/processed"), 
           pattern = "ab_\\d+_clust_k\\d.tif", 
           full.names = F) %>% 
  map( ~ .x %>% 
         str_replace(., "_clust_k\\d.tif", "")) %>% 
  map(toupper) %>% 
  flatten()
  

sum_cover <- function(x){
  list(x %>%
         filter(!is.nan(value)) %>% 
         group_by(value) %>%
         summarize(total_area = sum(coverage_area)) %>%
         mutate(proportion = total_area/sum(total_area)))
  
}

extract_cover <- function(r, v, name) {
  out <- exact_extract(r, 
                       v, 
                       coverage_area = TRUE, 
                       summarize_df = TRUE, 
                       fun = sum_cover) %>% 
    as.data.frame() %>% 
    mutate(site = name)
  return(out)
}


veg_prop_df <- pmap_df(list(clust_rast_ls, veg_zones_ls, names_ls), extract_cover)


em_veg_prop_df <- veg_prop_df %>% 
  mutate(is_emergent_veg = case_when(site == "AB_02" & value %in% c(2,3,4) ~ "Y",
                                     site == "AB_03" & value %in% c(3,4) ~ "Y",
                                     site == "AB_04" & value %in% c(2) ~ "Y",
                                     site == "AB_05" & value %in% c(1,2) ~ "Y",
                                     site == "AB_06" & value %in% c(3) ~ "Y",
                                     site == "AB_10" & value %in% c(1,2) ~ "Y",
                                     site == "AB_11" & value %in% c(1,3) ~ "Y",
                                     site == "AB_12" & value %in% c(2,3) ~ "Y",
                                     site == "AB_15" & value %in% c(2) ~ "Y", 
                                     TRUE ~ "N")) %>% 
  group_by(site, is_emergent_veg) %>% 
  summarise(prop_veg_area = sum(proportion)) %>% 
  filter(is_emergent_veg == "Y") %>% 
  select(-is_emergent_veg)
# Calculate total biomass -------------------------------------------------

total_veg_df <-  list.files(path = here("data/processed/"), 
           pattern = "\\clust_area.csv$",
           full.names = T) %>% 
  map(read_csv) %>% 
  bind_rows() %>% 
  group_by(site) %>% 
  summarise(total_em_area_m2 = sum(area))


# Save output -------------------------------------------------------------

left_join(total_veg_df, em_veg_prop_df) %>% 
  write_csv(., here("data/processed/emergent_veg_area_df.csv"))



# Calc spectra scaled biomass ---------------------------------------------

pred_wetland_biomass <- function(name, wetland, outer, inner, clust, bands, mod) {
  print("Process inputs")
  # preprocess wetland raster
  r <- wetland %>% subset(., 1:3) # subset to RGB
  names(r) <- c("red", "green", "blue") # rename bands
  
  # preprocess wetland veg edges
  out_edge <- outer %>% 
    st_transform(crs = 2956) # project to match raster
  inn_edge <- inner %>% 
    st_transform(crs = 2956) # project to match raster
  
  # extract veg zone between polygons
  veg_zone <- st_difference(out_edge, inn_edge)
  
  # crop raster and mask ouside pixels
  r_crop <- mask(r, veg_zone) %>% 
    crop(out_edge)
  
  plotRGB(r_crop)
  
  print("Calculate indices")
  r_ind <- rgb_indices(red = r_crop[[1]], # red band
                       green = r_crop[[2]], # blue band
                       blue  = r_crop[[3]], # green band
                       # all indices provided by the uavRst package
                       rgbi = "SAT") %>% 
    rast(.)
  
  r_mask <- clust %>% 
    subst(., c(c(1, 2, 3, 4)[-bands]), NA)
  
  print("Predict biomass density")
  r_pred <- mask(r_ind, r_mask) %>% 
    rename(mean.SAT = SAT) %>% 
    predict(., mod, na.rm = T) %>%
    mutate(mass_kg_m2 = exp(mean.SAT)) %>% 
    global(c("sum", "mean", "sd"), na.rm = T) %>% 
    as.data.frame() %>% 
    mutate(site = name) %>% 
    .[-1,] %>% 
    pivot_longer(cols = 1:3, 
                 names_to = "summ", 
                 values_to = "val") %>% 
    pivot_wider(names_from = summ, 
                values_from = val, 
                names_prefix = "mass_kg_m2_") %>% 
    mutate(res = res(wetland)[1]^2, 
           total_mass_Mg = (mass_kg_m2_sum*res)/1000) %>% 
    dplyr::select(-mass_kg_m2_sum)
  
  return(r_pred)
}

ab_02_pred <- pred_wetland_biomass("AB_02", 
                                   ab_02_rgb_resamp, 
                                   ab_02_outer, 
                                   ab_02_inner, 
                                   ab_02_clust, 
                                   c(2,3,4), 
                                   sat_mod)

ab_03_pred <- pred_wetland_biomass("AB_03", 
                                   ab_03_rgb_resamp, 
                                   ab_03_outer, 
                                   ab_03_inner, 
                                   ab_03_clust, 
                                   c(3,4), 
                                   sat_mod)

ab_04_pred <- pred_wetland_biomass("AB_04", 
                                   ab_04_rgb, 
                                   ab_04_outer, 
                                   ab_04_inner, 
                                   ab_04_clust, 
                                   c(2), 
                                   sat_mod)

ab_05_pred <- pred_wetland_biomass("AB_05", 
                                   ab_05_rgb_resamp, 
                                   ab_05_outer, 
                                   ab_05_inner, 
                                   ab_05_clust, 
                                   c(1,2), 
                                   sat_mod)

ab_06_pred <- pred_wetland_biomass("AB_06", 
                                   ab_06_rgb, 
                                   ab_06_outer, 
                                   ab_06_inner, 
                                   ab_06_clust, 
                                   c(3), 
                                   sat_mod)

ab_10_pred <- pred_wetland_biomass("AB_10", 
                                   ab_10_rgb, 
                                   ab_10_outer, 
                                   ab_10_inner, 
                                   ab_10_clust, 
                                   c(1,2), 
                                   sat_mod)

ab_11_pred <- pred_wetland_biomass("AB_11", 
                                   ab_11_rgb, 
                                   ab_11_outer, 
                                   ab_11_inner, 
                                   ab_11_clust, 
                                   c(1,3), 
                                   sat_mod)

ab_12_pred <- pred_wetland_biomass("AB_12", 
                                   ab_12_rgb, 
                                   ab_12_outer, 
                                   ab_12_inner, 
                                   ab_12_clust, 
                                   c(2,3), 
                                   sat_mod)

ab_15_pred <- pred_wetland_biomass("AB_15", 
                         ab_15_rgb, 
                         ab_15_outer, 
                         ab_15_inner, 
                         ab_15_clust, 
                         c(2), 
                         sat_mod)

spectra_biomass_df <- bind_rows(ab_02_pred,
                                ab_03_pred,
                                ab_04_pred,
                                ab_05_pred,
                                ab_06_pred,
                                ab_10_pred,
                                ab_11_pred,
                                ab_12_pred,
                                ab_15_pred)
spectra_biomass_df

write_csv(spectra_biomass_df, here("data/processed/spectra_biomass_df.csv"))

