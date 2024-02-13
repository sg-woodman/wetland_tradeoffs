

library(tidyverse)
library(here)
library(terra)
library(sf)
library(uavRst)
library(tidyterra)
library(cluster)
library(exactextractr)

# Load data----
## AB - Camrose ----
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

## AB - Brooks ----

ab_b_05_rgb <- rast(here("data/raw/drone_imagery/ABB5_Orthomosaic_TueAug08220555654003/ABB5_Orthomosaic_export_TueAug08220555654003.tif"))
ab_b_05_elev <- rast(here("data/raw/drone_imagery/ABB5_ElevationToolbox_TueAug08220602447940/ABB5_ElevationToolbox_export_TueAug08220602447940.tif")) %>% 
  subset(., 1:3)
ab_b_05_outer <- st_read(here("data/raw/wetland_polygons/AB_B_05_veg_outer.gpkg"))
ab_b_05_inner <- st_read(here("data/raw/wetland_polygons/AB_B_05_veg_inner.gpkg"))
ab_b_05_rgb_resamp <- resample(ab_b_05_rgb, ab_b_05_elev)

ab_b_06_rgb <- rast(here("data/raw/drone_imagery/ABB6_Orthomosaic_TueAug08220623188368/ABB6_Orthomosaic_export_TueAug08220623188368.tif"))
ab_b_06_elev <- rast(here("data/raw/drone_imagery/ABB6_ElevationToolbox_TueAug08223112899921/ABB6_ElevationToolbox_export_TueAug08223112899921.tif")) %>% 
  subset(., 1:3)
ab_b_06_outer <- st_read(here("data/raw/wetland_polygons/AB_B_06_veg_outer.gpkg"))
ab_b_06_inner <- st_read(here("data/raw/wetland_polygons/AB_B_06_veg_inner.gpkg"))
ab_b_06_rgb_resamp <- resample(ab_b_06_rgb, ab_b_06_elev)

ab_b_07_rgb <- rast(here("data/raw/drone_imagery/ABB7_Orthomosaic_TueAug08221059353540/ABB7_Orthomosaic_export_TueAug08221059353540.tif"))
ab_b_07_elev <- rast(here("data/raw/drone_imagery/ABB7_ElevationToolbox_TueAug08220747644602/ABB7_ElevationToolbox_export_TueAug08220747644602.tif")) %>% 
  subset(., 1:3)
ab_b_07_outer <- st_read(here("data/raw/wetland_polygons/AB_B_07_veg_outer.gpkg"))
ab_b_07_inner <- st_read(here("data/raw/wetland_polygons/AB_B_07_veg_inner.gpkg"))
ab_b_07_rgb_resamp <- resample(ab_b_07_rgb, ab_b_07_elev)

ab_b_08_rgb <- rast(here("data/raw/drone_imagery/ABB8_Orthomosaic_TueAug08220759089092/ABB8_Orthomosaic_export_TueAug08220759089092.tif"))
ab_b_08_elev <- rast(here("data/raw/drone_imagery/ABB8_ElevationToolbox_TueAug08221117249619/ABB8_ElevationToolbox_export_TueAug08221117249619.tif")) %>% 
  subset(., 1:3)
ab_b_08_outer <- st_read(here("data/raw/wetland_polygons/AB_B_08_veg_outer.gpkg"))
ab_b_08_outer_2 <- st_read(here("data/raw/wetland_polygons/AB_B_08_veg_outer_2.gpkg"))
ab_b_08_inner <- st_read(here("data/raw/wetland_polygons/AB_B_08_veg_inner.gpkg"))
ab_b_08_rgb_resamp <- resample(ab_b_08_rgb, ab_b_08_elev)

ab_b_14_rgb <- rast(here("data/raw/drone_imagery/ABB14_Orthomosaic_TueAug08220717494836/ABB14_Orthomosaic_export_TueAug08220717494836.tif"))
ab_b_14_elev <- rast(here("data/raw/drone_imagery/ABB14_ElevationToolbox_TueAug08222553186193/ABB14_ElevationToolbox_export_TueAug08222553186193.tif")) %>% 
  subset(., 1:3)
ab_b_14_outer <- st_read(here("data/raw/wetland_polygons/AB_B_14_veg_outer.gpkg"))
ab_b_14_inner <- st_read(here("data/raw/wetland_polygons/AB_B_14_veg_inner.gpkg"))
ab_b_14_rgb_resamp <- resample(ab_b_14_rgb, ab_b_14_elev)

## AB - Vulcan ----
ab_db_03_rgb <- rast(here("data/raw/drone_imagery/ABDB3_Orthomosaic_WedSep20182359150528/ABDB3_Orthomosaic_export_WedSep20182359150528.tif"))
ab_db_03_elev <- rast(here("data/raw/drone_imagery/ABDB3_ElevationToolbox_WedSep20182404342731/ABDB3_ElevationToolbox_export_WedSep20182404342731.tif")) %>% 
  subset(., 1:3)
ab_db_03_outer <- st_read(here("data/raw/wetland_polygons/AB_DB_03_veg_outer.gpkg"))
ab_db_03_inner <- st_read(here("data/raw/wetland_polygons/AB_DB_03_veg_inner.gpkg"))
ab_db_03_rgb_resamp <- resample(ab_db_03_rgb, ab_db_03_elev)

ab_db_06_rgb <- rast(here("data/raw/drone_imagery/ABDB6_Orthomosaic_TueAug08214151658091/ABDB6_Orthomosaic_export_TueAug08214151658091.tif"))
ab_db_06_elev <- rast(here("data/raw/drone_imagery/ABDB6_ElevationToolbox_TueAug08215211180124/ABDB6_ElevationToolbox_export_TueAug08215211180124.tif")) %>% 
  subset(., 1:3)
ab_db_06_outer <- st_read(here("data/raw/wetland_polygons/AB_DB_06_veg_outer.gpkg"))
ab_db_06_outer_2 <- st_read(here("data/raw/wetland_polygons/AB_DB_06_veg_outer_2.gpkg"))
ab_db_06_inner <- st_read(here("data/raw/wetland_polygons/AB_DB_06_veg_inner.gpkg"))
ab_db_06_rgb_resamp <- resample(ab_db_06_rgb, ab_db_06_elev)

ab_db_09_rgb <- rast(here("data/raw/drone_imagery/ABDB9_Orthomosaic_TueAug08220320715342/ABDB9_Orthomosaic_export_TueAug08220320715342.tif"))
ab_db_09_elev <- rast(here("data/raw/drone_imagery/ABDB9_ElevationToolbox_TueAug08220706970423/ABDB9_ElevationToolbox_export_TueAug08220706970423.tif")) %>% 
  subset(., 1:3)
ab_db_09_outer <- st_read(here("data/raw/wetland_polygons/AB_DB_09_veg_outer.gpkg"))
ab_db_09_inner <- st_read(here("data/raw/wetland_polygons/AB_DB_09_veg_inner.gpkg"))
ab_db_09_rgb_resamp <- resample(ab_db_09_rgb, ab_db_09_elev)

ab_db_15_rgb <- rast(here("data/raw/drone_imagery/ABDB15_Orthomosaic_TueAug08220357079803/ABDB15_Orthomosaic_export_TueAug08220357079803.tif"))
ab_db_15_elev <- rast(here("data/raw/drone_imagery/ABDB15_ElevationToolbox_TueAug08220420279588/ABDB15_ElevationToolbox_export_TueAug08220420279588.tif")) %>% 
  subset(., 1:3)
ab_db_15_outer <- st_read(here("data/raw/wetland_polygons/AB_DB_15_veg_outer.gpkg"))
ab_db_15_outer_2 <- st_read(here("data/raw/wetland_polygons/AB_DB_15_veg_outer_2.gpkg"))
ab_db_15_inner <- st_read(here("data/raw/wetland_polygons/AB_DB_15_veg_inner.gpkg"))
ab_db_15_rgb_resamp <- resample(ab_db_15_rgb, ab_db_15_elev)

## SK - Moosejaw ----

sk_c_06_rgb <- rast(here("data/raw/drone_imagery/SK_images/PK6_RGB.tif"))
sk_c_06_elev <- rast(here("data/raw/drone_imagery/SK_images/PK6_dtm.tif"))
sk_c_06_outer <- st_read(here("data/raw/wetland_polygons/SK_C_06_veg_outer.gpkg"))
sk_c_06_outer_2 <- st_read(here("data/raw/wetland_polygons/SK_C_06_veg_outer_2.gpkg"))
sk_c_06_inner <- st_read(here("data/raw/wetland_polygons/SK_C_06_veg_inner.gpkg"))
sk_c_06_rgb_resamp <- resample(sk_c_06_rgb, sk_c_06_elev)

sk_c_10_rgb <- rast(here("data/raw/drone_imagery/SK_images/PK10_RGB.tif"))
sk_c_10_elev <- rast(here("data/raw/drone_imagery/SK_images/PK10_dtm.tif"))
sk_c_10_outer <- st_read(here("data/raw/wetland_polygons/SK_C_10_veg_outer.gpkg"))
sk_c_10_outer_2 <- st_read(here("data/raw/wetland_polygons/SK_C_10_veg_outer_2.gpkg"))
sk_c_10_inner <- st_read(here("data/raw/wetland_polygons/SK_C_10_veg_inner.gpkg"))
sk_c_10_rgb_resamp <- resample(sk_c_10_rgb, sk_c_10_elev)

sk_c_11_rgb <- rast(here("data/raw/drone_imagery/SK_images/PK11_RGB.tif"))
sk_c_11_elev <- rast(here("data/raw/drone_imagery/SK_images/PK11_dtm.tif"))
sk_c_11_outer <- st_read(here("data/raw/wetland_polygons/SK_C_11_veg_outer.gpkg"))
sk_c_11_outer_2 <- st_read(here("data/raw/wetland_polygons/SK_C_11_veg_outer_2.gpkg"))
sk_c_11_inner <- st_read(here("data/raw/wetland_polygons/SK_C_11_veg_inner.gpkg"))
sk_c_11_rgb_resamp <- resample(sk_c_11_rgb, sk_c_11_elev)

sk_c_13_rgb <- rast(here("data/raw/drone_imagery/SK_images/PK13_RGB.tif"))
sk_c_13_elev <- rast(here("data/raw/drone_imagery/SK_images/PK13_dtm.tif"))
sk_c_13_outer <- st_read(here("data/raw/wetland_polygons/SK_C_13_veg_outer.gpkg"))
sk_c_13_outer_2 <- st_read(here("data/raw/wetland_polygons/SK_C_13_veg_outer_2.gpkg"))
sk_c_13_inner <- st_read(here("data/raw/wetland_polygons/SK_C_13_veg_inner.gpkg"))
sk_c_13_rgb_resamp <- resample(sk_c_13_rgb, sk_c_13_elev)

sk_c_16_rgb <- rast(here("data/raw/drone_imagery/SK_images/PK16_RGB.tif"))
sk_c_16_elev <- rast(here("data/raw/drone_imagery/SK_images/PK16_dtm.tif"))
sk_c_16_outer <- st_read(here("data/raw/wetland_polygons/SK_C_16_veg_outer.gpkg"))
sk_c_16_inner <- st_read(here("data/raw/wetland_polygons/SK_C_16_veg_inner.gpkg"))
sk_c_16_rgb_resamp <- resample(sk_c_16_rgb, sk_c_16_elev)



# Function

get_wetland_cluster <- function(wetland, outer, inner, n_clus) {
  print("Process inputs")
  # preprocess wetland raster
  r <- wetland %>% subset(., 1:3) # subset to RGB
  names(r) <- c("red", "green", "blue") # rename bands
  
  # preprocess wetland veg edges
  out_edge <- outer %>% 
    st_transform(crs = 2957) # project to match raster
  inn_edge <- inner %>% 
    st_transform(crs = 2957) # project to match raster
  
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

## AB - Camrose ----
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

## AB - Brooks ----
ab_b_05_clust <- get_wetland_cluster(
  ab_b_05_rgb_resamp, 
  ab_b_05_outer, 
  ab_b_05_inner, 
  4)
plot(ab_b_05_clust)
writeRaster(ab_b_05_clust, here("data/processed/ab_b_05_clust_k4.tif"), overwrite = T) # use k2, band 2

ab_b_06_clust <- get_wetland_cluster(
  ab_b_06_rgb_resamp, 
  ab_b_06_outer, 
  ab_b_06_inner, 
  4)
plot(ab_b_06_clust)
writeRaster(ab_b_06_clust, here("data/processed/ab_b_06_clust_k4.tif"), overwrite = T) # use k4, bands 1, 4

ab_b_07_clust <- get_wetland_cluster(
  ab_b_07_rgb_resamp, 
  ab_b_07_outer, 
  ab_b_07_inner, 
  6)
plot(ab_b_07_clust)
writeRaster(ab_b_07_clust, here("data/processed/ab_b_07_clust_k6.tif"), overwrite = T) # use k6, bands 4&5, outer veg 2

ab_b_08_clust <- get_wetland_cluster(
  ab_b_08_rgb_resamp, 
  ab_b_08_outer_2, 
  ab_b_08_inner, 
  7)
plot(ab_b_08_clust)
writeRaster(ab_b_08_clust, here("data/processed/ab_b_08_clust_k7_2.tif"), overwrite = T) # use k6, bands 1, 4, 5

ab_b_14_clust <- get_wetland_cluster(
  ab_b_14_rgb_resamp, 
  ab_b_14_outer, 
  ab_b_14_inner, 
  5)
plot(ab_b_14_clust)
writeRaster(ab_b_14_clust, here("data/processed/ab_b_14_clust_k5.tif"), overwrite = T)

## AB - Vulcan ----
ab_db_03_clust <- get_wetland_cluster(
  ab_db_03_rgb_resamp, 
  ab_db_03_outer, 
  ab_db_03_inner, 
  5)
plot(ab_db_03_clust)
writeRaster(ab_db_03_clust, here("data/processed/ab_db_03_clust_k5.tif"), overwrite = T)

ab_db_06_clust <- get_wetland_cluster(
  ab_db_06_rgb_resamp, 
  ab_db_06_outer_2, 
  ab_db_06_inner, 
  5)
plot(ab_db_06_clust)
writeRaster(ab_db_06_clust, here("data/processed/ab_db_06_clust_k5_2.tif"), overwrite = T) # use k5, band 2, veg outer 2

ab_db_09_clust <- get_wetland_cluster(
  ab_db_09_rgb_resamp, 
  ab_db_09_outer, 
  ab_db_09_inner, 
  5)
plot(ab_db_09_clust)
writeRaster(ab_db_09_clust, here("data/processed/ab_db_09_clust_k5.tif"), overwrite = T)

ab_db_15_clust <- get_wetland_cluster(
  ab_db_15_rgb_resamp, 
  ab_db_15_outer_2, 
  ab_db_15_inner, 
  5)
plot(ab_db_15_clust)
writeRaster(ab_db_15_clust, here("data/processed/ab_db_15_clust_k5_2.tif"), overwrite = T) # use k2, band 1

## SK - Moosejaw ----
sk_c_06_clust <- get_wetland_cluster(
  sk_c_06_rgb_resamp, 
  sk_c_06_outer_2, 
  sk_c_06_inner, 
  4)
plot(sk_c_06_clust)
writeRaster(sk_c_06_clust, here("data/processed/sk_c_06_clust_k4_2.tif"), overwrite = T) # use k2, band 2, veg outer 2

sk_c_10_clust <- get_wetland_cluster(
  sk_c_10_rgb_resamp, 
  sk_c_10_outer_2, 
  sk_c_10_inner, 
  5)
plot(sk_c_10_clust)
writeRaster(sk_c_10_clust, here("data/processed/sk_c_10_clust_k5_2.tif"), overwrite = T) # use k3, band 1, veg outer 2

sk_c_11_clust <- get_wetland_cluster(
  sk_c_11_rgb_resamp, 
  sk_c_11_outer_2, 
  sk_c_11_inner, 
  5)
plot(sk_c_11_clust)
writeRaster(sk_c_11_clust, here("data/processed/sk_c_11_clust_k5_2.tif"), overwrite = T) # use k5, bands 3&4, veg outer 2


sk_c_13_clust <- get_wetland_cluster(
  sk_c_13_rgb_resamp, 
  sk_c_13_outer_2, 
  sk_c_13_inner, 
  5)
plot(sk_c_13_clust)
writeRaster(sk_c_13_clust, here("data/processed/sk_c_13_clust_k5_2.tif"), overwrite = T) # use k4, bands 2&4, veg outer 2

sk_c_16_clust <- get_wetland_cluster(
  sk_c_16_rgb_resamp, 
  sk_c_16_outer, 
  sk_c_16_inner, 
  7)
plot(sk_c_16_clust)
writeRaster(sk_c_16_clust, here("data/processed/sk_c_16_clust_k7.tif"), overwrite = T) # use k4, band 3


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

ab_b_05_clust <- rast(here("data/processed/ab_b_05_clust_k2.tif")) # use band 2
ab_b_06_clust <- rast(here("data/processed/ab_b_06_clust_k4.tif")) # use band 1&2
ab_b_07_clust <- rast(here("data/processed/ab_b_07_clust_k6.tif")) # use band 4&5
ab_b_08_clust <- rast(here("data/processed/ab_b_08_clust_k6_2.tif")) # use band 1,4&5

ab_db_06_clust <- rast(here("data/processed/ab_db_06_clust_k5_2.tif")) # use band 2
ab_db_15_clust <- rast(here("data/processed/ab_db_15_clust_k2.tif")) # use band 1

sk_c_06_clust <- rast(here("data/processed/sk_c_06_clust_k2_2.tif")) # use band 2
sk_c_10_clust <- rast(here("data/processed/sk_c_10_clust_k3_2.tif")) # use band 1
sk_c_11_clust <- rast(here("data/processed/sk_c_11_clust_k5_2.tif")) # use band 3&4
sk_c_13_clust <- rast(here("data/processed/sk_c_13_clust_k4_2.tif")) # use band 2&4
sk_c_16_clust <- rast(here("data/processed/sk_c_16_clust_k4.tif")) # use band 3

## AB - Camrose ---- 
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

## AB - Brooks ----
ab_b_05_area <- expanse(ab_b_05_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(2)) %>% 
  mutate(site = "AB_B_05")
write_csv(ab_b_05_area, here("data/processed/ab_b_05_clust_area.csv"))

ab_b_06_area <- expanse(ab_b_06_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(1,2)) %>% 
  mutate(site = "AB_B_06")
write_csv(ab_b_06_area, here("data/processed/ab_b_06_clust_area.csv"))

ab_b_07_area <- expanse(ab_b_07_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(4,5)) %>% 
  mutate(site = "AB_B_07")
write_csv(ab_b_07_area, here("data/processed/ab_b_07_clust_area.csv"))

ab_b_08_area <- expanse(ab_b_08_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(1,4,5)) %>% 
  mutate(site = "AB_B_08")
write_csv(ab_b_08_area, here("data/processed/ab_b_08_clust_area.csv"))

## AB - Vulcan ----
ab_db_06_area <- expanse(ab_db_06_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(2)) %>% 
  mutate(site = "AB_DB_06")
write_csv(ab_db_06_area, here("data/processed/ab_db_06_clust_area.csv"))

ab_db_15_area <- expanse(ab_db_15_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(1)) %>% 
  mutate(site = "AB_DB_15")
write_csv(ab_db_15_area, here("data/processed/ab_db_15_clust_area.csv"))

## SK - Moosejaw ---- 
sk_c_06_area <- expanse(sk_c_06_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(2)) %>% 
  mutate(site = "SK_C_06")
write_csv(sk_c_06_area, here("data/processed/sk_c_06_clust_area.csv"))

sk_c_10_area <- expanse(sk_c_10_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(1)) %>% 
  mutate(site = "SK_C_10")
write_csv(sk_c_10_area, here("data/processed/sk_c_10_clust_area.csv"))

sk_c_11_area <- expanse(sk_c_11_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(3,4)) %>% 
  mutate(site = "SK_C_11")
write_csv(sk_c_11_area, here("data/processed/sk_c_11_clust_area.csv"))

sk_c_13_area <- expanse(sk_c_13_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(2,4)) %>% 
  mutate(site = "SK_C_13")
write_csv(sk_c_13_area, here("data/processed/sk_c_13_clust_area.csv"))

sk_c_16_area <- expanse(sk_c_16_clust, unit = "m", byValue = T) %>% 
  as.data.frame() %>% 
  filter(value %in% c(3)) %>% 
  mutate(site = "SK_C_16")
write_csv(sk_c_16_area, here("data/processed/sk_c_16_clust_area.csv"))

# Calculate veg extent ----------------------------------------------------

get_veg_zone_ab <- function(outer, inner) {
  out_edge <- outer %>% 
    st_transform(crs = 2956) # project to match raster
  inn_edge <- inner %>% 
    st_transform(crs = 2956)
  veg_zone <- st_difference(out_edge, inn_edge)
  return(veg_zone)
}

get_veg_zone_sk <- function(outer, inner) {
  out_edge <- outer %>% 
    st_transform(crs = 2957) # project to match raster
  inn_edge <- inner %>% 
    st_transform(crs = 2957)
  veg_zone <- st_difference(out_edge, inn_edge)
  return(veg_zone)
}

## Do not process since these were dry
avoids <- c("AB_DB_01", "AB_DB_02", "AB_DB_03", "AB_DB_04", "AB_DB_05", 
            "AB_DB_07", "AB_DB_08", "AB_DB_09", "AB_DB_10", "AB_DB_11",
            "AB_DB_12", "AB_DB_13", "AB_DB_14", "AB_DB_16", 
            "AB_B_03", "AB_B_09", "AB_B_10", "AB_B_11", "AB_B_12", 
            "AB_B_13", "AB_B_14", "AB_B_15")

outer_veg_poly_ls <- list.files(path = here("data/raw/wetland_polygons"), 
                       pattern = "veg_outer.gpkg", 
                       full.names = T) %>% 
  .[map_lgl(., ~ !any(str_detect(., avoids)))] %>% 
  map(st_read)

inner_veg_poly_ls <- list.files(path = here("data/raw/wetland_polygons"), 
                             pattern = "veg_inner", 
                             full.names = T) %>% 
  .[map_lgl(., ~ !any(str_detect(., avoids)))] %>% 
  map(st_read)

veg_zones_ls_ab <- map2(outer_veg_poly_ls[1:15], inner_veg_poly_ls[1:15], get_veg_zone_ab)
veg_zones_ls_sk <- map2(outer_veg_poly_ls[16:20], inner_veg_poly_ls[16:20], get_veg_zone_sk)

veg_zones_ls <- c(veg_zones_ls_ab, veg_zones_ls_sk)

clust_rast_ls <- list.files(path = here("data/processed"), 
           pattern = "clust_k\\d", 
           full.names = T) %>% 
  map(rast)

names_ls <- list.files(path = here("data/processed"), 
           pattern = "clust_k\\d", 
           full.names = F) %>% 
  map( ~ .x %>% 
         str_replace(., "_clust_k\\d.tif", "")) %>% 
  map( ~ .x %>% 
         str_replace(., "_clust_k\\d_\\d.tif", "")) %>% 
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
                                     site == "AB_B_05" & value %in% c(2) ~ "Y", 
                                     site == "AB_B_06" & value %in% c(1,2) ~ "Y", 
                                     site == "AB_B_07" & value %in% c(4,5) ~ "Y", 
                                     site == "AB_B_08" & value %in% c(1,4,5) ~ "Y", 
                                     site == "AB_DB_06" & value %in% c(2) ~ "Y", 
                                     site == "AB_DB_15" & value %in% c(1) ~ "Y", 
                                     site == "SK_C_06" & value %in% c(2) ~ "Y", 
                                     site == "SK_C_10" & value %in% c(1) ~ "Y", 
                                     site == "SK_C_11" & value %in% c(3,4) ~ "Y", 
                                     site == "SK_C_13" & value %in% c(2,4) ~ "Y", 
                                     site == "SK_C_16" & value %in% c(3) ~ "Y", 
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

pred_wetland_biomass_ab <- function(name, wetland, outer, inner, clust, bands, mod) {
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
    subst(., c(c(1, 2, 3, 4, 5, 6)[-bands]), NA)
  
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

pred_wetland_biomass_sk <- function(name, wetland, outer, inner, clust, bands, mod) {
  print("Process inputs")
  # preprocess wetland raster
  r <- wetland %>% subset(., 1:3) # subset to RGB
  names(r) <- c("red", "green", "blue") # rename bands
  
  # preprocess wetland veg edges
  out_edge <- outer %>% 
    st_transform(crs = 2957) # project to match raster
  inn_edge <- inner %>% 
    st_transform(crs = 2957) # project to match raster
  
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

## AB - Camrose ----
ab_02_pred <- pred_wetland_biomass_ab("AB_02", 
                                   ab_02_rgb_resamp, 
                                   ab_02_outer, 
                                   ab_02_inner, 
                                   ab_02_clust, 
                                   c(2,3,4), 
                                   sat_mod)

ab_03_pred <- pred_wetland_biomass_ab("AB_03", 
                                   ab_03_rgb_resamp, 
                                   ab_03_outer, 
                                   ab_03_inner, 
                                   ab_03_clust, 
                                   c(3,4), 
                                   sat_mod)

ab_04_pred <- pred_wetland_biomass_ab("AB_04", 
                                   ab_04_rgb, 
                                   ab_04_outer, 
                                   ab_04_inner, 
                                   ab_04_clust, 
                                   c(2), 
                                   sat_mod)

ab_05_pred <- pred_wetland_biomass_ab("AB_05", 
                                   ab_05_rgb_resamp, 
                                   ab_05_outer, 
                                   ab_05_inner, 
                                   ab_05_clust, 
                                   c(1,2), 
                                   sat_mod)

ab_06_pred <- pred_wetland_biomass_ab("AB_06", 
                                   ab_06_rgb, 
                                   ab_06_outer, 
                                   ab_06_inner, 
                                   ab_06_clust, 
                                   c(3), 
                                   sat_mod)

ab_10_pred <- pred_wetland_biomass_ab("AB_10", 
                                   ab_10_rgb, 
                                   ab_10_outer, 
                                   ab_10_inner, 
                                   ab_10_clust, 
                                   c(1,2), 
                                   sat_mod)

ab_11_pred <- pred_wetland_biomass_ab("AB_11", 
                                   ab_11_rgb, 
                                   ab_11_outer, 
                                   ab_11_inner, 
                                   ab_11_clust, 
                                   c(1,3), 
                                   sat_mod)

ab_12_pred <- pred_wetland_biomass_ab("AB_12", 
                                   ab_12_rgb, 
                                   ab_12_outer, 
                                   ab_12_inner, 
                                   ab_12_clust, 
                                   c(2,3), 
                                   sat_mod)

ab_15_pred <- pred_wetland_biomass_ab("AB_15", 
                                      ab_15_rgb, 
                                      ab_15_outer, 
                                      ab_15_inner, 
                                      ab_15_clust, 
                                      c(2), 
                                      sat_mod)
                         
## AB - Brooks ----
ab_b_05_pred <- pred_wetland_biomass_ab("AB_B_05", 
                                   ab_b_05_rgb_resamp, 
                                   ab_b_05_outer, 
                                   ab_b_05_inner, 
                                   ab_b_05_clust, 
                                   c(2), 
                                   sat_mod)

ab_b_06_pred <- pred_wetland_biomass_ab("AB_B_06", 
                                   ab_b_06_rgb_resamp, 
                                   ab_b_06_outer, 
                                   ab_b_06_inner, 
                                   ab_b_06_clust, 
                                   c(1,2), 
                                   sat_mod)

ab_b_07_pred <- pred_wetland_biomass_ab("AB_B_07", 
                                   ab_b_07_rgb_resamp, 
                                   ab_b_07_outer, 
                                   ab_b_07_inner, 
                                   ab_b_07_clust, 
                                   c(4,5), 
                                   sat_mod)

ab_b_08_pred <- pred_wetland_biomass_ab("AB_B_08", 
                                   ab_b_08_rgb_resamp, 
                                   ab_b_08_outer_2, 
                                   ab_b_08_inner, 
                                   ab_b_08_clust, 
                                   c(1,4,5), 
                                   sat_mod)

## AB - Vulcan ----
ab_db_06_pred <- pred_wetland_biomass_ab("AB_DB_06", 
                                        ab_db_06_rgb_resamp, 
                                        ab_db_06_outer_2, 
                                        ab_db_06_inner, 
                                        ab_db_06_clust, 
                                        c(2), 
                                        sat_mod)

ab_db_15_pred <- pred_wetland_biomass_ab("AB_DB_15", 
                                         ab_db_15_rgb_resamp, 
                                         ab_db_15_outer, 
                                         ab_db_15_inner, 
                                         ab_db_15_clust, 
                                         c(1), 
                                         sat_mod)

## SK - Moosejaw ----
sk_c_06_pred <- pred_wetland_biomass_sk("SK_C_06", 
                                        sk_c_06_rgb_resamp, 
                                        sk_c_06_outer_2, 
                                        sk_c_06_inner, 
                                        sk_c_06_clust, 
                                        c(2), 
                                        sat_mod)

sk_c_10_pred <- pred_wetland_biomass_sk("SK_C_10", 
                                        sk_c_10_rgb_resamp, 
                                        sk_c_10_outer_2, 
                                        sk_c_10_inner, 
                                        sk_c_10_clust, 
                                        c(1), 
                                        sat_mod)

sk_c_11_pred <- pred_wetland_biomass_sk("SK_C_11", 
                                        sk_c_11_rgb_resamp, 
                                        sk_c_11_outer_2, 
                                        sk_c_11_inner, 
                                        sk_c_11_clust, 
                                        c(3,4), 
                                        sat_mod)

sk_c_13_pred <- pred_wetland_biomass_sk("SK_C_13", 
                                        sk_c_13_rgb_resamp, 
                                        sk_c_13_outer_2, 
                                        sk_c_13_inner, 
                                        sk_c_13_clust, 
                                        c(2,4), 
                                        sat_mod)

sk_c_16_pred <- pred_wetland_biomass_sk("SK_C_16", 
                                        sk_c_16_rgb_resamp, 
                                        sk_c_16_outer, 
                                        sk_c_16_inner, 
                                        sk_c_16_clust, 
                                        c(3), 
                                        sat_mod)

spectra_biomass_df <- bind_rows(ab_02_pred,
                                ab_03_pred,
                                ab_04_pred,
                                ab_05_pred,
                                ab_06_pred,
                                ab_10_pred,
                                ab_11_pred,
                                ab_12_pred,
                                ab_15_pred,
                                ab_b_05_pred,
                                ab_b_06_pred,
                                ab_b_07_pred,
                                ab_b_08_pred,
                                ab_db_06_pred,
                                ab_db_15_pred,
                                sk_c_06_pred, 
                                sk_c_10_pred, 
                                sk_c_11_pred, 
                                sk_c_13_pred, 
                                sk_c_16_pred)
spectra_biomass_df

write_csv(spectra_biomass_df, here("data/processed/spectra_biomass_df.csv"))

