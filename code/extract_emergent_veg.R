## ---------------------------
##
## Script name: extract_emergent_veg
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
##   
##
## ---------------------------

options(scipen = 6, digits = 4)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

## Classification raster

fl_b1_clust <- rast(here("output/clust_all_b1_k3.tif"))
fl_b2_clust <- rast(here("output/clust_all_b2_k3.tif"))
fl_b3_clust <- rast(here("output/clust_all_b3_k3.tif"))

## Zones

fl_b1_regions <- vect(here("data/processed/fl_b1_regions.gpkg"))
fl_b2_regions <- vect(here("data/processed/fl_b2_regions.gpkg"))
fl_b3_regions <- vect(here("data/processed/fl_b3_regions.gpkg"))


# Extract veg -------------------------------------------------------------

## Basin 1
r <- as.numeric(fl_b1_clust)
names(r) <- "class"

b1_veg <- terra::extract(r, fl_b1_regions, ID = T, weights = T) %>%
  as.data.frame() %>%   
  # join wedge section areas using the ID column as guide
  left_join(., fl_b1_regions %>%
              expanse(unit = "m") %>% 
              as.data.frame() %>%
              rename(zone_area_m2 = ".") %>% 
              mutate(ID = row_number())) %>%
  # calc area of each class as function of amount of cell in section
  mutate(area_m2 = weight*(1.2*1.2)) %>%
  # sum within class and section id
  group_by(ID, class) %>%
  summarise(class_area_m2 = sum(area_m2),
            zone_area_m2 = mean(zone_area_m2)) %>%
  # calc total area and prop of each class
  mutate(total_area_m2 = sum(class_area_m2),
         class_area_ha = class_area_m2/10000,
         zone_area_ha = zone_area_m2/10000,
         prop_area = class_area_m2/total_area_m2,
         method = "clara_all_data")

### K = 3
b1_veg_plot <- b1_veg %>% 
  mutate(veg_type = case_when(class == 2 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  ggplot(aes(x = ID, y = em_area_ha)) + 
  geom_col(color = "black") + 
  scale_y_continuous(limits = c(0, 50), 
                     expand = c(0,0),
                     name = "Emergent vegetation (ha)") + 
  scale_x_continuous(name = "Zone") + 
  ggtitle("Basin 1") +
  theme_classic()

b1_prop_plot <- b1_veg %>% 
  mutate(veg_type = case_when(class == 2 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  ggplot(aes(x = ID, y = prop_area)) + 
  geom_col(color = "black") + 
  scale_y_continuous(limits = c(0, 100), 
                     expand = c(0,0),
                     name = "Emergent vegetation (%)") + 
  scale_x_continuous(name = "Zone") + 
  ggtitle("Basin 1") +
  theme_classic()

### K = 4
b1_veg_plot <- b1_veg %>% 
  mutate(veg_type = case_when(class == 2 | class == 3 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  ggplot(aes(x = ID, y = em_area_ha)) + 
  geom_col(color = "black") + 
  scale_y_continuous(limits = c(0, 25), 
                     expand = c(0,0),
                     name = "Emergent vegetation (ha)") + 
  scale_x_continuous(name = "Zone") + 
  ggtitle("Basin 1") +
  theme_classic()

b1_prop_plot <- b1_veg %>% 
  mutate(veg_type = case_when(class == 2 | class == 3 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  ggplot(aes(x = ID, y = prop_area)) + 
  geom_col(color = "black") + 
  scale_y_continuous(limits = c(0, 100), 
                     expand = c(0,0),
                     name = "Emergent vegetation (%)") + 
  scale_x_continuous(name = "Zone") + 
  ggtitle("Basin 1") +
  theme_classic()

b1_veg_df <- b1_veg %>% 
  mutate(basin = 1, 
         veg_type = case_when(class == 2 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(basin, ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  rename(region = ID)

## Basin 2

r <- as.numeric(fl_b2_clust)
names(r) <- "class"
plot(r)

b2_veg <- terra::extract(r, fl_b2_regions, ID = T, weights = T) %>%
  as.data.frame() %>%   
  # join wedge section areas using the ID column as guide
  left_join(., fl_b2_regions %>%
              expanse(unit = "m") %>% 
              as.data.frame() %>%
              rename(zone_area_m2 = ".") %>% 
              mutate(ID = row_number())) %>%
  # calc area of each class as function of amount of cell in section
  mutate(area_m2 = weight*(1.2*1.2)) %>%
  # sum within class and section id
  group_by(ID, class) %>%
  summarise(class_area_m2 = sum(area_m2),
            zone_area_m2 = mean(zone_area_m2)) %>%
  # calc total area and prop of each class
  mutate(total_area_m2 = sum(class_area_m2),
         class_area_ha = class_area_m2/10000,
         zone_area_ha = zone_area_m2/10000,
         prop_area = class_area_m2/total_area_m2,
         method = "clara_all_data")

b2_veg_plot <- b2_veg %>% 
  mutate(veg_type = case_when(class == 2 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  ggplot(aes(x = ID, y = em_area_ha)) + 
  geom_col(color = "black") + 
  scale_y_continuous(limits = c(0, 50), 
                     expand = c(0,0),
                     name = "Emergent vegetation (ha)") + 
  scale_x_continuous(name = "Zone") + 
  ggtitle("Basin 2") +
  theme_classic()

b2_prop_plot <- b2_veg %>% 
  mutate(veg_type = case_when(class == 2  ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  ggplot(aes(x = ID, y = prop_area)) + 
  geom_col(color = "black") + 
  scale_y_continuous(limits = c(0, 100), 
                     expand = c(0,0),
                     name = "Emergent vegetation (%)") + 
  scale_x_continuous(name = "Zone") + 
  ggtitle("Basin 2") +
  theme_classic()

b2_veg_df <- b2_veg %>% 
  mutate(basin = 2, 
         veg_type = case_when(class == 2 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(basin, ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  rename(region = ID)

## Basin 3

r <- as.numeric(fl_b3_clust)
names(r) <- "class"
plot(r)

b3_veg <- terra::extract(r, fl_b3_regions, ID = T, weights = T) %>%
  as.data.frame() %>%   
  # join wedge section areas using the ID column as guide
  left_join(., fl_b3_regions %>%
              expanse(unit = "m") %>% 
              as.data.frame() %>%
              rename(zone_area_m2 = ".") %>% 
              mutate(ID = row_number())) %>%
  # calc area of each class as function of amount of cell in section
  mutate(area_m2 = weight*(1.2*1.2)) %>%
  # sum within class and section id
  group_by(ID, class) %>%
  summarise(class_area_m2 = sum(area_m2),
            zone_area_m2 = mean(zone_area_m2)) %>%
  # calc total area and prop of each class
  mutate(total_area_m2 = sum(class_area_m2),
         class_area_ha = class_area_m2/10000,
         zone_area_ha = zone_area_m2/10000,
         prop_area = class_area_m2/total_area_m2,
         method = "clara_all_data")

b3_veg_plot <- b3_veg %>% 
  mutate(veg_type = case_when(class == 1 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  ggplot(aes(x = ID, y = em_area_ha)) + 
  geom_col(color = "black") + 
  scale_y_continuous(limits = c(0, 50), 
                     expand = c(0,0),
                     name = "Emergent vegetation (ha)") + 
  scale_x_continuous(name = "Zone") + 
  ggtitle("Basin 3") +
  theme_classic()

b3_prop_plot <- b3_veg %>% 
  mutate(veg_type = case_when(class == 1  ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  ggplot(aes(x = ID, y = prop_area)) + 
  geom_col(color = "black") + 
  scale_y_continuous(limits = c(0, 100), 
                     expand = c(0,0),
                     name = "Emergent vegetation (%)") + 
  scale_x_continuous(name = "Zone") + 
  ggtitle("Basin 3") +
  theme_classic()

b3_veg_df <- b3_veg %>% 
  mutate(basin = 3, 
         veg_type = case_when(class == 1 ~ "em_veg",
                              TRUE ~ "no_veg")) %>% 
  group_by(basin, ID, veg_type) %>% 
  summarise(em_area_ha = round(sum(class_area_ha), 1),
            prop_area = round(sum(prop_area)*100)) %>% 
  filter(veg_type == "em_veg") %>% 
  rename(region = ID)


# Combine data ------------------------------------------------------------

## DF
fl_em_veg_df <- bind_rows(b1_veg_df, b2_veg_df, b3_veg_df)

## Plots
library(patchwork)

(b1_veg_plot + b2_veg_plot + b3_veg_plot) / 
  (b1_prop_plot + b2_prop_plot + b3_prop_plot) 



# Sample matching ---------------------------------------------------------

sample_region_df <- tibble(
  basin =c(1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3),
  regions =c(4,5,1,2,3,4,3,4,5,5,1,1,2,2,2,3,3,3,3,4,4,4,4,4),
  sample_id =c("B1-1", "B1-2", "B1-3", "B1-4", "B1-5", "B1-6", 
                "B2-1", "B2-2", "B2-3", "B2-4", "B2-5", 
                "B4-1", "B4-2", "B4-3", "B4-4", 
                "B3E-1", "B3E-2", "B3E-3", "B3E-4", 
                "B3W-1", "B3W-2", "B3W-T1", "B3W-T2", "B3W-T3"))


# Save output -------------------------------------------------------------

fl_em_veg_df <- write_csv(fl_em_veg_df, here("output/fl_em_veg_df.csv"))

sample_region_df <- write_csv(sample_region_df, here("output/sample_region_df.csv"))


