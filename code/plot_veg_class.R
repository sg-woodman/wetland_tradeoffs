library(tidyterra)
library(terra)
library(ggspatial)


## Worldview

fl_multi_spec <- rast(here("data/raw/fl_multi_spec.tif"))

## Boundaries
fl_b1_shoreline_boundary <- vect(here("data/raw/fl_b1_shoreline_boundary.gpkg"))
fl_b2_shoreline_boundary <- vect(here("data/raw/fl_b2_shoreline_boundary.gpkg"))
fl_b3_shoreline_boundary <- vect(here("data/raw/fl_b3_shoreline_boundary.gpkg"))

## Veg class
fl_b1_clust <- rast(here("output/clust_all_b1_k3.tif")) 
levels(fl_b1_clust) <- data.frame(value=1:3, class=c("Bare", "Vegetation", "Water"))

fl_b2_clust <- rast(here("output/clust_all_b2_k3.tif"))
levels(fl_b2_clust) <- data.frame(value=1:3, class=c("Bare", "Vegetation", "Water"))

fl_b3_clust <- rast(here("output/clust_all_b3_k3.tif"))
levels(fl_b3_clust) <- data.frame(value=1:3, class=c("Bare", "Vegetation", "Water"))


worldview_plot <- ggplot() +
  geom_spatraster_rgb(data = fl_multi_spec, 
                      r = 5, g = 3, b = 2) + 
  annotation_scale(location = "bl", width_hint = 0.2) +
  theme_classic()

fl_class_plot <- ggplot() + 
  geom_spatvector(data = fl_b1_shoreline_boundary, fill = "lightblue") +
  geom_spatvector(data = fl_b2_shoreline_boundary, fill = "lightblue") +
  geom_spatvector(data = fl_b3_shoreline_boundary, fill = "lightblue") +
  geom_spatraster(data = fl_b1_clust) + 
  geom_spatraster(data = fl_b2_clust) + 
  geom_spatraster(data = fl_b3_clust) + 
  scale_fill_manual(name = "Class", 
                    values = c("#EABE7C", "#679436", "#064789"), 
                    na.value = NA, na.translate=FALSE) + 
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_classic() + 
  theme(legend.position = c(0.15, 0.15))
  
fl_b1_class_plot <- ggplot() + 
  geom_spatvector(data = fl_b1_shoreline_boundary, fill = "white") +
  geom_spatraster(data = fl_b1_clust) + 
  scale_fill_manual(name = "Class", 
                  values = c("#EABE7C", "#679436", "#064789"), 
                  na.value = NA, na.translate=FALSE) + 
  ggtitle("Basin 1") + 
  annotation_scale(location = "bl", width_hint = 0.2) +
  theme_classic() + 
  theme(legend.position = c(0.1, 0.85))

fl_b2_class_plot <- ggplot() + 
  geom_spatvector(data = fl_b2_shoreline_boundary, fill = "white") +
  geom_spatraster(data = fl_b2_clust) + 
  scale_fill_manual(name = "Class", 
                    values = c("#EABE7C", "#679436", "#064789"), 
                    na.value = NA, na.translate=FALSE) + 
  ggtitle("Basin 2") + 
  annotation_scale(location = "bl", width_hint = 0.2) +
  theme_classic() + 
  theme(legend.position = c(0.15, 0.2))

fl_b3_class_plot <- ggplot() + 
  geom_spatvector(data = fl_b3_shoreline_boundary, fill = "white") +
  geom_spatraster(data = fl_b3_clust) + 
  scale_fill_manual(name = "Class", 
                    values = c("#EABE7C", "#679436", "#064789"), 
                    na.value = NA, na.translate=FALSE) + 
  ggtitle("Basin 3") + 
  annotation_scale(location = "bl", width_hint = 0.2) +
  theme_classic() + 
  theme(legend.position = c(0.9, 0.85))


ggsave(here("output/fl_veg_class_map.pdf"), 
       plot = fl_class_plot,
       dpi = 300, dev = "pdf", height = 18, width = 16, unit = "cm")

ggsave(here("output/fl_b1_veg_class_map.pdf"), 
       plot = fl_b1_class_plot,
       dpi = 300, dev = "pdf", height = 18, width = 16, unit = "cm")

ggsave(here("output/fl_b2_veg_class_map.pdf"), 
       plot = fl_b2_class_plot,
       dpi = 300, dev = "pdf", height = 18, width = 16, unit = "cm")

ggsave(here("output/fl_b3_veg_class_map.pdf"), 
       plot = fl_b3_class_plot,
       dpi = 300, dev = "pdf", height = 18, width = 16, unit = "cm")

library(patchwork)

fl_b1_class_plot + fl_b2_class_plot + fl_b3_class_plot



