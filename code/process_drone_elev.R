install.packages("SpatialPack")
library(SpatialPack)
library(spatialEco)



RGB(ab_15_elev) <- c(1, 2, 3)
plot(colorize(ab_15_elev, "hsv"))
plot(colorize(ab_15_elev, "hsi"))
plot(colorize(ab_15_elev, "hsl"))
plot(colorize(ab_15_elev, "rgb"))

plot(colorize(ab_15_elev, "col", stretch = "lin", grays = T))

has.colors(ab_15_elev)
coltab(ab_15_elev)

ab_15_elev_grey <- app(ab_15_elev %>% 
                      subset(., 1:3), 
                    "mean")
NAflag(ab_15_elev_grey) <- 0
plot(ab_15_elev_grey)

ab_15_mask <- ab_15_elev$AB15_ElevationToolbox_export_TueAug08213357906466_4 > 0 
plot(ab_15_mask)

ab_15_veg_zone <- st_difference(ab_15_outer %>% 
                                  st_transform(2956), 
                                ab_15_inner %>% 
                                  st_transform(2956))
ab_15_crop <- mask(ab_15_elev_grey, ab_15_veg_zone) %>% 
  crop(ab_15_outer %>% 
         st_transform(2956))
plot(ab_15_crop)

inv_ab_15_elev_grey <- raster.invert(ab_15_crop)

rs <- terra::stretch(inv_ab_15_elev_grey)
plot(rs)

inv_ab_15_elev_grey_full <- raster.invert(ab_15_elev_grey)
rs2 <- terra::stretch(inv_ab_15_elev_grey_full)
plot(rs2)
plotRGB(ab_15_elev %>% crop(ab_15_outer %>% 
                          st_transform(2956)))

rs2 %>% 
  crop(ab_15_outer %>% 
         st_transform(2956)) %>% 
  plot()

## Test
jamba::rgb2col(
  as.vector(ab_15_elev$AB15_ElevationToolbox_export_TueAug08213357906466_1),
  as.vector(ab_15_elev$AB15_ElevationToolbox_export_TueAug08213357906466_2),
  as.vector(ab_15_elev$AB15_ElevationToolbox_export_TueAug08213357906466_3), 
  as.vector(ab_15_elev$AB15_ElevationToolbox_export_TueAug08213357906466_4))
