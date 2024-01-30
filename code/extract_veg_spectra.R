## ---------------------------
##
## Script name: extract_veg_spectra
##
## Purpose of script: Using the manually corrected vegetation sampling points 
##    (corrected to account for inaccuracy of phone GPS using DMS) a polygon 
##    is created to match the shape of the Daubenmire smapling frame (50 cm x 
##    20 cm). Various raw and calculated spectral values are then extracted from 
##    within the polygon 
##
## Author: Samuel Woodman
##
## Date Created: 2023-11-27
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
library(buffeRs)
library(terra)
library(exactextractr)
library(uavRst)

# Functions ---------------------------------------------------------------

## convert a point to a rectangle 
rect_around_point <- function(x, xsize, ysize){
  bbox <- st_bbox(x)
  bbox <- bbox +c(xsize/2,ysize/2,-xsize/2,-ysize/2)
  return(st_as_sfc(bbox))
}


# Load data ---------------------------------------------------------------

veg_coords <- st_read(here("data/processed/veg_coords_manually_corrected.gpkg")) 

# Check units of points
st_crs(veg_coords, parameters = TRUE)$units_gdal
st_crs(veg_coords)

# create polygon around each point with same dimensions as the sampling frame
veg_frame_vect <- veg_coords %>% 
  # convert to crs with metres as unit
  st_transform(2957) %>% # 2956 for AB, 2957 for SK
  # xplit each point to a list object
  group_split(site, sample) %>% 
  # generate polygon with horizontal width of 50 cm and vertical length of 20 cm
  map(\(x) rect_around_point(x, 0.5, 0.2)) %>% 
  # convert so sf abject from sfc (need to do for binding)
  map(st_as_sf) %>% 
  # bind to a single df
  bind_rows() %>% 
  # add metadate (sites, samples, mass)
  cbind(., veg_coords %>% 
          mutate(lat = sf::st_coordinates(.)[,2],
                 lon = sf::st_coordinates(.)[,1]) %>% 
          group_split(site, sample) %>% 
          map(\(x) st_set_geometry(x, NULL))%>% 
              bind_rows())  %>% 
  mutate(site_matcher = str_replace(site, "_0", "")) %>% 
  mutate(site_matcher = str_replace(site_matcher, "_", "")) %>% 
  mutate(site_matcher = str_replace(site_matcher, "SKC", "PK")) %>% 
  mutate(site_matcher = str_replace(site_matcher, "_1", "1"))
  
st_crs(veg_frame_vect)
st_write(veg_frame_vect, here("data/processed/veg_sample_frames.gpkg"), 
         overwrite = T, delete_dsn = TRUE)

hist(log(veg_frame_vect$mass_kg))

create_veg_frames <- function() { 
  veg_coords %>% 
    # convert to crs with metres as unit
    st_transform("ESRI:102001") %>% 
    # xplit each point to a list object
    group_split(site, sample) %>% 
    # generate polygon with horizontal width of 50 cm and vertical length of 20 cm
    # vary degree between 0 and 170 to account for random orientation
    # full (0 to 360ª) could double count some pixels since the polygon is symmetrical 
    map(\(x) buffer_rectangle(x, 0.5, 0.2, degree = sample(seq(0,170,10), 1, replace=F))) %>% 
    # convert so sf abject from sfc (need to do for binding)
    map(., st_as_sf) %>% 
    # bind to a single df
    bind_rows() %>% 
    # add metadate (sites, samples, mass)
    cbind(veg_coords %>% 
            mutate(lat = sf::st_coordinates(.)[,2],
                   lon = sf::st_coordinates(.)[,1]) %>% 
            group_split(site, sample) %>% 
            map(\(x) st_set_geometry(x, NULL))%>% 
            bind_rows(), .)
  }

test <- map(1:5, ~ create_veg_frames()) %>% 
  bind_rows(., .id = "iteration")


# Load raster -------------------------------------------------------------

veg_site_vec <- veg_frame_vect %>% 
  pull(site) %>% 
  unique() %>% 
  str_remove(., "_0") %>% 
  str_remove(., "_") %>% 
  str_replace(., "SKC", "PK") %>% 
  str_replace(., "_1", "1") %>%
  paste(., collapse = "|")


drone_list <- list.files(path = here("data/raw/drone_imagery"), 
           pattern = "*.tif$", 
           recursive = T, 
           full.names = T) %>% 
  keep(., ~!str_detect(.x, "ElevationToolbox")) %>% 
  keep(., ~!str_detect(.x, "_dtm")) %>% 
  keep(., ~str_detect(.x, veg_site_vec)) %>% 
  keep(., ~!str_detect(.x, "AB11|AB12|ABDB10|ABDB11|ABDB13|ABDB14|ABDB15ABDB16")) %>% 
  keep(., ~!str_detect(.x, "TueAug08220421259267/ABDB1"))

outline_names <- list.files(path = here("data/raw/wetland_polygons/"), 
           pattern = "*gpkg",
           full.names = F) %>% 
  keep(., ~str_detect(.x, "outline")) %>% 
  as_vector() %>% 
  str_replace(., "_outline.gpkg", "") %>% 
  str_replace(., "_0", "_") %>% 
  str_replace(., "_", "") %>% 
  str_replace(., "SKC", "PK") %>% 
  str_replace(., "_1", "1") %>% 
  str_replace(., "B_", "B") %>% 
  str_replace(., "_6", "6")
  

ab_outlines <- list.files(path = here("data/raw/wetland_polygons"), 
           pattern = "*gpkg",
           full.names = T) %>% 
  keep(., ~str_detect(.x, "outline")) %>% 
  map(st_read) %>% 
  # convert so sf abject from sfc (need to do for binding)
  map(\(x) st_transform(x, st_crs(2957))) %>% # 2956 for AB, 2957 for SK
  # bind to a single df
  bind_rows() %>% 
  filter(., !st_is_empty(.)) %>% 
  mutate(site_matcher = outline_names)



wetland_area_df <- ab_outlines %>% 
  mutate(full_wetland_area_m2 = as.numeric(st_area(.))) %>% 
  st_set_geometry(NULL) %>%
  .[1:9,] %>% 
  bind_cols(list.files(path = here("data/raw/wetland_polygons/"), 
                       pattern = "*gpkg",
                       full.names = F) %>% 
              keep(., ~str_detect(.x, "outline")) %>% 
              as_vector() %>% 
              str_replace(., "_outline.gpkg", "") %>% 
              .[1:9] %>% 
              as.data.frame() %>% 
              rename(site = ".")) %>% 
  dplyr::select(-site_matcher) %>% 
  mutate(site = case_when(site == "AB_4" ~ "AB_04",
                           site == "AB_5" ~ "AB_05", 
                           TRUE ~ site))

write_csv(wetland_area_df, here("data/processed/wetland_area_df.csv"))


get_dat <- safely(function(ID) {
  p <- veg_frame_vect %>% 
    filter(site_matcher == ID)
  
  o <- ab_outlines %>% 
    filter(site_matcher == ID)
  
  r <- drone_list %>% 
    keep(., ~str_detect(.x, ID)) %>% 
    as_vector() %>% 
    rast()
  
  r_crop <- mask(r, o) %>% 
    crop(., o)
  
  r_ind <- rgb_indices(red = r_crop[[1]], # red band
                       green = r_crop[[2]], # blue band
                       blue  = r_crop[[3]], # green band
                       # all indices provided by the uavRst package
                       rgbi = c("VVI", "VARI", "NDTI", "RI", "SCI",
                                "BI", "SI", "HI", "TGI", "GLI",
                                "NGRDI", "GLAI", "CI", "SAT", "SHP")) %>% 
    rast(.)
  
  r_all <- c(r_crop, r_ind) 
  
  out <- exact_extract(r_all, p, "mean", append_cols = c("site", "site_matcher", 
                                                         "sample", "elevation_m", 
                                                         "mass_kg", "lat", "lon"))
  out
    
  return(out)
  })


veg_extract_14_16 <- veg_frame_vect %>% 
  pull(site) %>% 
  unique() %>% 
  str_remove(., "_0") %>% 
  str_remove(., "_") %>% 
  str_replace(., "SKC", "PK") %>% 
  str_replace(., "_1", "1") %>% 
  as.list() %>% 
  .[14:16] %>% 
  map(get_dat)

veg_extract_14_16 %>% 
  set_names(veg_frame_vect %>% 
              pull(site) %>% 
              unique() %>% 
              str_remove(., "_0") %>% 
              str_remove(., "_") %>% 
              str_replace(., "SKC", "PK") %>% 
              str_replace(., "_1", "1") %>% 
              as.list() %>% 
              .[14:16]) %>% 
  map("result") %>% 
  compact() %>% 
  bind_rows() %>% 
  colnames()

  

test_14_16 <- veg_extract_14_16 %>% 
  set_names(veg_frame_vect %>% 
              pull(site) %>% 
              unique() %>% 
              str_remove(., "_0") %>% 
              str_remove(., "_") %>% 
              str_replace(., "SKC", "PK") %>% 
              str_replace(., "_1", "1") %>% 
              as.list() %>% 
              .[14:16]) %>% 
  map("result") %>% 
  compact() %>% 
  bind_rows() %>% 
 mutate(red = coalesce(mean.PK6_RGB_1,
                       mean.PK10_RGB_1,
                       mean.PK11_RGB_1),
        green = coalesce(mean.PK6_RGB_2,
                         mean.PK10_RGB_2,
                         mean.PK11_RGB_2),
        blue = coalesce(mean.PK6_RGB_3,
                        mean.PK10_RGB_3,
                        mean.PK11_RGB_3)) %>% 
  select(-ends_with(c("_1", "_2", "_3", "_4")))

write_csv(test, here("tmp/veg_extract_1_9.csv"))
write_csv(test_10_14, here("tmp/veg_extract_10_13.csv"))
write_csv(test_14_16, here("tmp/veg_extract_14_16.csv"))
write_csv(test_17_18, here("tmp/veg_extract_17_18.csv"))


# Fit model ---------------------------------------------------------------

df <- list.files(path = here("tmp"), 
           pattern = "^veg_extract_", 
           full.names = T) %>% 
  map_df(read_csv) %>% 
  # fix typo in AB_15 veg 3
  mutate(mass_kg = case_when(mass_kg > 0.75 ~ 0.097, 
                             TRUE ~ mass_kg),
         mass_kg_m2 = mass_kg/(0.5*0.2)) %>% 
  relocate(mass_kg_m2, .after = mass_kg)

df %>% 
  pivot_longer(cols = 7:25, 
               names_to = "vari", 
               values_to = "vals") %>% 
  #filter(mass_kg < 0.75) %>% 
  ggplot(aes(x = vals, y = mass_kg_m2)) + 
  geom_point() + 
  scale_y_log10(name = "Log mass (kg/m2)") +
  scale_x_log10() + 
  facet_wrap(~vari, scales = "free_x") + 
  theme_classic() 

colnames(df)

df %>% 
  ggplot(aes(x = mean.GLI, y = mass_kg_m2)) +# also SAT, TGI
  geom_point() + 
  scale_y_log10(name = "Log mass (kg)") +
  scale_x_log10(name = "Green leaf index") + 
  theme_classic()
  
df %>% 
  ggplot(aes(x = mean.SAT, y = mass_kg_m2)) +# also SAT, TGI
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_y_log10(name = "Log mass (kg)") +
  scale_x_log10(name = "SAT") + 
  theme_classic()


mod_df <- df %>% 
  dplyr::select(-c(site, site_matcher, sample, elevation_m)) %>% 
  mutate(mean.HI = case_when(mean.HI == "Inf" ~ NA_real_,
                             mean.HI == "-Inf" ~ NA_real_,
                             TRUE ~ mean.HI),
         mean.SHP = case_when(mean.SHP == "-Inf" ~ NA_real_,
                              TRUE ~ mean.SHP)) %>% 
  na.omit()

mod_df %>% 
  pivot_longer(starts_with("mean."), 
               names_to = "vari", 
               values_to = "vals") %>% 
  group_by(vari) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(log(mass_kg_m2) ~ log(vals), data = .)),
         glance = map(model, broom::glance)) %>% 
  unnest(glance) %>% 
  arrange(desc(adj.r.squared), p.value)
  


sat_mod <- lm(log(mass_kg_m2) ~ log(mean.SAT), data = mod_df)
summary(sat_mod)

print(extract_eq(sat_mod, use_coefs = T))

lmodel2::lmodel2(log(mass_kg) ~ log(mean.SAT), data = mod_df)

library(ggpmisc)
library(scales)
biomass_sat_fig <- mod_df %>% 
  ggplot(aes(x = mean.SAT, y = mass_kg)) + # also SAT, TGI
  geom_point(size = 15) + 
  scale_y_log10(name = "Biomass (kg)") + 
  scale_x_log10(name = "Overall saturation index") + 
  geom_smooth(method = "lm", se = F,
              linetype = "solid", colour = "red", size = 5) + 
  stat_poly_eq(mapping = use_label(c("adj.R2", "p.value")),
               rr.digits = 2, p.digits = 2, small.p = T,
               size = 15, colour = "black") + 
  theme_classic() + 
  theme(panel.border = element_rect(fill = NA, linewidth = 1), 
        text = element_text("Helvetica", size = 66),
        plot.margin = margin(0.1,1,0.1,0.1, "cm"))

ggsave(plot = biomass_sat_fig, here("output/biomass_sat_fig.png"),
       bg='transparent',
       height = 45.31, width = 45.31, units = "cm", dpi = 300)


df %>% 
  ggplot(aes(x = mean.SAT, y = mass_kg)) +# also SAT, TGI
  geom_point() + 
  #scale_x_continuous(trans = log_trans()) +
  #scale_y_continuous(trans = log_trans()) +
  scale_y_log10(name = "Log mass (kg)") +
  scale_x_log10(name = "SAT") + 
  stat_ma_line(method = "OLS") +
  stat_ma_eq(use_label(c("eq", "R2", "p")),
             method = "OLS") + 
  theme_classic()

ggplot(df, aes(x = mean.SAT, y = mass_kg)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
    parse = TRUE, coef.digits = 3, f.digits = 3, p.digits = 3,
    rr.digits = 3
  ) +
  scale_x_continuous(trans = "log") + 
  scale_y_log10()

library(ggpubr)
df %>% 
  ggplot(aes(x = log(mean.SAT), y = log(mass_kg))) +# also SAT, TGI
  geom_point() +
  geom_smooth(method="lm") 

df %>% 
  ggplot(aes(x = mean.SAT, y = mass_kg)) +# also SAT, TGI
  geom_point() + 
  scale_y_log10(name = "Log mass (kg)") +
  scale_x_log10(name = "SAT") + 
  geom_smooth(method="lm") + 
  stat_regline_equation() + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")),
           label.x.npc = "centre") + 
  theme_classic()


library(MASS)

# Fit the full model 
full.model <- lm(mass_kg ~., data = mod_df)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = T)
summary(step.model)


plot(p)
plotRGB(r)
lines(p, lwd = 1, col = "red")
