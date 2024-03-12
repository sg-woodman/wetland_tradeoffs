## ---------------------------
##
## Script name: process_nutrient
##
## Purpose of script: Take the C, N, P and other nutrient data from 
##  Element and UofC add reformat to fit the skeleton df
##
## Author: Samuel Woodman
##
## Date Created: 2024-02-14
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
library(readxl)
library(janitor)
library(ggpubr)


# Load data ---------------------------------------------------------------

skeleton_df <- read_csv("data/raw/skeleton_df.csv")

veg_df <- read_xlsx(here("data/raw/veg_biomass.xlsx")) %>% 
  mutate(mass_kg = case_when(mass_kg == 0.970 ~ 0.097,
                             TRUE ~ mass_kg)) %>% 
  # fix columns
  mutate(site = str_replace_all(site, "_", "-"), # convert _ to -
         date = as_date(date), # convert date to date format
         mass_kg_m2 = mass_kg/0.1) %>% # convert to m2 using sample frame dimensions (0.5 m x 0.2 m)
  mutate(veg_sample = str_replace(sample, "Veg", "")) %>% 
  select(site, veg_sample, mass_kg_m2, elevation_m)


element_df <- read_csv("data/raw/nutrient_element_data.csv") %>% 
  pivot_longer(cols = -variable, names_to = "site", values_to = "vals") %>% 
  pivot_wider(names_from = variable, values_from = vals) %>% 
  separate(col = site, into = c("site", "veg", "veg_sample"), sep = " ") %>% 
  select(-veg) %>% 
  janitor::clean_names() %>%
  rename_with(~paste0('percent_', .), where(is.numeric)) %>% 
  mutate(across(where(is.numeric),~.x / 100))

iso_df <- read_xlsx("data/raw/n_c_isotope.xlsx", 
                    sheet = "13C & 15N orgnc", range = "A12:G82") %>% 
  janitor::clean_names() %>% 
  separate(sample_id, into = c("site", "veg_sample"), sep = "_") %>% 
  select(-c(number, li_ms_id)) %>% 
  mutate(veg_sample = str_replace(veg_sample, "veg", "")) %>% 
  mutate(across(starts_with("perc"), ~.x / 100))

dom_spp <- read_xlsx("data/raw/veg_biomass_dominate_spp.xlsx") %>% 
  clean_names() %>% 
  select(site, veg_sample = sample, species = species_type_bull_rush_cattail_grasses) %>% 
  mutate(site = str_replace_all(site, "_", "-"),
         veg_sample = str_replace(veg_sample, "Veg", ""),
         veg_type = case_when(species == "grasses" ~ "Grass", 
                              species == "NA" ~ NA_character_,
                              TRUE ~ 'Emergent'))

df <- skeleton_df %>% 
  left_join(iso_df, by = "site") %>% 
  left_join(., element_df) %>% 
  left_join(., veg_df) %>% 
  left_join(., dom_spp) %>% 
  filter(land_class != "Both") %>% 
  mutate(c_g_m2 = mass_kg_m2 * percent_c*1000,
         n_g_m2 = mass_kg_m2 * percent_n*1000,
         p_g_m2 = mass_kg_m2 * percent_phosphorus*1000,
         k_g_m2 = mass_kg_m2 * percent_potassium*1000,
         ca_g_m2 = mass_kg_m2 * percent_calcium*1000,
         mg_g_m2 = mass_kg_m2 * percent_magnesium*1000,
         s_g_m2 = mass_kg_m2 * percent_sulfur*1000,
         na_g_m2 = mass_kg_m2 * percent_sodium*1000) %>% 
  select(site, province, latitude_y, longtitude_x, elevation_m, 
         landuse, land_class, species, veg_type, veg_sample, ends_with("_m2")) %>% 
  filter(!is.na(mass_kg_m2)) %>% 
  mutate(water_status = case_when(site == "AB-B-15" |
                                     site == "AB-DB-01" | 
                                     site == "AB-DB-03" | 
                                     site == "AB-DB-12" ~ "dry",
                                   TRUE ~ "wet"))

write_csv(df, here("data/processed/nutrient_mass_df.csv"))

library(lme4)
library(marginaleffects)

emmeans::emmeans(m, "land_class", type = "response") %>% summary(infer = T)
emmeans::emmeans(m, pairwise ~ land_class, type = "response") %>% summary(infer = T)

m <- lmer(log(n_g_m2) ~ land_class + (1|site), data = df)

summary(m)
hist(resid(m))


avg_comparisons(m, type = "response", transform = "exp")
avg_predictions(m, type = "response", by = "land_class", transform = "exp") %>% 
  as.data.frame() %>% 
  ggplot(aes(x = land_class, y = estimate, colour = land_class)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1,
                colour = "black") + 
  geom_point(size = 3) + 
  scale_colour_manual(values = c("#4fad3c","#f4be4a")) + 
  theme_classic2() + 
  theme(axis.title.x = element_blank(), 
        legend.position = 'none')

test <- df %>% 
  filter(veg_type == "Emergent") %>% 
  group_by(site, land_class, province) %>% 
  summarise(c_g_m2 = mean(c_g_m2),
            n_g_m2 = mean(n_g_m2),
            p_g_m2 = mean(p_g_m2))

t.test(n_g_m2 ~ land_class, data = test)
anova(aov(p_g_m2 ~ land_class*province, data = test))
summary(lm(c_g_m2 ~ land_class*province, data = test))

test %>% 
  ggplot(aes(x = province, y = n_g_m2, fill = land_class)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#4fad3c","#f4be4a")) + 
  theme_classic2() + 
  theme(axis.title.x = element_blank())


df %>% 
  group_by(site, land_class) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  lm(n_g_m2 ~ land_class, data = .) %>%
  avg_comparisons(.)

df %>% 
  group_by(site, land_class) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  lm(n_g_m2 ~ land_class, data = .) %>%
  avg_predictions(., type = "response", by = "land_class") %>% 
  as.data.frame() %>% 
  ggplot(aes(x = land_class, y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1,
                colour = "black") + 
  geom_point(aes(colour = land_class), size = 3) + 
  scale_colour_manual(values = c("#4fad3c","#f4be4a")) + 
  theme_classic2() + 
  theme(axis.title.x = element_blank(), 
        legend.position = 'none')

df %>% 
  group_by(site, land_class) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  lm(k_g_m2 ~ land_class, data = .) %>%
  avg_predictions(., type = "response", by = "land_class") %>% 
  as.data.frame() %>% 
  ggplot(aes(x = land_class, y = estimate)) + 
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1,
                colour = "black") + 
  geom_point(aes(colour = land_class), size = 3) + 
  scale_colour_manual(values = c("#4fad3c","#f4be4a")) + 
  theme_classic2() + 
  theme(axis.title.x = element_blank(), 
        legend.position = 'none')

mod_log <- lm(log(na_g_m2) ~ land_class, data = df)
mod <- lm(na_g_m2 ~ land_class, data = df)
mod_sqrt <- lm(sqrt(ca_g_m2) ~ land_class, data = df)

ggplot(mod_log, aes(x = .resid)) +
  geom_histogram()
ggplot(mod, aes(x = .resid)) +
  geom_histogram()
ggplot(mod_sqrt, aes(x = .resid)) +
  geom_histogram()

summary(mod)

df %>% 
  ggplot(aes(x = land_class, y = p_g_m2, fill = land_class)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#4fad3c","#f4be4a")) + 
  scale_y_log10(name = "Total biomass (Mg)") + 
  theme_classic2() + 
  theme(axis.title.x = element_blank(), 
        legend.position = 'none')
  
df %>% 
  ggplot(aes(x = land_class, y = n_g_m2, fill = land_class)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#4fad3c","#f4be4a")) + 
  scale_y_log10() + 
  theme_classic2() + 
  theme(axis.title.x = element_blank(), 
        legend.position = 'none')
