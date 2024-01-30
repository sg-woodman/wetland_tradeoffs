## ---------------------------
##
## Script name: explore
##
## Purpose of script: Preliminary exploration of the BCRC data
##
## Author: Samuel Woodman
##
## Date Created: 2023-11-20
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
library(ggtext)
library(patchwork)
library(ggpubr)
library(gridtext)
library(ggpmisc)

# Load data ---------------------------------------------------------------

skeleton_df <- read_csv(here("data/raw/skeleton_df.csv"))
veg_df <- read_xlsx(here("data/raw/veg_biomass.xlsx")) %>% 
  mutate(date = as_date(date),
         mass_kg_m2 = mass_kg/0.1) %>% 
  group_by(site) %>% 
  summarise(mean_veg_kg_m2 = mean(mass_kg_m2, na.rm = T),
            sd_veg_kg_m2 = sd(mass_kg_m2, na.rm = T),
            n = n(), 
            se_veg_kg_m2 = sd_veg_kg_m2/sqrt(n))
aqua_prod_df <- read_csv(here("data/processed/aqua_prod_2022.csv")) %>% 
  mutate(site = str_replace(site, "-", "_")) %>% 
  group_by(site, depth) %>% 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>% 
  mutate(aerial_gpp = gpp.985*(depth/100),
         aerial_r = r.985*(depth/100)) %>% 
  select(-c(lat, long))
emergent_veg_area_df <- read_csv(here("data/processed/emergent_veg_area_df.csv"))
spectra_biomass_df <- read_csv(here("data/processed/spectra_biomass_df.csv"))
wetland_area_df <- read_csv(here("data/processed/wetland_area_df.csv"))
flux_df <- read_xlsx("/Users/sam/Downloads/BCRC-ppr-ghg-2022.xlsx",
                     sheet = 2) %>%
  janitor::clean_names() %>% 
  unite("site", c(province,site), sep = "_") %>% 
  dplyr::select(site, date, starts_with("flux")) %>% 
  mutate(flux_n2o = as.numeric(flux_n2o)) %>% 
  group_by(site) %>% 
  summarise(across(where(is.numeric), \(x) mean(x,  na.rm = T)))
bubble_trap_df <- read_xlsx("/Users/sam/Downloads/Bubble trap calculations_withsamplelosscorrection.xlsx") %>% 
  janitor::clean_names() %>% 
  dplyr::select(site, sampling_date, ch4mmol_m_2_d_1) %>% 
  group_by(site) %>% 
  summarise(mean_ch4mmol_m_2_d_1 = mean(ch4mmol_m_2_d_1, na.rm = T))
diversity_df <- read_csv(here("data/processed/diversity_df.csv"))
## Join dfs

df <- skeleton_df %>% 
  left_join(., veg_df) %>% 
  left_join(., aqua_prod_df) %>% 
  left_join(., emergent_veg_area_df) %>% 
  left_join(., wetland_area_df) %>% 
  left_join(., spectra_biomass_df) %>% 
  left_join(., flux_df) %>% 
  left_join(., bubble_trap_df) %>% 
  mutate(total_density = (total_mass_Mg*1000)/full_wetland_area_m2) %>% 
  left_join(., diversity_df)

view(df)

write_csv(df, here("data/processed/tradeoff_df.csv"))

# Plotting ----------------------------------------------------------------

## View mean measured veg density per site
df %>% 
  filter(!is.na(mean_veg_kg_m2)) %>% 
  ggplot(aes(y = site, x = mean_veg_kg_m2, fill = land_class)) +
  scale_y_discrete(expand = c(0,0)) + 
  annotate("rect", xmin = 0, xmax = 8, ymin = 6.5, ymax = 9.5,
           alpha = .7,fill = "grey") + 
  geom_col() + 
  geom_errorbarh(aes(xmin = mean_veg_kg_m2 - se_veg_kg_m2, 
                     xmax = mean_veg_kg_m2 + se_veg_kg_m2),
                 height = 0.25) + 
  scale_x_continuous(name = "Mean emergent veg density (kg m<sup>-2</sup>)",
                     expand = c(0,0)) + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())


## Compare Grassland vs Cropland

df %>% 
  filter(province == "AB" & region == "Camrose") %>% 
  ggplot(aes(x = land_class, y = mean_veg_kg_m2, fill = land_class)) + 
  geom_boxplot() + 
  theme_classic() + 
  theme(legend.position = "none")

df %>% 
  filter(province == "AB" & region == "Camrose") %>% 
  lm(mean_veg_kg_m2 ~ land_class, data = .) %>% 
  summary()

df %>% 
  lm(mean_veg_kg_m2 ~ land_class*region, data = .) %>% 
  summary()


## Does emergent veg density predict aquatic production

df %>% 
  filter(!is.na(pr.985)) %>% 
  arrange(desc(pr.985)) %>% 
  select(site, ends_with(".985"))

pr_plot <- df %>%
  ggplot(aes(x = mass_kg_m2_mean, y = pr.985)) +
  geom_point() +
  # geom_text(
  #   label = df$site,
  #   nudge_x = 0.25,
  #   nudge_y = 0.025,
  #   check_overlap = T) + 
  scale_y_continuous(name = "P:R",
                     limits = c(0, 1.5)) + 
  scale_x_continuous(name = "Mean emergent vegetation density (kg m<sup>-2</sup>)") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())

gpp_plot <- df %>%
  ggplot(aes(x = mass_kg_m2_mean, y = gpp.985)) +
  geom_point() +
  # geom_text(
  #   label = df$site,
  #   nudge_x = 0.15,
  #   nudge_y = 0.25,
  #   size = 2,
  #   check_overlap = T) +  
  scale_y_continuous(name = "GPP",
                     limits = c(0, 8)) + 
  scale_x_continuous(name = "Mean emergent vegetation density (kg m<sup>-2</sup>)") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())

r_plot <- df %>%
  ggplot(aes(x = mass_kg_m2_mean, y = r.985)) +
  geom_point() +
  # geom_text(
  #   label = df$site,
  #   nudge_x = 0.15,
  #   nudge_y = 0.25,
  #   size = 2,
  #   check_overlap = T) +  
  scale_y_continuous(name = "R",
                     limits = c(0, 12)) + 
  scale_x_continuous(name = "Mean emergent veg density (kg m<sup>-2</sup>)") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())

pr_plot + (gpp_plot / r_plot)

df %>% 
  ggplot(aes(x = mass_kg_m2_mean, y = SUVA254)) + 
  geom_point()

colnames(df)
summary(lm(pr.985 ~ mean_veg_kg_m2, data = df))
summary(lm(gpp.985 ~ mean_veg_kg_m2, data = df))
summary(lm(r.985 ~ mean_veg_kg_m2, data = df))
summary(lm(SUVA254 ~ mean_veg_kg_m2, data = df))

## Does total emergent veg biomass predict aquatic production


### Use for non spectral appraoch
# df %>% 
#   filter(region == "Camrose") %>% 
#   group_by(land_class) %>% 
#   mutate(filled_biomass_kg_m2 = case_when(is.na(mean_veg_kg_m2) ~ mean(mean_veg_kg_m2, na.rm = T),
#                                           TRUE ~ as.numeric(mean_veg_kg_m2))) %>% 
#   mutate(total_biomass_kg = filled_biomass_kg_m2*total_em_area_m2) 

pr_biomass_plot <- df %>% 
  ggplot(aes(x = total_mass_Mg, y = pr.985)) +
  geom_point() +
  # geom_text(
  #   label = df %>% filter(region == "Camrose") %>% pull(site),
  #   nudge_x = 0.25,
  #   nudge_y = 0.035,
  #   check_overlap = T,
  #   size = 3) + 
  scale_y_continuous(name = "P:R",
                     limits = c(0, 1.3)) + 
  scale_x_continuous(name = "Total emergent veg (Mg)") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())

gpp_biomass_plot <- df %>% 
  ggplot(aes(x = total_mass_Mg, y = gpp.985)) +
  geom_point() +
  # geom_text(
  #   label = df %>% filter(region == "Camrose") %>% pull(site),
  #   nudge_x = 0.15,
  #   nudge_y = 0.5,
  #   size = 2,
  #   check_overlap = T) +  
  scale_y_continuous(name = "GPP",
                     limits = c(0, 9)) + 
  scale_x_continuous(name = "Total emergent veg (Mg)") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())

r_biomass_plot <- df %>% 
  ggplot(aes(x = total_mass_Mg, y = r.985)) +
  geom_point() +
  # geom_text(
  #   label = df %>% filter(region == "Camrose") %>% pull(site),
  #   nudge_x = 0.15,
  #   nudge_y = 0.75,
  #   size = 2,
  #   check_overlap = T) +  
  scale_y_continuous(name = "R",
                     limits = c(0, 15)) + 
  scale_x_continuous(name = "Total emergent veg (Mg)") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())

df %>% 
  filter(region == "Camrose") %>% 
  group_by(land_class) %>% 
  mutate(filled_biomass_kg_m2 = case_when(is.na(mean_veg_kg_m2) ~ mean(mean_veg_kg_m2, na.rm = T),
                                          TRUE ~ as.numeric(mean_veg_kg_m2))) %>% 
  mutate(total_biomass_kg = filled_biomass_kg_m2*total_em_area_m2) %>% 
  ggplot(aes(x = total_biomass_kg/1000, y = pctdo)) +
  geom_point() +
  geom_text(
    label = df %>% filter(region == "Camrose") %>% pull(site),
    nudge_x = 0.15,
    nudge_y = 0.75,
    size = 2,
    check_overlap = T) +  
  scale_y_continuous(name = "R",
                     limits = c(0, 15)) + 
  scale_x_continuous(name = "Total emergent veg (Mg)") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())

pr_biomass_plot + (gpp_biomass_plot / r_biomass_plot)

# Does proportion of emergent veg control producitivity
pr_prop_plot <- df %>% 
  ggplot(aes(x = prop_veg_area, y = pr.985)) +
  geom_point() +
  # geom_text(
  #   label = df$site,
  #   nudge_x = 0.01,
  #   nudge_y = 0.035,
  #   check_overlap = T,
  #   size = 3) + 
  scale_y_continuous(name = "P:R",
                     limits = c(0, 1.3)) + 
  scale_x_continuous(name = "Proportion emergent veg") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())

gpp_prop_plot <- df %>% 
  ggplot(aes(x = prop_veg_area, y = gpp.985)) +
  geom_point() +
  # geom_text(
  #   label = df$site,
  #   nudge_x = 0.01,
  #   nudge_y = 0.45,
  #   check_overlap = T,
  #   size = 3) + 
  scale_y_continuous(name = "GPP",
                     limits = c(0, 9)) + 
  scale_x_continuous(name = "Proportion emergent veg") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())

r_prop_plot <- df %>% 
  ggplot(aes(x = prop_veg_area, y = r.985)) +
  geom_point() +
  # geom_text(
  #   label = df$site,
  #   nudge_x = 0.01,
  #   nudge_y = 0.8,
  #   check_overlap = T,
  #   size = 3) + 
  scale_y_continuous(name = "R",
                     limits = c(0, 16)) + 
  scale_x_continuous(name = "Proportion emergent veg") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())

pr_prop_plot + (gpp_prop_plot / r_prop_plot)


df %>% 
  ggplot(aes(x = prop_veg_area, y = pctdo)) +
  geom_point() +
  geom_text(
    label = df$site,
    nudge_x = 0.01,
    nudge_y = 0.035,
    check_overlap = T,
    size = 3) + 
  scale_y_continuous(name = "P:R",
                     limits = c(0, 1.3)) + 
  scale_x_continuous(name = "Proportion emergent veg") + 
  theme_classic() + 
  theme(axis.title.x = element_markdown())


# Does total wetland emergent vegeation density control producitiv --------
poster_theme <- function() {
  theme_classic() + 
    theme(axis.title.y = element_markdown(colour = 'white'),
          axis.title.x = element_blank(),
          text = element_text("Helvetica", size = 54, colour = "white"),
          axis.line = element_line(colour = 'white', size = 3),
          axis.ticks = element_line(colour = "white"),
          axis.text = element_text(colour = "white"),
          panel.background = element_rect(fill="transparent"),
          plot.background = element_rect(fill="transparent", color=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill="transparent"),
          legend.box.background = element_rect(fill="transparent"))
}


land_use_density_fig <- df %>%
  filter(land_class %in% c("Cropland", "Grassland")) %>% 
  droplevels() %>% 
  ggplot(aes(x = land_class, y = total_density, fill = land_class)) + 
  geom_boxplot(size = 1.5, outlier.size = 8) + 
  scale_y_continuous(name = "Density of wetland emergent<br>vegetation (kg m<sup>-2</sup>)") + 
  scale_fill_manual(values = c("#4fad3c", "#f4be4a")) + 
  theme_classic() + 
  theme(axis.title.y = element_markdown(colour = 'black'),
        axis.title.x = element_blank(),
        panel.border = element_rect(fill = NA, linewidth = 1), 
        text = element_text("Helvetica", size = 54, colour = 'black'),
        plot.margin = margin(0.1,1,0.1,0.1, "cm"),
        legend.position = "none")

ggsave(plot = land_use_density_fig, here("output/land_use_density_fig.png"),
       height = 35, width = 35.5*0.927027, units = "cm", dpi = 300)

df %>%
  lm(aerial_gpp ~ land_class, data = .) %>% 
  summary()

aer_gpp_vs_total_den_fig <- df %>%
  ggplot(aes(x = total_density, y = aerial_gpp)) +
  geom_point(size = 8, colour = "white") +
  geom_smooth(method = "lm", se = F,
              linetype = "dashed", colour = "red", size = 3) + 
  stat_poly_eq(mapping = use_label(c("adj.R2", "p.value")),
               rr.digits = 2, p.digits = 2, small.p = T,
               size = 10, colour = "white") + 
  scale_y_continuous(name = "GPP<sub>*aq*</sub> (g O<sub>2</sub> m<sup>-2</sup> d<sup>-1</sup>)",
                     limits = c(0, 11),
                     breaks = seq(0, 10, 5)) + 
  scale_x_continuous(limits = c(0, 0.6), 
                     breaks = seq(0, 0.6, 0.2)) + 
  poster_theme()

df %>%
  lm(aerial_gpp ~ total_density, data = .) %>% 
  summary()

aer_r_vs_total_den_fig <- df %>%
  ggplot(aes(x = total_density, y = aerial_r)) +
  geom_point(size = 8, colour = "white") +
  geom_smooth(method = "lm", se = F,
              linetype = "solid", colour = "red", size = 3) + 
  stat_poly_eq(mapping = use_label(c("adj.R2", "p.value")),
               rr.digits = 2, p.digits = 2, small.p = T,
               size = 10, colour = "white") + 
  scale_y_continuous(name = "R<sub>*aq*</sub> (g O<sub>2</sub> m<sup>-2</sup> d<sup>-1</sup>)",
                     limits = c(0, 16),
                     breaks = seq(0, 15, 5)) + 
  scale_x_continuous(limits = c(0, 0.6), 
                     breaks = seq(0, 0.6, 0.2)) + 
  poster_theme()

df %>%
  lm(aerial_r ~ total_density, data = .) %>% 
  summary()

co2_vs_total_den_fig <- df %>% 
  ggplot(aes(x = total_density, y = flux_co2)) +
  geom_point(size = 8, colour = "white") +
  # geom_smooth(method = "lm", se = F,
  #             linetype = "solid", colour = "red", size = 3) + 
  # stat_poly_eq(mapping = use_label(c("adj.R2", "p.value")),
  #              rr.digits = 2, p.digits = 2, 
  #              size = 10, colour = "white") + 
  scale_y_continuous(name = "CO<sub>2 *aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)",
                     limits = c(0, 300),
                     breaks = seq(0, 300, 100)) + 
  scale_x_continuous(limits = c(0, 0.6), 
                     breaks = seq(0, 0.6, 0.2)) + 
  poster_theme()

df %>%
  lm(flux_co2 ~ total_density, data = .) %>% 
  summary()

ch4_vs_total_den_fig <- df %>% 
  mutate(total_density = (total_mass_Mg*1000)/full_wetland_area_m2) %>% 
  ggplot(aes(x = total_density, y = flux_ch4)) +
  geom_point(size = 8, colour = "white") +
  geom_smooth(method = "lm", se = F,
              linetype = "solid", colour = "red", size = 3) + 
  stat_poly_eq(mapping = use_label(c("adj.R2", "p.value")),
               rr.digits = 2, p.digits = 2, small.p = T,
               size = 10, colour = "white") + 
  scale_y_continuous(name = "CH<sub>4 *aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)",
                     limits = c(0, 80),
                     breaks = seq(0, 80, 40)) + 
  scale_x_continuous(limits = c(0, 0.6), 
                     breaks = seq(0, 0.6, 0.2)) + 
  poster_theme()

df %>%
  lm(flux_ch4 ~ total_density, data = .) %>% 
  summary()

agu_fig <- ggarrange(aer_gpp_vs_total_den_fig, aer_r_vs_total_den_fig, 
          co2_vs_total_den_fig, ch4_vs_total_den_fig,
          common.legend = TRUE,
          ncol = 2, nrow = 2, align = "hv") + 
  theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"))

agu_fig_output <- annotate_figure(agu_fig,
                bottom = richtext_grob("<span style='font-size:48pt; color:white'>Density of wetland emergent vegetation (kg m<sup>-2</sup>)</span>"))

ggsave(plot = agu_fig_output, here("output/agu_main_poster_fig.png"),
       bg='transparent',
       height = 50, width = 60, units = "cm", dpi = 300)

# W/H
142.24/96.52

116.84*1.473684
91.44*1.473684

p <- (aer_gpp_vs_total_den_fig + aer_r_vs_total_den_fig) / 
  (co2_vs_total_den_fig + ch4_vs_total_den_fig) & 
  plot_annotation(theme = theme(plot.background = element_rect(fill = 'transparent',
                                                               colour = NA)))

p_out <- wrap_elements(panel = p) +
  labs(tag = "Density of wetland emergent vegetation (kg m<sup>-2</sup>)") +
  theme(axis.title.y = element_markdown(colour = 'white'),
        axis.title.x = element_blank(),
        text = element_text("Helvetica", size = 48, colour = "white"),
        axis.line = element_line(colour = 'white', size = 3),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(fill="transparent"),
        plot.tag = element_markdown(size = 48, colour = "white"),
        plot.tag.position = "bottom")

ggsave(plot = p_out, here("output/density_poster_fig.png"),
       bg='transparent',
       height = 45.31, width = 53.72, units = "cm", dpi = 300)


# Does total emergent veg biomass predict GHG fluxes ----------------------

co2_biomass_plot <- df %>% 
  ggplot(aes(x = total_mass_Mg, y = flux_co2)) +
  geom_point() + 
  scale_y_continuous(name = "CO2 flux") + 
  scale_x_continuous(name = "Total emergent veg biomass (Mg)") + 
  theme_classic()

ch4_biomass_plot <- df %>% 
  ggplot(aes(x = total_mass_Mg, y = flux_ch4)) +
  geom_point() + 
  scale_y_continuous(name = "CH4 flux") + 
  scale_x_continuous(name = "Total emergent veg biomass") +
  # scale_y_log10(name = "Log CH4 flux") + 
  # scale_x_log10(name = "Log total emergent veg biomass") +
  theme_classic()

n2o_biomass_plot <- df %>% 
  ggplot(aes(x = total_mass_Mg, y = flux_n2o)) +
  geom_point() + 
  scale_y_continuous(name = "N20 flux", 
                     limits = c(-0.005, 0.02)) + 
  scale_x_continuous(name = "Total emergent veg biomass (Mg)") + 
  theme_classic()

co2_biomass_plot / ch4_biomass_plot / n2o_biomass_plot




# Does diversity corralate with vegetation --------------------------------

scatter <- function(dat, x, y) {
  out <- dat %>% 
    ggplot(aes(x = {{x}}, y = {{y}})) + 
    geom_point() + 
    theme_classic() + 
    theme(legend.position = "none")
  return(out)
}

scatter_eq <- function(dat, x, y) {
  out <- dat %>% 
    ggplot(aes(x = {{x}}, y = {{y}})) + 
    geom_point() + 
    geom_smooth(method = "lm", se = F, colour = "black") +
    stat_poly_eq(mapping = use_label(c("adj.R2", "p.value")),
                 rr.digits = 2, p.digits = 2, small.p = T, 
                 colour = "black") + 
    theme_classic() + 
    theme(legend.position = "none")
  return(out)
}

## Total density of emergent vegetation
den_rich <- scatter(df, total_density, wet_bird_richness) + labs(x = "Density of wetland emergent vegetation (kg m<sup>-2</sup>)") + theme(axis.title.x = element_markdown())
den_shan <- scatter(df, total_density, wet_bird_shannon) + labs(x = "Density of wetland emergent vegetation (kg m<sup>-2</sup>)") + theme(axis.title.x = element_markdown())
den_simp <- scatter(df, total_density, wet_bird_simpson) + labs(x = "Density of wetland emergent vegetation (kg m<sup>-2</sup>)") + theme(axis.title.x = element_markdown())
scatter(df, total_density, inverse_simpson)
scatter(df, total_density, unbias_simpson)
scatter(df, total_density, fisher_alpha)
den_even <- scatter(df, total_density, wet_bird_evenness) + labs(x = "Density of wetland emergent vegetation (kg m<sup>-2</sup>)") + theme(axis.title.x = element_markdown())

total_den_plot <- (den_rich + den_even) / (den_shan + den_simp)

## All diversity metrics decrease with higher total density

## EM veg density 
scatter(df, mean_veg_kg_m2, wet_bird_richness)
scatter(df, mean_veg_kg_m2, shannon) 
scatter(df, mean_veg_kg_m2, simpson)
scatter(df, mean_veg_kg_m2, inverse_simpson)
scatter(df, mean_veg_kg_m2, unbias_simpson)
scatter(df, mean_veg_kg_m2, fisher_alpha)
scatter(df, mean_veg_kg_m2, evenness)
## All diversity metrics show a weak decrease with higher total density

## Total biomass of emergent vegetation
scatter(df, total_mass_Mg, wet_bird_richness) + scale_x_log10(name = "Log total mass (Mg)")
scatter(df, total_mass_Mg, shannon) + scale_x_log10(name = "Log total mass (Mg)")
scatter(df, total_mass_Mg/full_wetland_area_m2, simpson) + scale_x_log10(name = "Log total mass (Mg)")
scatter(df, total_mass_Mg, inverse_simpson) + scale_x_log10(name = "Log total mass (Mg)")
scatter(df, total_mass_Mg, unbias_simpson) + scale_x_log10(name = "Log total mass (Mg)")
scatter(df, total_mass_Mg, fisher_alpha) + scale_x_log10(name = "Log total mass (Mg)")
scatter(df, total_mass_Mg, evenness) + scale_x_log10(name = "Log total mass (Mg)")
## All diversity metrics decrease with higher total density (logged)
  
## Area of entire wetland
area_rich <- scatter(df, full_wetland_area_m2, wet_bird_richness) + labs(x = "Wetland area (m<sup>-2</sup>)") + theme(axis.title.x = element_markdown())
area_shan <- scatter(df, full_wetland_area_m2, wet_bird_shannon) + labs(x = "Wetland area (m<sup>-2</sup>)") + theme(axis.title.x = element_markdown())
area_simp <- scatter(df, full_wetland_area_m2, wet_bird_simpson) + labs(x = "Wetland area (m<sup>-2</sup>)") + theme(axis.title.x = element_markdown())
scatter(df, full_wetland_area_m2, inverse_simpson) + scale_x_log10(name = "Log wetland area (m2)")
scatter(df, full_wetland_area_m2, unbias_simpson) + scale_x_log10(name = "Log wetland area (m2)")
scatter(df, full_wetland_area_m2, fisher_alpha) + scale_x_log10(name = "Log wetland area (m2)")
area_even <- scatter(df, full_wetland_area_m2, wet_bird_evenness) + labs(x = "Wetland area (m<sup>-2</sup>)") + theme(axis.title.x = element_markdown())

wet_area_plot <- (area_rich + area_even) / (area_shan + area_simp)

## All diversity metrics show a weak decrease as wetland area increased (logged)


## GPP
gpp_rich <- scatter(df, aerial_gpp, wet_bird_richness) + labs(x = "GPP<sub>*aq*</sub> (g O<sub>2</sub> m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.x = element_markdown())
gpp_shan <- scatter(df, aerial_gpp, wet_bird_shannon) + labs(x = "GPP<sub>*aq*</sub> (g O<sub>2</sub> m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.x = element_markdown())
gpp_simp <- scatter(df, aerial_gpp, wet_bird_simpson) + labs(x = "GPP<sub>*aq*</sub> (g O<sub>2</sub> m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.x = element_markdown())
scatter(df, aerial_gpp, inverse_simpson)
scatter(df, aerial_gpp, unbias_simpson)
scatter(df, aerial_gpp, fisher_alpha)
gpp_even <- scatter(df, aerial_gpp, wet_bird_evenness) + labs(x = "GPP<sub>*aq*</sub> (g O<sub>2</sub> m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.x = element_markdown())

gpp_div_plot <- (gpp_rich + gpp_even) / (gpp_shan + gpp_simp) 

## Shannon, Simpson, Inverse Simpson, Unbiased Simpson, and Evenness decreased with higher GPP 

## R
r_rich <- scatter(df, aerial_r, wet_bird_richness) + labs(x = "R<sub>*aq*</sub> (g O<sub>2</sub> m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.x = element_markdown())
r_shan <- scatter(df, aerial_r, wet_bird_shannon) + labs(x = "R<sub>*aq*</sub> (g O<sub>2</sub> m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.x = element_markdown())
r_simp <- scatter(df, aerial_r, wet_bird_simpson) + labs(x = "R<sub>*aq*</sub> (g O<sub>2</sub> m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.x = element_markdown())
scatter(df, aerial_r, inverse_simpson)
scatter(df, aerial_r, unbias_simpson)
scatter(df, aerial_r, fisher_alpha)
r_even <- scatter(df, aerial_r, wet_bird_evenness) + labs(x = "R<sub>*aq*</sub> (g O<sub>2</sub> m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.x = element_markdown())

r_div_plot <- (r_rich + r_even) / (r_shan + r_simp) 

## Shannon, Simpson, Inverse Simpson, Unbiased Simpson, and Evenness decreased with higher R 

## GPP:R
gpp_r_rich <- scatter(df, aerial_gpp/aerial_r, wet_bird_richness) + labs(x = "GPP:R") 
gpp_r_shan <- scatter(df, aerial_gpp/aerial_r, shannon) + labs(x = "GPP:R")
gpp_r_simp <- scatter(df, aerial_gpp/aerial_r, simpson) + labs(x = "GPP:R")
scatter(df, aerial_r, inverse_simpson)
scatter(df, aerial_r, unbias_simpson)
scatter(df, aerial_r, fisher_alpha)
gpp_r_even <- scatter(df, aerial_gpp/aerial_r, evenness) + labs(x = "GPP:R")

gpp_r_div_plot <- (gpp_r_rich + gpp_r_even) / (gpp_r_shan + gpp_r_simp) 

## CO2
co2_rich <- scatter_eq(df, wet_bird_richness, flux_co2) + labs(y = "CO<sub>2 *aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())
co2_shan <- scatter_eq(df, wet_bird_shannon, flux_co2) + labs(y = "CO<sub>2 *aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())
co2_simp <- scatter_eq(df, wet_bird_simpson, flux_co2) + labs(y = "CO<sub>2 *aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())
scatter(df, inverse_simpson, flux_co2)
scatter(df, unbias_simpson, flux_co2)
scatter(df, fisher_alpha, flux_co2)
co2_even <- scatter_eq(df, wet_bird_evenness, flux_co2) + labs(y = "CO<sub>2 *aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())

co2_div_plot <- (co2_rich + co2_even) / (co2_shan + co2_simp) 

## CO2 fluxes decreases as Shannon, Simpson, Inverse Simpson, Unbiased Simpson, and Evenness diversity indices increased

## CH4
ch4_rich <- scatter(df, wet_bird_richness, flux_ch4) + labs(y = "CH<sub>4 *aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())
ch4_shan <- scatter(df, wet_bird_shannon, flux_ch4) + labs(y = "CH<sub>4 *aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())
ch4_simp <- scatter(df, wet_bird_simpson, flux_ch4) + labs(y = "CH<sub>4 *aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())
scatter(df, inverse_simpson, flux_ch4)
scatter(df, unbias_simpson, flux_ch4)
scatter(df, fisher_alpha, flux_ch4)
ch4_even <- scatter(df, wet_bird_evenness, flux_ch4) + labs(y = "CH<sub>4 *aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())

ch4_div_plot <- (ch4_rich + ch4_even) / (ch4_shan + ch4_simp) 

## CH4 fluxes decreases as Shannon, Simpson, and Inverse Simpson diversity indices increased

## N2O
n2o_rich <- scatter(df, wet_bird_richness, flux_n2o) + scale_y_log10() + labs(y = "N<sub>2</sub>O <sub>*aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())
n2o_shan <- scatter(df, wet_bird_shannon, flux_n2o) + scale_y_log10() + labs(y = "N<sub>2</sub>O <sub>*aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())
n2o_simp <- scatter(df, wet_bird_simpson, flux_n2o) + scale_y_log10() + labs(y = "N<sub>2</sub>O <sub>*aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())
scatter(df, inverse_simpson, flux_n2o) + scale_y_log10()
scatter(df, unbias_simpson, flux_n2o) + scale_y_log10()
scatter(df, fisher_alpha, flux_n2o) + scale_y_log10()
n2o_even <- scatter(df, wet_bird_evenness, flux_n2o) + scale_y_log10() + labs(y = "N<sub>2</sub>O <sub>*aq*</sub> (mmol m<sup>-2</sup> d<sup>-1</sup>)") + theme(axis.title.y = element_markdown())

n2o_div_plot <- (n2o_rich + n2o_even) / (n2o_shan + n2o_simp) 

## N2O increases with higher richness and fisher's alpha

## Beta diversity
betadiver(help = T)
beta_div <- betadiver(community_matrix)

## Beta Dispersion
ab_comm_mat <- birdnet_detections_joined %>% 
  filter(PROVINCE == "AB") %>% 
  mutate(year = year(date),
         month = month(date),
         jday = yday(date)) %>% 
  group_by(unique_id, PROVINCE, treatment, Scientific.name) %>% 
  tally() %>% 
  ungroup() %>% 
  dplyr::select(unique_id, Scientific.name, n) %>% 
  as.data.frame() %>% 
  matrify()
dim(ab_comm_mat)
ab_group <- birdnet_detections_joined %>% 
  filter(PROVINCE == "AB") %>% 
  select(unique_id, treatment) %>% 
  distinct() %>% 
  pull(treatment) %>% 
  factor()
dis <- vegdist(ab_comm_mat)
beta_disp <- betadisper(dis, ab_group)
anova(beta_disp)
TukeyHSD(beta_disp)
plot(beta_disp) 

## Adonis
adon <- adonis2(ab_comm_mat ~ ab_group, perm=999)

## Mantel
veg.dist <- vegdist(community_matrix) # Bray-Curtis
env.dist <- vegdist(scale(community_matrix), "euclid")
mantel(veg.dist, env.dist)
mantel(veg.dist, env.dist, method="spear")


df %>% 
  ggplot(aes(x = province, y = richness, fill = land_class)) + 
  geom_boxplot() + 
  theme_classic() + 
  theme(legend.position = "none")

df %>% 
  filter(province == "AB") %>% 
  lm(mean_veg_kg_m2 ~ land_class, data = .) %>% 
  summary()
  

