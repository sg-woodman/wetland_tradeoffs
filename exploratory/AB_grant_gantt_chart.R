library(plan)
library(tidyverse)

# https://jtr13.github.io/cc19/gantt-charts.html
# https://www.molecularecologist.com/2019/01/03/simple-gantt-charts-in-r-with-ggplot2-and-the-tidyverse/

df <- data.frame(group = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                 task=c("1) Collate data", 
                        "2) Q.1. Emergent \nvegetation coupling", "2.1) Data analysis", "2.2) Manuscript prep", 
                        "3) Q.2&3. Scale \nwetland nutrients", "3.1) Calculate extent of \nemergent vegetation \nacross the PPR", "3.2) Model & map areal \nvegetation C, N, and P \ncontent for PPR", "3.3) Manuscript prep",
                        "4) Conferences", "4) Conferences"), 
                 start=c("2024-06-01", 
                         "2025-01-01", "2025-01-01", "2025-04-01", 
                         "2025-07-01", "2025-07-01", "2026-01-01", "2026-04-01",
                         "2025-12-08", "2026-12-14"),
                 end=c("2024-08-31", 
                       "2025-06-30", "2025-03-31", "2025-06-30", 
                       "2026-12-31", "2026-03-30", "2026-06-30", "2026-12-31",
                       "2025-12-14", "2026-12-20"),
                 task_type=c("Main", 
                         "Main", "Sub", "Sub", 
                         "Main", "Sub", "Sub", "Sub",
                         "Main", "Main"))

tidy_df <- df %>% 
  mutate(start = as.Date(start), end = as.Date(end)) %>% 
  pivot_longer(cols = c("start", "end"), names_to = "date_type", values_to = "date")

tidy_df %>% 
  ggplot(aes(x=fct_rev(fct_inorder(task)), y=date, colour=task_type, group = group)) +
  geom_line(linewidth=10) +
  coord_flip() +
  scale_y_date(date_breaks = "3 month",
               date_labels = "%b, %y") +
  scale_colour_manual(name = "Task type", 
                      values = c("#1b4079", "#228B22")) + 
  labs(title="Bogard et al. OCS & U of L Collaborative Research Grant Gantt Chart",
       x = "Task",
       y = "Date") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.minor = element_line(colour="white", size=0.5),
        legend.position=c(0.85, 0.85),
        #legend.spacing.y = unit(1.0, 'cm'),
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(hjust = 0.5))


## second draft
df <- data.frame(group = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
                 task=c("1) Collate data", 
                        "2) Q.1. Emergent vegetation & \naquatic productivity coupling",
                        "3) Q.2&3. Scaling & mapping \nwetland nutrients", "3.1) Calculate extent of \nemergent vegetation \nacross the PPR", "3.2) Model & map areal \nvegetation C, N, and P \ncontent for PPR",
                        "4) Conferences", "4) Conferences",
                        "5) OSC Seminar \nSeries presentation",
                        "6) Group meetings", "6) Group meetings", "6) Group meetings", "6) Group meetings",
                        "6) Group meetings", "6) Group meetings", "6) Group meetings", "6) Group meetings",
                        "7) Annual repoting", "7) Annual repoting"), 
                 start=c("2024-06-01", 
                         "2024-09-01", 
                         "2025-03-01", "2025-03-01", "2025-12-01",
                         "2024-12-09", "2025-12-15",
                         "2025-10-31",
                         "2024-06-01", "2024-09-01", "2024-12-01", "2025-03-01",
                         "2025-06-01", "2025-09-01", "2025-12-01", "2026-03-01",
                         "2025-06-01", "2026-06-01"),
                 end=c("2024-08-31", 
                       "2025-02-28",
                       "2026-04-30", "2025-11-30", "2026-04-30", 
                       "2024-12-13", "2025-12-19",
                       "2025-11-03",
                       "2024-06-04", "2024-09-04", "2024-12-04", "2025-03-04",
                       "2025-06-04", "2025-09-04", "2025-12-04", "2026-03-04",
                       "2025-06-04", "2026-06-04"),
                 task_type=c("Main", 
                             "Main",
                             "Main", "Sub", "Sub", 
                             "Main", "Main",
                             "Main",
                             "Main", "Main", "Main", "Main",
                             "Main", "Main", "Main", "Main",
                             "Main", "Main"))

tidy_df <- df %>% 
  mutate(start = as.Date(start), end = as.Date(end)) %>% 
  pivot_longer(cols = c("start", "end"), names_to = "date_type", values_to = "date")

tidy_df %>% 
  ggplot(aes(x=fct_rev(fct_inorder(task)), y=date, colour=task_type, group = group)) +
  geom_line(linewidth=10) +
  coord_flip() +
  scale_y_date(date_breaks = "3 month",
               date_labels = "%b, %y") +
  scale_colour_manual(name = "Task type", 
                      values = c("#1b4079", "#228B22")) + 
  labs(title="Bogard et al. OCS & U of L Collaborative Research Grant Gantt Chart",
       x = "Task",
       y = "Date") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.minor = element_line(colour="white", size=0.5),
        legend.position=c(0.9, 0.875),
        legend.spacing.y = unit(0, 'cm'),
        legend.key.size = unit(0.85, "cm"),
        plot.title = element_text(hjust = 0.5))

