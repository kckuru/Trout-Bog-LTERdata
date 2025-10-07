########################
## LTER TB Color Data ##
########################

library(tidyverse)
library(lubridate)

data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl87_v13.csv")

# Filter for Trout Bog lake
tb_data <- data %>%
  filter(lakeid == "TB")

# Ensure date format and extract year/month
tb_data <- tb_data %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  )

# Filter for summer months: May (5) to August (8)
summer_tb <- tb_data %>%
  filter(month %in% 5:8)

# Normalize absorbance to 1 cm path length
summer_tb <- summer_tb %>%
  mutate(value_1cm = value / cuvette)

# Summarize by year
summer_summary <- summer_tb %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(value_1cm, na.rm = TRUE),
    sd_value = sd(value_1cm, na.rm = TRUE),
    n_samples = n()
  ) %>%
  arrange(year)

print(summer_summary)

library(ggplot2)

ggplot(summer_summary, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +  
  geom_point(color = "#D7191C", size = 3, alpha = 0.8) +  
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#FDAE61", linetype = "dashed", linewidth = 0.8) +  
  labs(
    title = "Summer (May–August) Color Trends for Trout Bog Lake",
    subtitle = "Mean absorbance normalized to a 1 cm pathlength",
    x = "Year",
    y = "Mean Absorbance (1 cm)",
    caption = "Data: NTL-LTER"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    panel.grid.minor = element_blank()
  )
