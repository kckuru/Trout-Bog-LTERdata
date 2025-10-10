#######################
## LTER Calcium Data ##
#######################

library(tidyverse)
library(lubridate)

calcium_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl2_v13.csv")

# Filter for Trout Bog lake
tb_calcium <- calcium_data %>%
  filter(lakeid == "TB") %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  ) %>%
  filter(month %in% 5:8)  # Summer months: May–August

# Summarize calcium by year
summer_summary_calcium <- tb_calcium %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(ca, na.rm = TRUE),       # calcium values in mg/L
    sd_value = sd(ca, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year) %>%
  filter(!is.na(mean_value))   # remove years with no data

print(summer_summary_calcium)


lm_model_calcium <- lm(mean_value ~ year, data = summer_summary_calcium)
model_summary_calcium <- summary(lm_model_calcium)

p_value_ca <- signif(model_summary_calcium$coefficients[2,4], 2)
r_squared_ca <- round(model_summary_calcium$r.squared, 2)

annotation_text_ca <- paste0(
  "p = ", p_value_ca, "\n",
  "R² = ", r_squared_ca
)

ca_trends_over_years <- ggplot(summer_summary_calcium, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#FDAE61", fill = "#FDAE61", alpha = 0.2, linetype = "dashed") +
  geom_label(
    aes(x = min(year) + 2, y = max(mean_value, na.rm = TRUE)),
    label = annotation_text_ca,
    hjust = 0, size = 4,
    fill = "white", color = "black", alpha = 0.8
  ) +
  scale_x_continuous(breaks = unique(summer_summary_calcium$year)) +
  labs(
    title = "Summer (May–August) Calcium Trends for Trout Bog Lake",
    subtitle = "Mean calcium ± SE",
    x = "Year",
    y = "Mean Calcium (mg/L)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20", size = 10, angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ca_trends_over_years
