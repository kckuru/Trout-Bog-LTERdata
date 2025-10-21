##########################
## LTER Sodium Data ##
#########################

library(tidyverse)
library(lubridate)

# Load manganese data
sodium_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl2_v13.csv")

# Filter for Trout Bog lake
tb_sodium <- sodium_data %>%
  filter(lakeid == "TB")

# Ensure date format and extract year/month
tb_sodium <- tb_sodium %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  )

# Filter for summer months (May-August, months 5-8)
summer_tb_sodium <- tb_sodium %>%
  filter(month %in% c(5, 6, 7, 8))

# Calculate summer summary statistics by year
summer_summary_sodium <- summer_tb_sodium %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(na, na.rm = TRUE),
    sd_value = sd(na, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year) %>%
  filter(!is.na(mean_value))

summer_summary_sodium

lm_model_sodium <- lm(mean_value ~ year, data = summer_summary_sodium)
model_summary_sodium <- summary(lm_model_sodium)

p_value_sodium <- signif(model_summary_sodium$coefficients[2,4], 2)
r_squared_sodium <- round(model_summary_sodium$r.squared, 2)

annotation_text_sodium <- paste0(
  "p = ", p_value_sodium, "\n",
  "R² = ", r_squared_sodium
)

sodium_trends_over_years <- ggplot(summer_summary_sodium, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#FDAE61", fill = "#FDAE61", alpha = 0.2, linetype = "dashed") +
  geom_label(
    aes(x = min(year) + 2, y = max(mean_value, na.rm = TRUE)),
    label = annotation_text_sodium,
    hjust = 0, size = 4,
    fill = "white", color = "black", alpha = 0.8
  ) +
  scale_x_continuous(breaks = unique(summer_summary_sodium$year)) +
  labs(
    title = "Summer (May–August) Sodium Trends for Trout Bog Lake",
    subtitle = "Mean Sodium ± SE",
    x = "Year",
    y = "Mean Sodium (mg/L)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20", size = 10, angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

sodium_trends_over_years

