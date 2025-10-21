##########################
## LTER Manganese Data ##
#########################

library(tidyverse)
library(lubridate)

# Load manganese data
manganese_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl2_v13.csv")

# Filter for Trout Bog lake
tb_manganese <- manganese_data %>%
  filter(lakeid == "TB")

# Ensure date format and extract year/month
tb_manganese <- tb_manganese %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  )

# Filter for summer months (May-August, months 5-8)
summer_tb_manganese <- tb_manganese %>%
  filter(month %in% c(5, 6, 7, 8))

# Calculate summer summary statistics by year
summer_summary_manganese <- summer_tb_manganese %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(mn, na.rm = TRUE),
    sd_value = sd(mn, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year) %>%
  filter(!is.na(mean_value))

summer_summary_manganese

lm_model_manganese <- lm(mean_value ~ year, data = summer_summary_manganese)
model_summary_manganese <- summary(lm_model_manganese)

p_value_mn <- signif(model_summary_manganese$coefficients[2,4], 2)
r_squared_mn <- round(model_summary_manganese$r.squared, 2)

annotation_text_mn <- paste0(
  "p = ", p_value_mn, "\n",
  "R² = ", r_squared_mn
)

mn_trends_over_years <- ggplot(summer_summary_manganese, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#FDAE61", fill = "#FDAE61", alpha = 0.2, linetype = "dashed") +
  geom_label(
    aes(x = min(year) + 2, y = max(mean_value, na.rm = TRUE)),
    label = annotation_text_mn,
    hjust = 0, size = 4,
    fill = "white", color = "black", alpha = 0.8
  ) +
  scale_x_continuous(breaks = unique(summer_summary_manganese$year)) +
  labs(
    title = "Summer (May–August) Manganese Trends for Trout Bog Lake",
    subtitle = "Mean Manganese ± SE",
    x = "Year",
    y = "Mean Manganese (mg/L)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20", size = 10, angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

mn_trends_over_years

