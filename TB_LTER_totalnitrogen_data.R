##################
## LTER TN Data ##
##################

library(tidyverse)
library(lubridate)

# Load the new CSV
data_nitrogen <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl1_v14.csv")

# Filter for Trout Bog lake
tb_nitrogen <- data_nitrogen %>%
  filter(lakeid == "TB")

# Ensure date format and extract year/month
tb_nitrogen <- tb_nitrogen %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  )

# Filter for summer months: May (5) to August (8)
summer_tb_nitrogen <- summer_tb_nitrogen %>%
  mutate(totnf_mgL = totnf / 1000)

summer_summary_nitrogen <- summer_tb_nitrogen %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(totnf_mgL, na.rm = TRUE),
    sd_value = sd(totnf_mgL, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year) %>%
  filter(!is.na(mean_value))

summer_summary_nitrogen

# Linear model
lm_model_nitrogen <- lm(mean_value ~ year, data = summer_summary_nitrogen)
model_summary_nitrogen <- summary(lm_model_nitrogen)

slope_n <- round(coef(lm_model_nitrogen)[2], 4)
p_value_n <- signif(model_summary_nitrogen$coefficients[2,4], 2)
r_squared_n <- round(model_summary_nitrogen$r.squared, 2)

annotation_text_n <- paste0(
  "Slope = ", slope_n, " mg/L/year\n",
  "p = ", p_value_n, "\n",
  "R² = ", r_squared_n
)

tn_trends_over_years <- ggplot(summer_summary_nitrogen, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#FDAE61", fill = "#FDAE61", alpha = 0.2, linetype = "dashed") +
  geom_label(
    aes(x = min(year) + 2, y = max(mean_value, na.rm = TRUE)),
    label = annotation_text_n,
    hjust = 0, size = 4,
    fill = "white", color = "black", alpha = 0.8
  ) +
  scale_x_continuous(breaks = unique(summer_summary_nitrogen$year)) +
  labs(
    title = "Summer (May–August) Total Nitrogen Trends for Trout Bog Lake",
    subtitle = "Mean total nitrogen ± SE",
    x = "Year",
    y = "Mean Total Nitrogen (mg/L)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20", size = 10, angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

tn_trends_over_years



