#############################
## LTER Precipitation Data ##
#############################

library(tidyverse)
library(lubridate)

# Load precipitation data
precip_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl18_v10.csv")

# Inspect data
glimpse(precip_data)
names(precip_data)

# Summarize total annual precipitation
annual_precip <- precip_data %>%
  group_by(year4) %>%
  summarise(total_precip_mm = sum(precip, na.rm = TRUE))

# Plot
ggplot(annual_precip, aes(x = year4, y = total_precip_mm)) +
  geom_col(fill = "skyblue", color = "grey30") +
  geom_line(linewidth = 1.1, color = "steelblue4") +
  geom_point(size = 2, color = "navy") +
  labs(
    title = "Total Annual Precipitation - Minocqua Dam",
    x = "Year",
    y = "Total Annual Precipitation (mm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Fit linear model to assess trend
lm_model_precip <- lm(total_precip_mm ~ year4, data = annual_precip)
model_summary_precip <- summary(lm_model_precip)
p_value_precip <- signif(model_summary_precip$coefficients[2,4], 2)
r_squared_precip <- round(model_summary_precip$r.squared, 2)
annotation_text_precip <- paste0(
  "p = ", p_value_precip, "\n",
  "R² = ", r_squared_precip
)

# Add annotation to plot
annual_precip_plot <- ggplot(annual_precip, aes(x = year4, y = total_precip_mm)) +
  geom_col(fill = "skyblue", color = "grey30") +
  geom_line(linewidth = 1.1, color = "steelblue4") +
  geom_point(size = 2, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange", fill = "darkorange", alpha = 0.2, linetype = "dashed") +
  geom_label(
    aes(x = min(year4) + 2, y = max(total_precip_mm, na.rm = TRUE)),
    label = annotation_text_precip,
    hjust = 0,
    vjust = 1,
    size = 5,
    fill = "white",
    color = "black"
  ) +
  labs(
    title = "Total Annual Precipitation",
    x = "Year",
    y = "Total Annual Precipitation (mm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

annual_precip_plot
