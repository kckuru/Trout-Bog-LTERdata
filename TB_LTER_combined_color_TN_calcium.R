################################################
## Multi-panel Plot: Color, Nitrogen, Calcium ##
################################################

library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)  # for combining plots

# --- 1. Color Data ---
color_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl87_v13.csv")

summer_summary_color <- color_data %>%
  filter(lakeid == "TB") %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  ) %>%
  filter(month %in% 5:8) %>%
  mutate(value_1cm = value / cuvette) %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(value_1cm, na.rm = TRUE),
    sd_value = sd(value_1cm, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year)

lm_color <- lm(mean_value ~ year, data = summer_summary_color)
model_color <- summary(lm_color)

annotation_text_color <- paste0(
  "p = ", signif(model_color$coefficients[2,4], 2), "\n",
  "R² = ", round(model_color$r.squared, 2)
)

plot_color <- ggplot(summer_summary_color, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "blue", size = 2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_label(aes(x = min(year) + 2, y = max(mean_value)),
             label = annotation_text_color,
             hjust = 0, size = 4,
             fill = "white", color = "black", alpha = 0.8) +
  scale_x_continuous(breaks = unique(summer_summary_color$year)) +
  labs(
    title = "Trout Bog Color Trends",
    subtitle = "Mean absorbance ± SE",
    x = "Year",
    y = "Absorbance (1 cm)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 2. Nitrogen Data ---
nitrogen_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl1_v14.csv")

summer_summary_nitrogen <- nitrogen_data %>%
  filter(lakeid == "TB") %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  ) %>%
  filter(month %in% 5:8) %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(totnf, na.rm = TRUE),
    sd_value = sd(totnf, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year) %>%
  filter(!is.na(mean_value))

lm_nitrogen <- lm(mean_value ~ year, data = summer_summary_nitrogen)
model_nitrogen <- summary(lm_nitrogen)

annotation_text_nitrogen <- paste0(
  "p = ", signif(model_nitrogen$coefficients[2,4], 2), "\n",
  "R² = ", round(model_nitrogen$r.squared, 2)
)

plot_nitrogen <- ggplot(summer_summary_nitrogen, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "green", size = 2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_label(aes(x = min(year) + 2, y = max(mean_value, na.rm = TRUE)),
             label = annotation_text_nitrogen,
             hjust = 0, size = 4,
             fill = "white", color = "black", alpha = 0.8) +
  scale_x_continuous(breaks = unique(summer_summary_nitrogen$year)) +
  labs(
    title = "Trout Bog Total Nitrogen",
    subtitle = "Mean total nitrogen ± SE",
    x = "Year",
    y = "Total Nitrogen (mg/L)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 3. Calcium Data ---
calcium_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl2_v13.csv")

summer_summary_calcium <- calcium_data %>%
  filter(lakeid == "TB") %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  ) %>%
  filter(month %in% 5:8) %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(ca, na.rm = TRUE),
    sd_value = sd(ca, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year) %>%
  filter(!is.na(mean_value))

lm_calcium <- lm(mean_value ~ year, data = summer_summary_calcium)
model_calcium <- summary(lm_calcium)

annotation_text_calcium <- paste0(
  "p = ", signif(model_calcium$coefficients[2,4], 2), "\n",
  "R² = ", round(model_calcium$r.squared, 2)
)

plot_calcium <- ggplot(summer_summary_calcium, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "red", size = 2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_label(aes(x = min(year) + 2, y = max(mean_value, na.rm = TRUE)),
             label = annotation_text_calcium,
             hjust = 0, size = 4,
             fill = "white", color = "black", alpha = 0.8) +
  scale_x_continuous(breaks = unique(summer_summary_calcium$year)) +
  labs(
    title = "Trout Bog Calcium",
    subtitle = "Mean calcium ± SE",
    x = "Year",
    y = "Calcium (mg/L)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Combine plots ---
combined_plot <- plot_color / plot_nitrogen / plot_calcium +
  plot_layout(ncol = 1) &
  theme(plot.title = element_text(face = "bold"))

combined_plot
