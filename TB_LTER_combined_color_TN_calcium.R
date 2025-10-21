################################################################################
## Multi-panel Plot: Long-term Trends in Trout Bog Water Quality
## Color, Nitrogen, and Calcium Data Analysis
################################################################################

# Libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

# Define consistent color palette for all three parameters
color_palette <- c(
  color = "#0571b0",      # Blue - water color
  nitrogen = "#5aae61",   # Green - nitrogen
  calcium = "#ca0020"     # Red - calcium
)

################################################################################
## SECTION 1: WATER COLOR DATA (Absorbance)
################################################################################

color_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl87_v13.csv")

summer_summary_color <- color_data %>%
  filter(lakeid == "TB") %>%
  mutate(sampledate = as.Date(sampledate),
         year = year(sampledate),
         month = month(sampledate)) %>%
  filter(month %in% 5:8) %>%
  mutate(value_1cm = value / cuvette) %>%
  group_by(year) %>%
  summarise(mean_value = mean(value_1cm, na.rm = TRUE),
            sd_value = sd(value_1cm, na.rm = TRUE),
            se_value = sd_value / sqrt(n()),
            n_samples = n()) %>%
  arrange(year)

lm_color <- lm(mean_value ~ year, data = summer_summary_color)
model_color <- summary(lm_color)

p_val_color <- model_color$coefficients[2,4]
p_text_color <- ifelse(p_val_color < 0.001, "p < 0.001",
                       paste0("p = ", round(p_val_color, 3)))
annotation_text_color <- paste0("Linear trend:\n",
                                "R² = ", round(model_color$r.squared, 2), "\n",
                                p_text_color)

plot_color <- ggplot(summer_summary_color, aes(x = year, y = mean_value)) +
  geom_smooth(method = "lm", se = TRUE,
              color = color_palette["color"],
              fill = color_palette["color"],
              alpha = 0.12, linewidth = 0.8) +
  geom_line(color = color_palette["color"], linewidth = 0.7, alpha = 0.5) +
  geom_point(color = color_palette["color"], size = 2.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.6, color = "gray30", alpha = 0.7, linewidth = 0.6) +
  annotate("label",
           x = max(summer_summary_color$year) - 1,
           y = min(summer_summary_color$mean_value, na.rm = TRUE) +
             0.15 * diff(range(summer_summary_color$mean_value, na.rm = TRUE)),
           label = annotation_text_color,
           hjust = 1, vjust = 0,
           size = 3,
           fill = "white", color = "black",
           alpha = 0.8,
           label.padding = unit(0.15, "lines"),
           label.size = 0.25) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(tag = "A", title = "Trout Bog Color Trends",
       x = "Year", y = "Mean Absorbance (1 cm)") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.tag = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 14))

################################################################################
## SECTION 2: TOTAL NITROGEN DATA
################################################################################

nitrogen_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl1_v14.csv")

summer_summary_nitrogen <- nitrogen_data %>%
  filter(lakeid == "TB") %>%
  mutate(sampledate = as.Date(sampledate),
         year = year(sampledate),
         month = month(sampledate)) %>%
  filter(month %in% 5:8) %>%
  group_by(year) %>%
  summarise(mean_value = mean(totnf, na.rm = TRUE),
            sd_value = sd(totnf, na.rm = TRUE),
            se_value = sd_value / sqrt(n()),
            n_samples = n()) %>%
  arrange(year) %>%
  filter(!is.na(mean_value))

lm_nitrogen <- lm(mean_value ~ year, data = summer_summary_nitrogen)
model_nitrogen <- summary(lm_nitrogen)

p_val_nitrogen <- model_nitrogen$coefficients[2,4]
p_text_nitrogen <- ifelse(p_val_nitrogen < 0.001, "p < 0.001",
                          paste0("p = ", round(p_val_nitrogen, 3)))
annotation_text_nitrogen <- paste0("Linear trend:\n",
                                   "R² = ", round(model_nitrogen$r.squared, 2), "\n",
                                   p_text_nitrogen)

plot_nitrogen <- ggplot(summer_summary_nitrogen, aes(x = year, y = mean_value)) +
  geom_smooth(method = "lm", se = TRUE,
              color = color_palette["color"],
              fill = color_palette["color"],
              alpha = 0.12, linewidth = 0.8) +
  geom_line(color = color_palette["nitrogen"], linewidth = 0.7, alpha = 0.5) +
  geom_point(color = color_palette["nitrogen"], size = 2.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.4, color = "gray40", alpha = 0.6, linewidth = 0.5) +
  annotate("label",
           x = max(summer_summary_nitrogen$year) - 1,
           y = min(summer_summary_nitrogen$mean_value, na.rm = TRUE) +
             0.15 * diff(range(summer_summary_nitrogen$mean_value, na.rm = TRUE)),
           label = annotation_text_nitrogen,
           hjust = 1, vjust = 0,
           size = 3,
           fill = "white", color = "black",
           alpha = 0.8,
           label.padding = unit(0.15, "lines"),
           label.size = 0.25) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(tag = "B", title = "Trout Bog Total Nitrogen",
       x = "Year", y = "Mean Total Nitrogen (mg/L)") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.tag = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 14))

################################################################################
## SECTION 3: CALCIUM DATA
################################################################################

calcium_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl2_v13.csv")

summer_summary_calcium <- calcium_data %>%
  filter(lakeid == "TB") %>%
  mutate(sampledate = as.Date(sampledate),
         year = year(sampledate),
         month = month(sampledate)) %>%
  filter(month %in% 5:8) %>%
  group_by(year) %>%
  summarise(mean_value = mean(ca, na.rm = TRUE),
            sd_value = sd(ca, na.rm = TRUE),
            se_value = sd_value / sqrt(n()),
            n_samples = n()) %>%
  arrange(year) %>%
  filter(!is.na(mean_value))

lm_calcium <- lm(mean_value ~ year, data = summer_summary_calcium)
model_calcium <- summary(lm_calcium)

p_val_calcium <- model_calcium$coefficients[2,4]
p_text_calcium <- ifelse(p_val_calcium < 0.001, "p < 0.001",
                         paste0("p = ", round(p_val_calcium, 3)))
annotation_text_calcium <- paste0("Linear trend:\n",
                                  "R² = ", round(model_calcium$r.squared, 2), "\n",
                                  p_text_calcium)

plot_calcium <- ggplot(summer_summary_calcium, aes(x = year, y = mean_value)) +
  geom_smooth(method = "lm", se = TRUE,
              color = color_palette["color"],
              fill = color_palette["color"],
              alpha = 0.12, linewidth = 0.8) +
  geom_line(color = color_palette["calcium"], linewidth = 0.7, alpha = 0.5) +
  geom_point(color = color_palette["calcium"], size = 2.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.4, color = "gray40", alpha = 0.6, linewidth = 0.5) +
  annotate("label",
           x = max(summer_summary_calcium$year) - 1,
           y = min(summer_summary_calcium$mean_value, na.rm = TRUE) +
             0.15 * diff(range(summer_summary_calcium$mean_value, na.rm = TRUE)),
           label = annotation_text_calcium,
           hjust = 1, vjust = 0,
           size = 3,
           fill = "white", color = "black",
           alpha = 0.8,
           label.padding = unit(0.15, "lines"),
           label.size = 0.25) +
  scale_x_continuous(breaks = seq(1980, 2025, by = 5)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(tag = "C", title = "Trout Bog Calcium",
       x = "Year", y = "Mean Calcium (mg/L)") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.tag = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 14))

################################################################################
## SECTION 4: COMBINE PLOTS INTO MULTI-PANEL FIGURE
################################################################################

combined_plot <- plot_color / plot_nitrogen / plot_calcium +
  plot_layout(ncol = 1) &
  theme(plot.title = element_text(face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

combined_plot
