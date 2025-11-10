#################################################################
## Multi-panel Plot: Long-term Trends in Trout Bog Water Quality
#################################################################

# Libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

# Define consistent color palette (using each parameter's actual color)
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
slope_color <- model_color$coefficients[2,1]
p_text_color <- ifelse(p_val_color < 0.001, "p < 0.001",
                       paste0("p = ", round(p_val_color, 3)))
annotation_text_color <- paste0("y = ", round(slope_color, 4), "x + ", 
                                round(coef(lm_color)[1], 2), "\n",
                                "R² = ", round(model_color$r.squared, 3), ", ",
                                p_text_color)

plot_color <- ggplot(summer_summary_color, aes(x = year, y = mean_value)) +
  geom_smooth(method = "lm", se = TRUE,
              color = color_palette["color"],
              fill = color_palette["color"],
              alpha = 0.15, linewidth = 1) +
  geom_line(color = color_palette["color"], linewidth = 0.5, alpha = 0.4) +
  geom_point(color = color_palette["color"], size = 2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0, color = color_palette["color"], alpha = 0.4, linewidth = 0.5) +
  annotate("text",
           x = min(summer_summary_color$year) + 2,
           y = max(summer_summary_color$mean_value, na.rm = TRUE) * 0.95,
           label = annotation_text_color,
           hjust = 0, vjust = 1,
           size = 3.2,
           color = "gray20",
           fontface = "italic") +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(tag = "A", title = "Water Color (Absorbance)",
       x = NULL, y = "Mean Absorbance (1 cm)") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    plot.tag = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(5, 10, 5, 10)
  )

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
slope_nitrogen <- model_nitrogen$coefficients[2,1]
p_text_nitrogen <- ifelse(p_val_nitrogen < 0.001, "p < 0.001",
                          paste0("p = ", round(p_val_nitrogen, 3)))
annotation_text_nitrogen <- paste0("y = ", round(slope_nitrogen, 2), "x + ", 
                                   round(coef(lm_nitrogen)[1], 0), "\n",
                                   "R² = ", round(model_nitrogen$r.squared, 3), ", ",
                                   p_text_nitrogen)

plot_nitrogen <- ggplot(summer_summary_nitrogen, aes(x = year, y = mean_value)) +
  geom_smooth(method = "lm", se = TRUE,
              color = color_palette["nitrogen"],
              fill = color_palette["nitrogen"],
              alpha = 0.15, linewidth = 1) +
  geom_line(color = color_palette["nitrogen"], linewidth = 0.5, alpha = 0.4) +
  geom_point(color = color_palette["nitrogen"], size = 2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0, color = color_palette["nitrogen"], alpha = 0.4, linewidth = 0.5) +
  annotate("text",
           x = min(summer_summary_nitrogen$year) + 2,
           y = max(summer_summary_nitrogen$mean_value, na.rm = TRUE) * 0.95,
           label = annotation_text_nitrogen,
           hjust = 0, vjust = 1,
           size = 3.2,
           color = "gray20",
           fontface = "italic") +
  scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(tag = "B", title = "Total Nitrogen",
       x = NULL, y = "Mean Total Nitrogen (µg/L)") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    plot.tag = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(5, 10, 5, 10)
  )

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
slope_calcium <- model_calcium$coefficients[2,1]
p_text_calcium <- ifelse(p_val_calcium < 0.001, "p < 0.001",
                         paste0("p = ", round(p_val_calcium, 3)))
annotation_text_calcium <- paste0("y = ", round(slope_calcium, 4), "x + ", 
                                  round(coef(lm_calcium)[1], 2), "\n",
                                  "R² = ", round(model_calcium$r.squared, 3), ", ",
                                  p_text_calcium)

plot_calcium <- ggplot(summer_summary_calcium, aes(x = year, y = mean_value)) +
  geom_smooth(method = "lm", se = TRUE,
              color = color_palette["calcium"],
              fill = color_palette["calcium"],
              alpha = 0.15, linewidth = 1) +
  geom_line(color = color_palette["calcium"], linewidth = 0.5, alpha = 0.4) +
  geom_point(color = color_palette["calcium"], size = 2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0, color = color_palette["calcium"], alpha = 0.4, linewidth = 0.5) +
  annotate("text",
           x = min(summer_summary_calcium$year) + 2,
           y = min(summer_summary_calcium$mean_value, na.rm = TRUE) * 1.15,
           label = annotation_text_calcium,
           hjust = 0, vjust = -1.8,
           size = 3.2,
           color = "gray20",
           fontface = "italic") +
  scale_x_continuous(breaks = seq(1980, 2025, by = 5)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(tag = "C", title = "Calcium",
       x = "Year", y = "Mean Calcium (mg/L)") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    plot.tag = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(5, 10, 5, 10)
  )

################################################################################
## SECTION 4: COMBINE PLOTS INTO MULTI-PANEL FIGURE
################################################################################

combined_plot <- plot_color / plot_nitrogen / plot_calcium +
  plot_layout(ncol = 1)

combined_plot 


