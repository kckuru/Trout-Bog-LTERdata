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
# Water color indicates dissolved organic matter content
# Higher absorbance = darker water = more dissolved organic carbon

# Import color data from NTL-LTER dataset
color_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl87_v13.csv")

# Calculate summer mean absorbance values with standard errors
summer_summary_color <- color_data %>%
  # Filter for Trout Bog lake only
  filter(lakeid == "TB") %>%
  # Parse dates and extract temporal components
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  ) %>%
  # Restrict to summer months (May through August)
  filter(month %in% 5:8) %>%
  # Standardize absorbance to 1 cm cuvette path length
  # Different cuvettes have different path lengths; this normalizes measurements
  mutate(value_1cm = value / cuvette) %>%
  # Calculate annual statistics
  group_by(year) %>%
  summarise(
    mean_value = mean(value_1cm, na.rm = TRUE),           # Annual mean
    sd_value = sd(value_1cm, na.rm = TRUE),               # Standard deviation
    se_value = sd_value / sqrt(n()),                       # Standard error
    n_samples = n()                                         # Sample size
  ) %>%
  arrange(year)

# Fit linear regression model to assess temporal trend
# Tests: Is water color changing over time?
lm_color <- lm(mean_value ~ year, data = summer_summary_color)
model_color <- summary(lm_color)

# Create annotation text with p-value and R-squared
# Format p-value for readability
p_val_color <- model_color$coefficients[2,4]
p_text_color <- ifelse(p_val_color < 0.001, "p < 0.001", 
                       paste0("p = ", round(p_val_color, 3)))

annotation_text_color <- paste0(
  "Linear trend:\n",
  "R² = ", round(model_color$r.squared, 2), "\n",
  p_text_color
)

# Create color trend plot with improvements
plot_color <- ggplot(summer_summary_color, aes(x = year, y = mean_value)) +
  # Add regression line with 95% confidence interval (behind other elements)
  geom_smooth(method = "lm", se = TRUE, 
              color = color_palette["color"], 
              fill = color_palette["color"], 
              alpha = 0.12, linewidth = 0.8) +
  # Add connecting line between years (lighter to emphasize regression)
  geom_line(color = color_palette["color"], linewidth = 0.7, alpha = 0.5) +
  # Add points for each year
  geom_point(color = color_palette["color"], size = 2.5, alpha = 0.9) +
  # Add error bars showing ± 1 standard error
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.6, color = "gray30", alpha = 0.7, linewidth = 0.6) +
  # Add statistical annotation in bottom right to avoid data
  annotate("label", 
           x = max(summer_summary_color$year) - 8, 
           y = min(summer_summary_color$mean_value) + 0.03,
           label = annotation_text_color,
           hjust = 0, size = 3.5,
           fill = "white", color = "black", alpha = 0.85,
           label.padding = unit(0.3, "lines")) +
  # Clean x-axis: show every 5 years
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  # Start y-axis at 0 for honest visual representation
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  # Add labels with interpretation
  labs(
    tag = "A",  # Panel label for multi-panel figure
    title = "Trout Bog Color Trends",
    x = "Year",
    y = "Mean Absorbance (1 cm)"
  ) +
  # Use minimal theme for clean appearance
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.tag = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30")
  )

################################################################################
## SECTION 2: TOTAL NITROGEN DATA
################################################################################
# Total nitrogen includes all forms: nitrate, nitrite, ammonia, organic N
# Important indicator of nutrient status and potential eutrophication

# Import nitrogen data from NTL-LTER dataset
nitrogen_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl1_v14.csv")

# Calculate summer mean total nitrogen with standard errors
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
    mean_value = mean(totnf, na.rm = TRUE),      # totnf = total nitrogen filtered
    sd_value = sd(totnf, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year) %>%
  # Remove years with no nitrogen data
  filter(!is.na(mean_value))

# Fit linear regression for nitrogen trend
lm_nitrogen <- lm(mean_value ~ year, data = summer_summary_nitrogen)
model_nitrogen <- summary(lm_nitrogen)

# Create annotation with regression statistics
p_val_nitrogen <- model_nitrogen$coefficients[2,4]
p_text_nitrogen <- ifelse(p_val_nitrogen < 0.001, "p < 0.001", 
                          paste0("p = ", round(p_val_nitrogen, 4)))

annotation_text_nitrogen <- paste0(
  "Linear trend:\n",
  "R² = ", round(model_nitrogen$r.squared, 2), "\n",
  p_text_nitrogen
)

# Create nitrogen trend plot with improvements
plot_nitrogen <- ggplot(summer_summary_nitrogen, aes(x = year, y = mean_value)) +
  # Add regression line with 95% confidence interval
  geom_smooth(method = "lm", se = TRUE, 
              color = color_palette["color"], 
              fill = color_palette["color"], 
              alpha = 0.12, linewidth = 0.8) +
  geom_line(color = color_palette["nitrogen"], linewidth = 0.7, alpha = 0.5) +
  geom_point(color = color_palette["nitrogen"], size = 2.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.4, color = "gray40", alpha = 0.6, linewidth = 0.5) +
  # Position annotation to avoid data
  annotate("label",
           x = min(summer_summary_nitrogen$year) + 3,
           y = max(summer_summary_nitrogen$mean_value, na.rm = TRUE) - 150,
           label = annotation_text_nitrogen,
           hjust = 0, size = 3.5,
           fill = "white", color = "black", alpha = 0.85,
           label.padding = unit(0.3, "lines")) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    tag = "B",
    title = "Trout Bog Total Nitrogen",
    x = "Year",
    y = "Mean Total Nitrogen (mg/L)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.tag = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30")
  )

################################################################################
## SECTION 3: CALCIUM DATA
################################################################################
# Calcium is a major ion indicating weathering and groundwater input
# Important for aquatic organisms (e.g., zooplankton exoskeletons)

# Import calcium data from NTL-LTER dataset
calcium_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl2_v13.csv")

# Calculate summer mean calcium concentrations with standard errors
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
    mean_value = mean(ca, na.rm = TRUE),         # ca = calcium concentration
    sd_value = sd(ca, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year) %>%
  # Remove years with no calcium data
  filter(!is.na(mean_value))

# Fit linear regression for calcium trend
lm_calcium <- lm(mean_value ~ year, data = summer_summary_calcium)
model_calcium <- summary(lm_calcium)

# Create annotation with regression statistics
p_val_calcium <- model_calcium$coefficients[2,4]
p_text_calcium <- paste0("p = ", round(p_val_calcium, 2))

annotation_text_calcium <- paste0(
  "Linear trend:\n",
  "R² = ", round(model_calcium$r.squared, 2), "\n",
  p_text_calcium, " (NS)"  # NS = not significant
)

# Create calcium trend plot with improvements
plot_calcium <- ggplot(summer_summary_calcium, aes(x = year, y = mean_value)) +
  # Add regression line (will be nearly flat due to no trend)
  geom_smooth(method = "lm", se = TRUE, 
              color = color_palette["color"], 
              fill = color_palette["color"], 
              alpha = 0.12, linewidth = 0.8) +
  geom_line(color = color_palette["calcium"], linewidth = 0.7, alpha = 0.5) +
  geom_point(color = color_palette["calcium"], size = 2.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.4, color = "gray40", alpha = 0.6, linewidth = 0.5) +
  annotate("label",
           x = min(summer_summary_calcium$year) + 3,
           y = max(summer_summary_calcium$mean_value, na.rm = TRUE) - 0.15,
           label = annotation_text_calcium,
           hjust = 0, size = 3.5,
           fill = "white", color = "black", alpha = 0.85,
           label.padding = unit(0.3, "lines")) +
  scale_x_continuous(breaks = seq(1980, 2025, by = 5)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    tag = "C",
    title = "Trout Bog Calcium",
    x = "Year",
    y = "Mean Calcium (mg/L)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.tag = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30")
  )

################################################################################
## SECTION 4: COMBINE PLOTS INTO MULTI-PANEL FIGURE
################################################################################
# Stack three plots vertically using patchwork syntax
# The "/" operator stacks plots vertically
# The "&" operator applies theme to all plots

combined_plot <- plot_color / plot_nitrogen / plot_calcium +
  # Arrange in single column (3 rows)
  plot_layout(ncol = 1) &
  # Apply theme adjustments to all panels
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

combined_plot
