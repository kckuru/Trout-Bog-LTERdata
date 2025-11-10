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
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year)


print(summer_summary)

#########################
## Doing some plotting ##
#########################

library(ggplot2)

# Linear model
lm_model <- lm(mean_value ~ year, data = summer_summary)
model_summary <- summary(lm_model)

# Slope, p-value, and R²
slope <- round(coef(lm_model)[2], 4)
p_value <- signif(model_summary$coefficients[2,4], 2) 
r_squared <- round(model_summary$r.squared, 2) 

annotation_text <- paste0(
  "p = ", p_value, "\n",
  "R² = ", r_squared
)

# Plot
abs_trends_over_years <- ggplot(summer_summary, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "blue", size = 2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#FDAE61", fill = "#FDAE61", alpha = 0.2, linetype = "dashed") +
  geom_label(
    aes(x = min(year) + 2, y = max(mean_value)),
    label = annotation_text,
    hjust = 0, size = 4,
    fill = "white", color = "black", alpha = 0.8
  ) +
  # 🔽 Add this line
  scale_x_continuous(breaks = unique(summer_summary$year)) +
  labs(
    title = "Summer (May–August) Color Trends for Trout Bog Lake",
    subtitle = "Mean absorbance normalized to a 1 cm pathlength",
    x = "Year",
    y = "Mean Absorbance (1 cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20", size = 10, angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

abs_trends_over_years


###############################
## Filter for post-2010 data ##
###############################

summer_post2010 <- summer_summary %>%
  filter(year > 2010)

# Linear model
lm_model <- lm(mean_value ~ year, data = summer_post2010)
model_summary <- summary(lm_model)

# Slope, p-value, and R²
slope <- round(coef(lm_model)[2], 4)
p_value <- signif(model_summary$coefficients[2,4], 2)
r_squared <- round(model_summary$r.squared, 2)

annotation_text <- paste0(
  "p = ", p_value, "\n",
  "R² = ", r_squared
)

# Plot (post-2010 only)
abs_trends_post2010 <- ggplot(summer_post2010, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "blue", size = 2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#FDAE61", fill = "#FDAE61", 
              alpha = 0.2, linetype = "dashed") +
  geom_label(
    aes(x = min(year) + 1, y = max(mean_value)),
    label = annotation_text,
    hjust = 0, size = 4,
    fill = "white", color = "black", alpha = 0.8
  ) +
  scale_x_continuous(breaks = unique(summer_post2010$year)) +
  labs(
    title = "Summer (May–August) Color Trends for Trout Bog Lake (Post-2010)",
    subtitle = "Mean absorbance normalized to a 1 cm pathlength",
    x = "Year",
    y = "Mean Absorbance (1 cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20", size = 10, angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

abs_trends_post2010
