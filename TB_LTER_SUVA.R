####################
## LTER SUVA Data ##
####################

library(tidyverse)
library(lubridate)

# --- 1. Color data ---
color_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl87_v13.csv") %>%
  filter(lakeid == "TB") %>%
  mutate(
    sampledate = as.Date(sampledate),
    value_1cm = value / cuvette
  ) %>%
  select(lakeid, sampledate, value_1cm)

# --- 2. DOC data ---
doc_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl1_v14.csv") %>%
  filter(lakeid == "TB") %>%
  mutate(sampledate = as.Date(sampledate)) %>%
  select(lakeid, sampledate, doc_mgL = doc)

# --- 3. Merge color and DOC ---
suva_data <- left_join(color_data, doc_data, by = c("lakeid", "sampledate"))

# --- 4. Calculate SUVA ---
suva_data <- suva_data %>%
  mutate(suva = value_1cm / doc_mgL,
         month = month(sampledate),
         year  = year(sampledate)) %>%
  filter(month %in% 5:8)  # Summer months May–August

# --- 5. Summarize SUVA by year ---
suva_summary <- suva_data %>%
  group_by(year) %>%
  summarise(
    mean_suva = mean(suva, na.rm = TRUE),
    sd_suva   = sd(suva, na.rm = TRUE),
    se_suva   = sd_suva / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year)

print(suva_summary)

# Linear model
lm_suva <- lm(mean_suva ~ year, data = suva_summary)
summary(lm_suva)

# Plot SUVA trends
library(ggplot2)

ggplot(suva_summary, aes(x = year, y = mean_suva)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "blue", size = 2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_suva - se_suva, ymax = mean_suva + se_suva),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#FDAE61", fill = "#FDAE61", alpha = 0.2, linetype = "dashed") +
  labs(
    title = "Summer SUVA Trends for Trout Bog Lake",
    subtitle = "Absorbance normalized to 1 cm pathlength / DOC",
    x = "Year",
    y = "SUVA (L mg^-1 m^-1)"
  ) +
  theme_minimal(base_size = 14)


