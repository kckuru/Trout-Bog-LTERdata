#################################################################
## Trout Bog N:P Stoichiometry and Redfield Ratio
#################################################################

library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

# ==============================================================
# SECTION 1: LOAD LTER NUTRIENT DATA
# ==============================================================

nutrient_data <- read_csv(
  "~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl1_v14.csv"
)


# ==============================================================
# SECTION 2: PREPARE TOTAL N AND TOTAL P
# ==============================================================

summer_np <- nutrient_data %>%
  
  filter(
    lakeid == "TB",
    depth == 0
  ) %>%
  
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  ) %>%
  
  filter(
    month %in% 5:8
  ) %>%
  
  select(
    sampledate,
    year,
    month,
    depth,
    total_N = totnuf,
    total_P = totpuf
  ) %>%
  
  filter(
    !is.na(total_N),
    !is.na(total_P),
    total_N > 0,
    total_P > 0
  ) %>%
  
  mutate(
    
    # Convert concentration to µmol/L
    N_umol = total_N / 14.01,
    
    P_umol = total_P / 30.97,
    
    # Molar N:P
    NP_ratio = N_umol / P_umol
  )


# Look at the data
print(summer_np)


annual_np <- summer_np %>%
  group_by(year) %>%
  summarise(
    mean_N = mean(total_N, na.rm = TRUE),
    median_N = median(total_N, na.rm = TRUE),
    
    mean_P = mean(total_P, na.rm = TRUE),
    median_P = median(total_P, na.rm = TRUE),
    
    mean_NP = mean(NP_ratio, na.rm = TRUE),
    median_NP = median(NP_ratio, na.rm = TRUE),
    
    min_NP = min(NP_ratio, na.rm = TRUE),
    max_NP = max(NP_ratio, na.rm = TRUE),
    
    n = n(),
    
    .groups = "drop"
  ) %>%
  arrange(year)

print(annual_np, n = Inf)


summer_np %>%
  summarise(
    n_total = n(),
    n_below_redfield = sum(NP_ratio < 16),
    percent_below_redfield = mean(NP_ratio < 16) * 100,
    n_above_redfield = sum(NP_ratio > 16),
    percent_above_redfield = mean(NP_ratio > 16) * 100,
    median_NP = median(NP_ratio),
    mean_NP = mean(NP_ratio)
  )


p_NP_components <- annual_np %>%
  
  select(
    year,
    median_N,
    median_P
  ) %>%
  
  pivot_longer(
    cols = c(median_N, median_P),
    names_to = "nutrient",
    values_to = "concentration"
  ) %>%
  
  mutate(
    nutrient = recode(
      nutrient,
      median_N = "Total N",
      median_P = "Total P"
    )
  ) %>%
  
  ggplot(
    aes(
      x = year,
      y = concentration
    )
  ) +
  
  geom_line(
    linewidth = 0.7
  ) +
  
  geom_point(
    size = 2
  ) +
  
  facet_wrap(
    ~ nutrient,
    scales = "free_y",
    ncol = 1
  ) +
  
  labs(
    title = "Long-term summer nutrient concentrations in Trout Bog",
    x = "Year",
    y = "Median concentration (µg/L)"
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      face = "bold"
    )
  )

p_NP_components


# ==============================================================
# LONG-TERM TRENDS IN TN, TP, AND N:P
# ==============================================================

lm_N <- lm(median_N ~ year, data = annual_np)
lm_P <- lm(median_P ~ year, data = annual_np)
lm_NP <- lm(median_NP ~ year, data = annual_np)

summary(lm_N)
summary(lm_P)
summary(lm_NP)


lm_log_NP <- lm(
  log10(median_NP) ~ year,
  data = annual_np
)

summary(lm_log_NP)


nutrient_data %>%
  filter(
    lakeid == "TB",
    depth == 0
  ) %>%
  mutate(
    month = month(sampledate)
  ) %>%
  filter(month %in% 5:8) %>%
  summarise(
    median_TN = median(totnuf, na.rm = TRUE),
    median_NO3NO2 = median(no3no2, na.rm = TRUE),
    median_NO2 = median(no2, na.rm = TRUE),
    median_NH4 = median(nh4, na.rm = TRUE)
  )

######################################
## Looking at dissolved inorganic N ##
######################################


din_data <- nutrient_data %>%
  filter(
    lakeid == "TB",
    depth == 0
  ) %>%
  mutate(
    month = month(sampledate),
    year = year(sampledate),
    
    DIN = no3no2 + nh4,
    
    DIN_fraction = DIN / totnuf * 100
  ) %>%
  filter(
    month %in% 5:8,
    !is.na(DIN),
    DIN > 0
  )

annual_din <- din_data %>%
  group_by(year) %>%
  summarise(
    median_DIN = median(DIN, na.rm = TRUE),
    mean_DIN = mean(DIN, na.rm = TRUE),
    
    median_DIN_fraction = median(
      DIN_fraction,
      na.rm = TRUE
    ),
    
    n = n(),
    
    .groups = "drop"
  )

print(annual_din, n = Inf)


lm_DIN <- lm(median_DIN ~ year, data = annual_din)

summary(lm_DIN)

lm_log_DIN <- lm(log10(median_DIN) ~ year, data = annual_din)

summary(lm_log_DIN)

lm_DIN_fraction <- lm(
  median_DIN_fraction ~ year,
  data = annual_din %>% filter(!is.na(median_DIN_fraction))
)

summary(lm_DIN_fraction)


