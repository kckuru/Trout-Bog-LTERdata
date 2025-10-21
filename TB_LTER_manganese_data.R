##########################
## LTER Manganese Data ##
#########################

library(tidyverse)
library(lubridate)

manganese_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl2_v13.csv")

# Filter for Trout Bog lake
tb_manganese <- data_manganese %>%
  filter(lakeid == "TB")

# Ensure date format and extract year/month
tb_manganese <- tb_manganese %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  )

summer_summary_manganese <- summer_tb_manganese %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(totnf_mgL, na.rm = TRUE),
    sd_value = sd(totnf_mgL, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year) %>%
  filter(!is.na(mean_value))

summer_summary_manganese