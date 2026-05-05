library(tidyverse)
library(lubridate)
library(patchwork)

zoo_raw <- read_csv(
  "~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl37_v14.csv",
  show_col_types = FALSE
) %>%
  filter(lakeid == "TB") %>%
  mutate(
    sample_date = as.Date(sample_date),
    year = year(sample_date),
    month = month(sample_date)
  ) %>%
  filter(month %in% 5:8)

zoo_raw %>%
  filter(str_detect(species_name, regex("daphnia", ignore_case = TRUE))) %>%
  distinct(species_name) %>%
  arrange(species_name)
