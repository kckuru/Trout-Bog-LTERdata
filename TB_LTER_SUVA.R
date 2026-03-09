##############################
## Trout Bog Summer Browning ##
##############################
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

# --- Color palette ---
col_line  <- "#2A7F7F"  # deep teal
col_point <- "#1A5C5C"  # darker teal
col_trend <- "#8B4513"  # saddle brown for trend line

# --- 1. Load and process color data ---
color_raw <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl87_v13.csv") %>%
  filter(lakeid == "TB", wavelength %in% c(254, 440)) %>%
  mutate(
    sampledate = as.Date(sampledate),
    value_1cm  = value / cuvette,
    month      = month(sampledate),
    year       = year(sampledate)
  ) %>%
  filter(month %in% 5:8)

# --- 2. Average duplicates THEN pivot wide ---
color_wide <- color_raw %>%
  group_by(lakeid, sampledate, month, year, wavelength) %>%
  summarise(value_1cm = mean(value_1cm, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = wavelength,
    values_from = value_1cm,
    names_prefix = "A"
  ) %>%
  mutate(A440_m = A440 * 100)  # convert A440: cm⁻¹ → m⁻¹

# --- 3. Load surface DOC ---
doc_data <- read_csv("~/Documents/Kuru_Projects/NTL-LTER_data/Trout-Bog-LTERdata/ntl1_v14.csv") %>%
  filter(lakeid == "TB", depth == 0) %>%
  mutate(sampledate = as.Date(sampledate)) %>%
  group_by(lakeid, sampledate) %>%
  summarise(doc_mgL = mean(doc, na.rm = TRUE), .groups = "drop")

# --- 4. Merge and calculate SUVA254 ---
color_doc <- left_join(color_wide, doc_data, by = c("lakeid", "sampledate")) %>%
  mutate(SUVA254 = (A254 * 100) / doc_mgL)

# --- 5. Yearly summaries ---
ratio_summary <- color_doc %>%
  group_by(year) %>%
  summarise(
    mean_doc   = mean(doc_mgL,  na.rm = TRUE),
    mean_color = mean(A440_m,   na.rm = TRUE),
    mean_SUVA  = mean(SUVA254,  na.rm = TRUE),
    se_SUVA    = sd(SUVA254,    na.rm = TRUE) / sqrt(sum(!is.na(SUVA254))),
    n_samples  = sum(!is.na(SUVA254)),
    .groups = "drop"
  )

# --- 6. Linear model on SUVA ---
lm_SUVA    <- lm(mean_SUVA ~ year, data = ratio_summary)
lm_p       <- summary(lm_SUVA)$coefficients[2, 4]
lm_slope   <- round(summary(lm_SUVA)$coefficients[2, 1], 3)
lm_r2      <- round(summary(lm_SUVA)$r.squared, 2)
lm_p_label <- ifelse(lm_p < 0.001, "p < 0.001", paste0("p = ", round(lm_p, 3)))
lm_label   <- paste0("slope = ", lm_slope, ",  ", lm_p_label, ",  R² = ", lm_r2)

# --- 7. Shared theme ---
theme_browning <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    axis.line        = element_line(color = "gray40"),
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10)
  )

# --- 8. Three-panel figure ---
p1 <- ggplot(ratio_summary, aes(x = year, y = mean_doc)) +
  geom_line(color  = col_line,  linewidth = 1) +
  geom_point(color = col_point, size = 2) +
  labs(
    title    = "Dissolved organic matter is increasing",
    subtitle = "Annual summer means (May–Aug), Trout Bog Lake",
    y        = expression(DOC~(mg~C~L^{-1})),
    x        = NULL
  ) +
  theme_browning

p2 <- ggplot(ratio_summary, aes(x = year, y = mean_color)) +
  geom_line(color  = col_line,  linewidth = 1) +
  geom_point(color = col_point, size = 2) +
  labs(
    title    = "Water is darkening",
    subtitle = "Annual summer means (May–Aug)",
    y        = expression(A[440]~(m^{-1})),
    x        = NULL
  ) +
  theme_browning

# p3: SE ribbon removed — trend CI alone is sufficient and cleaner
p3 <- ggplot(ratio_summary, aes(x = year, y = mean_SUVA)) +
  geom_smooth(method   = "lm", se = TRUE,
              linetype  = "dashed",
              color     = col_trend,
              fill      = col_trend,
              alpha     = 0.15) +
  geom_line(color  = col_line,  linewidth = 1) +
  geom_point(color = col_point, size = 2) +
  labs(
    title    = expression(bold("DOM is becoming more aromatic  (SUVA"[254]*")")),
    subtitle = lm_label,
    y        = expression(SUVA[254]~(L~mg^{-1}~m^{-1})),
    x        = "Year"
  ) +
  theme_browning +
  theme(plot.title = element_text(face = "bold", size = 13))  # force bold on expression title

# --- 9. Combine with panel tags ---
final_plot <- p1 / p2 / p3 +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(plot.tag = element_text(size = 11,
                                          face = "bold",
                                          color = "gray30"))
  )

final_plot
