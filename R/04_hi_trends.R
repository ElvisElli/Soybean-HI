# =============================================================================
# Harvest Index vs. Year of Cultivar Release — Genetic Gain Analysis
# Figure 5  (single-panel)
# =============================================================================
# Author: Elvis F. Elli
# Data:   data/hi_yor.csv  (digitised from 6 published studies)
# Output: figures/fig5_hi_trends.tiff, .pdf
# =============================================================================

# --- Project root ------------------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)
cat("Project root:", here(), "\n")

# --- Packages ----------------------------------------------------------------
library(ggplot2)
library(ggpmisc)
library(ggrepel)
library(dplyr)
library(scales)
library(grid)

# --- Data --------------------------------------------------------------------
df <- read.csv(here("data", "hi_yor.csv"), header = TRUE)
colnames(df) <- c("Study", "YOR", "HI")
df <- df[complete.cases(df), ]
df$Study <- trimws(df$Study)

df$Study <- dplyr::recode(df$Study,
  "Balbao et al. (2018)"    = "Balboa et al. (2018)",
  "Koester et al. 2014)"    = "Koester et al. (2014)",
  "Waqar et al (2025)"      = "Waqar et al. (2025)"
)

study_order <- c(
  "Rowntree et al. (2014)",
  "Koester et al. (2014)",
  "Li et al. (2017)",
  "Balboa et al. (2018)",
  "Umburanas et al. (2022)",
  "Waqar et al. (2025)"
)
df$Study <- factor(df$Study, levels = study_order)

# --- Colour palette ----------------------------------------------------------
pal <- c(
  "Rowntree et al. (2014)"  = "#1f78b4",
  "Koester et al. (2014)"   = "#2ca25f",
  "Li et al. (2017)"        = "#e31a1c",
  "Balboa et al. (2018)"    = "#ff7f00",
  "Umburanas et al. (2022)" = "#6a3d9a",
  "Waqar et al. (2025)"     = "#b15928"
)

# --- Per-study stats ---------------------------------------------------------
stats_df <- df %>%
  group_by(Study) %>%
  summarise(
    n          = n(),
    yor_min    = min(YOR),
    yor_max    = max(YOR),
    hi_mean    = mean(HI),
    slope      = coef(lm(HI ~ YOR))[2],
    intercept  = coef(lm(HI ~ YOR))[1],
    r2         = summary(lm(HI ~ YOR))$r.squared,
    .groups    = "drop"
  ) %>%
  mutate(
    rel_slope = slope / hi_mean * 100,
    # predicted y at right end of each line
    hi_end    = intercept + slope * yor_max,
    # arrow: span 12 years near the centre of each line
    arr_x0    = (yor_min + yor_max) / 2 - 6,
    arr_x1    = (yor_min + yor_max) / 2 + 6,
    arr_y0    = intercept + slope * arr_x0,
    arr_y1    = intercept + slope * arr_x1,
    # two-line label: study name + slope stats
    label     = paste0(
      as.character(Study), "\n",
      sprintf("β = %.3f×10⁻³ yr⁻¹ (%.2f%% yr⁻¹)",
              slope * 1000, rel_slope)
    )
  )

# --- Build smooth predicted lines (one per study) ---------------------------
line_df <- do.call(rbind, lapply(seq_len(nrow(stats_df)), function(i) {
  s <- stats_df[i, ]
  yor_seq <- seq(s$yor_min, s$yor_max, length.out = 100)
  data.frame(
    Study     = as.character(s$Study),
    YOR       = yor_seq,
    HI        = s$intercept + s$slope * yor_seq
  )
}))
line_df$Study <- factor(line_df$Study, levels = study_order)

# --- Plot parameters ---------------------------------------------------------
x_lim <- c(1918, 2080)   # extra right margin for labels
y_lim <- c(0.14, 0.68)

p <- ggplot() +

  # --- Regression lines ------------------------------------------------------
  geom_line(data  = line_df,
            aes(x = YOR, y = HI, colour = Study),
            linewidth = 1.0) +

  # --- Directional arrows on each line (show slope direction) ----------------
  geom_segment(
    data  = stats_df,
    aes(x = arr_x0, xend = arr_x1,
        y = arr_y0, yend = arr_y1,
        colour = Study),
    arrow       = arrow(length = unit(0.20, "cm"), type = "closed"),
    linewidth   = 1.3,
    show.legend = FALSE
  ) +

  # --- Repelled labels at end of each line ------------------------------------
  geom_text_repel(
    data  = stats_df,
    aes(x = yor_max, y = hi_end, label = label, colour = Study),
    hjust          = 0,
    nudge_x        = 3,
    direction      = "y",
    segment.size   = 0.35,
    segment.color  = "grey50",
    segment.linetype = "dotted",
    box.padding    = 0.3,
    point.padding  = 0.2,
    size           = 2.8,
    lineheight     = 1.25,
    show.legend    = FALSE,
    seed           = 42,
    xlim           = c(2025, 2080)   # keep labels in right margin
  ) +

  # --- Axes & scales ---------------------------------------------------------
  scale_colour_manual(values = pal, name = NULL) +

  scale_x_continuous(
    name   = "Year of Cultivar Release",
    limits = x_lim,
    breaks = seq(1920, 2020, 20),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name   = "Harvest Index (HI)",
    limits = y_lim,
    breaks = seq(0.15, 0.65, 0.10),
    labels = number_format(accuracy = 0.01),
    expand = c(0, 0)
  ) +

  # --- Theme -----------------------------------------------------------------
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    legend.position  = "none",
    axis.title       = element_text(size = 11, face = "plain", color = "black"),
    axis.text        = element_text(size = 10, color = "black"),
    axis.ticks       = element_line(color = "black", linewidth = 0.4),
    axis.line        = element_line(color = "black", linewidth = 0.5),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin      = margin(10, 5, 10, 10, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# --- Save --------------------------------------------------------------------
ggsave(here("figures", "fig5_hi_trends.tiff"),
       plot = p, width = 8.5, height = 5, dpi = 400, bg = "white")
ggsave(here("figures", "fig5_hi_trends.pdf"),
       plot = p, width = 8.5, height = 5, device = cairo_pdf)
cat("Saved: figures/fig5_hi_trends.tiff & .pdf\n")

# --- Console summary ---------------------------------------------------------
cat("\nPer-study slope summary:\n")
stats_df %>%
  select(Study, n, yor_min, yor_max, hi_mean, slope, rel_slope, r2) %>%
  mutate(
    slope     = round(slope * 1000, 3),
    rel_slope = round(rel_slope, 2),
    r2        = round(r2, 3),
    hi_mean   = round(hi_mean, 3)
  ) %>%
  rename("slope_x1e3" = slope, "rel_slope_%_yr" = rel_slope) %>%
  print(width = 120)
