# =============================================================================
# Harvest Index vs. Year of Cultivar Release — Genetic Gain Analysis
# Figure 5: two versions
#   fig5_hi_trends.tiff/.pdf         — single panel, all lines together
#   fig5_hi_trends_facets.tiff/.pdf  — multi-panel, one facet per study
# =============================================================================
# Author: Elvis F. Elli
# Data:   data/hi_yor.csv
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)
cat("Project root:", here(), "\n")

library(ggplot2)
library(ggpmisc)
library(ggrepel)
library(dplyr)
library(scales)

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

# --- Per-study regression stats ----------------------------------------------
stats_df <- df %>%
  group_by(Study) %>%
  summarise(
    n         = n(),
    yor_min   = min(YOR),
    yor_max   = max(YOR),
    hi_mean   = mean(HI),
    slope     = coef(lm(HI ~ YOR))[2],
    intercept = coef(lm(HI ~ YOR))[1],
    r2        = summary(lm(HI ~ YOR))$r.squared,
    .groups   = "drop"
  ) %>%
  mutate(
    rel_slope = slope / hi_mean * 100,
    hi_end    = intercept + slope * yor_max,
    # ASCII-safe label: avoid Unicode superscripts that fail in TIFF/PDF devices
    label = paste0(
      as.character(Study), "\n",
      sprintf("b = %.2f x10-3 yr-1 (%.2f%% yr-1)",
              slope * 1000, rel_slope)
    )
  )

# --- Smooth predicted lines (one per study) ----------------------------------
line_df <- do.call(rbind, lapply(seq_len(nrow(stats_df)), function(i) {
  s <- stats_df[i, ]
  yor_seq <- seq(s$yor_min, s$yor_max, length.out = 100)
  data.frame(
    Study = as.character(s$Study),
    YOR   = yor_seq,
    HI    = s$intercept + s$slope * yor_seq
  )
}))
line_df$Study <- factor(line_df$Study, levels = study_order)

# =============================================================================
# FIGURE 5A — Single panel (all lines together, labelled at right end)
# =============================================================================
x_lim <- c(1918, 2085)
y_lim <- c(0.14, 0.68)

p_single <- ggplot() +

  geom_line(data = line_df,
            aes(x = YOR, y = HI, colour = Study),
            linewidth = 1.0) +

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
    xlim           = c(2025, 2085)
  ) +

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

ggsave(here("figures", "fig5_hi_trends.tiff"),
       plot = p_single, width = 8.5, height = 5, dpi = 400, bg = "white")
ggsave(here("figures", "fig5_hi_trends.pdf"),
       plot = p_single, width = 8.5, height = 5, device = cairo_pdf)
cat("Saved: figures/fig5_hi_trends.tiff & .pdf\n")

# =============================================================================
# FIGURE 5B — Multi-panel faceted version (one panel per study)
# =============================================================================

# Facet label: study name + slope line
facet_labels <- setNames(
  paste0(
    study_order, "\n",
    sprintf("b = %.2f x10-3 yr-1  R2 = %.3f",
            stats_df$slope[match(study_order, stats_df$Study)] * 1000,
            stats_df$r2[match(study_order, stats_df$Study)])
  ),
  study_order
)

p_facets <- ggplot(df, aes(x = YOR, y = HI, colour = Study)) +

  geom_point(alpha = 0.55, size = 1.2, shape = 16) +

  geom_line(data = line_df,
            aes(x = YOR, y = HI, colour = Study),
            linewidth = 0.9, inherit.aes = FALSE) +

  facet_wrap(~ Study,
             ncol    = 3,
             scales  = "free_x",
             labeller = labeller(Study = facet_labels)) +

  scale_colour_manual(values = pal) +

  scale_y_continuous(
    name   = "Harvest Index (HI)",
    limits = c(0.10, 0.70),
    breaks = seq(0.10, 0.70, 0.10),
    labels = number_format(accuracy = 0.01),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name   = "Year of Cultivar Release",
    expand = c(0.03, 0)
  ) +

  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    legend.position  = "none",
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.4),
    strip.text       = element_text(size = 8, lineheight = 1.2, margin = margin(3, 3, 3, 3)),
    axis.title       = element_text(size = 10, face = "plain", color = "black"),
    axis.text        = element_text(size = 8,  color = "black"),
    axis.ticks       = element_line(color = "black", linewidth = 0.3),
    axis.line        = element_line(color = "black", linewidth = 0.4),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.4),
    panel.spacing    = unit(0.6, "lines"),
    plot.margin      = margin(10, 10, 10, 10, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(here("figures", "fig5_hi_trends_facets.tiff"),
       plot = p_facets, width = 10, height = 7, dpi = 400, bg = "white")
ggsave(here("figures", "fig5_hi_trends_facets.pdf"),
       plot = p_facets, width = 10, height = 7, device = cairo_pdf)
cat("Saved: figures/fig5_hi_trends_facets.tiff & .pdf\n")

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
  rename("slope_x1e3" = slope, "rel_%_yr" = rel_slope) %>%
  print(width = 120)
