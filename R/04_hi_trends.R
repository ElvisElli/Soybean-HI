# =============================================================================
# Harvest Index vs. Year of Cultivar Release — Genetic Gain Analysis
# Figure 5
# =============================================================================
# Author: Elvis F. Elli
# Data:   data/hi_yor.csv  (extracted from meta-analysis.xlsx → YOR sheet)
# Output: figures/fig5_hi_trends.tiff, .pdf
#
# Source studies:
#   Rowntree et al. (2014) — Crop Science
#   Balboa et al. (2018)   — Crop Science
#   Li et al. (2017)       — Plant Breeding
#   Koester et al. (2014)  — Plant & Soil (eru187)
#   Umburanas et al. (2022) — Sci Reports
#   Waqar et al. (2025)    — Field Crops Research
# =============================================================================

# --- Project root (works in RStudio, Rscript, or source()) ------------------
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)
cat("Project root:", here(), "\n")

# --- Packages ----------------------------------------------------------------
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(scales)

# --- Data --------------------------------------------------------------------
df <- read.csv(here("data", "hi_yor.csv"), header = TRUE)
colnames(df) <- c("Study", "YOR", "HI")
df <- df[complete.cases(df), ]
df$Study <- trimws(df$Study)

# Standardise study labels (fix typos in source data)
df$Study <- dplyr::recode(df$Study,
  "Balbao et al. (2018)"    = "Balboa et al. (2018)",
  "Koester et al. 2014)"    = "Koester et al. (2014)",
  "Waqar et al (2025)"      = "Waqar et al. (2025)"
)

# Ordered factor so facets appear chronologically
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
study_colors <- c(
  "Rowntree et al. (2014)"  = "#1f78b4",
  "Koester et al. (2014)"   = "#33a02c",
  "Li et al. (2017)"        = "#e31a1c",
  "Balboa et al. (2018)"    = "#ff7f00",
  "Umburanas et al. (2022)" = "#6a3d9a",
  "Waqar et al. (2025)"     = "#b15928"
)

# --- Plot --------------------------------------------------------------------
p <- ggplot(df, aes(x = YOR, y = HI, colour = Study, fill = Study)) +

  geom_point(alpha = 0.6, size = 1.8, shape = 21,
             colour = "white", stroke = 0.3) +

  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +

  stat_poly_eq(
    aes(label = paste(after_stat(eq.label),
                      after_stat(rr.label), sep = "~~~")),
    formula = y ~ x,
    parse   = TRUE,
    size    = 2.8,
    label.x = "left",
    label.y = "top",
    color   = "black"
  ) +

  facet_wrap(~Study, ncol = 3, scales = "free_x") +

  scale_colour_manual(values = study_colors, guide = "none") +
  scale_fill_manual(values   = study_colors, guide = "none") +

  scale_y_continuous(
    name   = "Harvest Index (HI)",
    limits = c(0.15, 0.70),
    breaks = seq(0.2, 0.70, 0.10),
    labels = number_format(accuracy = 0.01),
    expand = c(0, 0)
  ) +
  labs(x = "Year of Cultivar Release") +

  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    axis.title       = element_text(size = 10, face = "plain", color = "black"),
    axis.text        = element_text(size = 9,  color = "black"),
    axis.ticks       = element_line(color = "black", linewidth = 0.4),
    axis.line        = element_line(color = "black", linewidth = 0.5),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.5),
    strip.text       = element_text(size = 9),
    plot.margin      = margin(10, 15, 10, 10, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# --- Save --------------------------------------------------------------------
ggsave(here("figures", "fig5_hi_trends.tiff"),
       plot = p, width = 9, height = 6, dpi = 400, bg = "white")
ggsave(here("figures", "fig5_hi_trends.pdf"),
       plot = p, width = 9, height = 6, device = cairo_pdf)
cat("Saved: figures/fig5_hi_trends.tiff & .pdf\n")

# --- Per-study regression summary --------------------------------------------
cat("\nPer-study linear regression (HI ~ Year of Cultivar Release):\n")
df %>%
  group_by(Study) %>%
  summarise(
    n     = n(),
    slope = round(coef(lm(HI ~ YOR))[2], 5),
    r2    = round(summary(lm(HI ~ YOR))$r.squared, 3),
    p     = round(coef(summary(lm(HI ~ YOR)))[2, 4], 4),
    .groups = "drop"
  ) %>%
  print()
