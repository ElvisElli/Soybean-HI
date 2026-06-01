# =============================================================================
# Harvest Index vs. Year of Cultivar Release — Genetic Gain Analysis
# Figure 5
# =============================================================================
# Data extracted from: Rowntree et al. (2014), Balboa et al. (2018),
#   Li et al. (2017), Koester et al. (2014), Umburanas et al. (2022),
#   Waqar et al. (2025)
# Author: Elvis F. Elli
# =============================================================================

# --- Set working directory to project root -----------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  root <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  root     <- dirname(dirname(normalizePath(sub("--file=", "", file_arg[1]))))
}
setwd(root)
cat("Project root:", getwd(), "\n")

# --- Packages ----------------------------------------------------------------
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(scales)

# --- Data --------------------------------------------------------------------
df <- read.csv("data/hi_yor.csv", header = TRUE)
colnames(df) <- c("Study", "YOR", "HI")
df <- df[complete.cases(df), ]
df$Study <- trimws(df$Study)

# Consistent short labels for legend
df$Study_label <- dplyr::recode(df$Study,
  "Rowntree et al. (2014)"  = "Rowntree et al. (2014)",
  "Balbao et al. (2018)"    = "Balboa et al. (2018)",
  "Li et al. (2017)"        = "Li et al. (2017)",
  "Koester et al. 2014)"    = "Koester et al. (2014)",
  "Umburanas et al. (2022)" = "Umburanas et al. (2022)",
  "Waqar et al (2025)"      = "Waqar et al. (2025)"
)

# --- Colour palette ----------------------------------------------------------
study_colors <- c(
  "Rowntree et al. (2014)"  = "#1f78b4",
  "Balboa et al. (2018)"    = "#33a02c",
  "Li et al. (2017)"        = "#e31a1c",
  "Koester et al. (2014)"   = "#ff7f00",
  "Umburanas et al. (2022)" = "#6a3d9a",
  "Waqar et al. (2025)"     = "#b15928"
)

# --- Per-study R² annotation -------------------------------------------------
r2_df <- df %>%
  group_by(Study_label) %>%
  summarise(
    r2  = summary(lm(HI ~ YOR))$r.squared,
    x   = min(YOR) + 0.02 * diff(range(YOR)),
    y   = max(HI)  - 0.01 * diff(range(HI)),
    .groups = "drop"
  )

# --- Plot --------------------------------------------------------------------
p <- ggplot(df, aes(x = YOR, y = HI, colour = Study_label)) +

  geom_point(alpha = 0.55, size = 1.8, shape = 21,
             aes(fill = Study_label), colour = "white", stroke = 0.25) +

  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +

  stat_poly_eq(
    aes(label = after_stat(rr.label)),
    formula = y ~ x,
    parse   = TRUE,
    size    = 3.0,
    label.x = "left",
    label.y = "top"
  ) +

  scale_colour_manual(values = study_colors, name = "Study") +
  scale_fill_manual(values   = study_colors, name = "Study") +

  scale_x_continuous(
    name   = "Year of Cultivar Release",
    breaks = seq(1920, 2020, 20),
    expand = expansion(mult = 0.02)
  ) +
  scale_y_continuous(
    name   = "Harvest Index (HI)",
    limits = c(0.15, 0.70),
    breaks = seq(0.15, 0.70, 0.05),
    labels = number_format(accuracy = 0.01),
    expand = c(0, 0)
  ) +

  facet_wrap(~Study_label, ncol = 3, scales = "free_x") +

  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    axis.title       = element_text(size = 10, face = "plain", color = "black"),
    axis.text        = element_text(size = 9,  color = "black"),
    axis.ticks       = element_line(color = "black", linewidth = 0.4),
    axis.line        = element_line(color = "black", linewidth = 0.5),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.5),
    strip.text       = element_text(size = 9, face = "plain"),
    legend.position  = "none",
    plot.margin      = margin(10, 15, 10, 10, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# --- Save --------------------------------------------------------------------
ggsave("figures/fig5_hi_trends.tiff",
       plot = p, width = 9, height = 6, dpi = 400, bg = "white")
ggsave("figures/fig5_hi_trends.pdf",
       plot = p, width = 9, height = 6, device = cairo_pdf)

cat("Saved: figures/fig5_hi_trends.tiff & .pdf\n")

# --- Print per-study regression summary -------------------------------------
cat("\nPer-study linear regression (HI ~ Year of Release):\n")
df %>%
  group_by(Study_label) %>%
  do({
    m   <- lm(HI ~ YOR, data = .)
    s   <- summary(m)
    data.frame(
      n     = nrow(.),
      slope = round(coef(m)[2], 5),
      r2    = round(s$r.squared, 3),
      p     = round(coef(s)[2, 4], 4)
    )
  }) %>%
  print()
