# =============================================================================
# Harvest Index vs. Seed Yield with Biomass Isolines
# Figure 4
# =============================================================================
# Author: Elvis F. Elli
# Data:   data/biomass_yield.csv
# Output: figures/fig4_hi_yield.tiff, .pdf
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
df <- read.csv(here("data", "biomass_yield.csv"))
colnames(df) <- c("Biomass", "Yield")
df$Biomass <- suppressWarnings(as.numeric(df$Biomass))
df$Yield   <- suppressWarnings(as.numeric(df$Yield))
df <- df[complete.cases(df), ]

df$HI <- df$Yield / df$Biomass
df    <- df[df$HI <= 0.70, ]

# --- Biomass isoline parameters ----------------------------------------------
bm_values <- c(2000, 4000, 6000, 8000, 12000, 16000)
bm_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928")

x_max <- 0.70
y_max <- 9000

bm_df <- data.frame(BM = bm_values, Color = bm_colors) |>
  mutate(
    x_end   = ifelse(BM * x_max > y_max, y_max / BM, x_max),
    y_end   = ifelse(BM * x_max > y_max, y_max,       BM * x_max),
    x_lbl_r = x_end * 0.97,
    y_lbl_r = BM * x_lbl_r,
    exits   = y_lbl_r > y_max * 0.96,
    y_lbl   = ifelse(exits, y_max * 0.975, y_lbl_r),
    x_lbl   = ifelse(exits, y_lbl / BM,    x_lbl_r),
    y_lbl   = ifelse(BM == 16000, 8500, y_lbl),
    hjust   = ifelse(exits, 0, 1),
    label   = paste0(BM / 1000, " t ha⁻¹")
  )

# --- Plot --------------------------------------------------------------------
p <- ggplot(df, aes(x = HI, y = Yield)) +

  mapply(function(xs, ys, xe, ye, col) {
    geom_segment(x = xs, y = ys, xend = xe, yend = ye,
                 color = col, linewidth = 0.65,
                 linetype = "longdash", alpha = 0.85,
                 inherit.aes = FALSE)
  }, 0, 0, bm_df$x_end, bm_df$y_end, bm_df$Color,
  SIMPLIFY = FALSE) +

  geom_point(alpha = 0.55, size = 1.8, shape = 21,
             fill = "grey30", color = "white", stroke = 0.25) +

  geom_smooth(method = "lm", se = FALSE,
              color = "black", linewidth = 0.9, linetype = "solid") +

  stat_poly_eq(
    aes(label = paste(after_stat(eq.label),
                      after_stat(rr.label), sep = "~~~")),
    formula = y ~ x,
    parse   = TRUE,
    size    = 3.5,
    label.x = 0.05,
    label.y = 0.97,
    color   = "black"
  ) +

  geom_text(
    data = bm_df,
    aes(x = x_lbl, y = y_lbl, label = label, color = Color, hjust = hjust),
    vjust = -0.3, size = 2.8,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_color_identity() +

  scale_x_continuous(
    name   = "Harvest Index (HI)",
    limits = c(0, x_max),
    breaks = seq(0, x_max, 0.10),
    labels = number_format(accuracy = 0.01),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name   = expression(paste("Seed Yield (kg ha"^{-1}, ")")),
    limits = c(0, y_max),
    breaks = seq(0, y_max, 2000),
    labels = label_comma(),
    expand = c(0, 0)
  ) +

  theme_classic(base_size = 12, base_family = "sans") +
  theme(
    axis.title       = element_text(size = 11, face = "plain", color = "black"),
    axis.text        = element_text(size = 10, color = "black"),
    axis.ticks       = element_line(color = "black", linewidth = 0.4),
    axis.line        = element_line(color = "black", linewidth = 0.5),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin      = margin(10, 15, 10, 10, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# --- Save --------------------------------------------------------------------
ggsave(here("figures", "fig4_hi_yield.tiff"),
       plot = p, width = 6.5, height = 5.5, dpi = 400, bg = "white")
ggsave(here("figures", "fig4_hi_yield.pdf"),
       plot = p, width = 6.5, height = 5.5, device = cairo_pdf)
cat("Saved: figures/fig4_hi_yield.tiff & .pdf\n")
